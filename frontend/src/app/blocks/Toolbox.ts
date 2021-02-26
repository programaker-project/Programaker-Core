import { MatDialog } from '@angular/material/dialog';


import { MonitorService } from '../monitor.service';
import { MonitorMetadata } from '../monitor';

import { CustomSignalController } from './CustomSignalController';
import { CustomSignalService } from '../custom_signals/custom_signal.service';
import { CustomBlockService } from '../custom_block.service';
import { block_to_xml, get_block_category, get_block_toolbox_arguments,
         ScratchBlockArgument, ResolvedCustomBlock, CategorizedCustomBlock, BridgeData } from '../custom_block';

import { ToolboxController } from './ToolboxController';

import { TemplateController, ToolboxRegistrationHook } from './TemplateController';
import { TemplateService } from '../templates/template.service';
import { ServiceService } from '../service.service';
import { AvailableService } from '../service';
import { SessionService } from '../session.service';

import { alreadyRegisteredException, createDom } from './utils';
import { ConnectionService } from '../connection.service';
import { BridgeConnection } from '../connection';
import { iconDataToUrl } from '../utils';
import { ProgramContent } from 'app/program';
import { AssetService } from '../asset.service';


import * as jstz from 'jstz';
import { BridgeIndexData } from 'app/bridges/bridge';
import { AddConnectionDialogComponent } from 'app/connections/add-connection-dialog.component';
import { EditorController } from 'app/program-editors/editor-controller';
import { NgZone } from '@angular/core';
import { EnvironmentService } from 'app/environment.service';

declare const Blockly: any;

const MonitorPrimaryColor = '#CC55CC';
const MonitorSecondaryColor = '#773377';
const CustomPrimaryColor = '#777777';
const CustomSecondaryColor = '#E7E7E7';

// Constant Ids, to reference the time service.
const TimeServiceUuid = "0093325b-373f-4f1c-bace-4532cce79df4";
const UtcTimeOfDayBlockId = `services.${TimeServiceUuid}.utc_is_day_of_week`;

export type ToolboxRegistration = ((workspace: Blockly.WorkspaceSvg,
                                    controller: EditorController,
                                    ngZone: NgZone,
                                   ) => void);

export class Toolbox {
    templateController: TemplateController;
    customSignalController: CustomSignalController;
    controller: ToolboxController;

    constructor(
        private program: ProgramContent,
        private assetService: AssetService,
        private monitorService: MonitorService,
        private customBlockService: CustomBlockService,
        private dialog: MatDialog,
        templateService: TemplateService,
        private serviceService: ServiceService,
        customSignalService: CustomSignalService,
        private connectionService: ConnectionService,
        private sessionService: SessionService,
        private environmentService: EnvironmentService,
        private readOnly: boolean,
        toolboxController?: ToolboxController,
    ) {
        this.controller = toolboxController || new ToolboxController();
        this.templateController = new TemplateController(this.dialog, this.controller, templateService);
        this.customSignalController = new CustomSignalController(this.dialog, this.controller, customSignalService);
    }

    async inject(): Promise<[HTMLElement, ToolboxRegistration[], ToolboxController]> {
        let registrations: ToolboxRegistration[] = [];
        let toolboxXML: HTMLElement;

        if (!this.readOnly) {
            const availableConnectionsQuery = this.connectionService.getAvailableBridgesForNewConnectionOnProgram(this.program.id);

            const [monitors, custom_blocks, services, connections] = await Promise.all([
                this.monitorService.getMonitorsOnProgram(this.program.id),
                this.customBlockService.getCustomBlocksOnProgram(this.program.id, false),
                this.serviceService.getAvailableServicesOnProgram(this.program.id),
                this.connectionService.getConnectionsOnProgram(this.program.id),
            ]) as [MonitorMetadata[], ResolvedCustomBlock[], AvailableService[], BridgeConnection[]];

            this.controller.addCustomBlocks(custom_blocks);
            const categorized_blocks = this.categorize_blocks(custom_blocks,services);

            let availableConnections: BridgeIndexData[];
            try {
                availableConnections = await availableConnectionsQuery;
            }
            catch (error) {
                if ((error.name === 'HttpErrorResponse') && (error.status === 401)) {
                    // User cannot add connections, so skip adding them
                    availableConnections = [];
                }
                else {
                    throw error;
                }
            }


            registrations = registrations.concat(await this.injectBlocks(monitors, categorized_blocks, services, connections));
            toolboxXML = await this.injectToolbox(monitors, categorized_blocks);

            registrations = registrations.concat(this.addAvailableConnections(availableConnections, toolboxXML));
        }
        else {
            toolboxXML = await this.injectToolbox([], []);
        }

        this.controller.setToolbox(toolboxXML);

        return [toolboxXML, registrations, this.controller];
    }

    addAvailableConnections(availableBridges: BridgeIndexData[], toolboxXML: HTMLElement): ToolboxRegistrationHook[] {
        const hooks: ToolboxRegistrationHook[] = [];
        for (const bridge of availableBridges) {
            const category = createDom('category', {
                name: bridge.name,
                colour: Toolbox.get_bridge_color(bridge.id),
                secondaryColour: Toolbox.get_bridge_secondary_color(bridge.id),
                id: bridge.id,
            })

            const callbackKey = "AUTOMATE_CONNECT_" + bridge.id;
            category.appendChild(createDom('button', {
                text: "Connect to " + bridge.name,
                callbackKey: callbackKey,
            }));

            toolboxXML.appendChild(category);

            hooks.push((workspace: Blockly.WorkspaceSvg, editorController: EditorController, ngZone: NgZone) => {

                workspace.registerButtonCallback(callbackKey, (_x: Blockly.FlyoutButton) => {
                    ngZone.run(() => {
                        const dialogRef = this.dialog.open(AddConnectionDialogComponent, {
                            disableClose: false,
                            data: {
                                programId: this.program.id,
                                bridgeInfo: bridge,
                            }
                        });

                        dialogRef.afterClosed().subscribe(async (result) => {
                            if (!result) {
                                console.log("Cancelled");
                                return;
                            }

                            editorController.reloadToolbox();
                        });
                    });
                });
            });
        }

        return hooks;
    }

    categorize_blocks(custom_blocks: ResolvedCustomBlock[], services: AvailableService[]): CategorizedCustomBlock[]{
        const categorized_blocks: CategorizedCustomBlock[] = [];
        for (const service of services) {
            const category_blocks: ResolvedCustomBlock[] = [];
            for (const block of custom_blocks) {
                if (service.id === block.service_port_id) {
                    category_blocks.push(block);
                }
            }
            if(category_blocks.length > 0) {
                const bridge_data: BridgeData = {
                    bridge_name: service.name,
                    bridge_id: service.id,
                };

                categorized_blocks.push({
                    bridge_data: bridge_data,
                    resolved_custom_blocks: category_blocks,
                });
            }
        }

        return categorized_blocks;
    }

    async injectBlocks(monitors: MonitorMetadata[],
                 custom_blocks: CategorizedCustomBlock[],
                 services: AvailableService[],
                 connections: BridgeConnection[],
                ): Promise<ToolboxRegistration[]> {
        let registrations: ToolboxRegistrationHook[] = [];

        this.injectMonitorBlocks(monitors);
        await this.injectTimeBlocks();
        this.injectJSONBlocks();
        this.injectOperationBlocks(connections);
        registrations = registrations.concat(this.templateController.injectBlocks());
        registrations = registrations.concat(this.customSignalController.injectBlocks());
        this.injectCustomBlocks(custom_blocks, connections);
        this.patchBlocks();

        return registrations;
    }

    patchBlocks() {
        // Original: https://github.com/LLK/scratch-blocks/blob/d9e7bb7c497d23f55cc06103837ad448d786714b/blocks_vertical/data.js#L80
        Blockly.Blocks['data_changevariableby'] = {
            /**
             * Block to change variable by a certain value
             * @this Blockly.Block
             */
            init: function() {
                this.jsonInit({
                    "message0": "increment %1 by %2",
                    "args0": [
                        {
                            "type": "field_variable",
                            "name": "VARIABLE"
                        },
                        {
                            "type": "input_value",
                            "name": "VALUE"
                        }
                    ],
                    "category": Blockly.Categories.data,
                    "extensions": ["colours_data", "shape_statement"]
                });
            }
        };
    }

    injectTimeBlocks(): Promise<any> {
        Blockly.Blocks['time_trigger_at'] = {
            init: function () {
                this.jsonInit({
                    'id': 'time_trigger_at',
                    'message0': 'Every day at %1:%2:%3 (UTC)',
                    'args0': [
                        {
                            'type': 'input_value',
                            'name': 'HOUR'
                        },
                        {
                            'type': 'input_value',
                            'name': 'MINUTE'
                        },
                        {
                            'type': 'input_value',
                            'name': 'SECOND'
                        },
                    ],
                    'category': Blockly.Categories.event,
                    'extensions': ['colours_time', 'shape_hat']
                });
            }
        };

        Blockly.Blocks['time_get_utc_hour'] = {
            init: function () {
                this.jsonInit({
                    'id': 'time_get_utc_hour',
                    'message0': 'UTC hour',
                    'args0': [
                    ],
                    'category': Blockly.Categories.event,
                    'extensions': ['colours_time', 'output_string']
                });
            }
        };

        Blockly.Blocks['time_get_utc_minute'] = {
            init: function () {
                this.jsonInit({
                    'id': 'time_get_utc_minute',
                    'message0': 'UTC minute',
                    'args0': [
                    ],
                    'category': Blockly.Categories.event,
                    'extensions': ['colours_time', 'output_string']
                });
            }
        };

        Blockly.Blocks['time_get_utc_seconds'] = {
            init: function () {
                this.jsonInit({
                    'id': 'time_get_utc_seconds',
                    'message0': 'UTC seconds',
                    'args0': [
                    ],
                    'category': Blockly.Categories.event,
                    'extensions': ['colours_time', 'output_string']
                });
            }
        };


        const timeReady = this.assetService.getTimezoneData().then((tz) => {

            const detectedTz = jstz.determine().name();
            console.log("Autodetected Timezone:", detectedTz);

            const tzData = (tz
                .filter(v => v.status === 'Canonical' || v.status === 'Alias')
                .sort((a, b) => {
                    if (a.tz > b.tz) {
                        return 1;
                    }
                    if (a.tz < b.tz) {
                        return -1;
                    }
                    return 0;
                })
                .map((v) => {
                    return [v.tz, v.tz]
                }));

            // Add the detected timezone at the top
            tzData.unshift([detectedTz, detectedTz]);

            const tzArg = {
                'type': 'field_dropdown',
                'name': 'TIMEZONE4',
                'options': tzData
            };


            Blockly.Blocks['time_trigger_at_tz'] = {
                init: function () {
                    this.jsonInit({
                        'id': 'time_trigger_at',
                        'message0': 'Every day at %1:%2:%3 in %4',
                        'args0': [
                            {
                                'type': 'input_value',
                                'name': 'HOUR1'
                            },
                            {
                                'type': 'input_value',
                                'name': 'MINUTE2'
                            },
                            {
                                'type': 'input_value',
                                'name': 'SECOND3'
                            },
                            tzArg,
                        ],
                        'category': Blockly.Categories.event,
                        'extensions': ['colours_time', 'shape_hat']
                    });
                }
            };

            Blockly.Blocks['time_get_tz_hour'] = {
                init: function () {
                    this.jsonInit({
                        'id': 'time_get_tz_hour',
                        'message0': 'Hour at %1',
                        'args0': [
                            tzArg,
                        ],
                        'category': Blockly.Categories.event,
                        'extensions': ['colours_time', 'output_number']
                    });
                }
            };

            Blockly.Blocks['time_get_tz_minute'] = {
                init: function () {
                    this.jsonInit({
                        'id': 'time_get_tz_minute',
                        'message0': 'Minutes at %1',
                        'args0': [
                            tzArg,
                        ],
                        'category': Blockly.Categories.event,
                        'extensions': ['colours_time', 'output_number']
                    });
                }
            };

            Blockly.Blocks['time_get_tz_seconds'] = {
                init: function () {
                    this.jsonInit({
                        'id': 'time_get_tz_seconds',
                        'message0': 'Seconds at %1',
                        'args0': [
                            tzArg,
                        ],
                        'category': Blockly.Categories.event,
                        'extensions': ['colours_time', 'output_number']
                    });
                }
            };

            Blockly.Blocks['time_get_tz_day_of_month'] = {
                init: function () {
                    this.jsonInit({
                        'id': 'time_get_tz_day_of_month',
                        'message0': 'Day of the month at %1',
                        'args0': [
                            tzArg,
                        ],
                        'category': Blockly.Categories.event,
                        'extensions': ['colours_time', 'output_number']
                    });
                }
            };

            Blockly.Blocks['time_get_tz_month_of_year'] = {
                init: function () {
                    this.jsonInit({
                        'id': 'time_get_tz_month_of_hear',
                        'message0': 'Month of the year at %1 (January=1, Februrary=2, ...)',
                        'args0': [
                            tzArg,
                        ],
                        'category': Blockly.Categories.event,
                        'extensions': ['colours_time', 'output_number']
                    });
                }
            };

            Blockly.Blocks['time_get_tz_year'] = {
                init: function () {
                    this.jsonInit({
                        'id': 'time_get_tz_year',
                        'message0': 'Current year (at %1)',
                        'args0': [
                            tzArg,
                        ],
                        'category': Blockly.Categories.event,
                        'extensions': ['colours_time', 'output_number']
                    });
                }
            };

            Blockly.Blocks['time_get_tz_day_of_week'] = {
                init: function () {
                    this.jsonInit({
                        'id': 'time_get_tz_day_of_week',
                        'message0': 'Numeric day of week at %1 (Monday=1, Tuesday=2, ...)',
                        'args0': [
                            tzArg,
                        ],
                        'category': Blockly.Categories.event,
                        'extensions': ['colours_time', 'output_number']
                    });
                }
            };

        });

        Blockly.Blocks[UtcTimeOfDayBlockId] = {
            init: function () {
                this.jsonInit({
                    'id': UtcTimeOfDayBlockId,
                    'message0': 'Is current day of week %1',
                    'args0': [
                        {
                            'type': 'field_dropdown',
                            'name': 'DAY_OF_WEEK',
                            'options': [
                                ["Monday", "mon"],
                                ["Tuesday", "tue"],
                                ["Wednesday", "wed"],
                                ["Thursday", "thu"],
                                ["Friday", "fri"],
                                ["Saturday", "sat"],
                                ["Sunday", "sun"],
                            ]
                        },
                    ],
                    'category': Blockly.Categories.event,
                    'extensions': ['colours_time', 'output_boolean']
                });
            }
        };

        try {
            Blockly.Extensions.register('colours_time',  // Name of the new category
                function () {
                    this.setColourFromRawValues_('#85CCB3',  // Block inner color
                        '#1D1D5F',  // Category circle border color
                        '#1D1D5F'   // Block border color
                    );
                });
        } catch (e) {
            // If the extension was registered before
            // this would have thrown an inocous exception
            if (!alreadyRegisteredException(e)) {
                throw e;
            }
        }

        return timeReady;
    }

    injectJSONBlocks() {
        Blockly.Blocks['operator_json_parser'] = {
            init: function () {
                this.jsonInit({
                    'id': 'operator_json_parser',
                    'message0': 'Get key %1 of variable %2',
                    'args0': [
                        {
                            'type': 'input_value',
                            'name': 'KEY1'
                        },
                        {
                            'type': 'field_variable',
                            'name': 'VARIABLE2'
                        },
                    ],
                    'category': Blockly.Categories.event,
                    'extensions': ['colours_operators', 'output_string']
                });
            }
        };
    }

    injectOperationBlocks(connections: BridgeConnection[]) {
        const bridge_to_connection_map: { [ key: string ]: BridgeConnection[] } = {};
        const bridge_dropdown: [ string, string ][] = [];
        const full_connection_dropdown: [ string, string ][] = [];

        for (const conn of connections) {
            if (bridge_to_connection_map[conn.bridge_id] === undefined) {
                bridge_to_connection_map[conn.bridge_id] = [];
                bridge_dropdown.push([conn.bridge_name, conn.bridge_id]);
            }
            bridge_to_connection_map[conn.bridge_id].push(conn);

            full_connection_dropdown.push([conn.name || 'default', conn.connection_id]);
        }

        if (bridge_dropdown.length > 0) {
            Blockly.Blocks['trigger_on_bridge_connected'] = {
                init: function () {
                    this.jsonInit({
                        'id': 'trigger_on_bridge_connected',
                        'message0': 'When bridge %1 connects',
                        'args0': [
                            {
                                'type': 'field_dropdown',
                                'name': 'BRIDGE1',
                                options: bridge_dropdown,
                            },
                        ],
                        'category': Blockly.Categories.event,
                        'extensions': ['colours_advanced_operation', 'shape_hat']
                    });
                }
            };

            Blockly.Blocks['trigger_on_bridge_disconnected'] = {
                init: function () {
                    this.jsonInit({
                        'id': 'trigger_on_bridge_disconnected',
                        'message0': 'When bridge %1 connection STOPS',
                        'args0': [
                            {
                                'type': 'field_dropdown',
                                'name': 'BRIDGE1',
                                options: bridge_dropdown,
                            },
                        ],
                        'category': Blockly.Categories.event,
                        'extensions': ['colours_advanced_operation', 'shape_hat']
                    });
                }
            };
        }

        Blockly.Blocks['operator_select_connection'] = {
            init: function () {

                let currentBridgeSelected: string = null;
                let connectionDropDown = bridge_dropdown;
                if (connectionDropDown.length == 0) {
                    connectionDropDown = [[ 'No bridges found', '__not_found_error__' ]];
                }

                this.jsonInit({
                    'id': 'operator_select_connection',
                    'message0': 'On bridge %1 use connection %2',
                    'args0': [
                        {
                            'type': 'field_dropdown',
                            'name': 'BRIDGE1',
                            'options': connectionDropDown,
                        },
                        {
                            'type': 'field_dropdown',
                            'name': 'CONNECTION2',
                            'options': () => {
                                if (!currentBridgeSelected) {
                                    if (full_connection_dropdown.length > 0) {
                                        return full_connection_dropdown;
                                    }
                                    else {
                                        return [ [ "Bridge not found error", '__bridge_not_found_error' ] ];
                                    }
                                }
                                try {
                                    const options = [];

                                    let bridge = currentBridgeSelected;
                                    if (!currentBridgeSelected) {
                                        bridge = bridge_dropdown[0][0];
                                    }

                                    for (const conn of bridge_to_connection_map[bridge]) {
                                        options.push([conn.name || "default", conn.connection_id]);
                                    }

                                    return options;
                                }
                                catch (err) {
                                    console.error(err);
                                    return [ [ "Bridge not found error", '__bridge_not_found_error' ] ];
                                }
                            },
                        },
                    ],
                    message1: "%1",
                    args1: [
                        {
                            'type': 'input_statement',
                            'name': 'SUBSTACK3'
                        },
                    ],
                    lastDummyAlign2: "RIGHT",
                    message2: "when done, restore the previous connection",
                    args2: [],
                    'category': Blockly.Categories.control,
                    'previousStatement': null,
                    'nextStatement': null,
                    'extensions': ['colours_advanced_operation', 'shape_statement'],
                });

                const inputs = this.inputList[0].fieldRow;

                let bridgeDropdown: { onHide: () => void; value_: string; } = null;
                let connectionDropdown: { value_: any; getOptions: () => any; setValue: (arg0: any) => void; } = null;

                for (const input of inputs) {
                    if (input.name === 'BRIDGE1') {
                        bridgeDropdown = input;
                    }
                    else if (input.name === 'CONNECTION2') {
                        connectionDropdown = input;
                    }
                }

                if (!bridgeDropdown) {
                    return;
                }

                // Capture the BridgeDropdown selector.
                const hideBridgeDropdown = bridgeDropdown.onHide;
                bridgeDropdown.onHide = () => {
                    hideBridgeDropdown.bind(bridgeDropdown)();
                    currentBridgeSelected = bridgeDropdown.value_;

                    // Refresh connection dropdown
                    const oldValue = connectionDropdown.value_;
                    const newOptions = connectionDropdown.getOptions();

                    // If oldValue in newOptions, just pick the first
                    let found = false;
                    for (const option of newOptions) {
                        if (option[1] === oldValue) {
                            found = true;
                        }
                    }

                    if ((!found) && (newOptions.length > 0)) {
                        connectionDropdown.setValue(newOptions[0][1]);
                    }
                }

                // HACK: Extract the current bridge selected *after* the initialization
                //       "thread" is completed. Otherwise doing this here would mean that
                //       the value would not be correct when blocks are loaded from XML.
                setTimeout(() => {
                    currentBridgeSelected = bridgeDropdown.value_;
                }, 0);
            }
        };

        try {
            Blockly.Extensions.register('colours_advanced_operation',
                                        function () {
                                            this.setColourFromRawValues_("#009688",
                                                                         "#007668",
                                                                         "#007668");
                                        });
        } catch (e) {
            // If the extension was registered before
            // this would have thrown an inocous exception
            if (!alreadyRegisteredException(e)) {
                throw e;
            }
        }

    }

    genOperationCategory(): HTMLElement {

        const category = createDom('category', {
            name: 'Advanced operation',
            colour: "#009688",
            secondaryColour: "#007668",
            id: "operation",
        });

        category.appendChild(createDom('block',
                                       {
            type: "trigger_on_bridge_disconnected",
            id: "trigger_on_bridge_disconnected",
        }));

        category.appendChild(createDom('block',
                                       {
            type: "trigger_on_bridge_connected",
            id: "trigger_on_bridge_connected",
        }));

        category.appendChild(createDom('block',
                                       {
            type: "operator_select_connection",
            id: "operator_select_connection",
        }));

        return category;
    }

    injectMonitorBlocks(monitors: MonitorMetadata[]) {
        for (const monitor of monitors) {
            Blockly.Blocks['monitor.retrieve.' + monitor.id] = {
                init: function () {
                    this.jsonInit({
                        'id': 'monitor.retrieve.' + monitor.id,
                        'message0': 'Get ' + monitor.name,
                        'category': Blockly.Categories.event,
                        'extensions': ['colours_monitor', 'output_string']
                    });
                }
            };
        }

        if (monitors.length > 0) {
            try {
                Blockly.Extensions.register('colours_monitor',
                    function () {
                        this.setColourFromRawValues_(MonitorPrimaryColor,
                            MonitorSecondaryColor,
                            '#FF00FF');
                    });
            } catch (e) {
                // If the extension was registered before
                // this would have thrown an inocous exception
                if (!alreadyRegisteredException(e)) {
                    throw e;
                }
            }
        }
    }

    static get_bridge_color(bridge_id: string): string {
        return '#' + bridge_id.replace(/\D/g,'').substring(0,6);
    }

    static get_bridge_secondary_color(bridge_id: string): string {
        const color = Toolbox.get_bridge_color(bridge_id);
        const r = parseInt(color.substring(0, 2), 16);
        const g = parseInt(color.substring(2, 4), 16);
        const b = parseInt(color.substring(4, 6), 16);

        const sec_r = Math.max(r - 0x30, 0x00);
        const sec_g = Math.max(g - 0x30, 0x00);
        const sec_b = Math.max(b - 0x30, 0x00);

        const sec_color = (('00' + sec_r.toString(16)).substr(-2)
                           + ('00' + sec_g.toString(16)).substr(-2)
                           + ('00' + sec_b.toString(16)).substr(-2));

        return '#' + sec_color;
    }

    private static getConnectionForCategory(block_cat: CategorizedCustomBlock, connections: BridgeConnection[]): BridgeConnection {
        for (const conn of connections) {
            if (block_cat.bridge_data.bridge_id === conn.bridge_id) {
                return conn;
            }
        }

        return null;
    }

    private getIconForBlocks(block_cat: CategorizedCustomBlock, connections: BridgeConnection[]): string{
        const conn = Toolbox.getConnectionForCategory(block_cat, connections);
        if (!conn) {
            return null;
        }

        return iconDataToUrl(this.environmentService, conn.icon, block_cat.bridge_data.bridge_id);
    }

    injectCustomBlocks(categorized_custom_blocks: CategorizedCustomBlock[], connections: BridgeConnection[]) {
      for (const blocks of categorized_custom_blocks){
        for (const block of blocks.resolved_custom_blocks) {
          const toolbox = this;
          Blockly.Blocks[block.id] = {
              init: function () {
                  const block_args = get_block_toolbox_arguments(block);
                  const icon = toolbox.getIconForBlocks(blocks, connections);
                  let msg_prefix = `%${block_args.length + 1} `;
                  let label_arg: ScratchBlockArgument = {
                      type: 'field_label',
                      // The space after the label is duplicated (with the msg_prefix) for more clear separation
                      text: `[${blocks.bridge_data.bridge_name}] `,
                  };

                  if (icon) {
                      label_arg = {
                          type: 'field_image',
                          src: icon,
                          width: 48,
                          height: 24,
                          alt: blocks.bridge_data.bridge_name,
                          flip_rtl: false,
                      };
                  }
                  this.jsonInit({
                      'id': block.id,
                      'message0': msg_prefix + block.message,
                      'category': Blockly.Categories.event,
                      'extensions': ['colours_bridge_' + blocks.bridge_data.bridge_id,
                                     get_block_category(block)],
                      'args0': block_args.concat([ label_arg ])
                  });
              }
          };
        }

          if (blocks.resolved_custom_blocks.length > 0) {
            try {
                Blockly.Extensions.register('colours_bridge_' + blocks.bridge_data.bridge_id,
                    function () {
                        this.setColourFromRawValues_(
                            Toolbox.get_bridge_color(blocks.bridge_data.bridge_id),
                            CustomSecondaryColor,
                            '#222222');
                    });
            } catch (e) {
                // If the extension was registered before
                // this would have thrown an inocous exception
                if (!alreadyRegisteredException(e)) {
                    throw e;
                }
            }
          }

      }
    }

    gen_toolbox_xml_from_blocks(categorized_custom_blocks: CategorizedCustomBlock[]) {
        const categories = [];
        for (const custom_blocks of categorized_custom_blocks){
            if (custom_blocks.resolved_custom_blocks.length == 0) {
                continue;
            }

            const custom_blocks_xml = custom_blocks.resolved_custom_blocks.map(
                (block, _index, _array) => {
                    return block_to_xml(block);
                });

            const primary_color = Toolbox.get_bridge_color(custom_blocks.bridge_data.bridge_id);
            const secondary_color = Toolbox.get_bridge_secondary_color(
                custom_blocks.bridge_data.bridge_id);
            categories.push(`<category name="${custom_blocks.bridge_data.bridge_name}"
                                       id="${custom_blocks.bridge_data.bridge_name}"
                                       colour="${primary_color}"
                                       secondaryColour="${secondary_color}">
                                 ${custom_blocks_xml.join('\n')}
                             </category>`);
        }
        return categories.join('');
    }

    async injectToolbox(monitors: MonitorMetadata[], categorized_custom_blocks: CategorizedCustomBlock[]): Promise<HTMLElement> {
        (goog as any).provide('Blockly.Blocks.defaultToolbox');

        (goog as any).require('Blockly.Blocks');

        Blockly.Blocks.factoryDefaultToolbox = Blockly.Blocks.defaultToolbox;

        const eventsCategory = ''; //   '<category name="Events" id="events" colour="#FFD500" secondaryColour="#CC9900">' +
        //   '<block type="event_broadcast" id="event_broadcast">' +
        //     '<value name="BROADCAST_INPUT">' +
        //       '<shadow type="event_broadcast_menu"></shadow>' +
        //     '</value>' +
        //   '</block>' +
        //   '<block type="event_broadcastandwait" id="event_broadcastandwait">' +
        //     '<value name="BROADCAST_INPUT">' +
        //       '<shadow type="event_broadcast_menu"></shadow>' +
        //     '</value>' +
        //   '</block>' +
        // '</category>'

        const controlCategory = `
        <category name="Control" id="control" colour="#FFAB19" secondaryColour="#CF8B17">
          <block type="control_wait" id="control_wait">
            <value name="DURATION">
              <shadow type="math_positive_number">
                <field name="NUM">1</field>
              </shadow>
            </value>
          </block>
          <block type="control_repeat" id="control_repeat">
            <value name="TIMES">
              <shadow type="math_whole_number">
                <field name="NUM">10</field>
              </shadow>
            </value>
          </block>
          <!--
          <block type="control_forever" id="control_forever"></block>
          -->
          <block type="control_if" id="control_if"></block>
          <block type="control_if_else" id="control_if_else"></block>
          <block type="control_wait_until" id="control_wait_until"></block>
          <block type="control_repeat_until" id="control_repeat_until"></block>
          <!--
          <block type="control_stop" id="control_stop"></block>
          <block type="control_start_as_clone" id="control_start_as_clone"></block>
          <block type="control_create_clone_of" id="control_create_clone_of">
            <value name="CLONE_OPTION">
              <shadow type="control_create_clone_of_menu"></shadow>
            </value>
          </block>
          <block type="control_delete_this_clone" id="control_delete_this_clone"></block>
          -->
        </category>`;

        const operatorsCategory = `
        <category name="Operators" id="operators" colour="#40BF4A" secondaryColour="#389438">
            <block type="operator_add" id="operator_add">
              <value name="NUM1">
                <shadow type="math_number">
                  <field name="NUM"></field>
                </shadow>
              </value>
              <value name="NUM2">
                <shadow type="math_number">
                  <field name="NUM"></field>
                </shadow>
              </value>
            </block>
            <block type="operator_subtract" id="operator_subtract">
              <value name="NUM1">
                <shadow type="math_number">
                  <field name="NUM"></field>
                </shadow>
              </value>
              <value name="NUM2">
                <shadow type="math_number">
                  <field name="NUM"></field>
                </shadow>
              </value>
            </block>
            <block type="operator_multiply" id="operator_multiply">
              <value name="NUM1">
                <shadow type="math_number">
                  <field name="NUM"></field>
                </shadow>
              </value>
              <value name="NUM2">
                <shadow type="math_number">
                  <field name="NUM"></field>
                </shadow>
              </value>
            </block>
            <block type="operator_divide" id="operator_divide">
              <value name="NUM1">
                <shadow type="math_number">
                  <field name="NUM"></field>
                </shadow>
              </value>
              <value name="NUM2">
                <shadow type="math_number">
                  <field name="NUM"></field>
                </shadow>
              </value>
            </block>
            <!--
            <block type="operator_random" id="operator_random">
              <value name="FROM">
                <shadow type="math_number">
                  <field name="NUM">1</field>
                </shadow>
              </value>
              <value name="TO">
                <shadow type="math_number">
                  <field name="NUM">10</field>
                </shadow>
              </value>
            </block>
            -->
             <block type="operator_lt" id="operator_lt">
               <value name="OPERAND1">
                 <shadow type="text">
                   <field name="TEXT"></field>
                 </shadow>
               </value>
               <value name="OPERAND2">
                 <shadow type="text">
                   <field name="TEXT"></field>
                 </shadow>
               </value>
             </block>
            <block type="operator_equals" id="operator_equals">
              <value name="OPERAND1">
                <shadow type="text">
                  <field name="TEXT"></field>
                </shadow>
              </value>
              <value name="OPERAND2">
                <shadow type="text">
                  <field name="TEXT"></field>
                </shadow>
              </value>
            </block>
            <block type="operator_gt" id="operator_gt">
              <value name="OPERAND1">
                <shadow type="text">
                  <field name="TEXT"></field>
                </shadow>
              </value>
              <value name="OPERAND2">
                <shadow type="text">
                  <field name="TEXT"></field>
                </shadow>
              </value>
            </block>
            <block type="operator_and" id="operator_and"></block>
            <block type="operator_or" id="operator_or"></block>
            <block type="operator_not" id="operator_not"></block>
            <block type="operator_join" id="operator_join">
              <value name="STRING1">
                <shadow type="text">
                  <field name="TEXT">hello</field>
                </shadow>
              </value>
              <value name="STRING2">
                <shadow type="text">
                  <field name="TEXT">world</field>
                </shadow>
              </value>
            </block>
            <block type="operator_json_parser" id="operator_json_parser">
            <value name="KEY1">
              <shadow type="text">
                <field name="TEXT">key</field>
              </shadow>
            </value>
            <value name="VARIABLE2">
              <shadow type="text">
                <field name="TEXT">json</field>
              </shadow>
            </value>
            </block>
            <!--
            <block type="operator_letter_of" id="operator_letter_of">
              <value name="LETTER">
                <shadow type="math_whole_number">
                  <field name="NUM">1</field>
                </shadow>
              </value>
              <value name="STRING">
                <shadow type="text">
                  <field name="TEXT">world</field>
                </shadow>
              </value>
            </block>
            <block type="operator_length" id="operator_length">
              <value name="STRING">
                <shadow type="text">
                  <field name="TEXT">world</field>
                </shadow>
              </value>
            </block>
            -->
            <block type="operator_contains" id="operator_contains">
              <value name="STRING1">
                <shadow type="text">
                  <field name="TEXT">hello</field>
                </shadow>
              </value>
              <value name="STRING2">
                <shadow type="text">
                  <field name="TEXT">world</field>
                </shadow>
              </value>
            </block>
            <!--
            <block type="operator_mod" id="operator_mod">
              <value name="NUM1">
                <shadow type="math_number">
                  <field name="NUM"></field>
                </shadow>
              </value>
              <value name="NUM2">
                <shadow type="math_number">
                  <field name="NUM"></field>
                </shadow>
              </value>
            </block>
            <block type="operator_round" id="operator_round">
              <value name="NUM">
                <shadow type="math_number">
                  <field name="NUM"></field>
                </shadow>
              </value>
            </block>
            <block type="operator_mathop" id="operator_mathop">
              <value name="NUM">
                <shadow type="math_number">
                  <field name="NUM"></field>
                </shadow>
              </value>
            </block>
          -->
          </category>`;

        const variablesCategory = createDom('category', {
            name: "Variables",
            colour: "#FF8C1A",
            secondaryColour: "#DB6E00",
            custom: "VARIABLE",
            id: "variables",
        });

        const proceduresCategory = createDom('category', {
            name: 'More',
            colour: "#FF6680",
            secondaryColour: "#FF4D6A",
            custom: "PROCEDURE",
            id: "more",
        });

        const monitorsCategory = this.buildMonitorsCategory(monitors);

        const timeCategory = `
        <category name="Time" id="time" colour="#85CCB3" secondaryColour="#1D1D5F">
          <block type="time_trigger_at_tz" id="time_trigger_at_tz">
            <value name="HOUR1">
              <shadow type="math_positive_number">
                <field name="NUM">19</field>
              </shadow>
            </value>
            <value name="MINUTE2">
              <shadow type="math_positive_number">
                <field name="NUM">10</field>
              </shadow>
            </value>
            <value name="SECOND3">
              <shadow type="math_positive_number">
                <field name="NUM">00</field>
              </shadow>
            </value>
            <value name="TIMEZONE4">
              <shadow type="field_dropdown"></shadow>
            </value>
          </block>
          <block type="time_get_tz_hour" id="time_get_utc_hour">
            <value name="TIMEZONE">
              <shadow type="field_dropdown"></shadow>
            </value>
          </block>
          <block type="time_get_tz_minute" id="time_get_utc_minute">
            <value name="TIMEZONE">
              <shadow type="field_dropdown"></shadow>
            </value>
          </block>
          <block type="time_get_tz_seconds" id="time_get_utc_seconds">
            <value name="TIMEZONE">
              <shadow type="field_dropdown"></shadow>
            </value>
          </block>

          <block type="time_get_tz_day_of_month" id="time_get_tz_day_of_month">
            <value name="TIMEZONE">
              <shadow type="field_dropdown"></shadow>
            </value>
          </block>
          <block type="time_get_tz_month_of_year" id="time_get_tz_month_of_year">
            <value name="TIMEZONE">
              <shadow type="field_dropdown"></shadow>
            </value>
          </block>
          <block type="time_get_tz_year" id="time_get_tz_year">
            <value name="TIMEZONE">
              <shadow type="field_dropdown"></shadow>
            </value>
          </block>
          <block type="time_get_tz_day_of_week" id="time_get_tz_day_of_week">
            <value name="TIMEZONE">
              <shadow type="field_dropdown"></shadow>
            </value>
          </block>

        </category>
        `;

        const customCategory = this.gen_toolbox_xml_from_blocks(categorized_custom_blocks);

        const toolboxXML = createDom('xml', {
            id: "toolbox-categories",
            style: "display: none",
        });

        toolboxXML.innerHTML = [
            eventsCategory,
            controlCategory,
            monitorsCategory,
            timeCategory,
            customCategory,
            operatorsCategory,
        ].join('\n');

        toolboxXML.appendChild(variablesCategory);
        // toolboxXML.appendChild(proceduresCategory);

        if ((await this.sessionService.getSession()).tags.is_advanced) {
            toolboxXML.appendChild(await this.templateController.genCategory());
            toolboxXML.appendChild(await this.customSignalController.genCategory());
            toolboxXML.appendChild(this.genOperationCategory());
        }

        Blockly.Blocks.defaultToolbox = toolboxXML;

        return toolboxXML;
    }

    buildMonitorsCategory(monitors: MonitorMetadata[]): string {
        if (monitors.length === 0) {
            return '';
        }

        const category = [
            `<category name="Monitors"
                       id="monitors"
                       colour="${MonitorPrimaryColor}"
                       secondaryColour="${MonitorSecondaryColor}">`
        ];

        for (const monitor of monitors) {
            const block = `<block type="monitor.retrieve.${monitor.id}"
                                  id="monitor.retrieve.${monitor.id}">
                           </block>`;

            category.push(block);
        }

        category.push('</category>');
        return category.join('\n');
    }
}
