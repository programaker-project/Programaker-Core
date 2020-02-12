import { MatDialog } from '@angular/material/dialog';


import { MonitorService } from '../monitor.service';
import { MonitorMetadata } from '../monitor';

import { CustomSignalController } from './CustomSignalController';
import { CustomSignalService } from '../custom_signals/custom_signal.service';
import { CustomBlockService } from '../custom_block.service';
import { CustomBlock, block_to_xml, get_block_category, get_block_toolbox_arguments, ResolvedCustomBlock, CategorizedCustomBlock, BridgeData } from '../custom_block';

import { ToolboxController } from './ToolboxController';

import { TemplateController } from './TemplateController';
import { TemplateService } from '../templates/template.service';
import { ServiceService } from '../service.service';
import { AvailableService } from '../service';

import { alreadyRegisteredException, createDom } from './utils';

declare const Blockly;

const MonitorPrimaryColor = '#CC55CC';
const MonitorSecondaryColor = '#773377';
const CustomPrimaryColor = '#777777';
const CustomSecondaryColor = '#E7E7E7';

// Constant Ids, to reference the time service.
const TimeServiceUuid = "0093325b-373f-4f1c-bace-4532cce79df4";
const UtcTimeOfDayBlockId = `services.${TimeServiceUuid}.utc_is_day_of_week`;

export class Toolbox {
    monitorService: MonitorService;
    customBlockService: CustomBlockService;
    dialog: MatDialog;
    templateController: TemplateController;
    customSignalController: CustomSignalController;
    controller: ToolboxController;
    serviceService: ServiceService;

    constructor(
        monitorService: MonitorService,
        customBlockService: CustomBlockService,
        dialog: MatDialog,
        templateService: TemplateService,
        serviceService: ServiceService,
        customSignalService: CustomSignalService,
    ) {
        this.monitorService = monitorService;
        this.customBlockService = customBlockService;
        this.dialog = dialog;
        this.serviceService = serviceService;

        this.controller = new ToolboxController();
        this.templateController = new TemplateController(this.dialog, this.controller, templateService);
        this.customSignalController = new CustomSignalController(this.dialog, this.controller, customSignalService);
    }

    async inject(): Promise<[HTMLElement, Function[], ToolboxController]> {
        const monitors = await this.monitorService.getMonitors();
        const custom_blocks = await this.customBlockService.getCustomBlocks();
        const services = await this.serviceService.getAvailableServices();
        this.controller.addCustomBlocks(custom_blocks);

        const categorized_blocks =  this.categorize_blocks(custom_blocks,services);

        const registrations = this.injectBlocks(monitors, categorized_blocks, services);
        const toolboxXML = await this.injectToolbox(monitors, categorized_blocks);
        this.controller.setToolbox(toolboxXML);

        return [toolboxXML, registrations, this.controller];
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

    injectBlocks(monitors: MonitorMetadata[], custom_blocks: CategorizedCustomBlock[], services: AvailableService[]): Function[] {
        let registrations = [];

        this.injectMonitorBlocks(monitors);
        this.injectTimeBlocks();
        this.injectJSONBlocks();
        registrations = registrations.concat(this.templateController.injectBlocks());
        registrations = registrations.concat(this.customSignalController.injectBlocks());
        this.injectCustomBlocks(custom_blocks);

        return registrations;
    }

    injectTimeBlocks() {
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

    injectCustomBlocks(categorized_custom_blocks: CategorizedCustomBlock[]) {
      for (const blocks of categorized_custom_blocks){
        for (const block of blocks.resolved_custom_blocks) {
          Blockly.Blocks[block.id] = {
                init: function () {
                    this.jsonInit({
                        'id': block.id,
                        'message0': block.message,
                        'category': Blockly.Categories.event,
                        'extensions': ['colours_bridge_' + blocks.bridge_data.bridge_id,
                                       get_block_category(block)],
                        'args0': get_block_toolbox_arguments(block)
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
          <block type="time_trigger_at" id="time_trigger_at">
            <value name="HOUR">
              <shadow type="math_positive_number">
                <field name="NUM">19</field>
              </shadow>
            </value>
            <value name="MINUTE">
              <shadow type="math_positive_number">
                <field name="NUM">10</field>
              </shadow>
            </value>
            <value name="SECOND">
              <shadow type="math_positive_number">
                <field name="NUM">00</field>
              </shadow>
            </value>
          </block>
          <block type="time_get_utc_hour" id="time_get_utc_hour"></block>
          <block type="time_get_utc_minute" id="time_get_utc_minute"></block>
          <block type="time_get_utc_seconds" id="time_get_utc_seconds"></block>
          <block type="${UtcTimeOfDayBlockId}" id="${UtcTimeOfDayBlockId}">
            <value name="DAY_OF_WEEK">
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

        toolboxXML.appendChild(await this.templateController.genCategory());
        toolboxXML.appendChild(await this.customSignalController.genCategory());
        toolboxXML.appendChild(variablesCategory);
        // toolboxXML.appendChild(proceduresCategory);

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
