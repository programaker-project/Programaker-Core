import { CustomSignalCreateDialogComponent } from '../custom_signals/create-dialog.component';
import { alreadyRegisteredException, createDom } from './utils';
import { MatDialog } from '@angular/material';
import { ToolboxController } from './ToolboxController';
import { CustomSignalService } from '../custom_signals/custom_signal.service';
import { CustomSignal } from '../custom_signals/custom_signal';
declare const Blockly;

export class CustomSignalController {
    toolboxController: ToolboxController;
    customSignalsCategory: HTMLElement;
    dialog: MatDialog;
    customSignalService: CustomSignalService;

    registeredCustomSignalBlocks = false;
    availableCustomSignals: [string, string][];  // Name, Id
    customSignals: { [key: string]: CustomSignal };

    constructor(
        dialog: MatDialog,
        toolboxController: ToolboxController,
        customSignalService: CustomSignalService,
    ) {
        this.dialog = dialog;
        this.toolboxController = toolboxController;
        this.customSignalService = customSignalService;
        this.availableCustomSignals = [];
        this.customSignals = {};
    }

    register_custom_signal_blocks() {
        if ((!this.availableCustomSignals) || (this.availableCustomSignals.length === 0)) {
            return;
        }
        if (this.registeredCustomSignalBlocks) {
            return;
        }

        this.registeredCustomSignalBlocks = true;

        const availableCustomSignals = this.availableCustomSignals;

        Blockly.Blocks['automate_trigger_custom_signal'] = {
            init: function () {
                this.jsonInit({
                    'id': 'automate_trigger_custom_signal',
                    'message0': 'Trigger %1. Data %2',
                    'args0': [
                        {
                            'type': 'field_dropdown',
                            'name': 'SIGNAL_NAME',
                            'options': availableCustomSignals,
                        },
                        {
                            'type': 'input_value',
                            'name': 'VALUE'
                        },
                    ],
                    'category': Blockly.Categories.event,
                    'extensions': ['colours_custom_signals', 'shape_statement']
                });
            }
        };

        this.customSignalsCategory.appendChild(createDom('block',
            {
                type: "automate_trigger_custom_signal",
                id: "automate_trigger_custom_signal",
            }));

        Blockly.Blocks['automate_on_custom_signal'] = {
            init: function () {
                this.jsonInit({
                    'id': 'automate_on_custom_signal',
                    'message0': 'On signal %1. Save %2',
                    'args0': [
                        {
                            'type': 'field_dropdown',
                            'name': 'SIGNAL_NAME',
                            'options': availableCustomSignals,
                        },
                        {
                            'type': 'field_variable',
                            'name': 'VARIABLE2'
                        },
                    ],
                    'category': Blockly.Categories.event,
                    'extensions': ['colours_custom_signals', 'shape_hat']
                });
            }
        };

        this.customSignalsCategory.appendChild(createDom('block',
            {
                type: "automate_on_custom_signal",
                id: "automate_on_custom_signal",
            }));

        this.toolboxController.update();
    }

    injectBlocks(): Function[] {
        try {
            Blockly.Extensions.register('colours_custom_signals',
                function () {
                    this.setColourFromRawValues_('#d400e6', '#72007b', '#72007b');
                });
        } catch (e) {
            // If the extension was registered before
            // this would have thrown an inocous exception
            if (!alreadyRegisteredException(e)) {
                throw e;
            }
        }

        return [
            (workspace) => {
                workspace.registerButtonCallback('AUTOMATE_CREATE_CUSTOM_SIGNAL', (x, y, z) => {
                    this.create_custom_signal().then((custom_signal_name) => {
                        console.log("Custom signal:", custom_signal_name);

                        this.customSignalService.saveCustomSignal(custom_signal_name)
                            .then((custom_signal_creation) => {
                                if (!this.customSignalsCategory) {
                                    console.error("No custom signals toolbox found");
                                    return;
                                }

                                this.availableCustomSignals.push([custom_signal_name, custom_signal_creation.id]);
                                this.register_custom_signal_blocks();

                                this.toolboxController.update();
                            })
                            .catch(err => {
                                console.error(err);
                                alert("Error creating custom signal.\n" + err);
                            })
                    });
                });
            }
        ];
    }

    create_custom_signal(): Promise<string> {
        return new Promise((resolve, reject) => {
            const signal_data = { signal: { name: "" } };
            const dialogRef = this.dialog.open(CustomSignalCreateDialogComponent, {
                data: signal_data
            });

            dialogRef.afterClosed().subscribe(result => {
                if (!result) {
                    console.log("Cancelled");
                    reject();
                    return;
                }

                try {
                    resolve(signal_data.signal.name);
                }
                catch (err) {
                    reject(err);
                }
            });
        });
    }

    genCategory(): Promise<HTMLElement> {
        if (this.customSignalsCategory) {
            return Promise.resolve(this.customSignalsCategory);
        }

        const cat = createDom('category', {
            name: "Signals",
            colour: "#d400e6",
            secondaryColour: "#72007b",
            id: "signals",
        })

        cat.appendChild(createDom('button', {
            text: "New signal",
            callbackKey: "AUTOMATE_CREATE_CUSTOM_SIGNAL",
        }));

        this.customSignalsCategory = cat;

        return this.customSignalService.getCustomSignals().then((customSignals) => {
            for (const signal of customSignals) {
                this.customSignals[signal.id] = signal;
                this.availableCustomSignals.push([signal.name, signal.id]);
            }

            this.register_custom_signal_blocks();

            return cat;
        });
    }
}
