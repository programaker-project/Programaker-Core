import { MatDialog } from '@angular/material/dialog';
import { MatSnackBar } from '@angular/material/snack-bar';

import { CustomSignalCreateDialogComponent } from '../custom_signals/create-dialog.component';
import { alreadyRegisteredException, createDom } from './utils';
import { ToolboxController } from './ToolboxController';
import { CustomSignalService } from '../custom_signals/custom_signal.service';
import { CustomSignal } from '../custom_signals/custom_signal';
import { ToolboxRegistrationHook } from './TemplateController';
declare const Blockly : any;

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
        if (this.registeredCustomSignalBlocks) {
            return;
        }

        this.registeredCustomSignalBlocks = true;

        const availableCustomSignals = this.availableCustomSignals;
        if (availableCustomSignals.length === 0) {
            availableCustomSignals.push(["Unkown", '__plaza_internal_unlisted'])
        }

        // Create signal trigger block
        Blockly.Blocks['automate_trigger_custom_signal'] = {
            init: function () {
                this.jsonInit({
                    'id': 'automate_trigger_custom_signal',
                    'message0': 'Trigger %1. Send: %2',
                    'args0': [
                        {
                            'type': 'field_dropdown',
                            'name': 'SIGNAL',
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

        // Declare text field on trigger block
        const triggerBlock = createDom('block', {
            type: "automate_trigger_custom_signal",
            id: "automate_trigger_custom_signal",
        });
        const triggerBlockArgValue = createDom('value', { name: 'VALUE' });
        const triggerBlockArgValueShadow = createDom('shadow', {
            type: 'text',
        });
        triggerBlockArgValueShadow.appendChild(createDom('field', {
            name: 'TEXT'
        }));

        triggerBlockArgValue.appendChild(triggerBlockArgValueShadow);
        triggerBlock.appendChild(triggerBlockArgValue)

        // Create a signal receiver
        Blockly.Blocks['automate_on_custom_signal'] = {
            init: function () {
                this.jsonInit({
                    'id': 'automate_on_custom_signal',
                    'message0': 'On signal %1. Save to %2',
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

        if (this.customSignalsCategory) {
            this.customSignalsCategory.appendChild(createDom('block',
                                                             {
                type: "automate_on_custom_signal",
                id: "automate_on_custom_signal",
            }));
            this.customSignalsCategory.appendChild(triggerBlock);
        }

        this.toolboxController.update();
    }

    injectBlocks(): ToolboxRegistrationHook[] {
        try {
            Blockly.Extensions.register('colours_custom_signals',
                function () {
                    this.setColourFromRawValues_('#72007b', '#d400e6', '#310034');
                });
        } catch (e) {
            // If the extension was registered before
            // this would have thrown an inocous exception
            if (!alreadyRegisteredException(e)) {
                throw e;
            }
        }

        return [
            (workspace: Blockly.WorkspaceSvg) => {
                this.register_custom_signal_blocks();

                workspace.registerButtonCallback('AUTOMATE_CREATE_CUSTOM_SIGNAL', (b: Blockly.FlyoutButton) => {
                    this.create_custom_signal().then((custom_signal_name) => {

                        this.customSignalService.saveCustomSignal(custom_signal_name)
                            .then((custom_signal_creation) => {
                                if (!this.customSignalsCategory) {
                                    console.error("No custom signals toolbox found");
                                    return;
                                }

                                console.log('Signal created');

                                if (this.availableCustomSignals[0][1] === '__plaza_internal_unlisted') {
                                    this.availableCustomSignals.splice(0, 1);
                                }

                                this.availableCustomSignals.push([custom_signal_name, custom_signal_creation.id]);

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
            colour: "#72007b",
            secondaryColour: "#310034",
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

            return cat;
        });
    }
}
