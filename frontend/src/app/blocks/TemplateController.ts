import { TemplateCreateDialogComponent } from '../templates/create-dialog.component';
import { alreadyRegisteredException, createDom } from './utils';
import { MatDialog } from '@angular/material';
import { ToolboxController } from './ToolboxController';
declare const Blockly;

export class TemplateController {
    toolboxController: ToolboxController;
    templatesCategory: HTMLElement;
    dialog: MatDialog;

    constructor(
        dialog: MatDialog,
        toolboxController: ToolboxController,
    ) {
        this.dialog = dialog;
        this.toolboxController = toolboxController;
    }

    injectTemplateBlocks(): Function[] {
        try {
            Blockly.Extensions.register('colours_templates',
                function () {
                    this.setColourFromRawValues_('#40BF4A', '#389438', '#308438');
                });
        } catch (e) {
            // If the extension was registered before
            // this would have thrown an inocous exception
            if (!alreadyRegisteredException(e)) {
                throw e;
            }
        }

        const availableTemplates = [];
        let registered = false;
        const register_template_blocks = ((workspace) => {
            if (!availableTemplates) {
                return;
            }
            if (registered) {
                return;
            }

            registered = true;

            Blockly.Blocks['automate_match_template_check'] = {
                init: function () {
                    this.jsonInit({
                        'id': 'automate_match_template_check',
                        'message0': 'Does %1 match %2 ?',
                        'args0': [
                            {
                                'type': 'input_value',
                                'name': 'VALUE'
                            },
                            {
                                'type': 'field_dropdown',
                                'name': 'TEMPLATE_NAME',
                                'options': availableTemplates,
                            }
                        ],
                        'category': Blockly.Categories.event,
                        'extensions': ['colours_templates', 'output_string']
                    });
                }
            };

            this.templatesCategory.appendChild(createDom('block',
                {
                    type: "automate_match_template_check",
                    id: "automate_match_template_check",
                }));



            Blockly.Blocks['automate_match_template_stmt'] = {
                init: function () {
                    this.jsonInit({
                        'id': 'automate_match_template_stmt',
                        'message0': 'Match %1 with %2',
                        'args0': [
                            {
                                'type': 'input_value',
                                'name': 'VALUE'
                            },
                            {
                                'type': 'field_dropdown',
                                'name': 'TEMPLATE_NAME',
                                'options': availableTemplates,
                            }
                        ],
                        'category': Blockly.Categories.event,
                        'extensions': ['colours_templates', 'shape_statement']
                    });
                }
            };

            this.templatesCategory.appendChild(createDom('block',
                {
                    type: "automate_match_template_stmt",
                    id: "automate_match_template_stmt",
                }));
        });

        return [
            (workspace) => {
                workspace.registerButtonCallback('AUTOMATE_CREATE_TEMPLATE', (x, y, z) => {
                    this.create_template().then(([template_name, template_id]) => {

                        if (!this.templatesCategory) {
                            console.error("No templates toolbox found");
                            return;
                        }

                        availableTemplates.push([template_name, template_id]);
                        register_template_blocks(workspace);

                        this.toolboxController.update();
                    });
                });
            }
        ];
    }

    create_template(): Promise<[string, string]> {
        const variables = this.toolboxController.getStringVariables();
        return new Promise((resolve, reject) => {
            const _dialogRef = this.dialog.open(TemplateCreateDialogComponent, {
                data: { template: null, promise: { resolve, reject }, variables: variables }
            });

        });
    }



    genTemplatesCategory() {
        if (this.templatesCategory) {
            return this.templatesCategory;
        }

        const cat = createDom('category', {
            name: "Templates",
            colour: "#40BF4A",
            secondaryColour: "#389438",
        })

        cat.appendChild(createDom('button', {
            text: "New template",
            callbackKey: "AUTOMATE_CREATE_TEMPLATE",
        }));

        this.templatesCategory = cat;
        return cat;
    }
}
