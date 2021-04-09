import { TemplateCreateDialogComponent } from '../templates/create-dialog.component';
import { alreadyRegisteredException, createDom } from './utils';
import { MatDialog } from '@angular/material/dialog';
import { ToolboxController } from './ToolboxController';
import { TemplateService } from '../templates/template.service';
import { Template } from '../templates/template';
import { EditorController } from '../program-editors/editor-controller';
import { NgZone } from '@angular/core';

export type ToolboxRegistrationHook = (workspace: any, editorController: EditorController, ngZone: NgZone) => void;
declare const Blockly: any;

export class TemplateController {
    toolboxController: ToolboxController;
    templatesCategory: HTMLElement;
    dialog: MatDialog;
    templateService: TemplateService;

    registeredTemplateBlocks = false;
    availableTemplates: [string, string][];  // Name, Id
    templates: { [key: string]: Template };

    constructor(
        dialog: MatDialog,
        toolboxController: ToolboxController,
        templateService: TemplateService,
    ) {
        this.dialog = dialog;
        this.toolboxController = toolboxController;
        this.templateService = templateService;
        this.availableTemplates = [];
        this.templates = {};
    }

    register_template_blocks() {
        if (this.registeredTemplateBlocks) {
            return;
        }

        this.registeredTemplateBlocks = true;

        const availableTemplates = this.availableTemplates || [];

        if (availableTemplates.length === 0) {
            availableTemplates.push(["Unkown", '__plaza_internal_unlisted'])
        }

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

        if (this.templatesCategory) {
            this.templatesCategory.appendChild(createDom('block',
                                                         {
                type: "automate_match_template_check",
                id: "automate_match_template_check",
            }));

            this.templatesCategory.appendChild(createDom('block',
                                                         {
                type: "automate_match_template_stmt",
                id: "automate_match_template_stmt",
            }));
        }

        this.toolboxController.update();
    }

    injectBlocks(): ToolboxRegistrationHook[] {
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

        return [
            (workspace) => {
                this.register_template_blocks();

                workspace.registerButtonCallback('AUTOMATE_CREATE_TEMPLATE', (b: Blockly.FlyoutButton) => {
                    this.create_template().then(([template_name, template_content]) => {

                        this.templateService.saveTemplate(template_name, template_content)
                            .then((template_creation) => {
                                if (!this.templatesCategory) {
                                    console.error("No templates toolbox found");
                                    return;
                                }

                                if (this.availableTemplates[0][1] === '__plaza_internal_unlisted') {
                                    this.availableTemplates.splice(0, 1);
                                }

                                this.availableTemplates.push([template_name, template_creation.id]);
                                this.toolboxController.update();
                            })
                            .catch(err => {
                                console.error(err);
                                alert("Error creating template.\n" + err);
                            })
                    });
                });
            }
        ];
    }

    create_template(): Promise<[string, any[]]> {
        const variables = this.toolboxController.getStringVariables();
        return new Promise((resolve, reject) => {
            const _dialogRef = this.dialog.open(TemplateCreateDialogComponent, {
                data: { template: null, promise: { resolve, reject }, variables: variables }
            });

        });
    }

    genCategory(): Promise<HTMLElement> {
        if (this.templatesCategory) {
            return Promise.resolve(this.templatesCategory);
        }

        const cat = createDom('category', {
            name: "Templates",
            colour: "#40BF4A",
            secondaryColour: "#389438",
            id: "templates",
        })

        cat.appendChild(createDom('button', {
            text: "New template",
            callbackKey: "AUTOMATE_CREATE_TEMPLATE",
        }));

        this.templatesCategory = cat;

        return this.templateService.getTemplates().then((templates) => {
            for (const template of templates) {
                this.templates[template.id] = template;
                this.availableTemplates.push([template.name, template.id]);
            }

            return cat;
        });
    }
}
