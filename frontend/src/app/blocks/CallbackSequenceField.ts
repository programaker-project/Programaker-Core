/// <reference path="./blockly-core.d.ts" />

import { ResolvedDynamicSequenceBlockArgument } from "../custom_block";
import { CustomBlockService } from "app/custom_block.service";

export const TYPE = 'callback_sequence';

declare const goog: any;

export const SEQUENCE_SEPARATOR = '\\';

function tagDepth(parent: string, list: [string, string][]): string[][] {
    const newValues = [] as [string, string][];
    for (const [name, key] of list) {
        newValues.push([
            name,
            `${parent}${SEQUENCE_SEPARATOR}${key}`,
        ]);
    }

    return newValues;
}

export function cleanCallbackSequenceValue(value: string): string {
    const chunks = value.split(SEQUENCE_SEPARATOR);
    return chunks[chunks.length - 1];
}

function sequenceValidator(customBlockService: CustomBlockService, block: ResolvedDynamicSequenceBlockArgument, field: Blockly.FieldDropdown): ((value: string) => void) {
    return (value: string) => {
        if (value === 'Select') {
            return;
        }

        const foundName = ((field as any).menuGenerator_ as [string, string][]).find(([_x, y]) => y === value);
        let selectedName = foundName ? foundName[0] : null;

        if (selectedName && selectedName.match(/Go back [0-9]+ steps?/)) {
            // TODO: Properly extract this name
            selectedName = null;
        }

        const chunks = value ? value.split(SEQUENCE_SEPARATOR) : [];

        const selectedDepth = chunks.length;
        const selectedId = chunks[selectedDepth - 1];

        if (selectedDepth >= block.callback_sequence.length) {
            // Pull options from parent element.
            // TODO: Deduplicate with the `else`
            const depth = selectedDepth - 1;
            const parentId = chunks[chunks.length - 2];

            customBlockService.getCallbackOptionsOnSequence(block.program_id, block.bridge_id, block.callback_sequence[depth], parentId)
                .then((options: [string, string][]) => {

                    const prelude = [ ["Back to Top", ''] ];
                    for (let i = 1; i < depth; i++ ) {
                        prelude.push([
                            `Go back ${i} step` + (i === 1 ? '' : 's'),
                            value.split(SEQUENCE_SEPARATOR, depth - i).join(SEQUENCE_SEPARATOR)
                        ]);
                    }

                    const menu = prelude.concat(tagDepth(value.split(SEQUENCE_SEPARATOR, depth).join(SEQUENCE_SEPARATOR), options));

                    (field as any).menuGenerator_ = menu;

                    if (field.value_ !== value) {
                        (field.setValue as any)(value, true);
                    }
                    else {
                        // As the value might not change, manually trigger an update
                        Blockly.Events.fire(new Blockly.Events.BlockChange(
                            field.sourceBlock_, 'field', field.name, field.value_, value));
                    }

                    (field.setValue as any)(value, true);
                });

            return;
        }

        if (selectedDepth === 0) {
            (field as any).menuGenerator_ = block.first_level_options;
            (field.setValue as any)('Select', true);
        }
        else {
            customBlockService.getCallbackOptionsOnSequence(block.program_id, block.bridge_id, block.callback_sequence[selectedDepth], selectedId)
                .then((options: [string, string][]) => {

                    const prelude = [ ["Back to Top", ''] ];
                    for (let i = 1; i < selectedDepth; i++ ) {
                        prelude.push([
                            `Go back ${i} step` + (i === 1 ? '' : 's'),
                            value.split(SEQUENCE_SEPARATOR, selectedDepth - i).join(SEQUENCE_SEPARATOR)
                        ]);
                    }

                    const newName = selectedName ? `Select in ${selectedName}` : 'Select';
                    prelude.push([newName, value]);
                    const menu = prelude.concat(tagDepth(value, options));

                    (field as any).menuGenerator_ = menu;

                    if (field.value_ !== value) {
                        (field.setValue as any)(value, true);
                    }
                    else {
                        // As the value might not change, manually trigger an update
                        Blockly.Events.fire(new Blockly.Events.BlockChange(
                            field.sourceBlock_, 'field', field.name, field.value_, value));
                    }
                });
        }
    }
}


if (typeof goog !== 'undefined') {
    // Goog cannot be used from SSR... we don't need it anyway
    goog.provide('CallbackSequenceField');
    goog.require('Blockly.Field');
}

const CallbackSequenceField = function(customBlockService: CustomBlockService, opt_value: ResolvedDynamicSequenceBlockArgument | any, opt_validator: any): any {
    this.menuGenerator_ = (opt_value as ResolvedDynamicSequenceBlockArgument).first_level_options;
    this.trimOptions_();

    this.validator = sequenceValidator(customBlockService, opt_value, this);

    // Call parent's constructor.
    (Blockly.FieldDropdown as any).superClass_.constructor.call(this, '', opt_validator ? opt_validator : this.validator);
    this.addArgType('dropdown');
};

if (typeof goog !== 'undefined') {
    // Goog cannot be used from SSR... we don't need it anyway
    goog.inherits(CallbackSequenceField, Blockly.FieldDropdown);
}

CallbackSequenceField.prototype.setValue = function(newValue: string, fromValidator: boolean) {
    if (!fromValidator) {
        this.validator(newValue);
    }
    else {
        Blockly.FieldDropdown.prototype.setValue.call(this, newValue);
    }
}

export function registerCallbackSequenceField(customBlockService: CustomBlockService) {

    (CallbackSequenceField as any).fromJson = function(element: { options: ResolvedDynamicSequenceBlockArgument }): any {
        // @ts-ignore
        return new CallbackSequenceField(customBlockService, element.options, null);
    };

    (Blockly as any).Field.register(TYPE, CallbackSequenceField);
}
