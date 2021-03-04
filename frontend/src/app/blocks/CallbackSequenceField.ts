/// <reference path="./blockly-core.d.ts" />

import { ResolvedDynamicSequenceBlockArgument } from "../custom_block";
import { CustomBlockService } from "app/custom_block.service";

export const TYPE = 'callback_sequence';

declare const goog: any;
goog.provide('CallbackSequenceField');
goog.require('Blockly.Field');

function tagDepth(depth: number, list: [string, string][]): string[][] {
    const newValues = [] as [string, string][];
    for (const [name, key] of list) {
        newValues.push([
            name,
            `${depth}_${key}`,
        ]);
    }

    return newValues;
}

export function cleanCallbackSequenceValue(value: string): string {
    return value.replace(/^([0-9]+)_/, '');

}

function sequenceValidator(customBlockService: CustomBlockService, block: ResolvedDynamicSequenceBlockArgument, field: Blockly.FieldDropdown): ((value: string) => void) {
    return (value: string) => {
        if (value === 'Select') {
            console.log("Ignore looping...")
            return;
        }

        console.log("Selected", value, '(block=', block, ') on', field);
        console.log("SRVice", customBlockService);

        const foundName = ((field as any).menuGenerator_ as [string, string][]).find(([_x, y]) => y === value);
        const selectedName = foundName ? foundName[0] : null;

        const m = value.match(/([0-9]+)_(.*)/)
        const selectedDepth = parseInt(m[1]) + 1;
        const selectedId = m[2];

        if (selectedDepth >= block.callback_sequence.length) {
            console.log("Reached last level!");
            field.setValue(value);
            return;
        }

        console.log("SELECTED:", selectedId, "DEPTH", selectedDepth);

        if (selectedDepth === 0) {
            console.log("Top level");
            (field as any).menuGenerator_ = tagDepth(0, block.first_level_options);
            field.setValue('Select');
        }
        else {
            console.log("Pulling")
            customBlockService.getCallbackOptionsOnSequence(block.program_id, block.bridge_id, block.callback_sequence[selectedDepth], selectedId)
                .then((options: [string, string][]) => {
                    console.log("Result", options);
                    (field as any).menuGenerator_ = tagDepth(selectedDepth, options);
                    field.setValue(selectedName ? `Select in ${selectedName}` : 'Select');
                });
        }
    }
}

const CallbackSequenceField = function(customBlockService: CustomBlockService, opt_value: ResolvedDynamicSequenceBlockArgument | any, opt_validator: any): any {


    this.menuGenerator_ = tagDepth(0, (opt_value as ResolvedDynamicSequenceBlockArgument).first_level_options);
    this.trimOptions_();
    const firstTuple = this.getOptions()[0];

    // Call parent's constructor.
    (Blockly.FieldDropdown as any).superClass_.constructor.call(this, firstTuple[1],
                                                                opt_validator ? opt_validator : sequenceValidator(customBlockService,
                                                                                                                  opt_value, this));
    this.addArgType('dropdown');
};
goog.inherits(CallbackSequenceField, Blockly.FieldDropdown);

export function registerCallbackSequenceField(customBlockService: CustomBlockService) {
    (CallbackSequenceField as any).fromJson = function(element: { options: ResolvedDynamicSequenceBlockArgument }): any {
        // @ts-ignore
        return new CallbackSequenceField(customBlockService, element.options, null);
    };

    (Blockly as any).Field.register(TYPE, CallbackSequenceField);
}
