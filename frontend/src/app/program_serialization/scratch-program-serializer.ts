import { ToolboxController } from "../blocks/ToolboxController";

type CompiledScratchBlock = { id: string, type: string, args: any[] | {[key: string]: any}, contents: any[], save_to?: { index: number } };

export default class ScratchProgramSerializer {
    // ====================================================
    // External API
    // ====================================================
    constructor(private toolboxController: ToolboxController) {
    }

    public ToJson(xml: Element): { parsed: any, orig: any } {
        const subelements = Array.from(xml.childNodes);
        const variables = subelements.shift();
        const blocks = subelements;

        try {
            const serialized = {
                variables: this.serializeVariables(variables as HTMLElement),
                blocks: blocks.map(block => this.serializeColumn(block as HTMLElement)).filter(column => !!column),
            };

            return {
                parsed: serialized,
                orig: Blockly.Xml.domToPrettyText(xml),
            };
        }
        catch (e) {
            console.error("Serialization error:", e);
            throw e;
        }
    }

    // ====================================================
    // Internal functions
    // ====================================================
    private serializeColumn(block: HTMLElement): any {
        const blockType = ScratchProgramSerializer.cleanTypeName(block.getAttribute('type'));
        const knownBlock = this.toolboxController.customBlocks[blockType];

        if (knownBlock && knownBlock.block_type !== 'trigger') {
            return null; // This column cannot be executed
        }

        return this.serializeBlock(block);
    }

    private serializeBlock(block: HTMLElement, chain: any[] = null): any {
        if (chain === null) {
            chain = [];
        }

        let cleanedElement: CompiledScratchBlock = {
            id: block.id,
            type: ScratchProgramSerializer.cleanTypeName(block.getAttribute('type')),
            args: Array.from(block.childNodes)
                .filter((x: HTMLElement) => (x.tagName !== 'NEXT'
                                             && x.tagName !== 'STATEMENT'
                                             && x.tagName !== 'COMMENT'
                                           ))
                .map(node => this.serializeArg(node as HTMLElement)),
            contents: [],
        };

        this.fixArgOrder(cleanedElement);

        cleanedElement = this.rewriteCustomBlock(cleanedElement);
        this.replaceServices(cleanedElement);
        this.replaceMonitors(cleanedElement);

        const contents = Array.from(block.childNodes).filter((x: HTMLElement) => x.tagName === 'STATEMENT');
        if (contents.length == 1) {
            cleanedElement.contents = this.serializeBlock(contents[0].firstChild as HTMLElement);
        }
        else if (contents.length > 0) {
            const subContents = [];
            for (const subContent of contents) {
                subContents.push({ "contents": this.serializeBlock(subContent.firstChild as HTMLElement) });
            }

            cleanedElement.contents = subContents;
        }

        chain.push(cleanedElement);

        const next = Array.from(block.childNodes).filter((x: HTMLElement) => x.tagName === 'NEXT');
        if (next.length > 0) {
            return this.serializeBlock(next[0].firstChild as HTMLElement, chain);
        }

        return chain;
    }

    private fixArgOrder(cleanedElement: CompiledScratchBlock) {
        const order = (cleanedElement.args as any[]).map((value) => {
            if (!value.name) {
                return null;
            }

            const digits = ScratchProgramSerializer.getDigits(value.name);
            if (digits === null) {
                return null;
            }

            return [value, digits];
        });

        if (order.filter((value) => {
            return value === null;
        }).length > 0) {
            // Bail if the order of any value cannot be retrieved
            return;
        }

        cleanedElement.args = order.sort((x, y) => {
            return x[1] - y[1];
        }).map((composedValue) => {
            return composedValue[0];
        });
    }

    private static getDigits(s: string): number | null {
        const match = s.match(/\d+/);
        if (match === null) {
            return null;
        }

        return parseInt(match[0]);
    }

    private rewriteCustomBlock(element: CompiledScratchBlock) {
        const blockInfo = this.toolboxController.getBlockInfo(element.type);
        if (!blockInfo) {
            return element;
        }

        if (blockInfo.save_to) {
            element.save_to = blockInfo.save_to;
        }

        if (blockInfo.block_type === 'trigger') {
            return this.rewriteCustomTrigger(element, blockInfo);
        }

        return element;
    }

    private rewriteCustomTrigger(element: CompiledScratchBlock, blockInfo: any) {
        const args: any = {};
        if (blockInfo.save_to) {
            let save_to = null;
            if (blockInfo.save_to.type === 'argument') {
                save_to = { 'type': 'variable', 'value': (element.args as any[])[blockInfo.save_to.index].value };
            }

            args.monitor_save_value_to = save_to;
        }

        if (blockInfo.expected_value) {
            let expected_value = null;
            if (blockInfo.expected_value.type === 'argument') {
                expected_value = (element.args as any[])[blockInfo.expected_value.index];
            }

            args.monitor_expected_value = expected_value;
        }

        if (blockInfo.key) {
            args.key = blockInfo.key;
        }
        if (blockInfo.subkey) {
            if (blockInfo.subkey.type === 'argument') {
                args.subkey = {
                    'type': 'constant',
                    'value': (element.args as any[])[blockInfo.subkey.index].value,
                };
            }
            else {
                args.subkey = blockInfo.subkey;
            }
        }


        element.args = args;
        return element;
    }

    private replaceServices(element: CompiledScratchBlock) {

    }

    private replaceMonitors(element: CompiledScratchBlock) {
        switch (element.type) {
            case "time_trigger_at":
                // This implies a call to a monitor
                {
                    element.type = "wait_for_monitor";
                    element.args = {
                        "monitor_id": { "from_service": "0093325b-373f-4f1c-bace-4532cce79df4" }, // Timekeeping monitor ID
                        "monitor_expected_value": {
                            "type": "constant",
                            "value": ((element.args as any[])[0].value.replace(/^0*(\d)$/, "$1") + ':'
                                + (element.args as any[])[1].value.replace(/^0*(\d)$/, "$1") + ':'
                                + (element.args as any[])[2].value.replace(/^0*(\d)$/, "$1")
                            )
                        }
                    }
                    break;
                }

            case "time_trigger_at_tz":
                // This implies a call to a monitor
                {
                    element.type = "wait_for_monitor";
                    element.args = {
                        "monitor_id": { "from_service": "0093325b-373f-4f1c-bace-4532cce79df4" }, // Timekeeping monitor ID
                        "monitor_expected_value": {
                            "type": "constant",
                            "value": ((element.args as any[])[0].value.replace(/^0*(\d)$/, "$1") + ':'
                                + (element.args as any[])[1].value.replace(/^0*(\d)$/, "$1") + ':'
                                + (element.args as any[])[2].value.replace(/^0*(\d)$/, "$1")
                                     )
                        },
                        "timezone": (element.args as any[])[3].value,
                    }
                    break;
                }
        }
    }

    private static cleanTypeName(typeName: any): string {
        // Remove digits (used for parameter ordering) from end of type
        return typeName.toString().replace(/\d+$/, "");
    }

    private serializeArg(argument: HTMLElement): any {
        if (argument.tagName === 'FIELD') {
            let type = argument.getAttribute('name').toLowerCase();

            if (type.startsWith('val')) {
                type = "constant";
            }
            if (argument.getAttribute('id') === null) {
                type = 'constant';  // No block or value, but dropdown/constant
            }
            return {
                name: argument.getAttribute('name'),
                // Type here might be 'constant', 'variable' or 'list'
                type: ScratchProgramSerializer.cleanTypeName(type),
                value: argument.innerText,
            }
        }

        // If there's a second block, that block result is the arg value
        if ((argument.childNodes.length > 1)
            && ((argument.childNodes[1] as HTMLElement).tagName === 'BLOCK')) {

            return {
                name: argument.getAttribute('name'),
                type: 'block',
                value: this.serializeBlock(argument.childNodes[1] as HTMLElement),
            }
        }

        if ((argument.childNodes.length === 1)
            && ((argument.childNodes[0] as HTMLElement).tagName === 'BLOCK')) {

            return {
                name: argument.getAttribute('name'),
                type: 'block',
                value: this.serializeBlock(argument.childNodes[0] as HTMLElement),
            }
        }

        // If there's just one entry, it's only a shadow value.
        // return it's content as constant
        if (argument.childNodes.length !== 1) {
            console.error('Unexpected argument type', argument, 'defaulting to constant');
        }

        return {
            name: argument.getAttribute('name'),
            type: 'constant',
            value: (argument.childNodes[0].firstChild as HTMLElement).innerText,
        }
    }

    private serializeVariables(variables: HTMLElement): any {
        return Array.from(variables.childNodes)
            .map(variable =>
                this.serializeVariable(variable as HTMLElement));
    }

    private serializeVariable(variable: HTMLElement): any {
        return {
            id: variable.id,
            name: variable.innerText,
            type: ScratchProgramSerializer.cleanTypeName(variable.attributes.getNamedItem('type')),
        }
    }
}
