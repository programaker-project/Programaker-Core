import { ToolboxController } from "../blocks/ToolboxController";

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
                blocks: blocks.map(block => this.serializeBlock(block as HTMLElement)),
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
    private serializeBlock(block: HTMLElement, chain: any[] = null): any {
        if (chain === null) {
            chain = [];
        }

        let cleanedElement = {
            id: block.id,
            type: block.getAttribute('type'),
            args: Array.from(block.childNodes)
                .filter((x: HTMLElement) => x.tagName !== 'NEXT' && x.tagName !== 'STATEMENT')
                .map(node => this.serializeArg(node as HTMLElement)),
            contents: [],
        };

        cleanedElement = this.rewriteCustomBlock(cleanedElement);
        this.replaceServices(cleanedElement);
        this.replaceMonitors(cleanedElement);

        const contents = Array.from(block.childNodes).filter((x: HTMLElement) => x.tagName === 'STATEMENT');
        if (contents.length > 0) {
            cleanedElement.contents = this.serializeBlock(contents[0].firstChild as HTMLElement);
        }

        chain.push(cleanedElement);

        const next = Array.from(block.childNodes).filter((x: HTMLElement) => x.tagName === 'NEXT');
        if (next.length > 0) {
            return this.serializeBlock(next[0].firstChild as HTMLElement, chain);
        }

        return chain;
    }

    private rewriteCustomBlock(element) {
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

    private rewriteCustomTrigger(element, blockInfo) {
        const args: any = {};
        if (blockInfo.save_to) {
            let save_to = null;
            if (blockInfo.save_to.type === 'argument') {
                save_to = { 'type': 'variable', 'value': element.args[blockInfo.save_to.index].value };
            }

            args.monitor_save_value_to = save_to;
        }

        if (blockInfo.expected_value) {
            let expected_value = null;
            if (blockInfo.expected_value.type === 'argument') {
                expected_value = element.args[blockInfo.expected_value.index];
            }

            args.monitor_expected_value = expected_value;
        }

        if (blockInfo.key) {
            args.key = blockInfo.key;
        }

        element.args = args;
        return element;
    }

    private replaceServices(element) {

    }

    private replaceMonitors(element) {
        switch (element.type) {

            case "chat_whenreceivecommand":
                // This implies a call to a monitor
                {
                    element.type = "wait_for_monitor";
                    element.args = {
                        "monitor_id": { "from_service": "c8062378-9b53-4962-b4f4-e5a71e34d335" }, // Telegram monitor ID
                        "monitor_expected_value": element.args[0]
                    }
                    break;
                }

            case "chat_whenreceiveanycommandtovar":
                // This implies a call to a monitor and a subsequent save of its value.
                {
                    element.type = "wait_for_monitor";
                    element.args = {
                        "monitor_id": { "from_service": "c8062378-9b53-4962-b4f4-e5a71e34d335" }, // Telegram monitor ID
                        "monitor_expected_value": "any_value",
                        "monitor_save_value_to": element.args[0],
                    }
                    break;
                }

            case "time_trigger_at":
                // This implies a call to a monitor
                {
                    element.type = "wait_for_monitor";
                    element.args = {
                        "monitor_id": { "from_service": "0093325b-373f-4f1c-bace-4532cce79df4" }, // Timekeeping monitor ID
                        "monitor_expected_value": {
                            "type": "constant",
                            "value": (element.args[0].value.replace(/^0*(\d)$/, "$1") + ':'
                                + element.args[1].value.replace(/^0*(\d)$/, "$1") + ':'
                                + element.args[2].value.replace(/^0*(\d)$/, "$1")
                            )
                        }
                    }
                    break;
                }
        }
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
                type: type,  // Type here might be 'constant', 'variable' or 'list'
                value: argument.innerText,
            }
        }

        // If there's a second block, that block result is the arg value
        if ((argument.childNodes.length > 1)
            && ((argument.childNodes[1] as HTMLElement).tagName === 'BLOCK')) {

            return {
                type: 'block',
                value: this.serializeBlock(argument.childNodes[1] as HTMLElement),
            }
        }

        if ((argument.childNodes.length === 1)
            && ((argument.childNodes[0] as HTMLElement).tagName === 'BLOCK')) {

            return {
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
            type: variable.attributes.getNamedItem('type'),
        }
    }
}
