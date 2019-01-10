export default class ScratchProgramSerializer {
    // ====================================================
    // External API
    // ====================================================
    static ToJson(xml: Element): { parsed: any, orig: any } {
        const subelements = Array.from(xml.childNodes);
        const variables = subelements.shift();
        const blocks = subelements;

        try {
            const serialized = {
                variables: ScratchProgramSerializer.serializeVariables(variables as HTMLElement),
                blocks: blocks.map(block => ScratchProgramSerializer.serializeBlock(block as HTMLElement)),
            };

            return {
                parsed: serialized,
                orig: Blockly.Xml.domToPrettyText(xml),
            };
        }
        catch(e) {
            console.error("Serialization error:", e);
            throw e;
        }
    }

    // ====================================================
    // Internal functions
    // ====================================================
    static serializeBlock(block: HTMLElement, chain: any[]= null): any {
        if (chain === null) {
            chain = [];
        }

        let cleanedElement = {
            id: block.id,
            type: block.getAttribute('type'),
            args: Array.from(block.childNodes)
                .filter((x: HTMLElement) => x.tagName !== 'NEXT' && x.tagName !== 'STATEMENT')
                .map(node => ScratchProgramSerializer.serializeArg(node as HTMLElement)),
            contents: [],
        };

        ScratchProgramSerializer.replaceServices(cleanedElement);
        ScratchProgramSerializer.replaceMonitors(cleanedElement);

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

    static replaceServices(element) {

    }

    static replaceMonitors(element) {
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

            case "time_trigger_at":
                // This implies a call to a monitor
                {
                    element.type = "wait_for_monitor";
                    element.args = {
                        "monitor_id": { "from_service": "0093325b-373f-4f1c-bace-4532cce79df4" }, // Timekeeping monitor ID
                        "monitor_expected_value": {
                            "type": "constant",
                            "value": ( element.args[0].value.replace(/^0*(\d)$/, "$1")  + ':'
                                       + element.args[1].value.replace(/^0*(\d)$/, "$1")  + ':'
                                       + element.args[2].value.replace(/^0*(\d)$/, "$1")
                                     )
                        }
                    }
                    break;
                }
        }
    }

    static serializeArg(argument: HTMLElement): any {
        if (argument.tagName === 'FIELD') {
            return {
                type: argument.getAttribute('name').toLowerCase(),
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

    static serializeVariables(variables: HTMLElement): any {
        return Array.from(variables.childNodes)
                .map(variable =>
                    ScratchProgramSerializer.serializeVariable(variable as HTMLElement));
    }

    static serializeVariable(variable: HTMLElement): any {
        return {
            id: variable.id,
            name: variable.innerText,
            type: variable.attributes.getNamedItem('type'),
        }
    }
}
