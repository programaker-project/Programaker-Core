import { IXPathDescriptor } from "./PlazaApi.types";
import { Browser } from "./BrowserApi";

function build_xpath(node: HTMLElement): string {
    if (node.parentElement === null) {
        return `/${node.tagName.toLowerCase()}`;
    }

    const parent = node.parentElement;
    const sameLevel = Array.from(parent.childNodes);
    const sameType = sameLevel.filter((e: HTMLElement) => e.tagName === node.tagName);
    const index = sameType.indexOf(node);

    return (build_xpath(parent) + "/"
            + `${node.tagName.toLowerCase()}[${index + 1}]`);
}

function get_element_info(node: HTMLElement): { data: IXPathDescriptor, value: Node } {
    try {
        console.log(node);
        const xpath = build_xpath(node);
        const url = document.location.href;

        const data = { xpath, url };

        console.log(JSON.stringify(data));

        const value = document.evaluate( xpath, document, null, XPathResult.ANY_TYPE, null ).iterateNext();
        console.log("Value:", value);

        return { data, value };
    } catch (e) {
        console.error("Error retrieving element", node, "info:", e);
        return undefined;
    }
}

interface ISelectorScreenGui {
    cancelButton: HTMLButtonElement;
    submitButton: HTMLButtonElement;
    nameField: HTMLInputElement;

    container: HTMLDivElement;
    foundElement: HTMLDivElement;
    nameLabel: HTMLLabelElement;
    screen: HTMLDivElement;

    callToAction: HTMLDivElement;
}

interface ISelectorScreenOptions {
    username: string;
    token: string;
}

class SelectorScreen {
    private gui: ISelectorScreenGui;
    private elementInfo: any;
    private prevEvent: any;
    private gettingClick: boolean;
    private options: ISelectorScreenOptions;

    constructor() {
        this.gui = undefined;
        this.elementInfo = undefined;

        this.prevEvent = undefined;
        this.gettingClick = false;

        this.options = undefined;
    }

    public send_response(message) {
        Browser.runtime.sendMessage({
            command: "addMonitor",
            message,
            token: this.options.token,
            username: this.options.username,
        });
    }

    public set_options(options) {
        this.options = options;
    }

    public draw() {
        if (this.gui === undefined) {
            this.gui = draw_selector_screen();

            // Initialize
            this.get_click( (e) => {
                this.capture_element(e.target);
            });

            this.gui.cancelButton.onclick = () => {
                try {
                    this.cancel_get_click();
                    this.destroy();
                } catch (e) {
                    console.error(e);
                }
            };
        }

        return this.gui;
    }

    public submit() {
        this.send_response({
            name: this.gui.nameField.value.trim(),
            type: "xpath_v1",
            value: this.elementInfo.data,
        });

        this.destroy();
    }

    public get_click(cb) {
        this.prevEvent = document.body.onclick;
        this.gettingClick = true;
        document.body.onclick = (e) => {
            if (this.isOwnElement(e.target)) {
                return;
            }

            document.body.onclick = this.prevEvent;
            cb(e);
        };
    }

    public isOwnElement(node) {
        while (node !== null) {
            if (node === this.gui.container) {
                return true;
            }

            node = node.parentElement;
        }
    }

    public cancel_get_click() {
        if (this.gettingClick) {
            document.body.onclick = this.prevEvent;
            this.gettingClick = false;
        }
    }

    public destroy() {
        this.gui.container.parentElement.removeChild(this.gui.container);
    }

    public capture_element(e) {
        this.elementInfo = get_element_info(e);
        console.log("Element info", this.elementInfo);
        if (this.elementInfo !== undefined) {
            this.display_element(e, this.elementInfo.value);
        }
    }

    public display_element(node, value) {
        try {
            if (node.tagName.toLowerCase() === "img") {
                this.gui.foundElement.innerText = `Found image`;
            } else {
                let limitedStr = value.innerText.substr(0, 10);
                if (value.length >= 10) {
                    limitedStr += "...";
                }
                this.gui.foundElement.innerText = `Found: "${limitedStr}"`;
            }

            this.gui.foundElement.style.display = "inline";
            this.gui.nameLabel.style.display = "inline";
            this.gui.nameField.style.display = "inline";
            this.gui.submitButton.style.display = "inline";

            this.gui.callToAction.style.display = "none";

            this.gui.submitButton.onclick = () => { this.submit(); };
            this.gui.submitButton.disabled = true;
            this.gui.nameField.onkeypress = () => {
                const canSend = this.gui.nameField.value.trim() !== "";
                this.gui.submitButton.disabled = !canSend;
            };

        } catch (e) {
            console.error(e);
        }
    }
}

function draw_selector_screen(): ISelectorScreenGui {
    const body = document.body;

    // Floating element
    const container = document.createElement("div");
    body.appendChild(container);

    container.style.position = "fixed";
    container.style.bottom = "1ex";
    container.style.width = "100%";

    const screen = document.createElement("div");
    container.appendChild(screen);

    screen.style.width = "75%";
    screen.style.margin = "0 auto";
    screen.style.backgroundColor = "white";

    // External box
    const box = document.createElement("div");
    screen.appendChild(box);

    box.style.border = "1px solid #444";

    // Selector form
    const form = document.createElement("div");
    box.appendChild(form);

    // Call-to-action text
    const callToAction = document.createElement("div");
    form.appendChild(callToAction);

    callToAction.style.display = "inline";
    callToAction.style.textAlign = "center";
    callToAction.innerText = "Click on the element you want to monitor.";

    // Found element
    const foundElement = document.createElement("div");
    form.appendChild(foundElement);

    foundElement.style.display = "none";

    // Name field
    const nameLabel = document.createElement("label");
    form.appendChild(nameLabel);

    nameLabel.innerText = "Name";
    nameLabel.style.display = "none";

    const nameField = document.createElement("input");
    form.appendChild(nameField);

    nameField.placeholder = "Insert monitor name";
    nameField.type = "text";
    nameField.style.display = "none";
    nameField.id = "plazaAddonNameField";

    nameLabel.htmlFor = nameField.id;

    // Submit button
    const submitButton = document.createElement("button");
    form.appendChild(submitButton);

    submitButton.innerText = "Submit";
    submitButton.style.display = "none";

    // Cancel button
    const cancelButton = document.createElement("button");
    form.appendChild(cancelButton);

    cancelButton.innerText = "Cancel";

    return {
        callToAction,
        cancelButton,
        container,
        foundElement,
        nameField,
        nameLabel,
        screen,
        submitButton,
    };
}

try {
    const selectorScreen = new SelectorScreen();
    selectorScreen.draw();

    Browser.runtime.onMessage.addListener((message, sender, sendResponse) => {
        const scriptOptions = message.plazaInjectedOptions;
        if (scriptOptions) {
            selectorScreen.set_options(scriptOptions);
        }
    });
} catch (e) {
    console.error(e);
}
