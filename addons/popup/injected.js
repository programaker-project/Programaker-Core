"use strict";

(function(){
    const build_xpath = (node) => {
        if (node.parentElement === null) {
            return `/${node.tagName.toLowerCase()}`;
        }

        const parent = node.parentElement;
        const sameLevel = Array.from(parent.childNodes);
        const sameType = sameLevel.filter((e) => e.tagName === node.tagName);
        const index = sameType.indexOf(node);

        return (build_xpath(parent) + '/'
                + `${node.tagName.toLowerCase()}[${index + 1}]`);
    }

    const get_element_info = (node) => {
        try {
            console.log(node);
            const xpath = build_xpath(node);
            const url = document.location.href;

            const data = { xpath: xpath, url: url };

            console.log(JSON.stringify(data));

            const value = document.evaluate( xpath, document, null, XPathResult.ANY_TYPE, null ).iterateNext();
            console.log("Value:", value);

            return { data, value };
        }
        catch (e){
            console.error("Error retrieving element", node, "info:", e);
        }
    }

    class SelectorScreen {
        constructor() {
            this.gui = undefined;
            this.element_info = undefined;

            this.prev_event = undefined;
            this.getting_click = false;

            this.options = undefined;
        }

        send_response(message) {
            browser.runtime.sendMessage({
                command: "addMonitor",
                token: this.options.token,
                message: message,
                username: this.options.username,
            });
        }

        set_options(options) {
            this.options = options;
        }

        draw() {
            if (this.gui === undefined) {
                this.gui = draw_selector_screen();

                // Initialize
                this.get_click( (e) => {
                    this.capture_element(e.target);
                });

                this.gui.cancel_button.onclick = () => {
                    try {
                        this.cancel_get_click();
                        this.destroy();
                    }
                    catch (e){
                        console.error(e);
                    }
                }
            }

            return this.gui;
        }

        submit() {
            this.send_response({
                type: "xpath_v1",
                name: this.gui.nameField.value.trim(),
                value: this.element_info.data,
            });

            this.destroy();
        }

        get_click(cb) {
            this.prev_event = document.body.onclick;
            this.getting_click = true;
            document.body.onclick = (e) => {
                if (this.isOwnElement(e.target)) {
                    return;
                }

                document.body.onclick = this.prev_event;
                cb(e);
            };
        }

        isOwnElement(node) {
            while (node !== null) {
                if (node === this.gui.container) {
                    return true;
                }

                node = node.parentElement;
            }
        }

        cancel_get_click() {
            if (this.getting_click) {
                document.body.onclick = this.prev_event;
                this.getting_click = false;
            }
        }

        destroy() {
            this.gui.container.parentElement.removeChild(this.gui.container);
        }

        capture_element(e) {
            this.element_info = get_element_info(e);
            console.log("Element info", this.element_info);
            if (this.element_info !== undefined){
                this.display_element(e, this.element_info.value);
            }
        }

        display_element(node, value) {
            try {
                if (node.tagName.toLowerCase() == 'img'){
                    this.gui.found_element.innerText = `Found image`;
                }
                else {
                    let limited_str = value.innerText.substr(0, 10);
                    if (value.length >= 10) {
                        limited_str += '...';
                    }
                    this.gui.found_element.innerText = `Found: "${limited_str}"`;
                }

                this.gui.found_element.style.display = 'inline';
                this.gui.nameLabel.style.display = 'inline';
                this.gui.nameField.style.display = 'inline';
                this.gui.submit_button.style.display = 'inline';

                this.gui.call_to_action.style.display = 'none';

                this.gui.submit_button.onclick = () => { this.submit(); };
                this.gui.submit_button.disabled = true;
                this.gui.nameField.onkeypress = () => {
                    const can_send = this.gui.nameField.value.trim() != '';
                    this.gui.submit_button.disabled = !can_send;
                }
            } catch (e) {
                debugger;
                console.error(e);
            }
        }
    }

    const draw_selector_screen = () => {
        const body = document.body;

        // Floating element
        const container = document.createElement("div");
        body.appendChild(container);

        container.style.position = 'fixed';
        container.style.bottom = '1ex';
        container.style.width = '100%';

        const screen = document.createElement("div");
        container.appendChild(screen);

        screen.style.width = "75%";
        screen.style.margin = '0 auto';
        screen.style.backgroundColor = 'white';

        // External box
        const box = document.createElement("div");
        screen.appendChild(box);

        box.style.border = '1px solid #444';

        // Selector form
        const form = document.createElement("div");
        box.appendChild(form);

        // Call-to-action text
        const call_to_action = document.createElement("div");
        form.appendChild(call_to_action);

        call_to_action.style.display = 'inline';
        call_to_action.style.textAlign = 'center';
        call_to_action.innerText = 'Click on the element you want to monitor.';

        // Found element
        const found_element = document.createElement("div");
        form.appendChild(found_element);

        found_element.style.display = 'none';

        // Name field
        const nameLabel = document.createElement("label");
        form.appendChild(nameLabel);

        nameLabel.innerText = 'Name';
        nameLabel.style.display = 'none';

        const nameField = document.createElement("input");
        form.appendChild(nameField);

        nameField.placeholder = 'Insert monitor name';
        nameField.type = 'text';
        nameField.style.display = 'none';
        nameField.id = 'plazaAddonNameField';

        nameLabel.htmlFor = nameField.id;

        // Submit button
        const submit_button = document.createElement("button");
        form.appendChild(submit_button);

        submit_button.innerText = 'Submit';
        submit_button.style.display = 'none';

        // Cancel button
        const cancel_button = document.createElement("button");
        form.appendChild(cancel_button);

        cancel_button.innerText = 'Cancel';
        
        return {
            container,
            screen,
            nameField,
            nameLabel,
            call_to_action,
            found_element,
            submit_button,
            cancel_button,
        };
    }


    try {
        const selector_screen = new SelectorScreen();
        selector_screen.draw();

        const entry = chrome;
        entry.runtime.onMessage.addListener(function(message, _sender, _sendResponse) {
            var scriptOptions = message.plazaInjectedOptions;
            if (scriptOptions) {
                selector_screen.set_options(scriptOptions);
            }
        });
    }
    catch (e){
        console.error(e);
        debugger;
    }
})();
