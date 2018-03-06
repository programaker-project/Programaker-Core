'use strict';


Blockly.Blocks['chat_whenreceivecommand'] = {
    init: function() {
        this.jsonInit({
            "id": "chat_whenreceivecommand",
            "message0": "When received command %1",
            "args0": [
                {
                    "type": "input_value",
                    "name": "VALUE"
                }
            ],
            "category": Blockly.Categories.event,
            "extensions": ["colours_chat", "shape_hat"]
        });
    }
};

Blockly.Blocks['chat_say'] = {
    init: function() {
        this.jsonInit({
            "id": "chat_say",
            "message0": "Say %1",
            "args0": [
                {
                    "type": "input_value",
                    "name": "VALUE"
                }
            ],
            "category": Blockly.Categories.event,
            "extensions": ["colours_chat", "shape_statement"]
        });
    }
};

Blockly.Extensions.register('colours_chat',
                            function() {
                                this.setColourFromRawValues_('#5555CC', '#333377', '#0000FF');
                            });
