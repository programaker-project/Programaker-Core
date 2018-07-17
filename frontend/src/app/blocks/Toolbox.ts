import { MonitorService } from '../monitor.service';
import { MonitorMetadata } from '../monitor';
declare const Blockly;

const MonitorPrimaryColor = '#CC55CC';
const MonitorSecondaryColor = '#773377';

export class Toolbox {
    monitorService: MonitorService;

    constructor(
        monitorService: MonitorService
    ) {
        this.monitorService = monitorService;
    }

    async inject(): Promise<void> {
        const monitors = await this.monitorService.getMonitors();

        this.injectBlocks(monitors);
        this.injectToolbox(monitors);
    }

    injectBlocks(monitors: MonitorMetadata[]) {
        this.injectChatBlocks();
        this.injectMonitorBlocks(monitors);
    }

    injectChatBlocks() {
        Blockly.Blocks['chat_whenreceivecommand'] = {
            init: function() {
                this.jsonInit({
                    'id': 'chat_whenreceivecommand',
                    'message0': 'When received command %1',
                    'args0': [
                        {
                            'type': 'input_value',
                            'name': 'VALUE'
                        }
                    ],
                    'category': Blockly.Categories.event,
                    'extensions': ['colours_chat', 'shape_hat']
                });
            }
        };

        Blockly.Blocks['chat_say'] = {
            init: function() {
                this.jsonInit({
                    'id': 'chat_say',
                    'message0': 'Say %1',
                    'args0': [
                        {
                            'type': 'input_value',
                            'name': 'VALUE'
                        }
                    ],
                    'category': Blockly.Categories.event,
                    'extensions': ['colours_chat', 'shape_statement']
                });
            }
        };

        Blockly.Extensions.register('colours_chat',
                            function() {
                                this.setColourFromRawValues_('#5555CC', '#333377', '#0000FF');
                            });
   }

    injectMonitorBlocks(monitors: MonitorMetadata[]) {
        for (const monitor of monitors) {
            Blockly.Blocks['monitor.retrieve.' + monitor.id] = {
                init: function() {
                    this.jsonInit({
                        'id': 'monitor.retrieve.' + monitor.id,
                        'message0': 'Get ' + monitor.name,
                        'category': Blockly.Categories.event,
                        'extensions': ['colours_monitor', 'output_string']
                    });
                }
            };
        }

        if (monitors.length > 0) {
            Blockly.Extensions.register('colours_monitor',
                                        function() {
                                            this.setColourFromRawValues_(MonitorPrimaryColor,
                                                                         MonitorSecondaryColor,
                                                                         '#FF00FF');
                                        });
        }
    }

    injectToolbox(monitors: MonitorMetadata[]) {
        (goog as any).provide('Blockly.Blocks.defaultToolbox');

        (goog as any).require('Blockly.Blocks');

        Blockly.Blocks.factoryDefaultToolbox = Blockly.Blocks.defaultToolbox;

        const chatCategory = '<category name="Chat" colour="#5555CC" secondaryColour="#333377">' +
        '<block type="chat_whenreceivecommand" id="chat_whenreceivecommand">' +
          '<value name="VALUE">' +
            '<shadow type="text">' +
              '<field name="TEXT">/start</field>' +
            '</shadow>' +
          '</value>' +
        '</block>' +
        '<block type="chat_say" id="chat_say">' +
          '<value name="VALUE">' +
            '<shadow type="text">' +
              '<field name="TEXT">Hello!</field>' +
            '</shadow>' +
          '</value>' +
        '</block>' +
        '</category>';

        const eventsCategory = ''; //   '<category name="Events" colour="#FFD500" secondaryColour="#CC9900">' +
        //   '<block type="event_broadcast" id="event_broadcast">' +
        //     '<value name="BROADCAST_INPUT">' +
        //       '<shadow type="event_broadcast_menu"></shadow>' +
        //     '</value>' +
        //   '</block>' +
        //   '<block type="event_broadcastandwait" id="event_broadcastandwait">' +
        //     '<value name="BROADCAST_INPUT">' +
        //       '<shadow type="event_broadcast_menu"></shadow>' +
        //     '</value>' +
        //   '</block>' +
        // '</category>'

        const controlCategory = '<category name="Control" colour="#FFAB19" secondaryColour="#CF8B17">' +
        '<block type="control_wait" id="control_wait">' +
          '<value name="DURATION">' +
            '<shadow type="math_positive_number">' +
              '<field name="NUM">1</field>' +
            '</shadow>' +
          '</value>' +
        '</block>' +
        '<block type="control_repeat" id="control_repeat">' +
          '<value name="TIMES">' +
            '<shadow type="math_whole_number">' +
              '<field name="NUM">10</field>' +
            '</shadow>' +
          '</value>' +
        '</block>' +
        // '<block type="control_forever" id="control_forever"></block>' +
        // '<block type="control_if" id="control_if"></block>' +
        // '<block type="control_if_else" id="control_if_else"></block>' +
        // '<block type="control_wait_until" id="control_wait_until"></block>' +
        // '<block type="control_repeat_until" id="control_repeat_until"></block>' +
        // '<block type="control_stop" id="control_stop"></block>' +
      //   '<block type="control_start_as_clone" id="control_start_as_clone"></block>' +
      //   '<block type="control_create_clone_of" id="control_create_clone_of">' +
      //     '<value name="CLONE_OPTION">' +
      //       '<shadow type="control_create_clone_of_menu"></shadow>' +
      //     '</value>' +
      //   '</block>' +
      //   '<block type="control_delete_this_clone" id="control_delete_this_clone"></block>' +
      '</category>';

        const operatorsCategory =           '<category name="Operators" colour="#40BF4A" secondaryColour="#389438">' +
        //     '<block type="operator_add" id="operator_add">' +
        //       '<value name="NUM1">' +
        //         '<shadow type="math_number">' +
        //           '<field name="NUM"></field>' +
        //         '</shadow>' +
        //       '</value>' +
        //       '<value name="NUM2">' +
        //         '<shadow type="math_number">' +
        //           '<field name="NUM"></field>' +
        //         '</shadow>' +
        //       '</value>' +
        //     '</block>' +
        //     '<block type="operator_subtract" id="operator_subtract">' +
        //       '<value name="NUM1">' +
        //         '<shadow type="math_number">' +
        //           '<field name="NUM"></field>' +
        //         '</shadow>' +
        //       '</value>' +
        //       '<value name="NUM2">' +
        //         '<shadow type="math_number">' +
        //           '<field name="NUM"></field>' +
        //         '</shadow>' +
        //       '</value>' +
        //     '</block>' +
        //     '<block type="operator_multiply" id="operator_multiply">' +
        //       '<value name="NUM1">' +
        //         '<shadow type="math_number">' +
        //           '<field name="NUM"></field>' +
        //         '</shadow>' +
        //       '</value>' +
        //       '<value name="NUM2">' +
        //         '<shadow type="math_number">' +
        //           '<field name="NUM"></field>' +
        //         '</shadow>' +
        //       '</value>' +
        //     '</block>' +
        //     '<block type="operator_divide" id="operator_divide">' +
        //       '<value name="NUM1">' +
        //         '<shadow type="math_number">' +
        //           '<field name="NUM"></field>' +
        //         '</shadow>' +
        //       '</value>' +
        //       '<value name="NUM2">' +
        //         '<shadow type="math_number">' +
        //           '<field name="NUM"></field>' +
        //         '</shadow>' +
        //       '</value>' +
        //     '</block>' +
        //     '<block type="operator_random" id="operator_random">' +
        //       '<value name="FROM">' +
        //         '<shadow type="math_number">' +
        //           '<field name="NUM">1</field>' +
        //         '</shadow>' +
        //       '</value>' +
        //       '<value name="TO">' +
        //         '<shadow type="math_number">' +
        //           '<field name="NUM">10</field>' +
        //         '</shadow>' +
        //       '</value>' +
        //     '</block>' +
        //     '<block type="operator_lt" id="operator_lt">' +
        //       '<value name="OPERAND1">' +
        //         '<shadow type="text">' +
        //           '<field name="TEXT"></field>' +
        //         '</shadow>' +
        //       '</value>' +
        //       '<value name="OPERAND2">' +
        //         '<shadow type="text">' +
        //           '<field name="TEXT"></field>' +
        //         '</shadow>' +
        //       '</value>' +
        //     '</block>' +
        //     '<block type="operator_equals" id="operator_equals">' +
        //       '<value name="OPERAND1">' +
        //         '<shadow type="text">' +
        //           '<field name="TEXT"></field>' +
        //         '</shadow>' +
        //       '</value>' +
        //       '<value name="OPERAND2">' +
        //         '<shadow type="text">' +
        //           '<field name="TEXT"></field>' +
        //         '</shadow>' +
        //       '</value>' +
        //     '</block>' +
        //     '<block type="operator_gt" id="operator_gt">' +
        //       '<value name="OPERAND1">' +
        //         '<shadow type="text">' +
        //           '<field name="TEXT"></field>' +
        //         '</shadow>' +
        //       '</value>' +
        //       '<value name="OPERAND2">' +
        //         '<shadow type="text">' +
        //           '<field name="TEXT"></field>' +
        //         '</shadow>' +
        //       '</value>' +
        //     '</block>' +
        //     '<block type="operator_and" id="operator_and"></block>' +
        //     '<block type="operator_or" id="operator_or"></block>' +
        //     '<block type="operator_not" id="operator_not"></block>' +
            '<block type="operator_join" id="operator_join">' +
              '<value name="STRING1">' +
                '<shadow type="text">' +
                  '<field name="TEXT">hello</field>' +
                '</shadow>' +
              '</value>' +
              '<value name="STRING2">' +
                '<shadow type="text">' +
                  '<field name="TEXT">world</field>' +
                '</shadow>' +
              '</value>' +
            '</block>' +
        //     '<block type="operator_letter_of" id="operator_letter_of">' +
        //       '<value name="LETTER">' +
        //         '<shadow type="math_whole_number">' +
        //           '<field name="NUM">1</field>' +
        //         '</shadow>' +
        //       '</value>' +
        //       '<value name="STRING">' +
        //         '<shadow type="text">' +
        //           '<field name="TEXT">world</field>' +
        //         '</shadow>' +
        //       '</value>' +
        //     '</block>' +
        //     '<block type="operator_length" id="operator_length">' +
        //       '<value name="STRING">' +
        //         '<shadow type="text">' +
        //           '<field name="TEXT">world</field>' +
        //         '</shadow>' +
        //       '</value>' +
        //     '</block>' +
        //     '<block type="operator_contains" id="operator_contains">' +
        //       '<value name="STRING1">' +
        //         '<shadow type="text">' +
        //           '<field name="TEXT">hello</field>' +
        //         '</shadow>' +
        //       '</value>' +
        //       '<value name="STRING2">' +
        //         '<shadow type="text">' +
        //           '<field name="TEXT">world</field>' +
        //         '</shadow>' +
        //       '</value>' +
        //     '</block>' +
        //     '<block type="operator_mod" id="operator_mod">' +
        //       '<value name="NUM1">' +
        //         '<shadow type="math_number">' +
        //           '<field name="NUM"></field>' +
        //         '</shadow>' +
        //       '</value>' +
        //       '<value name="NUM2">' +
        //         '<shadow type="math_number">' +
        //           '<field name="NUM"></field>' +
        //         '</shadow>' +
        //       '</value>' +
        //     '</block>' +
        //     '<block type="operator_round" id="operator_round">' +
        //       '<value name="NUM">' +
        //         '<shadow type="math_number">' +
        //           '<field name="NUM"></field>' +
        //         '</shadow>' +
        //       '</value>' +
        //     '</block>' +
        //     '<block type="operator_mathop" id="operator_mathop">' +
        //       '<value name="NUM">' +
        //         '<shadow type="math_number">' +
        //           '<field name="NUM"></field>' +
        //         '</shadow>' +
        //       '</value>' +
        //     '</block>' +
          '</category>';

        const variablesCategory = '<category name="Variables" colour="#FF8C1A" secondaryColour="#DB6E00" custom="VARIABLE">' +
        '</category>';

        const proceduresCategory = '' // '<category name="More" colour="#FF6680" secondaryColour="#FF4D6A" custom="PROCEDURE">' +
        // '</category>';

        const monitorsCategory = this.buildMonitorsCategory(monitors);

        Blockly.Blocks.defaultToolbox = [
            '<xml id="toolbox-categories" style="display: none">',
            chatCategory,
            eventsCategory,
            controlCategory,
            monitorsCategory,
            operatorsCategory,
            variablesCategory,
            proceduresCategory,
            '</xml>'].join('\n');
    }

    buildMonitorsCategory(monitors: MonitorMetadata[]): string {
        if (monitors.length === 0) {
            return '';
        }

        const category = [
            `<category name="Monitors"
                       colour="${MonitorPrimaryColor}"
                       secondaryColour="${MonitorSecondaryColor}">`
        ];

        for (const monitor of monitors) {
            const block = `<block type="monitor.retrieve.${monitor.id}"
                                  id="monitor.retrieve.${monitor.id}">
                           </block>`;

            category.push(block);
        }

        category.push('</category>');
        return category.join('\n');
    }
}