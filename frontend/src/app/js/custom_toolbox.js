/**
 * @license
 * Visual Blocks Editor
 *
 * Copyright 2016 Massachusetts Institute of Technology
 * All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

'use strict';

goog.provide('Blockly.Blocks.defaultToolbox');

goog.require('Blockly.Blocks');

/**
 * @fileoverview Provide a default toolbox XML.
 */
Blockly.Blocks.factoryDefaultToolbox = Blockly.Blocks.defaultToolbox;

Blockly.Blocks.defaultToolbox = '<xml id="toolbox-categories" style="display: none">'+
  '<category name="Chat" colour="#5555CC" secondaryColour="#333377">'+
    '<block type="chat_whenreceivecommand" id="chat_whenreceivecommand">'+
      '<value name="VALUE">'+
        '<shadow type="text">'+
          '<field name="TEXT">/start</field>'+
        '</shadow>'+
      '</value>'+
    '</block>'+
    '<block type="chat_say" id="chat_say">'+
      '<value name="VALUE">'+
        '<shadow type="text">'+
          '<field name="TEXT">Hello!</field>'+
        '</shadow>'+
      '</value>'+
    '</block>'+
    '</category>'+
  //   '<category name="Events" colour="#FFD500" secondaryColour="#CC9900">'+
  //   '<block type="event_broadcast" id="event_broadcast">'+
  //     '<value name="BROADCAST_INPUT">'+
  //       '<shadow type="event_broadcast_menu"></shadow>'+
  //     '</value>'+
  //   '</block>'+
  //   '<block type="event_broadcastandwait" id="event_broadcastandwait">'+
  //     '<value name="BROADCAST_INPUT">'+
  //       '<shadow type="event_broadcast_menu"></shadow>'+
  //     '</value>'+
  //   '</block>'+
  // '</category>'+
  '<category name="Control" colour="#FFAB19" secondaryColour="#CF8B17">'+
    '<block type="control_wait" id="control_wait">'+
      '<value name="DURATION">'+
        '<shadow type="math_positive_number">'+
          '<field name="NUM">1</field>'+
        '</shadow>'+
      '</value>'+
    '</block>'+
    '<block type="control_repeat" id="control_repeat">'+
      '<value name="TIMES">'+
        '<shadow type="math_whole_number">'+
          '<field name="NUM">10</field>'+
        '</shadow>'+
      '</value>'+
    '</block>'+
    // '<block type="control_forever" id="control_forever"></block>'+
    // '<block type="control_if" id="control_if"></block>'+
    // '<block type="control_if_else" id="control_if_else"></block>'+
    // '<block type="control_wait_until" id="control_wait_until"></block>'+
    // '<block type="control_repeat_until" id="control_repeat_until"></block>'+
    // '<block type="control_stop" id="control_stop"></block>'+
  //   '<block type="control_start_as_clone" id="control_start_as_clone"></block>'+
  //   '<block type="control_create_clone_of" id="control_create_clone_of">'+
  //     '<value name="CLONE_OPTION">'+
  //       '<shadow type="control_create_clone_of_menu"></shadow>'+
  //     '</value>'+
  //   '</block>'+
  //   '<block type="control_delete_this_clone" id="control_delete_this_clone"></block>'+
  '</category>'+
  // '<category name="Sensing" colour="#4CBFE6" secondaryColour="#2E8EB8">'+
  //   '<block type="sensing_touchingobject" id="sensing_touchingobject">'+
  //     '<value name="TOUCHINGOBJECTMENU">'+
  //       '<shadow type="sensing_touchingobjectmenu"></shadow>'+
  //     '</value>'+
  //   '</block>'+
  //   '<block type="sensing_touchingcolor" id="sensing_touchingcolor">'+
  //     '<value name="COLOR">'+
  //       '<shadow type="colour_picker"></shadow>'+
  //     '</value>'+
  //   '</block>'+
  //   '<block type="sensing_coloristouchingcolor" id="sensing_coloristouchingcolor">'+
  //     '<value name="COLOR">'+
  //       '<shadow type="colour_picker"></shadow>'+
  //     '</value>'+
  //     '<value name="COLOR2">'+
  //       '<shadow type="colour_picker"></shadow>'+
  //     '</value>'+
  //   '</block>'+
  //   '<block type="sensing_distanceto" id="sensing_distanceto">'+
  //     '<value name="DISTANCETOMENU">'+
  //       '<shadow type="sensing_distancetomenu"></shadow>'+
  //     '</value>'+
  //   '</block>'+
  //   '<block type="sensing_keypressed" id="sensing_keypressed"></block>'+
  //   '<block type="sensing_mousedown" id="sensing_mousedown"></block>'+
  //   '<block type="sensing_mousex" id="sensing_mousex"></block>'+
  //   '<block type="sensing_mousey" id="sensing_mousey"></block>'+
  //   '<block type="sensing_setdragmode" id="sensing_setdragmode"></block>' +
  //   '<block type="sensing_loudness" id="sensing_loudness"></block>'+
  //   '<block type="sensing_timer" id="sensing_timer"></block>'+
  //   '<block type="sensing_resettimer" id="sensing_resettimer"></block>'+
  //   '<block type="sensing_of" id="sensing_of">'+
  //     '<value name="OBJECT">'+
  //       '<shadow type="sensing_of_object_menu"></shadow>'+
  //     '</value>'+
  //   '</block>'+
  //   '<block type="sensing_current" id="sensing_current"></block>'+
  //   '<block type="sensing_dayssince2000" id="sensing_dayssince2000"></block>'+
  // '</category>'+
  '<category name="Operators" colour="#40BF4A" secondaryColour="#389438">'+
//     '<block type="operator_add" id="operator_add">'+
//       '<value name="NUM1">'+
//         '<shadow type="math_number">'+
//           '<field name="NUM"></field>'+
//         '</shadow>'+
//       '</value>'+
//       '<value name="NUM2">'+
//         '<shadow type="math_number">'+
//           '<field name="NUM"></field>'+
//         '</shadow>'+
//       '</value>'+
//     '</block>'+
//     '<block type="operator_subtract" id="operator_subtract">'+
//       '<value name="NUM1">'+
//         '<shadow type="math_number">'+
//           '<field name="NUM"></field>'+
//         '</shadow>'+
//       '</value>'+
//       '<value name="NUM2">'+
//         '<shadow type="math_number">'+
//           '<field name="NUM"></field>'+
//         '</shadow>'+
//       '</value>'+
//     '</block>'+
//     '<block type="operator_multiply" id="operator_multiply">'+
//       '<value name="NUM1">'+
//         '<shadow type="math_number">'+
//           '<field name="NUM"></field>'+
//         '</shadow>'+
//       '</value>'+
//       '<value name="NUM2">'+
//         '<shadow type="math_number">'+
//           '<field name="NUM"></field>'+
//         '</shadow>'+
//       '</value>'+
//     '</block>'+
//     '<block type="operator_divide" id="operator_divide">'+
//       '<value name="NUM1">'+
//         '<shadow type="math_number">'+
//           '<field name="NUM"></field>'+
//         '</shadow>'+
//       '</value>'+
//       '<value name="NUM2">'+
//         '<shadow type="math_number">'+
//           '<field name="NUM"></field>'+
//         '</shadow>'+
//       '</value>'+
//     '</block>'+
//     '<block type="operator_random" id="operator_random">'+
//       '<value name="FROM">'+
//         '<shadow type="math_number">'+
//           '<field name="NUM">1</field>'+
//         '</shadow>'+
//       '</value>'+
//       '<value name="TO">'+
//         '<shadow type="math_number">'+
//           '<field name="NUM">10</field>'+
//         '</shadow>'+
//       '</value>'+
//     '</block>'+
//     '<block type="operator_lt" id="operator_lt">'+
//       '<value name="OPERAND1">'+
//         '<shadow type="text">'+
//           '<field name="TEXT"></field>'+
//         '</shadow>'+
//       '</value>'+
//       '<value name="OPERAND2">'+
//         '<shadow type="text">'+
//           '<field name="TEXT"></field>'+
//         '</shadow>'+
//       '</value>'+
//     '</block>'+
//     '<block type="operator_equals" id="operator_equals">'+
//       '<value name="OPERAND1">'+
//         '<shadow type="text">'+
//           '<field name="TEXT"></field>'+
//         '</shadow>'+
//       '</value>'+
//       '<value name="OPERAND2">'+
//         '<shadow type="text">'+
//           '<field name="TEXT"></field>'+
//         '</shadow>'+
//       '</value>'+
//     '</block>'+
//     '<block type="operator_gt" id="operator_gt">'+
//       '<value name="OPERAND1">'+
//         '<shadow type="text">'+
//           '<field name="TEXT"></field>'+
//         '</shadow>'+
//       '</value>'+
//       '<value name="OPERAND2">'+
//         '<shadow type="text">'+
//           '<field name="TEXT"></field>'+
//         '</shadow>'+
//       '</value>'+
//     '</block>'+
//     '<block type="operator_and" id="operator_and"></block>'+
//     '<block type="operator_or" id="operator_or"></block>'+
//     '<block type="operator_not" id="operator_not"></block>'+
    '<block type="operator_join" id="operator_join">'+
      '<value name="STRING1">'+
        '<shadow type="text">'+
          '<field name="TEXT">hello</field>'+
        '</shadow>'+
      '</value>'+
      '<value name="STRING2">'+
        '<shadow type="text">'+
          '<field name="TEXT">world</field>'+
        '</shadow>'+
      '</value>'+
    '</block>'+
//     '<block type="operator_letter_of" id="operator_letter_of">'+
//       '<value name="LETTER">'+
//         '<shadow type="math_whole_number">'+
//           '<field name="NUM">1</field>'+
//         '</shadow>'+
//       '</value>'+
//       '<value name="STRING">'+
//         '<shadow type="text">'+
//           '<field name="TEXT">world</field>'+
//         '</shadow>'+
//       '</value>'+
//     '</block>'+
//     '<block type="operator_length" id="operator_length">'+
//       '<value name="STRING">'+
//         '<shadow type="text">'+
//           '<field name="TEXT">world</field>'+
//         '</shadow>'+
//       '</value>'+
//     '</block>'+
//     '<block type="operator_contains" id="operator_contains">'+
//       '<value name="STRING1">'+
//         '<shadow type="text">'+
//           '<field name="TEXT">hello</field>'+
//         '</shadow>'+
//       '</value>'+
//       '<value name="STRING2">'+
//         '<shadow type="text">'+
//           '<field name="TEXT">world</field>'+
//         '</shadow>'+
//       '</value>'+
//     '</block>'+
//     '<block type="operator_mod" id="operator_mod">'+
//       '<value name="NUM1">'+
//         '<shadow type="math_number">'+
//           '<field name="NUM"></field>'+
//         '</shadow>'+
//       '</value>'+
//       '<value name="NUM2">'+
//         '<shadow type="math_number">'+
//           '<field name="NUM"></field>'+
//         '</shadow>'+
//       '</value>'+
//     '</block>'+
//     '<block type="operator_round" id="operator_round">'+
//       '<value name="NUM">'+
//         '<shadow type="math_number">'+
//           '<field name="NUM"></field>'+
//         '</shadow>'+
//       '</value>'+
//     '</block>'+
//     '<block type="operator_mathop" id="operator_mathop">'+
//       '<value name="NUM">'+
//         '<shadow type="math_number">'+
//           '<field name="NUM"></field>'+
//         '</shadow>'+
//       '</value>'+
//     '</block>'+
  '</category>'+
  '<category name="Variables" colour="#FF8C1A" secondaryColour="#DB6E00" custom="VARIABLE">' +
  '</category>' +
  // '<category name="More" colour="#FF6680" secondaryColour="#FF4D6A" custom="PROCEDURE">' +
  // '</category>' +
  '</xml>';
