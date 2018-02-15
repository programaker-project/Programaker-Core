import copy
import telegram
import command_parser
import json

def can_run_alternative(update, alternative, variables):
    text = update.message.text
    if alternative[0].type == 'chat_whenreceivecommand':
        if text == alternative[0].fields[0].value:
            return True

    return False

def run_program_with_offset(bot, update, program, state, update_offset):
    chat = update.message.chat_id
    state = copy.deepcopy(state)

    while update_offset < len(program):
        instruction = program[update_offset]
        if instruction.type == 'chat_say':
            bot.send_message(chat, instruction.fields[0].value)

        elif instruction.type == 'control_repeat':
            for i in range(int(instruction.fields[0].value)):
                run_program_with_offset(bot, update, instruction.inner_program, state, 0)

        update_offset += 1
    return update_offset, state

def run_alternative(bot, update, program, state):
    new_offset, state = run_program_with_offset(bot, update, program, state, 1)

    return state


def run(token, serialized_commands, state):
    if state is None:
        state = {}
    else:
        state = json.loads(state)

    update_offset = state.get('update_offset', 0)

    bot = telegram.Bot(token)
    commands = command_parser.parse(serialized_commands)
    updates = bot.get_updates(offset=update_offset + 1)

    for update in updates:
        for alternative in commands.ast:
            if can_run_alternative(update, alternative, commands.variables):
                state = run_alternative(bot, update, alternative, state)
                break


        state['update_offset'] = max(update.update_id, update_offset)
    return json.dumps(state)
