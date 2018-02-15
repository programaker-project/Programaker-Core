#!/usr/bin/env python3

import traceback
import chatbot_runner
import time
import db
SLEEP_TIME = 10

def run():
    for (token, commands, state) in db.get_chatbots():
        try:
            state = chatbot_runner.run(token, commands, state)
            db.set_state(token, state)
        except:
            traceback.print_exc()

def main():
    while True:
        run()
        time.sleep(SLEEP_TIME)

if __name__ == '__main__':
    main()
