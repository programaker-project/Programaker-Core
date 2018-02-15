#!/usr/bin/env python3

import chatbot_runner
import time
import db
SLEEP_TIME = 10

def run():
    for (token, commands, state) in db.get_chatbots():
        state = chatbot_runner.run(token, commands, state)
        db.set_state(token, state)

def main():
    while True:
        run()
        time.sleep(SLEEP_TIME)

if __name__ == '__main__':
    main()
