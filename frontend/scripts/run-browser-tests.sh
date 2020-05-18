#!/bin/sh

set -eux

npm install . && make

COMMAND=${TEST_COMMAND:-test-web}
export CHROME_BIN=`dirname "$0"`/launch-browser.sh

export DISPLAY=:99

Xvfb $DISPLAY &
npm run $COMMAND
