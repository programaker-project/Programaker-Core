#!/bin/sh

VM_ARGS_PATH=/app/release/releases/0.0.1/vm.args
cookie=`grep '^-setcookie' "${VM_ARGS_PATH}"`
echo 'Write `nodes()` to show where are you connected to.'
erl $cookie -hidden -name dummy-$RANDOM -remsh "backend@`hostname -f`"