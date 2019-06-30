#!/bin/sh

VM_ARGS_PATH=/app/release/releases/0.0.1/vm.args
cookie=`grep '^-setcookie' "${VM_ARGS_PATH}" |cut -d\" -f2`
echo 'Write `nodes()` to show where are you connected to.'
erl -setcookie "$cookie" -hidden -sname dummy-$RANDOM -remsh "backend@`hostname`"
