#!/bin/sh

VM_ARGS_PATH=/app/release/releases/0.0.1/vm.args
cookie=`grep '^-setcookie' "${VM_ARGS_PATH}"|sed -r 's/^-setcookie +("([^"]+)"|([^ ]+)) *$/\2\3/'`

echo 'Write `nodes()` to show where are you connected to.'
erl -setcookie "$cookie" -hidden -remsh "backend@`hostname -f`"
