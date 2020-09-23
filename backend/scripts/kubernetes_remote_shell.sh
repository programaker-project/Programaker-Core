#!/bin/sh

VM_ARGS_PATH=/app/release/releases/0.0.1/vm.args
cookie=`grep '^-setcookie' "${VM_ARGS_PATH}"|sed -r 's/^-setcookie +("([^"]+)"|([^ ]+)) *$/\2\3/'`
nametype=`grep -Eo '^-s?name' "${VM_ARGS_PATH}"`

if [ -z "$nametype" ];then
    echo "Error: Cannot get name type from ${VM_ARGS_PATH}" 2>>/dev/null
    echo "       No '-sname' or '-name' line found." 2>>/dev/null
    exit 2
fi

echo 'Write `nodes()` to show where are you connected to.'
erl -setcookie "$cookie" $nametype remote$RANDOM$RANDOM$RANDOM -hidden -remsh "backend@`hostname -f`"
