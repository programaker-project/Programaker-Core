#!/bin/sh

usage() {
         echo "Usage: $0 <command>" >&2
        }

if [ -z "$1" ];then
    usage
    exit 1
fi

VM_ARGS_PATH=/app/release/releases/0.0.1/vm.args
cookie=`grep '^-setcookie' "${VM_ARGS_PATH}"|sed -r 's/^-setcookie +("([^"]+)"|([^ ]+)) *$/\2\3/'`
nametype=`grep -Eo '^-s?name ' "${VM_ARGS_PATH}"`

node=backend@`hostname -f`

erl -setcookie "$cookie" -hidden $nametype "dummy-$RAND@`hostname -f`" -remsh "$node" \
    -eval "io:fwrite(\"~p~n\", [erpc:call('$node', fun() -> $1 end)]), erlang:halt()." \
    -noinput # Setting the nametype is required for `-noinput` to work correctly
