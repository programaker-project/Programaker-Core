#!/bin/sh

# Check if parameter passed
if [ -z "$1" ];then
    echo "Usage: $0 <backup sha1sum> '<' backup-file"
    exit 1
fi

# Check if it's tty or stdin is file
if [ -t 0 ];then
    echo "Detected TTY as input, try to send a file"
    echo "Usage: $0 <backup sha1sum> '<' backup-file"
    exit 1
fi

set -eu

VM_ARGS_PATH=/app/release/releases/0.0.1/vm.args
COOKIE=`grep '^-setcookie' "${VM_ARGS_PATH}"| cut -d\  -f2`

BACKUP_FILE=/tmp/backup_${RANDOM}_${RANDOM}

cat > "$BACKUP_FILE"

checksum=`sha1sum "$BACKUP_FILE" |cut -d\  -f1`

set +e
[ "$checksum" = "$1" ]
OK=$?
set -e

if [ $OK -ne 0 ];then
    echo "Checksum error: found    '$checksum'"
    echo "                expected '$1'"
    exit 2
fi

set -e

echo "-export(main/1).

main(_) ->
  {ok, _} = net_kernel:start(['dummy-$RANDOM@`hostname -f`', longnames]),
  true = erlang:set_cookie(node(), '$COOKIE'),
  true = net_kernel:connect_node('backend@`hostname -f`'),
  {atomic, _} = rpc:call('backend@`hostname -f`', mnesia, restore, ['$BACKUP_FILE', []]),
ok.
" > restore.tmp.escript

escript restore.tmp.escript

rm "$BACKUP_FILE"

echo 'Restoration complete, now restart the backend pods'
