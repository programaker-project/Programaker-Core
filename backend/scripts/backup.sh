#!/bin/sh

set -eu

VM_ARGS_PATH=/app/release/releases/0.0.1/vm.args
COOKIE=`grep '^-setcookie' "${VM_ARGS_PATH}"| cut -d\  -f2`

BACKUP_FILE=/tmp/backup_${RANDOM}_${RANDOM}

echo "-export(main/1).

main(_) ->
  {ok, _} = net_kernel:start(['dummy-$RANDOM@`hostname -f`', longnames]),
  true = erlang:set_cookie(node(), '$COOKIE'),
  true = net_kernel:connect_node('backend@`hostname -f`'),
  rpc:call('backend@`hostname -f`', mnesia, backup, ['$BACKUP_FILE']),
ok.
" > backup.tmp.escript

escript backup.tmp.escript

sha1sum "$BACKUP_FILE" >&2

cat "$BACKUP_FILE"

rm "$BACKUP_FILE"
