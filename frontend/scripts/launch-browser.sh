#!/bin/sh

set -eux

exec "$BROWSER_BIN" $BROWSER_OPTS "$@"
