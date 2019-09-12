#!/usr/bin/env bash

set -eux

TARGET=${1:-/app}

find "$TARGET" -name '*.erl' -exec /opt/erlang-formatter/fmt.sh {} \;
