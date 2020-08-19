#!/bin/sh

set -eu

# Start by pulling normal dependencies
rebar3 get-deps

# Then retrieve eargon2's submodules
cd _build/default/lib/eargon2/
git submodule  update  --init --recursive
