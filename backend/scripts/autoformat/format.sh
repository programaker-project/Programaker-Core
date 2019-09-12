#!/usr/bin/env bash

cd "$( dirname "$0" )"

IMAGE_NAME=erlang-formatter

set -eux

docker build -t "$IMAGE_NAME" .
cd ../../apps/
docker run -it --rm -v `pwd`:/app "$IMAGE_NAME" /app
