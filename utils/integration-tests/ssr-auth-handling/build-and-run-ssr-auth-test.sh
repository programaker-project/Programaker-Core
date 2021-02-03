#!/usr/bin/env bash

cd "$(dirname "$0")"

set -eu
set -x

BACKEND_IMAGE_UNOP=ssr-auth-test-back-base
BACKEND_IMAGE=ssr-auth-test-back-final
FRONTEND_IMAGE=ssr-auth-test-front-final

cd ../../../backend/

docker build -t "$BACKEND_IMAGE_UNOP" -f scripts/ci-partial.dockerfile .
sh ../utils/ci-preparations/optimize-backend-image.sh $BACKEND_IMAGE_UNOP $BACKEND_IMAGE

cd ../frontend
docker build --build-arg BUILD_COMMAND=build:programaker-ssr  -t $FRONTEND_IMAGE  -f scripts/ci-partial-ssr.dockerfile .

cd ../utils/integration-tests/ssr-auth-handling
bash test-ssr-auth-handling.sh "$BACKEND_IMAGE" "$FRONTEND_IMAGE"
