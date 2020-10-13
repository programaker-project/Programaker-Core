#!/bin/sh

set -eu

cd "$(dirname "$0")"

cd ../../backend

TAG=`git rev-parse HEAD`

LOCAL_NAME="programaker-backend-ci-base-preparation:$TAG"
REMOTE_NAME="programakerproject/ci-base-backend:$TAG"

docker build --no-cache -t "${LOCAL_NAME}" --target programaker-backend-ci-base .

docker tag "${LOCAL_NAME}" "${REMOTE_NAME}"

echo "Preparation ready, push the image with: docker push '${REMOTE_NAME}'"
