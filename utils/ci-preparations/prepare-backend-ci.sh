#!/bin/sh

set -eu

cd "$(dirname "$0")"

cd ../../backend

TAG=`git rev-parse HEAD`

LOCAL_NAME="plaza-backend-ci-base-preparation:$TAG"
REMOTE_NAME="plazaproject/ci-base-backend:$TAG"

docker build -t "${LOCAL_NAME}" --target plaza-backend-ci-base .

docker tag "${LOCAL_NAME}" "${REMOTE_NAME}"

echo "Preparation ready, push the image with: docker push '${REMOTE_NAME}'"
