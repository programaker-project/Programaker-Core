#!/bin/sh

set -eu

cd "$(dirname "$0")"

cd ../../frontend

TAG=`git rev-parse HEAD`

LOCAL_NAME="plaza-frontend-ci-base-preparation:$TAG"
REMOTE_NAME="plazaproject/ci-base-frontend:$TAG"

docker build -t "${LOCAL_NAME}" --target ci-base .

docker tag "${LOCAL_NAME}" "${REMOTE_NAME}"

echo "Preparation ready, push the image with: docker push '${REMOTE_NAME}'"
