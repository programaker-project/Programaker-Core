#!/bin/sh

set -eu

cd "$(dirname "$0")"

cd ../../frontend

TAG=`git rev-parse HEAD`

LOCAL_NAME="plaza-frontend-ci-base-preparation:$TAG"
REMOTE_NAME="plazaproject/ci-base-frontend:$TAG"

BROWSER_LOCAL_NAME="plaza-frontend-ci-base-preparation-browser:$TAG"
BROWSER_REMOTE_NAME="plazaproject/ci-base-frontend-browser:$TAG"

set -x

docker build --no-cache -t "${LOCAL_NAME}" --target ci-base .

docker tag "${LOCAL_NAME}" "${REMOTE_NAME}"

BROWSER_PARTIAL=scripts/frontend-browser-ci-partial.dockerfile
echo "FROM ${REMOTE_NAME}" > "${BROWSER_PARTIAL}"
cat >> "${BROWSER_PARTIAL}" <<EOF

# Add required dependencies
RUN apk add --no-cache xvfb chromium
EOF

docker build -t "${BROWSER_LOCAL_NAME}" -f "${BROWSER_PARTIAL}" .
rm -f "${BROWSER_PARTIAL}"

docker tag "${BROWSER_LOCAL_NAME}" "${BROWSER_REMOTE_NAME}"

set +x

echo "Preparation ready, push the images"
echo "  - docker push '${REMOTE_NAME}'"
echo "  - docker push '${BROWSER_REMOTE_NAME}'"
