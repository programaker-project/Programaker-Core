#!/bin/sh

set -eu

cd "$(dirname "$0")/../backend"

TAG_PREFIX=${TAG_PREFIX:-hotfix}
TAG=`date +%Y%m%d-%H%M%S`
PRE_BUILD=backend-pre-$TAG_PREFIX-$TAG

docker build -t $PRE_BUILD -f scripts/ci-partial.dockerfile .

sh ../utils/ci-preparations/optimize-backend-image.sh $PRE_BUILD registry.gitlab.com/programaker-project/programaker-core/backend:$TAG_PREFIX-$TAG

echo "docker push registry.gitlab.com/programaker-project/programaker-core/backend:$TAG_PREFIX-$TAG"
