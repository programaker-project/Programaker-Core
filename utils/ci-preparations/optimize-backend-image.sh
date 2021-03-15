#!/bin/sh

set -eu

SOURCE_IMAGE="$1"
DESTINATION_IMAGE="$2"
TMP_FILE="${3:-OPTIMIZED_IMAGE.dockerfile}"
if [ -f "${TMP_FILE}" ];then # Check that TMP_FILE does not exist
    echo "Unfinished build still on progress? Check $TMP_FILE"
    exit 1
fi

# Check base image of erlang:alpine
TEMPLATE="FROM alpine:3.12 as final

RUN apk add ncurses libstdc++ erlang

COPY --from=${SOURCE_IMAGE} /app/_build/default/rel/automate/ /app/release/
ADD ./scripts/ /app/scripts

# API server port
EXPOSE 8888

# Launch directly the release
CMD [\"/app/scripts/container_init.sh\"]
"

echo "$TEMPLATE" > "${TMP_FILE}"
docker build -t "${DESTINATION_IMAGE}" -f "${TMP_FILE}" .
rm "${TMP_FILE}"
