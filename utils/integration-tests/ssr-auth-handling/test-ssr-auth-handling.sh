#!/usr/bin/env bash

cd "$(dirname "$0")"

BACKIMG="$1" # ex: registry.gitlab.com/programaker-project/programaker-core/backend:41a5732bf19b2ce527da9e20675f49c464425fef
FRONTIMG="$2" # ex: registry.gitlab.com/programaker-project/programaker-core/programaker-frontend:41a5732bf19b2ce527da9e20675f49c464425fef
if [ -z "$BACKIMG" ] || [ -z "$FRONTIMG" ];then
    echo "$0 <BACKEND IMAGE> <FRONTEND IMAGE>"
    exit 1
fi

set -eu -o pipefail -o functrace

make

CLI=plaza-testbench-master/cli/cli

clean_and_exit(){
    trap - SIGINT SIGTERM ERR EXIT

    cleanup

    exit 1
}

failure() {
    local lineno=$1
    local msg=$2
    echo "[ERROR] Failed at $lineno: $msg"
}

cleanup() {
    trap - SIGINT SIGTERM ERR EXIT

    echo "==> Cleaning up"
    set +eu
    set -x

    docker rm -f "back-$TESTID"
    docker rm -f "front-$TESTID"
}

trap clean_and_exit SIGINT SIGTERM EXIT
trap 'failure ${LINENO} "$BASH_COMMAND"' ERR

TESTID="$RANDOM$RANDOM$RANDOM$RANDOM"


case "${CI_TYPE:-}" in
    gitlab)
        # Consider: https://stackoverflow.com/a/54252215
        export BACK_DOCKER=`docker run -d --name="back-$TESTID" --rm -p 8888:8888 "$BACKIMG"`
        export FRONT_DOCKER=`docker run -d --name="front-$TESTID" --link="$BACK_DOCKER":plaza-backend -p 80:80 --rm "$FRONTIMG"`
        BACK_HOST="docker"  # Given by DockerInDocker
        FRONT_HOST="docker" # Given by DockerInDocker
        FRONT_PORT=80
        ;;

    *)
        # Expect local execution
        export BACK_DOCKER=`docker run -d --name="back-$TESTID" --rm "$BACKIMG"`
        export FRONT_DOCKER=`docker run -d --name="front-$TESTID" --link="$BACK_DOCKER":plaza-backend --rm "$FRONTIMG"`
        BACK_HOST=`docker exec "$BACK_DOCKER" hostname -i|tr -d '\r\n'`
        FRONT_HOST=`docker exec "$FRONT_DOCKER" hostname -i|tr -d '\r\n'`
        FRONT_PORT=80
esac

# xfce4-terminal -H -e "docker logs -f $FRONT_DOCKER" &

# Re-wire the back's 80 to 8888 to support the default programaker front configuration
docker exec "$BACK_DOCKER" sh -c 'apk add socat && socat tcp-listen:80,reuseaddr,fork tcp-connect:127.0.0.1:8888' &

export PLAZA_ROOT="http://$BACK_HOST:8888"

for i in `seq 1 60`;do
    curl -s "$PLAZA_ROOT"/api/v0/ping >> /dev/null && break || sleep 1
done


USERNAME=test_$TESTID
PASSWD=test_$TESTID

$CLI register -name $USERNAME -password $PASSWD -email $USERNAME@test.com
$CLI login -name $USERNAME -password $PASSWD
TOKEN=`$CLI login -name $USERNAME -password $PASSWD|jq -r .token`

echo "=> Token: $TOKEN"

export FRONT="http://$FRONT_HOST:$FRONT_PORT"

LOGGED_SETTINGS=`links -receive-timeout 10 -retries 1 -dump "$FRONT/settings" -http.extra-header "Cookie: programaker-auth=$TOKEN"`
echo -e "=> Logged settings\n-------------\n$LOGGED_SETTINGS\n-------------\n\n\n"

ERRONEOUS_SETTINGS=`links -receive-timeout 10 -retries 1 -dump "$FRONT/settings" -http.extra-header "Cookie: programaker-auth=A-NON-EXISTING-TOKEN"`
echo -e "=> Token error settings\n-------------\n$ERRONEOUS_SETTINGS\n-------------\n\n\n"

ANONYMOUS_SETTINGS=`links -receive-timeout 10 -retries 1 -dump "$FRONT/settings"`
echo -e "=> Anonymous settings\n-------------\n$ANONYMOUS_SETTINGS\n-------------\n\n\n"


assert_unexpected_expected() {
    # $1 text
    # $2 Unexpected
    # $3 Expected
    if echo "$1" | grep -q "$2" ;then
        echo -e "Error: Found an unexpected '$2' in\n\n-------------\n$1\n" 1>&2
        exit 1
    else
        if echo "$1" | grep -q "$3" ;then
            true
        else
            echo -e "Error: Not found the expected '$3' in\n\n-------------\n$1\n" 1>&2
            exit 1
        fi
    fi
}

assert_logged() {
    assert_unexpected_expected "$1" Login 'Profile info'
}

assert_anonymous() {
    assert_unexpected_expected "$1" 'Profile info' Login
}

assert_logged "$LOGGED_SETTINGS"
assert_anonymous "$ERRONEOUS_SETTINGS"
assert_anonymous "$ANONYMOUS_SETTINGS"

echo "=> Completed successfully"


cleanup
