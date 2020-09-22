#!/bin/sh

cd "$(dirname "$0")"

IMAGE="$1"
if [ -z "$IMAGE" ];then
    echo "$0 <BACKEND IMAGE>"
    exit 1
fi

pip3 install -U --user -r requirements.txt || exit 1

cleanup(){
    docker rm -f "$DOCKER"
}


case "${CI_TYPE}" in
    gitlab)
        # Consider: https://stackoverflow.com/a/54252215
        export DOCKER=`docker run --rm -d -p 8888:8888 "$IMAGE"`
        DOCKER_HOST="docker" # Given by DockerINDocker
        ;;

    *)
        # Expect local execution
        export DOCKER=`docker run --rm -d "$IMAGE"`
        DOCKER_HOST=`docker exec "$DOCKER" hostname -i|tr -d '\r\n'`
esac

export API_TEST_ROOT="http://${DOCKER_HOST}:8888/api/v0"
export API_TEST_DOCKER="$DOCKER"

python3 gen_test_api_plan.py || cleanup

(docker logs -f "$DOCKER" > api_test_logs.txt; echo "Docker stopped" >&2) &

# Wait for the API to be ready
for i in `seq 1 60`;do
    curl -s "${API_TEST_ROOT}/ping" >>/dev/null && break
    printf '.' >&2
    sleep 1
done

curl -s "${API_TEST_ROOT}/ping" >>/dev/null
if [ $? -ne 0 ];then
    cleanup
    exit 1
fi

python3 run_plan.py plan.gv

ECODE=$?

cleanup
exit $ECODE
