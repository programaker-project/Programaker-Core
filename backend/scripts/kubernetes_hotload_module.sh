if [ -z "$3" ]
then
    echo "Usage: $0 <path to module.erl> <K8S namespace> <K8S pod name> [<K8S pod name> ...]"
    exit 0
fi

echo "$1"|grep -q '.erl$'
if [ $? -ne 0 ]
then
    echo "Error: Module argument must be an .erl file"
    exit 1
fi

set -e

SOURCE_NAME="$1"
MODULE_NAME=$(basename "$1"|sed 's/.erl$//')
COMPILED_FILE=$MODULE_NAME.beam
K8S_NAMESPACE="$2"

echo "= Compiling..."
erl -noinput -noshell -eval 'io:fwrite("Compiling: ~p~n", [compile:file("'"$SOURCE_NAME"'")]),erlang:halt().'

if [ ! -f "$COMPILED_FILE" ]
then
    echo "Error: Not found expected result file"
    exit 2
fi


# Skip two first arguments to expose the pod names
shift 2

for pod in "$@"
do
    echo ""
    echo "Pod: $pod"
    echo "= Uploading..."
    kubectl -n "$K8S_NAMESPACE" cp "$COMPILED_FILE" "$pod:/tmp"

    echo "  Checking..."
    kubectl -n "$K8S_NAMESPACE" exec "$pod" -- ls /tmp/"$COMPILED_FILE" >/dev/null

    # Here comes the trick...
    echo "= Loading..."
    kubectl -n "$K8S_NAMESPACE" exec "$pod" -- ash -xc "/app/scripts/run_erl.sh 'code:load_abs(\"/tmp/$MODULE_NAME\")' && /app/scripts/run_erl.sh 'code:soft_purge($MODULE_NAME)'"
done
