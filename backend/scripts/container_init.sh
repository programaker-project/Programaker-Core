#!/bin/sh

NODE_NAME_SUFFIX=${NODE_NAME_SUFFIX:-}
DEFAULT_DIR=/app/mnesia
MNESIA_DIR=${MNESIA_DIR:-$DEFAULT_DIR}

VM_ARGS_PATH=/app/release/releases/0.0.1/vm.args

# Set node name
if [ ! -z "${NODE_NAME_SUFFIX}" ];then
    nodename="`hostname`${NODE_NAME_SUFFIX}"

    echo "NodeName: backend@$nodename"
    sed 's/^-sname.*$/-name '"backend\\@$nodename"'/' -i "${VM_ARGS_PATH}"
else
    sed 's/^-sname.*$/-sname '"backend"'/' -i "${VM_ARGS_PATH}"
fi


# Set erlang cookie
if [ ! -z "${ERLANG_COOKIE}" ];then
    sed 's/^-setcookie.*$/-setcookie '"${ERLANG_COOKIE}"'/' -i "${VM_ARGS_PATH}"
else
    echo 'No cookie set on environment variable, this is probably not safe!'
fi

# Move cookie to HomeDir for easier remote_shell
cookie=`grep '^-setcookie' "${VM_ARGS_PATH}"  |cut -d\" -f2`

# Set mnesia directory
grep ^-mnesia "${VM_ARGS_PATH}" >/dev/null
if [ $? -eq 0 ];then
    sed '/^-mnesia/d' -i "${VM_ARGS_PATH}"
fi

echo $'\n''-mnesia dir '"'\"${MNESIA_DIR}\"'" >> "${VM_ARGS_PATH}"

echo '--- Config'
cat "${VM_ARGS_PATH}" | sed 's/^-setcookie\(...\).*\(...\)$/-setcookie\1[*REDACTED*]\2/'
echo '--- End of config'

/app/release/bin/automate foreground
