#!/bin/sh

DEFAULT_DIR=/app/mnesia
MNESIA_DIR=${MNESIA_DIR:-$DEFAULT_DIR}

VM_ARGS_PATH=/app/release/releases/0.0.1/vm.args

# Set erlang cookie
if [ ! -z "${ERLANG_COOKIE}" ];then
    sed 's/^-setcookie.*$/-setcookie '"${ERLANG_COOKIE}"'/' -i "${VM_ARGS_PATH}"
else
    echo 'No cookie set on environment variable, this is probably not safe!'
fi

# Set mnesia directory
grep ^-mnesia "${VM_ARGS_PATH}" >/dev/null
if [ $? -ne 0 ];then
    echo $'\n''-mnesia dir '"'\"${MNESIA_DIR}\"'" >> "${VM_ARGS_PATH}"
else
    MNESIA_DIR_ESCAPED=`echo "${MNESIA_DIR}"| sed 's/\\//\\\\\\//g'`
    sed 's/^-mnesia.*$/-mnesia dir '"\"${MNESIA_DIR_ESCAPED}\""'/' -i "${VM_ARGS_PATH}"
fi

/app/release/bin/automate foreground
