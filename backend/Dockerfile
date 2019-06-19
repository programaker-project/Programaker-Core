FROM erlang:alpine as plaza-backend-ci-base

# Build dependencies
RUN apk add git gcc libc-dev g++ make libtool autoconf automake

RUN mkdir /app
WORKDIR /app

# Pre-build dependencies
ADD rebar.config /app
RUN rebar3 get-deps && rebar3 compile
RUN rebar3 dialyzer

# Add application code
FROM plaza-backend-ci-base as develop
ADD . /app
RUN sh -x -c 'if [ ! -f config/sys.config ]; then cp -v config/sys.config.orig config/sys.config ; fi'

# Prepare release
RUN rebar3 release

# API server port
EXPOSE 8888

## If the dev is launched by itself, start with a shell
# CMD ["rebar3", "shell"]

# Launch directly the release
CMD ["_build/default/rel/automate/bin/automate", "foreground"]
