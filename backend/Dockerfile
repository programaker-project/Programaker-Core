FROM erlang:alpine as ci-base

# Build dependencies
RUN apk add git gcc libc-dev g++ make libtool autoconf automake

RUN mkdir /app
WORKDIR /app

# Pre-build dependencies
ADD rebar.config /app
RUN rebar3 get-deps && rebar3 compile

# Add application code
FROM ci-base as develop
ADD . /app
RUN sh -c 'if [ ! -f config/sys.config ]; then cp config/sys.config.orig config/sys.config ; fi'

# Prepare release
RUN rebar3 release

# If the dev is launched by itself, start with a shell
CMD ["rebar3", "shell"]
