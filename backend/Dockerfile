FROM erlang:24-alpine as programaker-backend-ci-base

# Build dependencies
RUN apk add git gcc libc-dev g++ make libtool autoconf automake

RUN mkdir /app
WORKDIR /app

# Pre-build dependencies
ADD rebar.config get-deps.sh /app/
RUN sh ./get-deps.sh && rebar3 compile
RUN rebar3 dialyzer

# Add application code
FROM programaker-backend-ci-base as develop
ADD . /app
RUN sh -x -c 'if [ ! -f config/sys.config ]; then cp -v config/sys.config.orig config/sys.config ; fi'

# Prepare release
RUN rebar3 release

FROM alpine:3.12 as final

RUN apk add ncurses libstdc++ erlang

COPY --from=develop /app/_build/default/rel/automate/ /app/release/
ADD ./scripts/ /app/scripts

# API server port
EXPOSE 8888

# Launch directly the release
CMD ["/app/scripts/container_init.sh"]
