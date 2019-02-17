FROM erlang:alpine as develop

# Build dependencies
RUN apk add git gcc libc-dev g++ make libtool autoconf automake

RUN mkdir /app
WORKDIR /app

# Pre-build dependencies
ADD rebar.config /app
RUN rebar3 get-deps && rebar3 compile

# Add application code
ADD . /app

# Prepare release
RUN rebar3 release

# If the dev is launched by itself, start with a shell
CMD ["rebar3", "shell"]
