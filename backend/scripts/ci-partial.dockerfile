FROM plazaproject/ci-base-backend:52db1adcf019a16a12fd9fe4d6c1c2534d68b561

ADD . /app
RUN sh -x -c 'if [ ! -f config/sys.config ]; then cp -v config/sys.config.orig config/sys.config ; fi'

# Prepare release
RUN rebar3 release

# If the dev is launched by itself, start with a shell
CMD ["rebar3", "shell"]
