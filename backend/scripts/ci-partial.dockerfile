FROM plazaproject/ci-base-backend:1a433d7f94bb7bbe2343fba4bde2b8e6e2d09683

ADD . /app
RUN sh -x -c 'if [ ! -f config/sys.config ]; then cp -v config/sys.config.orig config/sys.config ; fi'

# Prepare release
RUN rebar3 release

# If the dev is launched by itself, start with a shell
CMD ["rebar3", "shell"]
