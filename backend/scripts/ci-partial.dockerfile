FROM plazaproject/ci-base-backend:5e91faa7bb0593d8da302e8ee621871a1be56ccf

ADD . /app
RUN sh -x -c 'if [ ! -f config/sys.config ]; then cp -v config/sys.config.orig config/sys.config ; fi'

# Prepare release
RUN rebar3 release

# If the dev is launched by itself, start with a shell
CMD ["rebar3", "shell"]
