FROM plazaproject/ci-base-backend:b6576716e8343d59be05dc14a2799afe1502c124

ADD . /app
RUN sh -x -c 'if [ ! -f config/sys.config ]; then cp -v config/sys.config.orig config/sys.config ; fi'

# Prepare release
RUN rebar3 release

# If the dev is launched by itself, start with a shell
CMD ["rebar3", "shell"]
