REBAR3=rebar3
ERL=erl
FIND_DEPS=`find _build/default/lib/ -mindepth 1 -maxdepth  1 -exec printf "-pa {}/ebin " \;`
TERM=xterm -e

ENV_OPTS?=AUTOMATE_SYNC_PRIMARY=bg1@localhost AUTOMATE_SYNC_PEERS=bg1@localhost,bg2@localhost,bg3@localhost
OPTIONS?=+pc unicode -config config/sys.config
BACKGROUND_OPTIONS?=$(OPTIONS) -noshell -detached
APPLICATION_NAME=automate

KICKOFF=-eval "application:ensure_all_started($(APPLICATION_NAME))."
CONNECTION=-eval "net_kernel:connect_node(bg1@localhost)."

RUN_NODE=$(ERL) $(FIND_DEPS) $(OPTIONS) $(KICKOFF)
RUN_BG_NODE=$(ERL) $(FIND_DEPS) $(BACKGROUND_OPTIONS)
RUN_CMP_NODE=$(TERM) $(ERL) $(FIND_DEPS) $(OPTIONS)
RUN_ADM_NODE=$(ERL) $(FIND_DEPS) -hidden

.PHONY: all run compile get-deps

all: dialyze

run-locally-distributed: compile
	AUTOMATE_HTTP_PORT=8888 AUTOMATE_TELEGRAM_SERVICE_ENABLED=1 $(ENV_OPTS) $(RUN_CMP_NODE) \
		$(KICKOFF) -sname bg1@localhost </dev/null &
	AUTOMATE_HTTP_PORT=8889 $(ENV_OPTS) $(RUN_CMP_NODE) $(KICKOFF) -sname bg2@localhost </dev/null &
	AUTOMATE_HTTP_PORT=8890 $(ENV_OPTS) $(RUN_CMP_NODE) $(KICKOFF) -sname bg3@localhost </dev/null &
	echo -e "Run \033[7m "'$(RUN_ADM_NODE) $(CONNECTION)'" -sname fg@localhost \033[0m"

## This probably won't work
run-locally-distributed-bg: compile
	$(RUN_BG_NODE) $(KICKOFF) -config config/bg1 -sname bg1@localhost
	$(RUN_BG_NODE) $(CONNECTION) $(KICKOFF) -config config/bg2 -sname bg2@localhost
	$(RUN_BG_NODE) $(CONNECTION)  $(KICKOFF) -config config/bg3 -sname bg3@localhost
	echo -e "Run \033[7m "'$(RUN_ADM_NODE) $(CONNECTION)'" -sname fg@localhost \033[0m"

compile: get-deps
	$(REBAR3) compile

get-deps:
	$(REBAR3) get-deps

dialyze: compile
	$(REBAR3) dialyzer
