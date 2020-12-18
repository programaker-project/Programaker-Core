.PHONY: all tools

all: tools

plaza-testbench-master.zip:
	wget https://gitlab.com/kenkeiras/plaza-testbench/-/archive/master/plaza-testbench-master.zip

plaza-testbench-master: plaza-testbench-master.zip
	unzip -u $<
	touch $@ # So that Makefile considers this file ready

plaza-testbench-master/cli/cli: plaza-testbench-master
	sh -c "cd plaza-testbench-master/cli/ && go build"

tools: plaza-testbench-master/cli/cli
