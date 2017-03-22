.PHONY: all clean deps distclean compile rebar

# allow rebar binary to be set via environment variable
REBAR ?= ./rebar3

all:
	@$(REBAR) get-deps compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean

distclean: clean
	@$(REBAR) delete-deps

compile:
	@$(REBAR) compile

rebar:
	wget https://s3.amazonaws.com/rebar3/rebar3
	chmod +x rebar	3

