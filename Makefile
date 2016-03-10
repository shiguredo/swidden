.PHONY: all deps compile test clean

all: clean deps compile test

deps:
	@./rebar3 update

compile:
	@./rebar3 compile

test:
	@./rebar3 as test eunit 

clean:
	@./rebar3 clean

