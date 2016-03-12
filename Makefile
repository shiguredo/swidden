.PHONY: all upgrade compile test clean

all: clean upgrade compile test

upgrade:
	@./rebar3 upgrade

compile:
	@./rebar3 xref

test:
	@./rebar3 as test eunit -c all, cover

clean:
	@./rebar3 clean

