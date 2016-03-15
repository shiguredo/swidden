.PHONY: all upgrade compile test clean

all: clean upgrade compile test

upgrade:
	@./rebar3 do update, upgrade

compile:
	@./rebar3 xref

test:
	@./rebar3 as test eunit, cover

clean:
	@./rebar3 clean

