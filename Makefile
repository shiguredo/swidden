.PHONY: all upgrade compile test dialyzer clean github

all: clean upgrade compile test dialyzer

upgrade:
	@./rebar3 do update, upgrade

compile:
	@./rebar3 xref

test:
	@./rebar3 as test eunit, cover

dialyzer:
	@./rebar3 dialyzer

clean:
	@./rebar3 clean

github:
	$(MAKE) compile
	$(MAKE) dialyzer
	$(MAKE) test

publish:
	@./rebar3 hex publish
