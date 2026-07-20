.PHONY: all upgrade compile test dialyzer efmt-check fmt clean ci publish

all: clean upgrade efmt-check compile test dialyzer

upgrade:
	@./rebar3 do update, upgrade --all

compile:
	@./rebar3 xref

test:
	@./rebar3 as test eunit, cover

dialyzer:
	@./rebar3 dialyzer

efmt-check:
	@RUST_LOG=warn efmt --check --parallel --check-line-length 120

fmt:
	@efmt -w --parallel

clean:
	@./rebar3 clean

ci:
	$(MAKE) compile
	$(MAKE) dialyzer
	$(MAKE) test

publish:
	@./rebar3 hex publish package
