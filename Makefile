.PHONY: all upgrade compile test dialyzer efmt-check elint-check fmt clean ci publish

all: clean upgrade efmt-check elint-check compile test dialyzer

upgrade:
	@./rebar3 do update, upgrade --all

compile:
	@./rebar3 xref

test:
	@./rebar3 as test eunit, cover

dialyzer:
	@./rebar3 dialyzer

# prek.toml 経由で efmt / elint を実行する
efmt-check:
	@prek run efmt-check --all-files

elint-check:
	@prek run elint --all-files

fmt:
	@prek run efmt --all-files

clean:
	@./rebar3 clean

ci:
	$(MAKE) compile
	$(MAKE) dialyzer
	$(MAKE) test

publish:
	@./rebar3 hex publish package
