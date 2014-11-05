.PHONY: all compile deps clean test dev docs

REBAR_CONFIG = rebar.config
RELX_CONFIG = relx.config

APP_NAME = spam

all: clean deps test

deps: get-deps update-deps
	@./rebar -C $(REBAR_CONFIG) compile

update-deps:
	@./rebar -C $(REBAR_CONFIG) update-deps

get-deps:
	@./rebar -C $(REBAR_CONFIG) get-deps

compile:
	@./rebar -C $(REBAR_CONFIG) compile skip_deps=true
	@./rebar -C $(REBAR_CONFIG) xref skip_deps=true

dev: compile
	rm -rf dev
	@./relx -c $(RELX_CONFIG)

test: compile
	rm -rf .eunit
	@ERL_FLAGS="-smp enable" ./rebar -C $(REBAR_CONFIG) eunit skip_deps=true

docs:
	@./rebar -C $(REBAR_CONFIG) swidden_doc

clean:
	@./rebar -C $(REBAR_CONFIG) clean skip_deps=true

distclean: clean
	@./rebar -C $(REBAR_CONFIG) clean
	@./rebar -C $(REBAR_CONFIG) delete-deps
	rm -rf dev

dialyze-init:
	dialyzer --build_plt --apps erts kernel stdlib mnesia crypto public_key reltool
	dialyzer --add_to_plt --plt ~/.dialyzer_plt --output_plt $(APP_NAME).plt -c .
	dialyzer -c ebin -Wunmatched_returns -Werror_handling -Wrace_conditions -Wunderspecs

dialyze: compile
	dialyzer --check_plt --plt $(APP_NAME).plt -c .
	dialyzer -c ebin
