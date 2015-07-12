PROJECT=ocellus
REBAR=rebar

.PHONY: test

all:
	@$(REBAR) get-deps compile

edoc:
	@$(REBAR) doc

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

ct-test: all
	@$(REBAR) ct skip_deps=true

clean:
	@$(REBAR) clean

build_plt:
	@$(REBAR) build-plt

dialyzer:
	@$(REBAR) dialyze

console: all
	ERL_LIBS=deps erl -pa ebin -name $(PROJECT)@$(PROJECT).herokuapp.com -setcookie heroku -s $(PROJECT)

