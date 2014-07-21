PROJECT=ocellus
REBAR=rebar

all:
	@$(REBAR) get-deps compile

edoc:
	@$(REBAR) doc

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean

build_plt:
	@$(REBAR) build-plt

dialyzer:
	@$(REBAR) dialyze

console: all
	ERL_LIBS=deps erl -pa ebin -name $(PROJECT)@$(PROJECT).herokuapp.com -setcookie heroku -s $(PROJECT)

