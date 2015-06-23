-module(ocellus).
-export([start/0]).
-export([get_env/1, get_env/2]).

-define(APP, ?MODULE).

-spec start() -> ok.
start() ->
    ok = application:start(syntax_tools),
    ok = application:start(compiler),
    ok = application:start(goldrush),
    ok = application:start(lager),
    ok = application:start(crypto),
    ok = application:start(asn1),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(inets),
    ok = application:start(ocellus).

-spec get_env(atom()) -> {ok, term()} | undefined.
get_env(Name) ->
    application:get_env(?APP, Name).

-spec get_env(atom(), term()) -> term().
get_env(Name, Default) ->
    application:get_env(?APP, Name, Default).
