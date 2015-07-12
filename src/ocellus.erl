-module(ocellus).
-export([start/0]).
-export([get_env/1, get_env/2]).

-define(APP, ?MODULE).

-spec start() -> ok.
start() ->
    ok = application:ensure_all_started(ocellus).

-spec get_env(atom()) -> {ok, term()} | undefined.
get_env(Name) ->
    application:get_env(?APP, Name).

-spec get_env(atom(), term()) -> term().
get_env(Name, Default) ->
    application:get_env(?APP, Name, Default).
