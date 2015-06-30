-module(ocellus_stream_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []},
                         transient, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(StreamInitFun, EventRef) ->
    supervisor:start_child(?MODULE, [StreamInitFun, EventRef]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
    {ok, {{simple_one_for_one, 5, 10},
          [?CHILD(ocellus_stream, worker)]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
