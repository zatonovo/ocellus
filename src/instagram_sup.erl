-module(instagram_sup).
-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/1, start_child/2, start_child/3]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     transient, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Client) ->
    start_child(Client, []).

start_child(Name, Client) when is_atom(Name) ->
    start_child(Name, Client, []);

start_child(Client, Opts) ->
    supervisor:start_child(?MODULE, [Client, Opts]).

start_child(Name, Client, Opts) when is_atom(Name) ->
    supervisor:start_child(?MODULE, [Name, Client, Opts]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
    {ok, {{simple_one_for_one, 5, 10},
          [?CHILD(instagram, instagram, worker, [])]}}.
