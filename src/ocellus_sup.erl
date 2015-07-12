-module(ocellus_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    StreamRouter = ?CHILD(ocellus_stream_router, worker),
    StreamSup    = ?CHILD(ocellus_stream_sup, supervisor),
    TwitterSup   = ?CHILD(twitter_sup, supervisor),
    InstagramSup = ?CHILD(instagram_sup, supervisor),
    {ok, { {one_for_one, 5, 10},
           [StreamRouter, StreamSup, TwitterSup, InstagramSup]} }.
