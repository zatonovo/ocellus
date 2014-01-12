%% Route streaming events to processes. This acts as a simple lookup
%% table and mapping service to route streaming data to clients. The
%% reason for this is because many services only allow a single
%% streaming connection which contains all search terms. It is somewhat
%% like a multiplexer that needs to be demuxed.
%%
%% There is only one stream_event process necessary. This process
%% manages an ETS table with all the subscriptions.
%%
%% Usage:
%% Callback = fun(Pid, Provider, Event) -> Tweet = proplists:get_value(<<"text">>, Event), io:format("Erlang <3: ~s~n", [Tweet]) end.
%% ocellus_stream_router:add_provider(twitter, Callback).
-module(ocellus_stream_router).
-behaviour(gen_event).

-export([stream_event/2, register/3, unregister/3, get_callback/1]).
-export([start_link/0, add_provider/1, add_provider/2]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, 
  code_change/3, format_status/2, terminate/2]).

-record(state, {provider, channel_table, callback}).

%% Add a new event
stream_event(Provider, Event) ->
  gen_event:notify(?MODULE, {event, Provider, Event}).

%% Register a pid to a given provider and channel. The provider is the
%% pid of the streaming service, which would typically be captured in a
%% From variable.
register(Pid, Provider, Channel) ->
  gen_event:notify(?MODULE, {register, Pid, Provider, Channel}).

unregister(Pid, Provider, Channel) ->
  gen_event:notify(?MODULE, {unregister, Pid, Provider, Channel}).

%% Create a callback function to pass to the gen_oauth process
get_callback(_Provider) ->
  not_implemented.


start_link() ->
  gen_event:start_link({local,?MODULE}).

add_provider(Provider) -> add_provider(Provider, fun broadcast_event/3).

add_provider(Provider, Callback) ->
  gen_event:add_handler(?MODULE, {?MODULE,Provider}, {Provider, Callback}).
  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% GEN_EVENT CALLBACKS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Table structure is {{Provider,Channel}, Subscriber}
init({Provider, Callback}) ->
  TableId = ets:new(channels, [bag, protected, {keypos,1}]),
  {ok, #state{provider=Provider, channel_table=TableId, callback=Callback}}.


handle_event({register,Pid,Provider,Channel}, #state{provider=Provider}=State) ->
  lager:info("Inserting {{~p,~p}, ~p}", [Provider,Channel,Pid]),
  Tid = State#state.channel_table,
  ets:insert(Tid, {{Provider, Channel}, Pid}),
  {ok, State};

handle_event({unregister,Pid,Provider,Channel}, #state{provider=Provider}=State) ->
  lager:info("Deleting {{~p,~p}, ~p}", [Provider,Channel,Pid]),
  Tid = State#state.channel_table,
  ets:delete_object(Tid, {{Provider, Channel}, Pid}),
  {ok, State};

handle_event({event, Provider, []}, #state{provider=Provider}=State) ->
  {ok, State};

handle_event({event, Provider, Event}, #state{provider=Provider}=State) ->
  Callback = State#state.callback,
  % TODO: Make this configurable
  Channel = get_channel(Provider, Event),
  lager:info("Pushing event to {~p,~p}", [Provider,Channel]),
  Tid = State#state.channel_table,
  case ets:lookup(Tid, {Provider,Channel}) of
    [] -> 
      lager:info("No pids found for {~p,~p}", [Provider,Channel]),
      ok;
    Records ->
      lager:debug("Broadcasting to ~p pids", [length(Records)]),
      lists:map(fun({_,Pid}) -> Callback(Pid,Provider,Event) end, Records)
  end,
  {ok, State};

%% Pass-through when the Provider does not match the particular
%% event handler's Provider.
handle_event(Event, State) -> 
  lager:info("Unexpected event ~p", [Event]),
  {ok, State}.

handle_call(_Request, State) -> {ok, ok, State}.

handle_info(_Info, State) -> {ok, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

format_status(_Opt, [_PDict, State]) -> State.

terminate(_Args, _State) ->
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% INTERNAL HELPERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_channel(_Provider, _Event) ->
  anonymous_channel.

broadcast_event(Pid, Provider, Event) ->
  lager:debug("Sending {~p,~p} to ~p", [Provider,Event,Pid]),
  Pid ! {Provider,Event}.
