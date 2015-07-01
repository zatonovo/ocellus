-module(ocellus_stream).

-behaviour(gen_server).

%% API
-export([start_stream/2]).
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
          stream_init_fun   :: fun(),
          event_ref         :: term(),
          request_id        :: term()
         }).

%%%===================================================================
%%% API
%%%===================================================================
start_stream(StreamInitFun, EventRef) ->
    ocellus_stream_sup:start_child(StreamInitFun, EventRef).

start_link(StreamInitFun, EventRef) ->
    gen_server:start_link(?MODULE, [StreamInitFun, EventRef], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([StreamInitFun, EventRef]) ->
    init_stream(),
    {ok, #state{stream_init_fun=StreamInitFun, event_ref=EventRef}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(init_stream, #state{stream_init_fun=StreamInitFun}=State) ->
    case StreamInitFun() of
        {ok, RequestId} ->
            {noreply, State#state{request_id=RequestId}};
        {error, _} ->
            {stop, normal, State}
    end;
handle_info(Msg, #state{request_id=RequestId, event_ref=EventRef}=State) ->
    case handle_stream(Msg, RequestId) of
        ok ->
            {noreply, State};
        {ok, Data} ->
            ocellus_stream_router:stream_event(EventRef, Data),
            {noreply, State};
        {error, stream_ended} ->
            init_stream(),
            {noreply, State};
        {error, _} ->
            {stop, normal, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
init_stream() ->
    self() ! init_stream.

-spec handle_stream(term(), term()) ->
    ok | {event, term()} | {error, any()}.
handle_stream({http, {RequestId, stream_start, _Headers}}, RequestId) ->
    ok;
handle_stream({http, {RequestId, stream, Data}}, RequestId) ->
    {ok, Data};
handle_stream({http, {RequestId, stream_end, _Headers}}, RequestId) ->
    {error, stream_ended};
handle_stream({http, {RequestId, {{_, 401, _}, _Headers, _Body}}}, RequestId) ->
    Msg = "[~p] Authentication credentials rejected for ~p",
    lager:error(Msg, [?MODULE,RequestId]),
    {error, unauthorised};
handle_stream({http, {RequestId, {{_, 406, _}, _Headers, Body}}}, RequestId) ->
    Msg = "[~p] Invalid request for ~p",
    lager:error(Msg, [?MODULE,RequestId]),
    {error, {invalid_params, Body}};
handle_stream({http, {RequestId, {error, Reason}}}, RequestId) ->
    Msg = "[~p] Unexpected error for ~p: ~p",
    lager:error(Msg, [?MODULE,RequestId,Reason]),
    {error, {http_error, Reason}}.
