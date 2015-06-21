%% This is meant to be spawned by the gen_oauth process as a listener
%% for a streaming connection.
-module(ocellus_stream_listener).
-export([handle_stream/3]).



-spec handle_stream(atom(), term(), term()) -> {ok, terminate} | {ok, stream_end} | {error, term()}.
handle_stream(Provider, RequestId, OAuth) ->
  receive
    % stream opened
    {http, {RequestId, stream_start, _Headers}} ->
      handle_stream(Provider, RequestId, OAuth);

    % stream received data
    {http, {RequestId, stream, Data}} ->
      spawn(fun() -> ocellus_stream_router:stream_event(Provider, Data) end),
      handle_stream(Provider, RequestId, OAuth);

    % stream closed
    {http, {RequestId, stream_end, _Headers}} ->
      lager:info("[~p] Stream closed for ~p. Reconnect", [?MODULE,RequestId]),
      p_restart(OAuth),
      {ok, stream_end};

    % connected but received error cod
    % 401 unauthorised - authentication credentials rejected
    {http, {RequestId, {{_, 401, _}, _Headers, _Body}}} ->
      Msg = "[~p] Authentication credentials rejected for ~p",
      lager:error(Msg, [?MODULE,RequestId]),
      {error, unauthorised};

    % 406 not acceptable - invalid request to the search api
    {http, {RequestId, {{_, 406, _}, _Headers, Body}}} ->
      Msg = "[~p] Invalid request for ~p",
      lager:error(Msg, [?MODULE,RequestId]),
      {error, {invalid_params, Body}};

    % connection error
    % may happen while connecting or after connected
    {http, {RequestId, {error, Reason}}} ->
      Msg = "[~p] Unexpected error for ~p: ~p",
      lager:error(Msg, [?MODULE,RequestId,Reason]),
      {error, {http_error, Reason}};

    % message send by us to close the connection
    terminate ->
      {ok, terminate}
  end.

p_restart({Pid, Url, Params}) ->
  gen_oauth:http_stream(Pid, Url, Params).
