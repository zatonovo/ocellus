%% This is meant to be spawned by the gen_oauth process as a listener
%% for a streaming connection.
-module(ocellus_stream_listener).
-export([handle_stream/2]).



-spec handle_stream(term(), term()) -> {ok, terminate} | {ok, stream_end} | {error, term()}.
handle_stream(Provider, RequestId) ->
  receive
    % stream opened
    {http, {RequestId, stream_start, _Headers}} ->
        handle_stream(Provider, RequestId);

    % stream received data
    {http, {RequestId, stream, Data}} ->
        spawn(fun() ->
            DecodedData = decode(Data),
            %Callback(DecodedData)
            ocellus_stream_router:stream_event(Provider, DecodedData)
        end),
        handle_stream(Provider, RequestId);

    % stream closed
    {http, {RequestId, stream_end, _Headers}} ->
        {ok, stream_end};

    % connected but received error cod
    % 401 unauthorised - authentication credentials rejected
    {http, {RequestId, {{_, 401, _}, _Headers, _Body}}} ->
        {error, unauthorised};

    % 406 not acceptable - invalid request to the search api
    {http, {RequestId, {{_, 406, _}, _Headers, Body}}} ->
        {error, {invalid_params, Body}};

    % connection error
    % may happen while connecting or after connected
    {http, {RequestId, {error, Reason}}} ->
        {error, {http_error, Reason}};

    % message send by us to close the connection
    terminate ->
        {ok, terminate}
  end.

%% Courtesy of twerl
-spec decode(binary()) -> list().
decode(Data) ->
  case Data of
    <<"\r\n">> -> [];
    _ ->
      {Decoded} = jiffy:decode(Data),
      Decoded
  end.

