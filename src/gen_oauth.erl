%% Manage oauth connections
%% 
%% This is the primary interface for establishing oauth connections.
%% There are two oauth scenarios. The initial scenario is when a new
%% client is being setup. In this situation the complete oauth handshake
%% must commence. If a user access token has already been generated,
%% then it is unnecessary to go through the whole process. Instead, the
%% access token just needs to be loaded and used for requests.
-module(gen_oauth).
-behaviour(gen_fsm).
-export([behaviour_info/1]).
-export([start_link/2, start_link/3,
  get_request_token/3,
  get_authorization_url/3,
  get_access_token/3,
  set_access_token/3,
  set_consumer_credentials/2,
  set_stream_handler/2,
  http_get/3,
  http_post/3,
  http_stream/4
]).

%% FSM states
-export([started/3,
  consumer_okay/3,
  pending_verification/3,
  authenticated/2, authenticated/3
  %streaming/2, streaming/3
]).

%% gen_fsm callbacks
-export([init/1,
  handle_event/3, handle_sync_event/4,
  handle_info/3,
  terminate/3, code_change/4
]).

-define(POST_CONTENT_TYPE, "application/x-www-form-urlencoded").
-record(state, {consumer,
  request_token, request_secret, access_token, access_secret}).

behaviour_info(callbacks) ->
  [{init,1},
   {get_request_token,2},
   {get_authorization_url,2},
   {get_access_token,2},
   {set_access_token,3}].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PUBLIC API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(Consumer, Options) ->
  gen_fsm:start_link(?MODULE, Consumer, Options).

start_link(ServerName, Consumer, Options) ->
  gen_fsm:start_link(ServerName, ?MODULE, Consumer, Options).


get_request_token(Pid, Url, Params) ->
  gen_fsm:sync_send_event(Pid, {get_request_token, Url, Params}).


get_authorization_url(Pid, Url, Token) ->
  gen_fsm:sync_send_event(Pid, {get_authorization_url, Url, Token}).


get_access_token(Pid, Url, VerifierPin) ->
  gen_fsm:sync_send_event(Pid, {get_access_token, Url, VerifierPin}).

set_access_token(Pid, Token, Secret) ->
  gen_fsm:sync_send_event(Pid, {set_access_token, Token, Secret}).

set_consumer_credentials(_Pid, _Consumer) ->
  not_implemented.

set_stream_handler(_Pid, _Handler) ->
  not_implemented.

http_get(Pid, Url, Params) ->
  gen_fsm:sync_send_event(Pid, {http_get, Url, Params}).

http_post(Pid, Url, Params) ->
  gen_fsm:sync_send_event(Pid, {http_post, Url, Params}).

http_stream(Pid, Url, Params, Callback) ->
  gen_fsm:send_event(Pid, {http_stream, Url, Params, Callback}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% STATES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

started({get_request_token, Url, Params}, _From, State) ->
  Consumer = State#state.consumer,
  case oauth_get(Url, Params, Consumer) of
    {ok, Response={{_, 200, _}, _, _}} ->
      RParams = oauth:params_decode(Response),
      RToken = oauth:token(RParams),
      RSecret = oauth:token_secret(RParams),
      NextState = State#state{request_token=RToken, request_secret=RSecret},
      {reply, {ok, oauth:token(RParams)}, consumer_okay, NextState};
    {ok, Response} ->
      lager:warn("Unexpected response: ~p", Response),
      {reply, Response, started, State};
    Error ->
      {reply, Error, started, State}
  end;

%% Manually set the access token if you have already gone through 
%% the handshake for a given user.
started({set_access_token, Token, Secret}, _From, State) ->
  NextState = State#state{access_token=Token, access_secret=Secret},
  {reply, ok, authenticated, NextState}.

  
consumer_okay({get_authorization_url, Url, Token}, _From, State) ->
  AuthUrl = oauth:uri(Url, [{"oauth_token", Token}]),
  {reply, {ok, AuthUrl}, pending_verification, State}.


pending_verification({get_access_token, Url, VerifierPin}, _From, State) ->
  Params = [{"oauth_verifier", VerifierPin}],
  Consumer = State#state.consumer,
  RToken = State#state.request_token,
  RSecret = State#state.request_secret,
  case oauth_get(Url, Params, Consumer, RToken, RSecret) of
    {ok, Response={{_, 200, _}, _, _}} ->
      AParams = oauth:params_decode(Response),
      AToken = oauth:token(AParams),
      ASecret = oauth:token_secret(AParams),
      NextState = State#state{access_token=AToken, access_secret=ASecret},
      {reply, ok, authenticated, NextState};
    {ok, Response} ->
      lager:warn("Unexpected response: ~p", Response),
      {reply, Response, pending_verification, State};
    Error ->
      {reply, Error, pending_verification, State}
  end.


%% Params: HTTP query params
authenticated({http_get, Url, Params}, _From, State) ->
  Consumer = State#state.consumer,
  AToken = State#state.access_token,
  ASecret = State#state.access_secret,
  Response = case oauth_get(Url, Params, Consumer, AToken, ASecret) of
    {ok, {{_, 200, _}, Headers, Body}} ->
      ResponseBody = parse_response(Headers, Body),
      {ok, Headers, ResponseBody};
    {ok, RawResponse} -> RawResponse;
    Error -> Error
  end,
  {reply, Response, authenticated, State};

authenticated({http_post, Url, Params}, _From, State) ->
  Consumer = State#state.consumer,
  AToken = State#state.access_token,
  ASecret = State#state.access_secret,
  Response = case oauth_post(Url, Params, Consumer, AToken, ASecret) of
    {ok, {{_, 200, _}, Headers, Body}} ->
      ResponseBody = parse_response(Headers, Body),
      {ok, Headers, ResponseBody};
    {ok, RawResponse} -> RawResponse;
    Error -> Error
  end,
  {reply, Response, authenticated, State}.


%% Open a streaming connection
authenticated({http_stream, Url, Params, Callback}, State) ->
  Consumer = State#state.consumer,
  AToken = State#state.access_token,
  ASecret = State#state.access_secret,
  %ListenerPid = oauth_post_stream(Url, Params, Consumer, AToken, ASecret),
  %{noreply, authenticated, State#state{listener_pid=ListenerPid}}.
  oauth_post_stream(Url, Params, Callback, Consumer, AToken, ASecret),
  {noreply, authenticated, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% INTERNAL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
partition_oauth_params(Signed) ->
  PartitionFn = fun({K, _}) -> lists:prefix("oauth_", K) end,
  lists:partition(PartitionFn, Signed).

%% Get the response body from a HTTP response
parse_response(Headers, Body) -> 
  case proplists:get_value("content-type", Headers) of
    undefined ->
      Body;
    ContentType ->
      MediaType = hd(string:tokens(ContentType, ";")),
      case lists:suffix("/xml", MediaType) orelse lists:suffix("+xml", MediaType) of
        true ->
          {XML, []} = xmerl_scan:string(Body),
          XML;
        false ->
          Body
      end
  end.

oauth_get(Url, Params, Consumer) ->
  oauth_get(Url, Params, Consumer, "", "").

oauth_get(Url, Params, Consumer, Token, TokenSecret) ->
  Signed = oauth:sign("GET", Url, Params, Consumer, Token, TokenSecret),
  lager:info("Signed parameters: ~p", [Signed]),
  {AuthorizationParams, QueryParams} = partition_oauth_params(Signed),
  Request = {oauth:uri(Url, QueryParams), [oauth:header(AuthorizationParams)]},
  httpc:request(get, Request, [{autoredirect, false}], []).


oauth_post(Url, Params, Consumer, Token, TokenSecret) ->
  Signed = oauth:sign("POST", Url, Params, Consumer, Token, TokenSecret),
  {AuthorizationParams, QueryParams} = partition_oauth_params(Signed),

  Request = {Url,
    [oauth:header(AuthorizationParams)],
    ?POST_CONTENT_TYPE,
    oauth:uri_params_encode(QueryParams)},
  lager:info("Sending request:~n~p", [Request]),
  httpc:request(post, Request, [], []).


%% Params = [{K,V}]
oauth_post_stream(Url, Params, Callback, Consumer, Token, TokenSecret) ->
  Signed = oauth:sign("POST", Url, Params, Consumer, Token, TokenSecret),
  {AuthorizationParams, QueryParams} = partition_oauth_params(Signed),

  Request = {Url,
    [oauth:header(AuthorizationParams)],
    ?POST_CONTENT_TYPE,
    oauth:uri_params_encode(QueryParams)},
  % TODO: Spawn link instead
  Options = [{sync, false}, {stream, self}],
  lager:info("Sending request:~n~p", [Request]),
  httpc:set_options([{pipeline_timeout, 90000}]),
  case catch httpc:request(post, Request, [], Options) of
    {ok, RequestId} ->
      lager:info("Executing callback with RequestId ~p", [RequestId]),
      stream_listener:handle_stream(Callback, RequestId);
      %ListenerPid = spawn_link()
    {error, Reason} ->
      lager:error("Unable to connect: ~p", [Reason])
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% GEN_FSM %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Intialize with consumer credentials
init(Consumer) ->
  {ok, started, #state{consumer=Consumer}}.

%% Unused
handle_event(_Event, StateName, State=#state{}) ->
  {next_state, StateName, State}.

%% Unused
handle_sync_event(_Event, _From, StateName, State=#state{}) ->
  {next_state, StateName, State}.

%% Unused
handle_info(_Info, StateName, State=#state{}) ->
  {next_state, StateName, State}.

%% Unused
code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
  ok.
