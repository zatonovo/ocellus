%% Manage oauth connections
%% 
%% This is the primary interface for establishing oauth connections.
%% There are two oauth scenarios. The initial scenario is when a new
%% client is being setup. In this situation the complete oauth handshake
%% must commence. If a user access token has already been generated,
%% then it is unnecessary to go through the whole process. Instead, the
%% access token just needs to be loaded and used for requests.
%%
%% Here is a mapping of states and transitions
%% () => start_link => started
%% started => get_request_token => consumer_okay (step 1)
%% consumer_okay => get_authentication_url => pending_verification (step 2)
%% pending_verification => get_access_token => authenticated (step 3)
%%
%% See https://dev.twitter.com/docs/auth/implementing-sign-twitter
%% for the complete handshake
%%
%% For rest queries, one instance would be required for each user. Perhaps
%% in the future this will change, but it might be good to leave it this
%% way since it limits the number of synchronous requests available to a user.
%% @author Brian Lee Yung Rowe
-module(gen_oauth).
-behaviour(gen_fsm).
-export([behaviour_info/1]).
-export([start_link/3, start_link/4,
  server_name/2,
  get_request_token/3,
  get_authentication_url/3,
  get_authorization_url/3,
  get_access_token/3,
  set_access_token/2,
  set_stream_handler/2,
  http_get/3,
  http_post/3,
  http_stream/3,
  stop_stream/1
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
-record(state, {provider, stream_pid,
  consumer, request_token, request_secret, access_token, access_secret}).

behaviour_info(callbacks) ->
  [{init,1},
   {get_request_token,2},
   {get_authentication_url,2},
   {get_authorization_url,2},
   {get_access_token,2},
   {set_access_token,2}].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PUBLIC API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This is only appropriate for a single streaming instance. Otherwise, 
%% pass in the user name.
start_link(Provider, Consumer, Options) ->
  gen_fsm:start_link(?MODULE, [Provider, Consumer], Options).

%% UserName: The name of the user granting oauth for the given Provider
start_link(SessionId, Provider, Consumer, Options) ->
  ServerName = {local, server_name(SessionId, Provider)},
  gen_fsm:start_link(ServerName, ?MODULE, [Provider, Consumer], Options).


-spec server_name(binary(), atom()) -> atom().
server_name(SessionId, Provider) when is_binary(SessionId) ->
  server_name(binary_to_list(SessionId), Provider);

%-spec server_name(string(), atom()) -> atom().
server_name(SessionId, Provider) ->
  %lager:info("[~p] Provider: ~p, SessionId: ~p", [?MODULE,Provider,SessionId]),
  list_to_atom(string:join([atom_to_list(Provider), SessionId], ":")).


get_request_token(ServerRef, Url, Params) ->
  gen_fsm:sync_send_event(ServerRef, {get_request_token, Url, Params}).


get_authentication_url(ServerRef, Url, Token) ->
  gen_fsm:sync_send_event(ServerRef, {get_authentication_url, Url, Token}).

get_authorization_url(ServerRef, Url, Token) ->
  gen_fsm:sync_send_event(ServerRef, {get_authorization_url, Url, Token}).


get_access_token(ServerRef, Url, VerifierPin) ->
  gen_fsm:sync_send_event(ServerRef, {get_access_token, Url, VerifierPin}).

set_access_token(ServerRef, Access) ->
  gen_fsm:sync_send_event(ServerRef, {set_access_token, Access}).


set_stream_handler(_ServerRef, _Handler) ->
  not_implemented.

http_get(ServerRef, Url, Params) ->
  gen_fsm:sync_send_event(ServerRef, {http_get, Url, Params}).

http_post(ServerRef, Url, Params) ->
  gen_fsm:sync_send_event(ServerRef, {http_post, Url, Params}).

http_stream(ServerRef, Url, Params) ->
  gen_fsm:send_event(ServerRef, {http_stream, Url, Params}).

stop_stream(ServerRef) ->
  gen_fsm:send_event(ServerRef, stop_stream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% STATES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

started({get_request_token, Url, Params}, _From, State) ->
  Consumer = State#state.consumer,
  lager:info("[~p] Sending request to ~p", [?MODULE,Url]),
  case oauth_get(Url, Params, Consumer) of
    {ok, Response={{_, 200, _}, _, _}} ->
      lager:info("Got ok"),
      RParams = oauth:params_decode(Response),
      RToken = oauth:token(RParams),
      RSecret = oauth:token_secret(RParams),
      NextState = State#state{request_token=RToken, request_secret=RSecret},
      {reply, {ok, oauth:token(RParams)}, consumer_okay, NextState};
    {ok, Response} ->
      lager:warn("[~p] Unexpected response: ~p", [?MODULE,Response]),
      {reply, Response, started, State};
    Error ->
      {reply, Error, started, State}
  end;

%% Manually set the access token if you have already gone through 
%% the handshake for a given user.
started({set_access_token, {Token, Secret}}, From, State) when is_binary(Token) ->
  StrToken = binary_to_list(Token),
  StrSecret = binary_to_list(Secret),
  started({set_access_token, {StrToken, StrSecret}}, From, State);

started({set_access_token, {Token, Secret}}, _From, State) ->
  NextState = State#state{access_token=Token, access_secret=Secret},
  {reply, ok, authenticated, NextState}.

  
consumer_okay({get_authentication_url, Url, Token}, _From, State) ->
  AuthUrl = oauth:uri(Url, [{"oauth_token", Token}]),
  {reply, {ok, AuthUrl}, pending_verification, State};

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
      {reply, {AToken,ASecret}, authenticated, NextState};
    {ok, Response} ->
      lager:warn("[~p] Unexpected response: ~p", [?MODULE,Response]),
      {reply, Response, pending_verification, State};
    Error ->
      {reply, Error, pending_verification, State}
  end.


%% If a session already exists, then don't re-authenticate
authenticated({get_request_token, _Url, _Params}, _From, State) ->
  {reply, already_authenticated, authenticated, State};

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
authenticated({http_stream, Url, Params}, State) ->
  Provider = State#state.provider,
  Consumer = State#state.consumer,
  AToken = State#state.access_token,
  ASecret = State#state.access_secret,
  Pid = spawn_link(fun() ->
    oauth_post_stream(Url, Params, Provider, Consumer, AToken, ASecret)
  end),
  {next_state, authenticated, State#state{stream_pid=Pid}};

authenticated(stop_stream, #state{stream_pid=Pid}=State)
    when Pid /= undefined ->
  lager:info("[~p] Stopping stream", [?MODULE]),
  Pid ! terminate,
  {next_state, authenticated, State#state{stream_pid=undefined}}.

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
  lager:debug("[~p] Signed parameters: ~p", [?MODULE,Signed]),
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
  lager:info("[~p] Sending request:~n~p", [?MODULE,Request]),
  httpc:request(post, Request, [], []).


%% Params = [{K,V}]
oauth_post_stream(Url, Params, Provider, Consumer, Token, TokenSecret) ->
  lager:info("Using params: ~p", [Params]),
  Signed = oauth:sign("POST", Url, Params, Consumer, Token, TokenSecret),
  {AuthorizationParams, QueryParams} = partition_oauth_params(Signed),

  Request = {Url,
    [oauth:header(AuthorizationParams)],
    ?POST_CONTENT_TYPE,
    oauth:uri_params_encode(QueryParams)},
  Options = [{sync, false}, {stream, self}],
  lager:info("[~p] Sending request:~n~p", [?MODULE,Request]),
  httpc:set_options([{pipeline_timeout, 90000}]),
  case catch httpc:request(post, Request, [], Options) of
    {ok, RequestId} ->
      lager:info("[~p] Executing callback with RequestId ~p", [?MODULE,RequestId]),
      ocellus_stream_listener:handle_stream(Provider, RequestId);
    {error, Reason} ->
      lager:error("[~p] Unable to connect: ~p", [?MODULE,Reason])
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% GEN_FSM %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Intialize with consumer credentials
init([Provider, Consumer]) ->
  {ok, started, #state{provider=Provider, consumer=Consumer}}.

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
