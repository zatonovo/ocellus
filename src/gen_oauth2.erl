%%%-------------------------------------------------------------------
%% @doc This is a fsm responsible for establishing authorized oauth2
%% connections. It has only two states: pending_verification and authenticated.
%%
%% Steps to authorize a new user:
%%
%% % create new oauth2 client:
%% Client = gen_oauth2:new_client(ClientId, ClientSecret, RedirectURI,
%% GetTokenURL).
%% % start a process
%% {ok, Pid} = gen_oauth2:start_link(Client).
%% % get authorization url
%% {ok, AuthURL} = gen_oauth2:get_authorization_url(Pid).
%% % set access token
%% ok = gen_oauth2:set_access_token(Pid, AccessToken).
%% % client should be in authorized state at this point, and http_get calls
%% should be possible:
%% {ok, Response} = gen_oauth2:http_get(Pid, URL).
%%
%% If you already have an AccessToken, you can start gen_oauth2 process in
%% authorized state:
%% {ok, Pid} = gen_oauth2:start_link(Client, [{access_token, AccessToken}]).
%% @end
%%%-------------------------------------------------------------------
-module(gen_oauth2).
-behaviour(gen_fsm).

%% API
-export([get_authorization_url/1,
         get_access_token/1,
         http_get/2, http_get/3,
         identity/1,
         new_client/4,
         set_access_token/2]).

-export([start_link/1,
         start_link/2,
         start_link/3,
         stop/1]).

-record(oauth2_client,
        {
         client_id      :: string(),
         client_secret  :: string(),
         redirect_uri   :: string(),
         auth_url       :: string()
        }).
-opaque oauth2_client() :: #oauth2_client{}.
-export_type([oauth2_client/0]).

%% gen_fsm states
-export([authenticated/3,
         pending_verification/3]).

%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

% gen_fsm state
-record(state, {
          identity                      :: atom(),
          client                        :: oauth2_client(),
          access_token                  :: string(),
          http_get_fun = fun httpc:request/1 :: fun()
         }).

-export([behaviour_info/1]).
behaviour_info(callbacks) ->
  [{get_authorization_url,1},
   {get_access_token,1},
   {identity,1},
   {set_access_token,2}].

%%%===================================================================
%%% API
%%%===================================================================
new_client(ClientId, ClientSecret, RedirectURI, TokenURL) when is_list(ClientId),
                                                               is_list(ClientSecret),
                                                               is_list(RedirectURI),
                                                               is_list(TokenURL) ->
    #oauth2_client{client_id=ClientId,
                   client_secret=ClientSecret,
                   redirect_uri=RedirectURI,
                   auth_url=TokenURL}.

get_authorization_url(ServerRef) ->
    gen_fsm:sync_send_all_state_event(ServerRef, get_authorization_url).

get_access_token(ServerRef) ->
    gen_fsm:sync_send_event(ServerRef, get_access_token).

http_get(ServerRef, Url) ->
    http_get(ServerRef, Url, []).

http_get(ServerRef, Url, Params) ->
    gen_fsm:sync_send_event(ServerRef, {http_get, Url, Params}).

identity(ServerRef) ->
    gen_fsm:sync_send_all_state_event(ServerRef, identity).

set_access_token(ServerRef, AccessToken) ->
    gen_fsm:sync_send_event(ServerRef, {set_access_token, AccessToken}).

start_link(Client) ->
    start_link(Client, []).

start_link(Client, Opts) when is_record(Client, oauth2_client),
                              is_list(Opts) ->
    gen_fsm:start_link(?MODULE, [Client, Opts], []);
start_link(Name, Client) when is_atom(Name),
                              is_record(Client, oauth2_client) ->
    start_link(Name, Client, []).

start_link(Name, Client, Opts) when is_atom(Name),
                                    is_record(Client, oauth2_client),
                                    is_list(Opts) ->
    gen_fsm:start_link({local, Name}, ?MODULE, [Name, Client, Opts], []).

stop(ServerRef) ->
    gen_fsm:send_all_state_event(ServerRef, stop).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================
init([Name, Client, Opts]) ->
    State0 = #state{identity=Name, client=Client},
    State = set_options(State0, Opts),
    init_state(State);
init([Client, Opts]) ->
    State0 = #state{client=Client},
    State = set_options(State0, Opts),
    init_state(State).

pending_verification({set_access_token, AccessToken}, _From, State) ->
    {reply, ok, authenticated, State#state{access_token=AccessToken}};
pending_verification(get_access_token, _From, State) ->
    {reply, {error, not_authorized}, pending_verification, State};
pending_verification(Event, _From, State) ->
    lager:warning("[~p] Invalid request in pending_verification state: ~p",
                  [?MODULE,Event]),
    {reply, {error, invalid_request}, pending_verification, State}.

authenticated(get_access_token, _From, #state{access_token=AccessToken}=State) ->
    {reply, {ok, AccessToken}, authenticated, State};
authenticated({http_get, Url, Params}, _From, #state{access_token=AccessToken,
                                                     http_get_fun=GetFun}=State) ->
    Response = do_http_get(Url, [{access_token, AccessToken} | Params], GetFun),
    {reply, Response, authenticated, State};
authenticated(Event, _From, State) ->
    lager:warning("[~p] Invalid request in authenticated state: ~p",
                  [?MODULE,Event]),
    {reply, {error, invalid_request}, authenticated, State}.

handle_event(stop, _StateName, State) ->
    lager:info("[~p] Shutdown requested",[?MODULE]),
    {stop, normal, State}.

handle_sync_event(get_authorization_url, _From, StateName, #state{client=Client}=State) ->
    {reply, {ok, Client#oauth2_client.auth_url}, StateName, State};
handle_sync_event(identity, _From, StateName, #state{identity=Identity}=State) ->
    Response = case Identity of
                   undefined ->
                       {error, no_identity};
                   _ ->
                       {ok, Identity}
               end,
    {reply, Response, StateName, State};
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_http_get(Url, Params, GetFun) ->
    case GetFun(oauth:uri(Url, Params)) of
        {ok, {{_, 200, _}, Headers, Body}} ->
            {ok, {Headers, Body}};
        {ok, {{_, Code, _}, Headers, Body}} ->
            {error, {Code, Headers, Body}};
        {error, _} = Error ->
            Error
    end.

set_options(State, Opts) ->
    lists:foldl(fun do_set_options/2, State, Opts).

do_set_options({http_get_fun, Fun}, State) ->
    State#state{http_get_fun=Fun};
do_set_options({access_token, AccessToken}, State) ->
    State#state{access_token=AccessToken}.

init_state(#state{access_token=undefined}=State) ->
    {ok, pending_verification, State};
init_state(State) ->
    {ok, authenticated, State}.
