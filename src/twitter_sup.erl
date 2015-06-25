-module(twitter_sup).
-behaviour(supervisor).
 
-export([start_link/0]).
-export([init/1, add_user/2, stop_user/1, identify/0]).
 

start_link() ->
  supervisor:start_link({local,?MODULE}, ?MODULE, []).


init([]) ->
  {ok, {{simple_one_for_one, 3, 60},
  [{?MODULE,
    % No arguments by default. Call supervisor:start_child(Sup, [UserName])
    {twitter_client, start_link, []},
    temporary, 1000, worker, [twitter_client]}
    ]}}.

%% Call this to add a user process and begin the oauth handshake for that
%% user.
%% Flow:
%% Consumer = oauth_util:consumer_tokens(twitter).
%% Pid = twitter_sup:add_user("sessionid", Consumer).
%% {ok, RequestToken} = twitter_client:get_request_token(Pid, []).
%% {ok, AuthUrl} = twitter_client:get_authentication_url(Pid, RequestToken).
add_user(SessionId, Consumer) ->
  Ref = gen_oauth:server_name(SessionId,twitter),
  case whereis(Ref) of
    undefined ->
      {ok,Pid} = supervisor:start_child(?MODULE, [SessionId, Consumer]),
      Msg = "[~p] Started child ~p for session ~p",
      lager:info(Msg, [?MODULE,Pid,SessionId]),
      Pid;
    Pid -> Pid
  end.

%% Stop user twitter_client process. This is only local and does not affect
%% the persisted sessions.
stop_user(SessionId) ->
  Ref = gen_oauth:server_name(SessionId,twitter),
  case whereis(Ref) of
    undefined ->
      Msg = "[~p] Cowardly refusing to kill unknown child ~p", 
      lager:info(Msg,[?MODULE,SessionId]);
    Pid ->
      Msg = "[~p] Terminating child ~p for session ~p",
      lager:info(Msg, [?MODULE,Pid,SessionId]),
      supervisor:terminate_child(?MODULE, Pid)
  end.
  

identify() ->
  Children = supervisor:which_children(?MODULE),
  [ M:identify(Pid) || {_,Pid,_, [M]} <- Children ].
