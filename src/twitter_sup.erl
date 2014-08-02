-module(twitter_sup).
-behaviour(supervisor).
 
-export([start_link/0]).
-export([init/1, add_user/2, identify/0]).
 

start_link() ->
  supervisor:start_link({local,?MODULE}, ?MODULE, []).


init([]) ->
  {ok, {{simple_one_for_one, 3, 60},
  [{?MODULE,
    % No arguments by default. Call supervisor:start_child(Sup, [UserName])
    {twitter_oauth, start_link, []},
    temporary, 1000, worker, [twitter_oauth]}
    ]}}.

%% Call this to add a user process and begin the oauth handshake for that
%% user.
%% Flow:
%% Consumer = oauth_util:consumer_tokens(twitter).
%% Pid = twitter_sup:add_user("sessionid", Consumer).
%% {ok, RequestToken} = twitter_oauth:get_request_token(Pid, []).
%% {ok, AuthUrl} = twitter_oauth:get_authentication_url(Pid, RequestToken).
add_user(SessionId, Consumer) ->
  Ref = gen_oauth:server_name(SessionId,twitter),
  case whereis(Ref) of
    undefined ->
      Sup = ?MODULE,
      {ok,Pid} = supervisor:start_child(Sup, [SessionId, Consumer]),
      Pid;
    Pid -> Pid
  end.

identify() ->
  Children = supervisor:which_children(?MODULE),
  [ M:identify(Pid) || {_,Pid,_, [M]} <- Children ].
