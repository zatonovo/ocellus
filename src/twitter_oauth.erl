%% Provides an implementation of the Twitter API
%% @author Brian Lee Yung Rowe
-module(twitter_oauth).
-behaviour(gen_oauth).
-export([start_link/1, start_link/2,
  get_favorites/2, get_favorites/3,
  search_tweets/2, search_tweets/3,
  get_user_timeline/2, get_user_timeline/3,
  filter_stream/2, filter_stream/3,
  get_account_settings/1,
  get_user_info/2,
  get_rate_limits/1, get_rate_limits/2,
  get_friends/2, get_friends/3,
  get_followers/2, get_followers/3
]).
-export([init/1,
  get_request_token/2,
  get_authentication_url/2,
  get_authorization_url/2,
  get_access_token/2,
  set_access_token/2]).


start_link(Consumer) ->
  gen_oauth:start_link(twitter, Consumer, []).

start_link(SessionId, Consumer) ->
  gen_oauth:start_link(SessionId, twitter, Consumer, []).


% Params: Query parameters as proplist for final request
get_request_token(ServerRef, Params) ->
  Url = "https://twitter.com/oauth/request_token",
  gen_oauth:get_request_token(ServerRef, Url, Params).

% For sign-in with Twitter
get_authentication_url(ServerRef, Token) ->
  Url = "https://twitter.com/oauth/authenticate",
  gen_oauth:get_authentication_url(ServerRef, Url, Token).

% For 3-legged auth (limited permissions)
get_authorization_url(ServerRef, Token) ->
  Url = "https://twitter.com/oauth/authorize",
  gen_oauth:get_authorization_url(ServerRef, Url, Token).

get_access_token(SessionId, VerifierPin) when is_list(SessionId) ->
  ServerRef = gen_oauth:server_name(SessionId,twitter),
  get_access_token(ServerRef, VerifierPin);

get_access_token(ServerRef, VerifierPin) ->
  Url = "https://twitter.com/oauth/access_token",
  gen_oauth:get_access_token(ServerRef, Url, VerifierPin).

set_access_token(SessionId, Access) ->
  ServerRef = gen_oauth:server_name(SessionId,twitter),
  gen_oauth:set_access_token(ServerRef, Access).



get_account_settings(SessionId) ->
  Url = "https://api.twitter.com/1.1/account/settings.json",
  ServerRef = gen_oauth:server_name(SessionId,twitter),
  {ok, _Headers, Json} = gen_oauth:http_get(ServerRef, Url, []),
  jiffy:decode(Json).
  
% https://dev.twitter.com/docs/rate-limiting/1.1/limits
get_rate_limits(SessionId) ->
  Url = "https://api.twitter.com/1.1/application/rate_limit_status.json",
  ServerRef = gen_oauth:server_name(SessionId,twitter),
  {ok, _Headers, Json} = gen_oauth:http_get(ServerRef, Url, []),
  jiffy:decode(Json).

get_rate_limits(SessionId, Resources) ->
  Url = "https://api.twitter.com/1.1/application/rate_limit_status.json",
  ServerRef = gen_oauth:server_name(SessionId,twitter),
  Params = [{"resources",Resources}],
  {ok, _Headers, Json} = gen_oauth:http_get(ServerRef, Url, Params),
  jiffy:decode(Json).



% twitter_oauth:get_user_info(SessionId, "cartesianfaith"),
get_user_info(SessionId, UserId) ->
  get_user_info(SessionId, UserId, []).

get_user_info(SessionId, UserId, Params) ->
  Url = "https://api.twitter.com/1.1/users/show.json",
  p_get_request(Url, SessionId, UserId, Params).

% https://dev.twitter.com/docs/api/1.1/get/friends/ids
get_friends(SessionId, UserId) ->
  get_friends(SessionId, UserId, []).

get_friends(SessionId, UserId, Params) ->
  Url = "https://api.twitter.com/1.1/friends/ids.json",
  p_get_request(Url, SessionId, UserId, Params).


% https://dev.twitter.com/docs/api/1.1/get/followers/ids
get_followers(SessionId, UserId) ->
  get_followers(SessionId, UserId, []).

get_followers(SessionId, UserId, Params) ->
  Url = "https://api.twitter.com/1.1/followers/ids.json",
  p_get_request(Url, SessionId, UserId, Params).


get_favorites(SessionId, UserId) ->
  get_favorites(SessionId, UserId, []).

get_favorites(SessionId, UserId, Params) ->
  Url = "https://api.twitter.com/1.1/favorites/list.json",
  p_get_request(Url, SessionId, UserId, Params).


get_user_timeline(SessionId, UserId) ->
  get_user_timeline(SessionId, UserId, []).

get_user_timeline(SessionId, UserId, Params) ->
  Url = "https://api.twitter.com/1.1/statuses/user_timeline.json",
  p_get_request(Url, SessionId, UserId, Params).


search_tweets(ServerRef, Query) -> search_tweets(ServerRef, Query, []).

search_tweets(ServerRef, SearchQuery, Params) ->
  Url = "https://api.twitter.com/1.1/search/tweets.json",
  gen_oauth:http_get(ServerRef, Url, [{"q",SearchQuery} | Params]).

filter_stream(ServerRef, SearchQuery) ->
  filter_stream(ServerRef, SearchQuery, []).

filter_stream(ServerRef, SearchQuery, Params) ->
  Url = "https://stream.twitter.com/1.1/statuses/filter.json",
  gen_oauth:http_stream(ServerRef, Url, [{"track",SearchQuery} | Params]).


init(_Args) ->
  not_implemented.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
p_get_request(Url, SessionId, UserId, Params) when is_binary(SessionId) ->
  p_get_request(Url, binary_to_list(SessionId), UserId, Params);

p_get_request(Url, SessionId, UserId, Params) when is_binary(UserId) ->
  p_get_request(Url, SessionId, binary_to_list(UserId), Params);

p_get_request(Url, SessionId, UserId, Params) ->
  ServerRef = gen_oauth:server_name(SessionId,twitter),
  FullParams = [{"user_id",UserId} | Params],
  case gen_oauth:http_get(ServerRef, Url, FullParams) of
    {ok, _Headers, Json} -> jiffy:decode(Json);
    {{_,404,_},_} -> not_found
  end.

