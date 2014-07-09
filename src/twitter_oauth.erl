%% Provides an implementation of the Twitter API
%% @author Brian Lee Yung Rowe
-module(twitter_oauth).
-behaviour(gen_oauth).
-export([start_link/0, start_link/1,
  get_favorites/1,
  search_tweets/2, search_tweets/3,
  get_user_timeline/2,
  filter_stream/2, filter_stream/3,
  get_account_settings/1,
  get_rate_limits/2,
  get_friends/2, get_followers/2
]).
-export([init/1,
  set_consumer_key/2,
  get_request_token/2,
  get_authentication_url/2,
  get_authorization_url/2,
  get_access_token/2,
  set_access_token/2]).


start_link() ->
  gen_oauth:start_link(twitter, []).

start_link(ServerName) ->
  gen_oauth:start_link(ServerName, twitter, []).


% Params: Query parameters for final request
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

set_access_token(ServerRef, Access) ->
  gen_oauth:set_access_token(ServerRef, Access).

set_consumer_key(ServerRef, Consumer) ->
  gen_oauth:set_consumer_key(ServerRef, Consumer).



get_account_settings(SessionId) ->
  ServerRef = gen_oauth:server_name(SessionId,twitter),
  Url = "https://api.twitter.com/1.1/account/settings.json",
  gen_oauth:http_get(ServerRef, Url, []).
  
% https://dev.twitter.com/docs/rate-limiting/1.1/limits
get_rate_limits(SessionId, UserName) ->
  ServerRef = gen_oauth:server_name(SessionId,twitter),
  Url = "https://api.twitter.com/1.1/application/rate_limit_status.json",
  gen_oauth:http_get(ServerRef, Url, [{"screen_name",UserName}]).

% https://dev.twitter.com/docs/api/1.1/get/friends/ids
get_friends(SessionId, UserName) ->
  ServerRef = gen_oauth:server_name(SessionId,twitter),
  Url = "https://api.twitter.com/1.1/friends/ids.json",
  gen_oauth:http_get(ServerRef, Url, [{"screen_name",UserName}]).

% https://dev.twitter.com/docs/api/1.1/get/followers/ids
get_followers(ServerRef, UserName) ->
  Url = "https://api.twitter.com/1.1/followers/ids.json",
  gen_oauth:http_get(ServerRef, Url, [{"screen_name",UserName}]).

get_favorites(ServerRef) ->
  Url = "https://api.twitter.com/1.1/favorites/list.json",
  gen_oauth:http_get(ServerRef, Url, []).

search_tweets(ServerRef, Query) -> search_tweets(ServerRef, Query, []).

search_tweets(ServerRef, SearchQuery, Params) ->
  Url = "https://api.twitter.com/1.1/search/tweets.json",
  gen_oauth:http_get(ServerRef, Url, [{"q",SearchQuery} | Params]).

get_user_timeline(ServerRef, Params) ->
  Url = "https://api.twitter.com/1.1/statuses/user_timeline.json",
  gen_oauth:http_get(ServerRef, Url, Params).

filter_stream(ServerRef, SearchQuery) ->
  filter_stream(ServerRef, SearchQuery, []).

filter_stream(ServerRef, SearchQuery, Params) ->
  Url = "https://stream.twitter.com/1.1/statuses/filter.json",
  gen_oauth:http_stream(ServerRef, Url, [{"track",SearchQuery} | Params]).


init(_Args) ->
  not_implemented.
