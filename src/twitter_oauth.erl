%% Provides an implementation of the Twitter API
%% @author Brian Lee Yung Rowe
-module(twitter_oauth).
-behaviour(gen_oauth).
-export([start_link/2, start_link/3,
  get_favorites/1,
  search_tweets/2, search_tweets/3,
  get_user_timeline/2,
  filter_stream/3, filter_stream/4
]).
-export([init/1,
  get_request_token/2,
  get_authorization_url/2,
  get_access_token/2,
  set_access_token/3]).

start_link(Consumer, Options) ->
  gen_oauth:start_link(Consumer, Options).

start_link(ServerName, Consumer, Options) ->
  gen_oauth:start_link(ServerName, Consumer, Options).


% Params: Query parameters for final request
get_request_token(Pid, Params) ->
  Url = "https://twitter.com/oauth/request_token",
  gen_oauth:get_request_token(Pid, Url, Params).

get_authorization_url(Pid, Token) ->
  Url = "https://twitter.com/oauth/authorize",
  gen_oauth:get_authorization_url(Pid, Url, Token).

get_access_token(Pid, VerifierPin) ->
  Url = "https://twitter.com/oauth/access_token",
  gen_oauth:get_access_token(Pid, Url, VerifierPin).

set_access_token(Pid, Token, Secret) ->
  gen_oauth:set_access_token(Pid, Token, Secret).



get_favorites(Pid) ->
  Url = "https://api.twitter.com/1.1/favorites/list.json",
  gen_oauth:http_get(Pid, Url, []).

search_tweets(Pid, Query) -> search_tweets(Pid, Query, []).

search_tweets(Pid, SearchQuery, Params) ->
  Url = "https://api.twitter.com/1.1/search/tweets.json",
  gen_oauth:http_get(Pid, Url, [{"q",SearchQuery} | Params]).

get_user_timeline(Pid, Params) ->
  Url = "https://api.twitter.com/1.1/statuses/user_timeline.json",
  gen_oauth:http_get(Pid, Url, Params).

filter_stream(Pid, SearchQuery, Callback) ->
  filter_stream(Pid, SearchQuery, [], Callback).

filter_stream(Pid, SearchQuery, Params, Callback) ->
  Url = "https://stream.twitter.com/1.1/statuses/filter.json",
  gen_oauth:http_stream(Pid, Url, [{"track",SearchQuery} | Params], Callback).


init(_Args) ->
  not_implemented.
