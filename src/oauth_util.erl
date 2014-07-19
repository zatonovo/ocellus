%% OAuth tokens in application env are same as required by gen_oauth:
%% { "Token", "Secret", hmac_sha1 }
-module(oauth_util).
-export([consumer_tokens/1]).

%% This function provides a means for managing oauth tokens for 
%% multiple services.
consumer_tokens(Provider) ->
  StrProvider = string:to_upper(atom_to_list(Provider)), 
  Key = string:concat(StrProvider, "_CONSUMER_TOKEN"),
  case os:getenv(Key) of
    false ->
      {ok, Consumer} = application:get_env(me3rest, twitter_consumer),
      Consumer;
    Token ->
      Secret = os:getenv(string:concat(StrProvider, "_CONSUMER_SECRET")),
      {Token, Secret, hmac_sha1}
  end.

