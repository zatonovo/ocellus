ocellus
=======

Erlang application for connecting to streaming data services with oauth

Initial Handshake
-----------------

```erlang
Params = [{"track","erlang"}].
Consumer = {"Token", "Secret", hmac_sha1}.
{ok, Pid} = twitter_oauth:start_link(Consumer,[]).
{ok, Token} = twitter_oauth:get_request_token(Pid, Params).
AuthorizeUrl = twitter_oauth:get_authorization_url(Pid, Token).
Access = twitter_oauth:get_access_token(Pid, "Verifier PIN").
Tweets = twitter_oauth:search_tweets(Pid, "erlang").
```

Twitter
=======

REST API
--------

```erlang
Consumer = {"Token", "Secret", hmac_sha1}.
Access = {"Token", "Secret"}.
{ok, Pid} = twitter_oauth:start_link(Consumer,[]).
twitter_oauth:set_access_token(Pid, Access).
twitter_oauth:search_tweets(Pid, Query).
```

Streaming API
-------------

```erlang
Consumer = {"Token", "Secret", hmac_sha1}.
Access = {"Token", "Secret"}.
{ok, Pid} = twitter_oauth:start_link(Consumer,[]).
twitter_oauth:set_access_token(Pid, Access).
Callback =  fun(Data) ->
  Tweet = proplists:get_value(<<"text">>, Data),
  io:format("Erlang <3: ~s~n", [Tweet])
end.
twitter_oauth:filter_stream(Pid, Query, Callback).
```

