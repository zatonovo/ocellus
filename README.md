ocellus
=======

Erlang application for connecting to streaming data services with oauth

Initial Handshake
-----------------

```erlang
Params = [{"track","erlang"}].
Consumer = {"Token", "Secret", hmac_sha1}.
{ok, Pid} = twitter_oauth:start_link().
twitter_oauth:set_consumer_key(Pid, Consumer).
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
{ok, Pid} = twitter_oauth:start_link().
twitter_oauth:set_consumer_key(Pid, Consumer).
twitter_oauth:set_access_token(Pid, Access).
twitter_oauth:search_tweets(Pid, Query).
```

Streaming API
-------------

```erlang
Consumer = {"Token", "Secret", hmac_sha1}.
Access = {"Token", "Secret"}.
Query = "".
{ok, Pid} = twitter_oauth:start_link(Consumer).
twitter_oauth:set_access_token(Pid, Access).
Callback =  fun(Pid, Provider, Json) ->
  Data = json_util:decode(Json),
  Tweet = proplists:get_value(<<"text">>, Data),
  {User} = proplists:get_value(<<"user">>,Data),
  ScreenName = proplists:get_value(<<"screen_name">>,User),
  io:format("[~p] ~s says: ~s~n", [Provider, ScreenName, Tweet])
end.
ocellus_stream_router:add_provider(twitter, Callback).
ocellus_stream_router:register(self(), twitter, anonymous_channel).
twitter_oauth:filter_stream(Pid, Query).
```


Tasks
x. Push ocellus
x. Push me3api
x. Redeploy me3api on data-1
x. Redeploy dragonfly on dragonfly-1
. Test if stream has closed (maybe from request id?). If so, reinitiate
