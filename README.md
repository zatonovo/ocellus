ocellus
=======

Erlang application for connecting to streaming data services with oauth

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

