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

Consumer = {"1v19ot5exN8dKpVpImHHmiFhA","QUEatLR344UpSyBjHEc6J8X7mtgoIBPtFqnpb2hPJLSzPKTfD3",hmac_sha1}.
Access = {"16145405-fZveWgBbtq8m30aW447Pir7WOzcpWAr2fIRHqU30E","0fMdSzsZplCzK43OvRFSNNofYNR0sW9L8QjsBSeZOJt01"}.
Query = "#Science,#Ciencia,#Educación,#Education,#SocialScience,#CienciasSociales,#Communication,#Comunicación,#OpenData,@scimago,@thomsonreuters,@Scopus,@redescielo".

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

Configuration
. query id
. search terms
. consumer tokens
. access tokens
. max list size (n)

Database Callback Provider
. Decode tweet, add to buffer
. When length > n, save to DB

. Create table
. Create app
. [pzstore] Add insert query
. [pzstore] Add select query



