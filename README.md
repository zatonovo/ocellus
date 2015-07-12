ocellus
=======

Erlang application for connecting to streaming data services with oauth

This library provides a `gen_fsm` behavior around the oauth process to
manage the steps. On top of this behavior are specific handlers for
APIs that use oauth. Currently only the Twitter API is supported.

Twitter
=======

Getting Started
---------------
You need a Twitter account and sign up for their developer program.
Create an application and note the consumer token and secret.

REST API
--------

### Initial Handshake

If you are using the library for any arbitrary user, it's necessary to
be granted permission by that user. The simplest way is to use Twitter's
inline authorization process. For testing purposes (i.e. in the shell),
you can use the below method.

```erlang
Params = [{"track","erlang"}].
Consumer = {"Token", "Secret", hmac_sha1}.
{ok, Pid} = twitter_client:start_link().
twitter_client:set_consumer_key(Pid, Consumer).
{ok, Token} = twitter_client:get_request_token(Pid, Params).
AuthorizeUrl = twitter_client:get_authorization_url(Pid, Token).
Access = twitter_client:get_access_token(Pid, "Verifier PIN").
Tweets = twitter_client:search_tweets(Pid, "erlang").
```

### Using existing access tokens

Once a user authorizes an application, the access tokens can be reused until
they expire. Assuming these are stored, it's possible to shortcut the
oauth handshake and simply assign the access token. Note that if you generate
access tokens with your user account for a given application, you can enter
them directly. This is also useful for an "app-only" style authentication.

```erlang
Consumer = {"Token", "Secret", hmac_sha1}.
Access = {"Token", "Secret"}.
{ok, Pid} = twitter_client:start_link().
twitter_client:set_consumer_key(Pid, Consumer).
twitter_client:set_access_token(Pid, Access).
twitter_client:search_tweets(Pid, Query).
```

Streaming API
-------------
Twitter supports both a REST API and a streaming API over HTTP. To use
the streaming API, you need to define a callback function to handle each
tweet that's pushed to ocellus.

```erlang
Consumer = {"Token", "Secret", hmac_sha1}.
Access = {"Token", "Secret"}.
Query = "".
{ok, Pid} = twitter_client:start_link(Consumer).
twitter_client:set_access_token(Pid, Access).
Callback =  fun(Pid, Provider, Json) ->
  Data = json_util:decode(Json),
  Tweet = proplists:get_value(<<"text">>, Data),
  {User} = proplists:get_value(<<"user">>,Data),
  ScreenName = proplists:get_value(<<"screen_name">>,User),
  io:format("[~p] ~s says: ~s~n", [Provider, ScreenName, Tweet])
end.

% Add a handler for twitter events
ocellus_stream_router:add_provider(twitter, Callback).

% Register ourself as the recipient of messages. This is for more sophisticated
% routing of messages.
ocellus_stream_router:register(self(), twitter, anonymous_channel).

% Initiate the stream
twitter_client:filter_stream(Pid, Query).
```

Future
======

+ Add support for other APIs
