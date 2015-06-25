-module(instagram_client).
-behaviour(gen_oauth2).

-include("gen_oauth2.hrl").

-export([start/0, start/3, start/4, stop/1]).

% Authorization
-export([get_authorization_url/1,
         get_access_token/1,
         identity/1,
         set_access_token/2]).
% Users
-export([get_user/1, get_user/2,
         get_feed/1, get_feed/2,
         get_recent_media/1, get_recent_media/2,
         get_liked/1, get_liked/2,
         search_users/2, search_users/3]).
% Relationship
-export([get_follows/1,
         get_followed_by/1,
         get_requested_by/1,
         get_relationship/2]).
% Media
-export([get_media/2,
         get_media_by_shortcut/2,
         get_popular_media/1,
         search_media/2]).
% Comments
-export([get_media_comments/2]).
% Likes
-export([get_media_likes/2]).
% Tags
-export([get_tag/2,
         get_tag_recent_media/2, get_tag_recent_media/3,
         search_tags/2]).

-export([start_link/1, start_link/2, start_link/3]).

-define(CLIENT_TOKEN_URL(ClientId, RedirectURI),
        "https://instagram.com/oauth/authorize/"
        "?client_id=" ++ ClientId ++
        "&redirect_uri=" ++ http_uri:encode(RedirectURI) ++
        "&response_type=token").

-define(BASE_API_URL,       "https://api.instagram.com/v1").
-define(API_URL(Chunks),    lists:flatten([?BASE_API_URL | Chunks])).

-define(FEED_URL,                       ?API_URL(["/users/self/feed"])).
-define(FOLLOW_URL,                     ?API_URL(["/users/self/follows"])).
-define(FOLLOWED_BY_URL,                ?API_URL(["/users/self/followed-by"])).
-define(LIKED_URL,                      ?API_URL(["/users/self/media/liked"])).
-define(MEDIA_URL(MediaId),             ?API_URL(["/media/", MediaId])).
-define(MEDIA_COMMENTS_URL(MediaId),    ?API_URL(["/media/", MediaId,"/comments"])).
-define(MEDIA_LIKES_URL(MediaId),       ?API_URL(["/media/", MediaId,"/likes"])).
-define(MEDIA_POPULAR_URL,              ?API_URL(["/media/popular"])).
-define(MEDIA_SEARCH_URL,               ?API_URL(["/media/search"])).
-define(MEDIA_SHORTCUT_URL(ShortCode),  ?API_URL(["/media/shortcode/", ShortCode])).
-define(RECENT_MEDIA_URL,               ?API_URL(["/users/self/media/recent"])).
-define(REQUESTED_BY_URL,               ?API_URL(["/users/self/requested-by"])).
-define(RELATIONSHIP_URL(User),         ?API_URL(["/users/", User,"/relationship"])).
-define(SEARCH_URL,                     ?API_URL(["/users/search"])).
-define(TAG_URL(Tag),                   ?API_URL(["/tags/", Tag])).
-define(TAG_RECENT_MEDIA_URL(Tag),      ?API_URL(["/tags/", Tag, "/media/recent"])).
-define(TAG_SEARCH_URL,                 ?API_URL(["/tags/search"])).
-define(USER_URL(User),                 ?API_URL(["/users/", User])).

%%%===================================================================
%%% Start/stop API
%%%===================================================================
-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    {ok, ClientId}      = ocellus:get_env(instagram_client_id),
    {ok, ClientSecret}  = ocellus:get_env(instagram_client_secret),
    {ok, RedirectURI}   = ocellus:get_env(instagram_redirect_uri),
    start(ClientId, ClientSecret, RedirectURI).

-spec start(client_id(), client_secret(), redirect_uri()) ->
    {ok, pid()} | {error, term()}.
start(ClientId, ClientSecret, RedirectURI) when is_list(ClientId),
                                                      is_list(ClientSecret),
                                                      is_list(RedirectURI) ->
    Client = gen_oauth2:new_client(ClientId, ClientSecret, RedirectURI,
                                   ?CLIENT_TOKEN_URL(ClientId, RedirectURI)),
    instagram_sup:start_child(Client).

-spec start(atom(), client_id(), client_secret(), redirect_uri()) ->
    {ok, pid()} | {error, term()}.
start(Name, ClientId, ClientSecret, RedirectURI) when is_atom(Name),
                                                            is_list(ClientId),
                                                            is_list(ClientSecret),
                                                            is_list(RedirectURI) ->
    Client = gen_oauth2:new_client(ClientId, ClientSecret, RedirectURI,
                                   ?CLIENT_TOKEN_URL(ClientId, RedirectURI)),
    instagram_sup:start_child(Name, Client).

-spec stop(pid()) -> ok.
stop(ServerRef) ->
    gen_oauth2:stop(ServerRef).

%%%===================================================================
%%% Authorization API
%%%===================================================================
-spec get_authorization_url(pid()) -> {ok, authorization_url()}.
get_authorization_url(ServerRef) ->
    gen_oauth2:get_authorization_url(ServerRef).

-spec identity(pid()) -> {ok, atom()} | {error, no_identity}.
identity(ServerRef) ->
    gen_oauth2:identity(ServerRef).

-spec set_access_token(pid(), access_token()) ->
    ok | {error, invalid_request}.
set_access_token(ServerRef, AccessToken) ->
    gen_oauth2:set_access_token(ServerRef, AccessToken).

-spec get_access_token(pid()) ->
    {ok, access_token()}
  | {error, not_authorized}.
get_access_token(ServerRef) ->
    gen_oauth2:get_access_token(ServerRef).

%%%===================================================================
%%% User endpoints API
%%%===================================================================
-spec get_user(pid()) ->
    {ok, {http_headers(), http_body()}}
  | {error, {http_code(), http_headers(), http_body()}}.
get_user(ServerRef) ->
    get_user(ServerRef, "self").

-spec get_user(pid(), string()) ->
    {ok, {http_headers(), http_body()}}
  | {error, {http_code(), http_headers(), http_body()}}.
get_user(ServerRef, User) ->
    gen_oauth2:http_get(ServerRef, ?USER_URL(User), []).

-spec get_feed(pid()) ->
    {ok, {http_headers(), http_body()}}
  | {error, {http_code(), http_headers(), http_body()}}.
get_feed(ServerRef) ->
    get_feed(ServerRef, []).

-spec get_feed(pid(), [{count, pos_integer()} | {min_id, integer()} | {max_id, integer()}]) ->
    {ok, {http_headers(), http_body()}}
  | {error, {http_code(), http_headers(), http_body()}}.
get_feed(ServerRef, UrlParams) ->
    gen_oauth2:http_get(ServerRef, ?FEED_URL, UrlParams).

-spec get_recent_media(pid()) ->
    {ok, {http_headers(), http_body()}}
  | {error, {http_code(), http_headers(), http_body()}}.
get_recent_media(ServerRef) ->
    get_recent_media(ServerRef, []).

-spec get_recent_media(pid(), [{count, pos_integer()}
                             | {max_timestamp, integer()}
                             | {min_timestamp, integer()}
                             | {max_id, integer()}
                             | {min_id, integer()}]) ->
    {ok, {http_headers(), http_body()}}
  | {error, {http_code(), http_headers(), http_body()}}.
get_recent_media(ServerRef, UrlParams) ->
    gen_oauth2:http_get(ServerRef, ?RECENT_MEDIA_URL, UrlParams).

-spec get_liked(pid()) ->
    {ok, {http_headers(), http_body()}}
  | {error, {http_code(), http_headers(), http_body()}}.
get_liked(ServerRef) ->
    get_liked(ServerRef, []).

-spec get_liked(pid(), [{count, pos_integer()} | {max_like_id, integer()}]) ->
    {ok, {http_headers(), http_body()}}
  | {error, {http_code(), http_headers(), http_body()}}.
get_liked(ServerRef, UrlParams) ->
    gen_oauth2:http_get(ServerRef, ?LIKED_URL, UrlParams).

-spec search_users(pid(), string()) ->
    {ok, {http_headers(), http_body()}}
  | {error, {http_code(), http_headers(), http_body()}}.
search_users(ServerRef, Query) ->
    search_users(ServerRef, Query, []).

-spec search_users(pid(), string(), [{count, pos_integer()}]) ->
    {ok, {http_headers(), http_body()}}
  | {error, {http_code(), http_headers(), http_body()}}.
search_users(ServerRef, Query, UrlParams) ->
    gen_oauth2:http_get(ServerRef, ?SEARCH_URL, [{q, Query} | UrlParams]).

%%%===================================================================
%%% Relationship endpoints API
%%%===================================================================
-spec get_follows(pid()) ->
    {ok, {http_headers(), http_body()}}
  | {error, {http_code(), http_headers(), http_body()}}.
get_follows(ServerRef) ->
    gen_oauth2:http_get(ServerRef, ?FOLLOW_URL).

-spec get_followed_by(pid()) ->
    {ok, {http_headers(), http_body()}}
  | {error, {http_code(), http_headers(), http_body()}}.
get_followed_by(ServerRef) ->
    gen_oauth2:http_get(ServerRef, ?FOLLOWED_BY_URL).

-spec get_requested_by(pid()) ->
    {ok, {http_headers(), http_body()}}
  | {error, {http_code(), http_headers(), http_body()}}.
get_requested_by(ServerRef) ->
    gen_oauth2:http_get(ServerRef, ?REQUESTED_BY_URL).

-spec get_relationship(pid(), string()) ->
    {ok, {http_headers(), http_body()}}
  | {error, {http_code(), http_headers(), http_body()}}.
get_relationship(ServerRef, User) ->
    gen_oauth2:http_get(ServerRef, ?RELATIONSHIP_URL(User)).

%%%===================================================================
%%% Media endpoints API
%%%===================================================================
-spec get_media(pid(), string()) ->
    {ok, {http_headers(), http_body()}}
  | {error, {http_code(), http_headers(), http_body()}}.
get_media(ServerRef, MediaId) ->
    gen_oauth2:http_get(ServerRef, ?MEDIA_URL(MediaId)).

-spec get_media_by_shortcut(pid(), string()) ->
    {ok, {http_headers(), http_body()}}
  | {error, {http_code(), http_headers(), http_body()}}.
get_media_by_shortcut(ServerRef, ShortCode) ->
    gen_oauth2:http_get(ServerRef, ?MEDIA_SHORTCUT_URL(ShortCode)).

-spec get_popular_media(pid()) ->
    {ok, {http_headers(), http_body()}}
  | {error, {http_code(), http_headers(), http_body()}}.
get_popular_media(ServerRef) ->
    gen_oauth2:http_get(ServerRef, ?MEDIA_POPULAR_URL).

-spec search_media(pid(), [{lat, string()}
                         | {lng, string()}
                         | {min_timestamp, pos_integer()}
                         | {max_timestamp, pos_integer()}
                         | {distance, pos_integer()}]) ->
    {ok, {http_headers(), http_body()}}
  | {error, {http_code(), http_headers(), http_body()}}.
search_media(ServerRef, Query) ->
    gen_oauth2:http_get(ServerRef, ?MEDIA_SEARCH_URL, Query).

%%%===================================================================
%%% Comments endpoints API
%%%===================================================================
-spec get_media_comments(pid(), string()) ->
    {ok, {http_headers(), http_body()}}
  | {error, {http_code(), http_headers(), http_body()}}.
get_media_comments(ServerRef, MediaId) ->
    gen_oauth2:http_get(ServerRef, ?MEDIA_COMMENTS_URL(MediaId)).

%%%===================================================================
%%% Likes endpoints API
%%%===================================================================
-spec get_media_likes(pid(), string()) ->
    {ok, {http_headers(), http_body()}}
  | {error, {http_code(), http_headers(), http_body()}}.
get_media_likes(ServerRef, MediaId) ->
    gen_oauth2:http_get(ServerRef, ?MEDIA_LIKES_URL(MediaId)).

%%%===================================================================
%%% Tags endpoints API
%%%===================================================================
-spec get_tag(pid(), string()) ->
    {ok, {http_headers(), http_body()}}
  | {error, {http_code(), http_headers(), http_body()}}.
get_tag(ServerRef, Tag) ->
    gen_oauth2:http_get(ServerRef, ?TAG_URL(Tag)).

-spec get_tag_recent_media(pid(), string()) ->
    {ok, {http_headers(), http_body()}}
  | {error, {http_code(), http_headers(), http_body()}}.
get_tag_recent_media(ServerRef, Tag) ->
    get_tag_recent_media(ServerRef, Tag, []).

-spec get_tag_recent_media(pid(), string(), [{count, pos_integer()}
                                           | {min_tag_id, term()}
                                           | {max_tag_id, term()}]) ->
    {ok, {http_headers(), http_body()}}
  | {error, {http_code(), http_headers(), http_body()}}.
get_tag_recent_media(ServerRef, Tag, UrlParams) ->
    gen_oauth2:http_get(ServerRef, ?TAG_RECENT_MEDIA_URL(Tag), UrlParams).

-spec search_tags(pid(), string()) ->
    {ok, {http_headers(), http_body()}}
  | {error, {http_code(), http_headers(), http_body()}}.
search_tags(ServerRef, Query) ->
    gen_oauth2:http_get(ServerRef, ?TAG_SEARCH_URL, [{q, Query}]).

%%%===================================================================
%%% Internal
%%%===================================================================
-spec start_link(gen_oauth2:oauth2_client()) ->
    {ok, pid()} | {error, term()}.
start_link(Client) ->
    gen_oauth2:start_link(Client).

-spec start_link(atom() | gen_oauth2:oauth2_client(),
                 gen_oauth2:oauth2_client() | proplists:proplist()) ->
    {ok, pid()} | {error, term()}.
start_link(Name, Client) when is_atom(Name) ->
    gen_oauth2:start_link(Name, Client);
start_link(Client, Opts) ->
    gen_oauth2:start_link(Client, Opts).

-spec start_link(atom(), gen_oauth2:oauth2_client(), proplists:proplist()) ->
    {ok, pid()} | {error, term()}.
start_link(Name, Client, Opts) ->
    gen_oauth2:start_link(Name, Client, Opts).
