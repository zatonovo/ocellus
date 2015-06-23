-module(instagram_SUITE).

-export([all/0,
         init_per_suite/1,
         end_per_suite/1]).
-export([t_start_stop_named_process/1,
         t_start_stop_anonymous_process/1,
         t_set_get_access_token/1,
         t_error_start_same_named_process/1,
         t_error_unauthorized_http_get/1,
         t_error_limit_exception_http_get/1,
         t_authorized_http_get/1]).

-include_lib("common_test/include/ct.hrl").

-define(CLIENT_ID, "fake_client_id").
-define(CLIENT_SECRET, "fake_client_secret").
-define(CLIENT_REDIRECT_URL, "https://127.0.0.1/").
-define(CLIENT_ACCESS_TOKEN, "fake_access_token").
-define(CLIENT_TOKEN_URL,
        "https://instagram.com/oauth/authorize/"
        "?client_id=client_id&redirect_uri=redirect_uri&response_type=token").

all() ->
    [
     t_start_stop_named_process,
     t_start_stop_anonymous_process,
     t_set_get_access_token,
     t_error_start_same_named_process,
     t_error_unauthorized_http_get,
     t_error_limit_exception_http_get,
     t_authorized_http_get
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ocellus),
    Config.

end_per_suite(_Config) ->
    application:stop(ocellus),
    ok.

t_start_stop_named_process(_Config) ->
    {ok, Pid} = instagram:start(t_start_stop_named_process,
                                ?CLIENT_ID,
                                ?CLIENT_SECRET,
                                ?CLIENT_REDIRECT_URL),
    ok = test_helpers:keep_trying(
           Pid, fun() -> erlang:whereis(t_start_stop_named_process) end,
           10, 10),
    {ok, t_start_stop_named_process} = instagram:identity(Pid),
    ok = instagram:stop(Pid),
    ok = test_helpers:keep_trying(
           undefined, fun() -> erlang:whereis(t_start_stop_named_process) end,
           10, 10),
    ok.

t_start_stop_anonymous_process(_Config) ->
    {ok, Pid} = instagram:start(?CLIENT_ID,
                                ?CLIENT_SECRET,
                                ?CLIENT_REDIRECT_URL),
    {error, no_identity} = instagram:identity(Pid),
    ok = instagram:stop(Pid),
    ok.

t_set_get_access_token(_Config) ->
    {ok, Pid} = instagram:start(?CLIENT_ID,
                                ?CLIENT_SECRET,
                                ?CLIENT_REDIRECT_URL),
    {error, not_authorized} = instagram:get_access_token(Pid),
    ok = instagram:set_access_token(Pid, ?CLIENT_ACCESS_TOKEN),
    {ok, ?CLIENT_ACCESS_TOKEN} = instagram:get_access_token(Pid),
    ok = instagram:stop(Pid),
    ok.

t_error_start_same_named_process(_Config) ->
    {ok, Pid} = instagram:start(t_error_start_same_named_process,
                                ?CLIENT_ID,
                                ?CLIENT_SECRET,
                                ?CLIENT_REDIRECT_URL),
    {error, {already_started, Pid}} = instagram:start(t_error_start_same_named_process,
                                                      ?CLIENT_ID,
                                                      ?CLIENT_SECRET,
                                                      ?CLIENT_REDIRECT_URL),
    ok.

t_error_unauthorized_http_get(_Config) ->
    {ok, Pid} = instagram:start(?CLIENT_ID,
                                ?CLIENT_SECRET,
                                ?CLIENT_REDIRECT_URL),
    {error, invalid_request} = instagram:get_liked(Pid),
    ok.

t_authorized_http_get(_Config) ->
    ExpectedResponse = {ok, {[], "response"}},
    HttpGetFn        = fun(_Url) -> {ok, {{"", 200, ""}, [], "response"}} end,

    Client = gen_oauth2:new_client(?CLIENT_ID, ?CLIENT_SECRET, ?CLIENT_REDIRECT_URL,
                                   ?CLIENT_TOKEN_URL),
    {ok, Pid} = instagram_sup:start_child(Client, [{http_get_fun, HttpGetFn}]),
    {ok, ?CLIENT_TOKEN_URL} = instagram:get_authorization_url(Pid),
    ok = instagram:set_access_token(Pid, ?CLIENT_ACCESS_TOKEN),
    ExpectedResponse = instagram:get_liked(Pid),
    ok.

t_error_limit_exception_http_get(_Config) ->
    ExpectedResponse = {error, {429, [], "response"}},
    HttpGetFn        = fun(_Url) -> {ok, {{"", 429, ""}, [], "response"}} end,

    Client = gen_oauth2:new_client(?CLIENT_ID, ?CLIENT_SECRET, ?CLIENT_REDIRECT_URL,
                                   ?CLIENT_TOKEN_URL),
    {ok, Pid} = instagram_sup:start_child(Client, [{http_get_fun, HttpGetFn}]),
    {ok, ?CLIENT_TOKEN_URL} = instagram:get_authorization_url(Pid),
    ok = instagram:set_access_token(Pid, ?CLIENT_ACCESS_TOKEN),
    ExpectedResponse = instagram:get_liked(Pid),
    ok.
