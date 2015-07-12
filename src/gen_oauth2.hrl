-type access_token()        :: string().
-type authorization_url()   :: string().
-type client_id()           :: string().
-type client_secret()       :: string().
-type redirect_uri()        :: string().
-type http_headers()        :: proplists:proplist().
-type http_body()           :: string().
-type http_code()           :: pos_integer().
-type options()             :: [{http_get_fun, fun()}
                              | {access_token, string()}].
