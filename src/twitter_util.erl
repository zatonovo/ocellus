-module(twitter_util).
-export([get_rate_limit/2]).

-spec get_rate_limit(binary(), term()) -> list().
get_rate_limit(Key, {Drift,{RawLimits}}) ->
  Resource = get_resource(Key),
  {ResourcePList} = proplists:get_value(<<"resources">>, RawLimits),
  {Set} = proplists:get_value(Resource, ResourcePList),
  {Element} = proplists:get_value(Key, Set),
  ResetTs = proplists:get_value(<<"reset">>,Element) + Drift,
  {proplists:get_value(<<"remaining">>,Element), ResetTs}.


-spec get_resource(binary()) -> binary().
get_resource(Key) ->
  Parts = binary:split(Key, <<"/">>, [global]),
  lists:nth(2,Parts).
