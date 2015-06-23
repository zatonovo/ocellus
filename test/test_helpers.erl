-module(test_helpers).

-export([keep_trying/4]).

-include_lib("common_test/include/ct.hrl").

keep_trying(Match, F, Sleep, Tries) ->
    try
        case F() of
            Match ->
                ok;
            Unexpected ->
                throw({unexpected, Unexpected})
        end
    catch
        _:_=E ->
              timer:sleep(Sleep),
              case Tries-1 of
                  0 ->
                      error(E);
                  N ->
                      keep_trying(Match, F, Sleep, N)
              end
    end.
