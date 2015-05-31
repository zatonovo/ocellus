-module(json_util).
-export([decode/1]).

%% Courtesy of twerl
-spec decode(binary()) -> list().
decode(Data) ->
  case Data of
    <<"\r\n">> -> [];
    _ ->
      try
        {Decoded} = jiffy:decode(Data),
        Decoded
      catch
        _Error:Reason -> 
          lager:warning("Unable to JSON-decode because ~p: ~p",[Reason,Data]),
          []
      end
  end.

