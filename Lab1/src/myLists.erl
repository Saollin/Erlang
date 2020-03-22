%%%-------------------------------------------------------------------
%%% @author grzegorz
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. mar 2020 18:54
%%%-------------------------------------------------------------------
-module(myLists).
-author("grzegorz").

%% API
-export([contains/2, duplicateElements/1, sumFloats/1]).

contains([H | T], Value) ->
  if
    H =:= Value -> true;
    H =/= Value -> contains(T, Value)
  end;
contains([], _) -> false.

duplicateElements([]) -> [];
duplicateElements([T | H]) -> [T, T] ++ duplicateElements(H).

sumFloats(List) -> sumFloats(List, 0).

sumFloats([], Sum) -> Sum;
sumFloats([H | T], Sum) -> sumFloats(T, H + Sum).

