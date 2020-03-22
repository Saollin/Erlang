%%%-------------------------------------------------------------------
%%% @author grzegorz
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. mar 2020 21:18
%%%-------------------------------------------------------------------
-module(qsort).
-author("grzegorz").

%% API
-export([lessThan/2, grtEqThan/2, qs/1, randomElems/3, compareSpeeds/3]).

lessThan(List, Arg) ->
    [X || X <- List, X < Arg].

grtEqThan(List, Arg) ->
  [X || X <- List, X >= Arg].

qs([]) -> [];
qs([Pivot | Tail]) -> qs( lessThan(Tail, Pivot) ) ++ [Pivot] ++ qs( grtEqThan(Tail, Pivot) ).

randomElems(N, Min, Max) ->
  Range = Max - Min + 1,
  [(X + rand:uniform(Range)) rem Range + Min || X <- lists:seq(1, N)].

compareSpeeds(List, Func1, Func2) ->
  Time1 = element(1, timer:tc(Func1, [List])),
  Time2 = element(1, timer:tc(Func2, [List])),
  io:format("Time of first function: ~B~nTime of second function: ~B~n", [Time1, Time2]).
  %calling the function: "qsort:compareSpeeds(List, fun qsort:qs/1, fun lists:sort/1).