%%%-------------------------------------------------------------------
%%% @author grzegorz
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. kwi 2020 23:48
%%%-------------------------------------------------------------------
-module(parallel_computing).
-author("grzegorz").

%% API
-export([findMyParcelLocker/2, randomList/3, findSequential/2, compare/0, findParallel/2, parallelFunction/3]).
-export([findHalfParallel/2, halfParallelFunction/3]).

compare() ->
  LockersList = parallel_computing:randomList(1000, 0, 10000),
  PersonsList = parallel_computing:randomList(10000, 0, 10000),
  T1 = element(1, timer:tc(fun findSequential/2, [LockersList, PersonsList])),
  T2 = element(1, timer:tc(fun findParallel/2, [LockersList, PersonsList])),
  T3 = element(1, timer:tc(fun findHalfParallel/2, [LockersList, PersonsList])),
  io:format("Time of seqeuntial function: ~B~nTime of full parallel function: ~B~n", [T1, T2]),
  io:format("Time of partying counting on each core: ~B~n", [T3]).


findSequential(LList, PList) ->
  [findMyParcelLocker(LList, X) || X <- PList].

findParallel(LList, PList) ->
  ParentPid = self(),
  [ spawn(?MODULE, parallelFunction, [LList, P, ParentPid]) || P <- PList],
  [receive Result -> Result end].

findHalfParallel(LList, PList) ->
  ParentPid = self(),
  Size = floor(length(PList) / 8),
  [ spawn(?MODULE, halfParallelFunction, [LList, Part, ParentPid]) ||
    X <- lists:seq(0, 7), Part <- [lists:sublist(PList, (X * Size) + 1, Size)]],
  R = [receive Result -> Result end],
  lists:append(R).


parallelFunction(L, P, Pid) ->
  Result = findMyParcelLocker(L, P),
  Pid ! Result.

halfParallelFunction(LList, PList, Pid) ->
  Result = [findMyParcelLocker(LList, P) || P <- PList],
  Pid ! Result.

findMyParcelLocker(List, Person) ->
  Distances = [{{NewX, NewY}, Distance} || {NewX, NewY} <- List, Distance <- [distance(Person, {NewX, NewY})]],
  Sorted = lists:sort(fun({_, A}, {_, B}) -> A =< B end, Distances),
  {Person, element(1, lists:nth(1, Sorted))}.


distance({X1, Y1}, {X2, Y2}) ->
  math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

randomList(N, Min, Max) ->
  Range = Max - Min + 1,
  [{(rand:uniform(Range)) rem Range + Min, (rand:uniform(Range)) rem Range + Min}
    || _ <- lists:seq(1, N)].