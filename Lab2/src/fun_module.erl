%%%-------------------------------------------------------------------
%%% @author grzegorz
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. mar 2020 18:23
%%%-------------------------------------------------------------------
-module(fun_module).
-author("grzegorz").

%% API
-export([map/2, filter/2, sumOfDigits/1, isDivisibleBy3/1]).

map(_, []) -> [];
map(Func, [H | T]) -> [Func(H) |map(Func, T)].

filter(_, []) -> [];
filter(Func, [H | T]) ->
    case Func(H) of
      true -> [H | filter(Func, T)];
      false -> filter(Func, T)
    end.

sumOfDigits(Number) -> lists:foldl(fun (X,Y) -> X + Y - $0 end, 0, integer_to_list(Number)).

isDivisibleBy3(N) when N rem 3 =:= 0 -> true;
isDivisibleBy3(_) -> false.

