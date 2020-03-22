%%%-------------------------------------------------------------------
%%% @author grzegorz
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. mar 2020 19:09
%%%-------------------------------------------------------------------
-module(onpCalculator).
-author("grzegorz").

%% API
-export([onp/1]).

onp(List) when is_list(List) ->
  [Result] = lists:foldl(fun onp/2, [], string:tokens(List, " ")),
  Result.

onp("-", [X, Y | Stack])  -> [(Y - X) | Stack];
onp("+", [X, Y | Stack])  -> [(X + Y) | Stack];
onp("*", [X, Y | Stack])  -> [(X * Y) | Stack];
onp("/", [X, Y | Stack])  -> [(Y / X) | Stack];
onp("^", [X, Y | Stack])  -> [(math:pow(Y, X)) | Stack];
onp("pow", [X, Y | Stack])  -> [(math:pow(Y, X)) | Stack];
onp("sqrt", [X | Stack])  -> [(math:sqrt(X)) | Stack];
onp("sin", [X | Stack])   -> [(math:sin(X)) | Stack];
onp("cos", [X | Stack])   -> [(math:cos(X)) | Stack];
onp("tg", [X | Stack])    -> [(math:tan(X)) | Stack];
onp("tan", [X | Stack])   -> [(math:tan(X)) | Stack];
onp("ctg", [X | Stack])   -> [(1.0 / math:tan(X)) | Stack];
onp("double", [X | Stack]) -> [(2 * X) | Stack];
onp("doublesum", [X, Y | Stack]) -> [2 * (X + Y) | Stack];

onp(Num, Stack)           -> [parseNumber(Num) | Stack].

parseNumber(Num) ->
  case string:to_float(Num) of
    {error, _} -> list_to_integer(Num);
    {Float, _} -> Float
  end.