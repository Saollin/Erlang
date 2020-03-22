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
  [Result] = lists:foldl(fun pom/2, [], string:tokens(List, " ")),
  Result.

pom("-", [X, Y | Stack])  -> [(Y - X) | Stack];
pom("+", [X, Y | Stack])  -> [(X + Y) | Stack];
pom("*", [X, Y | Stack])  -> [(X * Y) | Stack];
pom("/", [X, Y | Stack])  -> [(Y / X) | Stack];
pom("^", [X, Y | Stack])  -> [(math:pow(Y, X)) | Stack];
pom("pow", [X, Y | Stack])  -> [(math:pow(Y, X)) | Stack];
pom("sqrt", [X | Stack])  -> [(math:sqrt(X)) | Stack];
pom("sin", [X | Stack])   -> [(math:sin(X)) | Stack];
pom("cos", [X | Stack])   -> [(math:cos(X)) | Stack];
pom("tg", [X | Stack])    -> [(math:tan(X)) | Stack];
pom("tan", [X | Stack])   -> [(math:tan(X)) | Stack];
pom("ctg", [X | Stack])   -> [(1.0 / math:tan(X)) | Stack];
pom("double", [X | Stack]) -> [(2 * X) | Stack];
pom("doublesum", [X, Y | Stack]) -> [2 * (X + Y) | Stack];

pom(Num, Stack)           -> [parseNumber(Num) | Stack].

parseNumber(Num) ->
  case string:to_float(Num) of
    {error, _} -> list_to_integer(Num);
    {Float, _} -> Float
  end.