%%%-------------------------------------------------------------------
%%% @author grzegorz
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. mar 2020 00:15
%%%-------------------------------------------------------------------
-module(power).
-author("grzegorz").

%% API
-export([power/2]).

power(_, 0) -> 1;
power(A, 1) -> A;
power(A, B)  -> A * power(A, B - 1).

