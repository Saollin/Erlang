%%%-------------------------------------------------------------------
%%% @author grzegorz
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. cze 2020 19:32
%%%-------------------------------------------------------------------
-module(qcheck).
-include_lib("eqc/include/eqc.hrl").
-author("grzegorz").

%% API
-compile([export_all]).

even_nat() ->
  ?SUCHTHAT(N, nat(),
    N /= 0 andalso N rem 2 == 0).

prop_nat() ->
  ?FORALL(N, even_nat(),
    ((N == 0) orelse (N * N >= 37))).

prop_smaller() ->
  ?FORALL(N, even_nat(),
    ((N =< 4) orelse (N * N >= 37))).

propWithAdditionalInfo() ->
  ?WHENFAIL(io:format("There was ~n"), prop_nat()).
