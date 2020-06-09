%%%-------------------------------------------------------------------
%%% @author grzegorz
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. cze 2020 19:48
%%%-------------------------------------------------------------------
-module(pollution_quickcheck_test).
-include_lib("eqc/include/eqc.hrl").
-author("grzegorz").

-compile([export_all]).
%% API
%%-export([]).

hours() ->
  choose(0, 23).

prop_hour() ->
  ?FORALL(H, hours(),
    ((H >= 0) and (H =< 23))).

minutes() ->
  choose(0, 59).

prop_minutes() ->
  ?FORALL(M, minutes(),
    ((M >= 0) and (M < 60))).

seconds() ->
  choose(0, 59).

prop_seconds() ->
  ?FORALL(S, seconds(),
    ((S >= 0) and (S < 60))).

hour() ->
  {hours(), minutes(), seconds()}.

calendarDate() ->
  {2020, choose(1, 12), choose(1, 30)}.

checkMonth(M) ->
  ((M > 0) and (M =< 12)).

checkDay(D) ->
  ((D > 0) and (D =< 30)).

prop_calendarDate() ->
  ?FORALL({Y, M, D}, calendarDate(),
    ?IMPLIES(Y == 2020, ?IMPLIES(checkMonth(M), checkDay(D)))).

myDate() ->
  {calendarDate(), hour()}.

type() ->
  elements(["PM10", "PM2.5", "PM5"]).

typeVal() ->
  ?LET({Int, Float}, {int(), real()}, abs(Int + Float)).

name() ->
  ?LET({N, M}, {elements(["Wola", "Gilowice", "Tychy"]), vector(3, char())},
    N ++ M).

coord() ->
  {choose(0, 90), choose(0, 180)}.

prop_stationInsert() ->
  ?FORALL({Name, Coord}, {name(), coord()},
    begin
      pollution_gen_server:addStation(Name, Coord),
      maps:is_key({Name, Coord}, pollution_gen_server:getMonitor())
    end).

prop_dateInsert() ->
  Name = "Wola",
  pollution_gen_server:addStation(Name, {30, 24}),
  ?FORALL({Date, Type, TypeVal},
    {myDate(), type(), typeVal()},
    begin
      pollution_gen_server:addValue(Name, Date, Type, TypeVal),
      (pollution_gen_server:getOneValue(Name, Date, Type)) == TypeVal
    end).

