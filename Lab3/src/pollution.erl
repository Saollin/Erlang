%%%-------------------------------------------------------------------
%%% @author grzegorz
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. kwi 2020 14:34
%%%-------------------------------------------------------------------
-module(pollution).
-author("grzegorz").

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/5, getOneValue/4, getStationMean/3, getDailyMean/3]).


createMonitor() ->
  #{}.

addStation(Name, Coord, Monitor) ->
  Keys = maps:keys(Monitor),
  FiltredList = lists:filter(fun(X) -> stationFilter(Name, Coord, X) end, Keys),
  IsStation = [] =/= FiltredList,
  case IsStation of
    true -> io:format("Station with such name or coordinates exists~n"), Monitor;
    false -> maps:put([Name, Coord], [], Monitor)
  end.

stationFilter(Name, Coord, List) ->
  lists:member(Name, List) orelse lists:member(Coord, List).

addValue(Name, _Arg1, _Arg2, _Arg3, _Arg4) ->
  erlang:error(not_implemented).

getDailyMean(_Arg0, _Arg1, _Arg2) ->
  erlang:error(not_implemented).

getStationMean(_Arg0, _Arg1, _Arg2) ->
  erlang:error(not_implemented).




getOneValue(_Arg0, _Arg1, _Arg2, _Arg3) ->
  erlang:error(not_implemented).

removeValue(_Arg0, _Arg1, _Arg2, _Arg3, _Arg4) ->
  erlang:error(not_implemented).