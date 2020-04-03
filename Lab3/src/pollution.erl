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

%% tworzy nowy monitor
createMonitor() ->
  #{}.

%% dodaje nową stację pomiarową, przy próbie dodania stacji z instniejącą nazwą lub
%% współrzędnymi wyświetla komunikat i zwraca poprzedni monitor
addStation(Name, Coord, Monitor) ->
  Keys = maps:keys(Monitor),
  FiltredList = lists:filter(fun(X) -> stationFilter(Name, Coord, X) end, Keys),
  IsStation = [] =/= FiltredList,
  case IsStation of
    true -> io:format("Station with such name or coordinates exists~n"), Monitor;
    false -> maps:put([Name, Coord], [], Monitor)
  end.

%% sprawdza podana nazwa lub współrzędne już istnieją dla danego monitoru
stationFilter(Name, Coord, List) ->
  lists:member(Name, List) orelse lists:member(Coord, List).

%% dodaje wartość do danej stacji, uniemożliwia dodanie do nieistniejącej stacji (sprawdza czy taka istnieje)
%% lub takich dwóch pomiarów o tych samych wszystkich wartościach (tej samej godzinie,
addValue(Name, Date, Type, Value, Monitor) ->
  Keys = maps:keys(Monitor),
  FiltredList = lists:filter(fun(X) -> lists:member(Name, X) end, Keys),
  StationExist = [] =/= FiltredList,
  case StationExist of
    false -> io:format("Station with such name or coordinates doesn't exist~n"), Monitor;
    true -> [Key] = FiltredList,
      DateOnlyWithHour = convertDate(Date),
      Mensurations = maps:get(Key, Monitor),
      MensurationExist = lists:any(fun(X) -> checkParameters(DateOnlyWithHour, Type, X) end, Mensurations),
      case MensurationExist of
        true -> io:format("Such mensuration exist~n"), Monitor;
        false -> Monitor#{Key := [{DateOnlyWithHour, Type, Value}] ++ Mensurations}
      end
  end.

%%zwraca Date bez minut i sekund (tylko z godziną), jeśli Data jest już w takiej formie nic nie zmienia
convertDate(Date) ->
  {Day, Time} = Date,
  IsTuple = is_tuple(Time),
  case IsTuple of
    false -> Date;
    true -> {Hour, _, _ } = Time,
      {Day, Hour}
  end.


%% sprawdza czy istnieje już pomiar o danej godzinie i typie
checkParameters(Date, Type, Tuple) ->
  List = tuple_to_list(Tuple),
  lists:member(Date, List) andalso lists:member(Type, List).

%%
removeValue(_Arg0, _Arg1, _Arg2, _Arg3, _Arg4) ->
  erlang:error(not_implemented).


getDailyMean(_Arg0, _Arg1, _Arg2) ->
  erlang:error(not_implemented).

getStationMean(_Arg0, _Arg1, _Arg2) ->
  erlang:error(not_implemented).




getOneValue(_Arg0, _Arg1, _Arg2, _Arg3) ->
  erlang:error(not_implemented).

