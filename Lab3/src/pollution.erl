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
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3]).

%% tworzy nowy monitor
createMonitor() ->
  #{}.

%% dodaje nową stację pomiarową, przy próbie dodania stacji z instniejącą nazwą lub
%% współrzędnymi wyświetla komunikat i zwraca poprzedni monitor
addStation(Name, Coord, Monitor) ->
  Keys = maps:keys(Monitor),
  FiltredList = lists:filter(fun(X) -> checkStation(Name, Coord, X) end, Keys),
  IsStation = [] =/= FiltredList,
  case IsStation of
    true -> io:format("Station with such name or coordinates exists~n"), Monitor;
    false -> maps:put([Name, Coord], [], Monitor)
  end.

%% sprawdza podana nazwa lub współrzędne już istnieją dla danego monitoru
checkStation(Name, Coord, List) ->
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

%% usuwa odczyt z danej stacji. Gdy stacja nie instnieje informuje o tym, gdy brak odczytu o takich parametrach nie informuje
%% w obu powyższych przypadkach zwraca niezmodyfikowany Monitor
removeValue(Name, Date, Type, Monitor) ->
  Keys = maps:keys(Monitor),
  FiltredList = lists:filter(fun(X) -> lists:member(Name, X) end, Keys),
  StationExist = [] =/= FiltredList,
  case StationExist of
    false -> io:format("Station with such name or coordinates doesn't exist~n"), Monitor;
    true -> [Key] = FiltredList,
      DateOnlyWithHour = convertDate(Date),
      Mensurations = maps:get(Key, Monitor),
      FiltredValues = lists:filter(fun(X) -> not (checkParameters(DateOnlyWithHour, Type, X)) end, Mensurations),
      Monitor#{Key := FiltredValues}
  end.

%% zwraca wartość pomiaru konkretnego typu, dla podanej stacji i daty (z godziną)
getOneValue(Name, Date, Type, Monitor) ->
  Keys = maps:keys(Monitor),
  FiltredList = lists:filter(fun(X) -> lists:member(Name, X) end, Keys),
  StationExist = [] =/= FiltredList,
  case StationExist of
    false -> io:format("Station with such name or coordinates doesn't exist~n"), {};
    true -> [Key] = FiltredList,
      DateOnlyWithHour = convertDate(Date),
      Mensurations = maps:get(Key, Monitor),
      [{_, _,FiltredValue}] = lists:filter(fun(X) -> checkParameters(DateOnlyWithHour, Type, X) end, Mensurations),
      FiltredValue
  end.

%% zwraca średnią wartość pomiarów danego typu dla podanej stacji
getStationMean(Name, Type, Monitor) ->
  Keys = maps:keys(Monitor),
  FiltredList = lists:filter(fun(X) -> lists:member(Name, X) end, Keys),
  StationExist = [] =/= FiltredList,
  case StationExist of
    false -> io:format("Station with such name or coordinates doesn't exist~n"), {};
    true -> [Key] = FiltredList,
      Mensurations = maps:get(Key, Monitor),
      FiltredValues = lists:filter(fun(X) -> lists:member(Type, tuple_to_list(X)) end, Mensurations),
      meanOfValues(FiltredValues)
  end.

%% pomocnicza funkcja wyciągąca z listy pomiarów tylko wartość i licząca średnią wartość
meanOfValues(List) ->
  ResultList = lists:flatmap(fun ({_,_, X}) -> [X] end, List),
  mean(ResultList).

%% funkcja wyliczająca średnią wartość zawartą w liście
mean(L) ->
  case length(L) of
    0 -> throw("There is no such mensurations! It's impossible to count mean!");
    _ -> lists:sum(L) / length(L)
  end.

%% funkcja wyliczająca średnią wartość pomiarów danego typu dla wszystkich stacji
getDailyMean(Date, Type, Monitor) ->
  DateOnlyWithHour = convertDate(Date),
  Values = maps:values(Monitor),
  FlattenValues = lists:flatten(Values),
  RightValues = lists:filter(fun(X) -> checkParameters(DateOnlyWithHour, Type, X) end, FlattenValues),
  meanOfValues(RightValues).


