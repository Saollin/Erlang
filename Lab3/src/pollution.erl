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
-export([getHourlyMean/4, getMaximumGradientStations/1]).
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

%% zwraca średnią wartość pomiarów danego typu dla podanej stacji, informuje gdy nie ma stacji o danej nazwie,
%% zwraca też 0 przy braku pasujących wartości
getStationMean(Name, Type, Monitor) ->
  Keys = maps:keys(Monitor),
  FiltredList = lists:filter(fun(X) -> lists:member(Name, X) end, Keys),
  StationExist = [] =/= FiltredList,
  case StationExist of
    false -> io:format("Station with such name or coordinates doesn't exist~n"), 0;
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
    0 -> io:format("There is no such mensurations! It's impossible to count mean!"), 0;
    _ -> lists:sum(L) / length(L)
  end.

%% funkcja wyliczająca średnią wartość pomiarów danego typu dla wszystkich stacji
getDailyMean(Date, Type, Monitor) ->
  DateOnlyWithHour = convertDate(Date),
  Values = maps:values(Monitor),
  FlattenValues = lists:flatten(Values),
  RightValues = lists:filter(fun(X) -> checkParameters(DateOnlyWithHour, Type, X) end, FlattenValues),
  meanOfValues(RightValues).

getHourlyMean(Name, Hour, Type, Monitor) ->
  Keys = maps:keys(Monitor),
  FiltredList = lists:filter(fun(X) -> lists:member(Name, X) end, Keys),
  StationExist = [] =/= FiltredList,
  case StationExist of
    false -> io:format("Station with such name or coordinates doesn't exist~n"), 0;
    true -> [Key] = FiltredList,
      Mensurations = maps:get(Key, Monitor),
      FiltredValues = lists:filter(fun(X) -> checkHourOfDay(Hour, Type, X) end, Mensurations),
      meanOfValues(FiltredValues)
  end.

checkHourOfDay(CheckHour, CheckType, Data) ->
  {{_, Hour}, Type, _} = Data,
  Hour =:= CheckHour andalso CheckType =:= Type.

getMaximumGradientStations(Monitor) ->
  countMaxGradient(Monitor, getUniqueTypes(Monitor), {0,{-1,-1},{-1,-1}}).

getUniqueTypes(Monitor) ->
  sets:to_list(sets:from_list([element(2,X) || X <- lists:flatten(maps:values(Monitor))])).

countMaxGradient(_, [], Max) ->
  case Max of
    {0,{-1,-1},{-1,-1}} -> throw("There is no sufficient number of mensurations.");
    _ -> Max
  end;
countMaxGradient(Monitor, [OneType|Tail], Max) ->
%%  BondedData = [{[Name, Coord], {Date, Type, Value}}, {[Name2, Coord2], {Date2, Type2, Value2}}, ...]
  BondedData = [{X, Y} || X <- maps:keys(Monitor), Y <- maps:get(X, Monitor)],
%%  DataWithCoords = [{Coords, Date, Type, Value}, ...]
  DataWithCoords = [{Coord, Date, Type, Value} || {[_, Coord], {Date, Type, Value}} <- BondedData],
  MaxValueForType = countOneValue(DataWithCoords, OneType, Max),
  countMaxGradient(Monitor, Tail, MaxValueForType).

countOneValue(DataWithCoords, Type, Max) ->
  DataOfType = lists:filter(fun(X) -> Type =:= element(3, X) end, DataWithCoords),
  GradientValue = [gradient( element(4,X), element(4,Y), element(1,X), element(1,Y)) ||
    X<-DataOfType, Y<-DataOfType, element(1,X) =/= element(1,Y)],
  NewMax = lists:foldl(fun({W, Coord1, Coord2}, Acc) -> max({W, Coord1, Coord2}, Acc) end, {}, GradientValue),
  if NewMax > Max -> NewMax;
    true -> Max
  end.

gradient(Value1, Value2, Coord1, Coord2) ->
  { (absolute(Value2 - Value1) / distance(Coord1, Coord2)) , Coord1, Coord2 }.

distance({X1, Y1}, {X2, Y2}) ->
  math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

absolute(X) ->
  if X >= 0 -> X;
    true -> (-X)
  end.