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
-export([getHourlyMean/4, getMaximumGradientStations/1, getDailyAverageDataCount/1, getDailyOverLimit/4]).
-export([getDailyAverageMensurationOfStation/2, getDailyUnderLimit/4]).

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
    true -> throw("Station with such name or coordinates exists"), Monitor;
    false -> maps:put({Name, Coord}, [], Monitor)
  end.

%% sprawdza podana nazwa lub współrzędne już istnieją dla danego monitoru
checkStation(Name, Coord, Tuple) ->
  List = tuple_to_list(Tuple),
  lists:member(Name, List) orelse lists:member(Coord, List).

%% dodaje wartość do danej stacji, uniemożliwia dodanie do nieistniejącej stacji (sprawdza czy taka istnieje)
%% lub takich dwóch pomiarów o tych samych wszystkich wartościach (tej samej godzinie,
addValue(Name, Date, Type, Value, Monitor) ->
  Keys = maps:keys(Monitor),
  FiltredList = lists:filter(fun(X) -> lists:member(Name, tuple_to_list(X)) end, Keys),
  StationExist = [] =/= FiltredList,
  case StationExist of
    false -> throw("Station with such name or coordinates doesn't exist"), Monitor;
    true -> [Key] = FiltredList,
      DateOnlyWithHour = convertDate(Date),
      Mensurations = maps:get(Key, Monitor),
      MensurationExist = lists:any(fun(X) -> checkParameters(DateOnlyWithHour, Type, X) end, Mensurations),
      case MensurationExist of
        true -> throw("Such mensuration exist"), Monitor;
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
  FiltredList = lists:filter(fun(X) -> lists:member(Name, tuple_to_list(X)) end, Keys),
  StationExist = [] =/= FiltredList,
  case StationExist of
    false -> throw("Station with such name or coordinates doesn't exist"), Monitor;
    true -> [Key] = FiltredList,
      DateOnlyWithHour = convertDate(Date),
      Mensurations = maps:get(Key, Monitor),
      FiltredValues = lists:filter(fun(X) -> not (checkParameters(DateOnlyWithHour, Type, X)) end, Mensurations),
      Monitor#{Key := FiltredValues}
  end.

%% zwraca wartość pomiaru konkretnego typu, dla podanej stacji i daty (z godziną)
getOneValue(Name, Date, Type, Monitor) ->
  Keys = maps:keys(Monitor),
  FiltredList = lists:filter(fun(X) -> lists:member(Name, tuple_to_list(X)) end, Keys),
  StationExist = [] =/= FiltredList,
  case StationExist of
    false -> throw("Station with such name or coordinates doesn't exist"), {};
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
  FiltredList = lists:filter(fun(X) -> lists:member(Name, tuple_to_list(X)) end, Keys),
  StationExist = [] =/= FiltredList,
  case StationExist of
    false -> throw("Station with such name or coordinates doesn't exist"), 0;
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
    0 -> throw("There is no such mensurations! It's impossible to count mean!"), 0;
    _ -> lists:sum(L) / length(L)
  end.

%% funkcja wyliczająca średnią wartość pomiarów danego typu dla wszystkich stacji
getDailyMean(Date, Type, Monitor) ->
  DateOnlyWithHour = convertDate(Date),
  Values = maps:values(Monitor),
  FlattenValues = lists:flatten(Values),
  RightValues = lists:filter(fun(X) -> checkParameters(DateOnlyWithHour, Type, X) end, FlattenValues),
  meanOfValues(RightValues).

%% funkcja wylicza średnią wartość pomiaru danego typu każdego dnia o podanej godzinie na danej stacji
getHourlyMean(Name, Hour, Type, Monitor) ->
  Keys = maps:keys(Monitor),
  FiltredList = lists:filter(fun(X) -> lists:member(Name, X) end, Keys),
  StationExist = [] =/= FiltredList,
  case StationExist of
    false -> throw("Station with such name or coordinates doesn't exist"), 0;
    true -> [Key] = FiltredList,
      Mensurations = maps:get(Key, Monitor),
%%    zostawiamy tylko dane o podanym typie i godzinie
      FiltredValues = lists:filter(fun(X) -> checkHourOfDay(Hour, Type, X) end, Mensurations),
      meanOfValues(FiltredValues)
  end.

%% funkcja pobiera godzine i typ z tupli z danymi i zwraca true jesli obie zgadzają się z podanym parametrem
checkHourOfDay(CheckHour, CheckType, Data) ->
  {{_, Hour}, Type, _} = Data,
  Hour =:= CheckHour andalso CheckType =:= Type.

%% funkcja oblicza średnią ilość pomiarów dziennie przypadającą na jedną stację
getDailyAverageDataCount(Monitor) ->
%%  StationsAndMensurations = [{{Name, Coord}, NumberOfMensurations / number of days}, ...]
  StationsAndMensurations = [{X, Y / Z} || X <- maps:keys(Monitor), %% X - station name
    Y <- [length(maps:get(X, Monitor))], % Y - Number of Mensurations
    Z <- [length(getUniqueDays(maps:get(X, Monitor)))]], % Z - number of days
  Mensurations = [X || {_, X} <- StationsAndMensurations],
  mean(Mensurations).

getUniqueDays(Values) ->
  sets:to_list(sets:from_list([element(1,X) || X <- Values])).

%% funkcja wylicza maksymalny gradient ze względu na odlegość dla podanego monitoru,
%% używa do tego funkcji countMaxGradient - jako Max używa tupli {0,{-1,-1},{-1,-1}}
getMaximumGradientStations(Monitor) ->
  countMaxGradient(Monitor, getUniqueTypes(Monitor), {0,{-1,-1},{-1,-1}}).

%% pomocnicza funcja zwraca set wszystkich zmierzonych typów pomiarów w postaci listy
getUniqueTypes(Monitor) ->
  sets:to_list(sets:from_list([element(2,X) || X <- lists:flatten(maps:values(Monitor))])).

%% właściwa funkcja wyliczająca maksymalny gradient, gdy brakuje wystarczającej ilości pomiarów, rzuca wyjątek
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

%% wylicza największy gradient dla podanego typu między wszystkimi stacjami pomiarowymi
countOneValue(DataWithCoords, Type, Max) ->
  DataOfType = lists:filter(fun(X) -> Type =:= element(3, X) end, DataWithCoords),
  GradientValue = [gradient( element(4,X), element(4,Y), element(1,X), element(1,Y)) ||
    X<-DataOfType, Y<-DataOfType, element(1,X) =/= element(1,Y)],
  NewMax = lists:foldl(fun({W, Coord1, Coord2}, Acc) -> max({W, Coord1, Coord2}, Acc) end, {}, GradientValue),
  if NewMax > Max -> NewMax;
    true -> Max
  end.

%% wylicza gradient dla dwóch wartości z dwóch stacji
gradient(Value1, Value2, Coord1, Coord2) ->
  { (absolute(Value2 - Value1) / distance(Coord1, Coord2)) , Coord1, Coord2 }.

%% wylicza dystans między dwiema stacjami
distance({X1, Y1}, {X2, Y2}) ->
  math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

%% wartość absolutna
absolute(X) ->
  if X >= 0 -> X;
    true -> (-X)
  end.

%% zwraca liczbę stacji na których została przekroczona norma dla danego parametru
getDailyOverLimit(Date, Type, Limit, Monitor) ->
  %%  StationsAndMensurations = [{{Name, Coord}, NumberOfMensurations / number of days}, ...]
  StationsAndMensurations = [{X, Y} || X <- maps:keys(Monitor), %% X - station name
    Y <- maps:get(X, Monitor),
    Date =:= element(1, element(1, Y)),
    Limit < element(3, Y),
    Type =:= element(2, Y)],
  StationsAndMensurations,
  length(StationsAndMensurations).



%% zwraca średnią dzienną ilość pomiarów dla danej stacji
getDailyAverageMensurationOfStation(Name, Monitor) ->
  Keys = maps:keys(Monitor),
  FiltredList = lists:filter(fun(X) -> lists:member(Name, tuple_to_list(X)) end, Keys),
  StationExist = [] =/= FiltredList,
  case StationExist of
    false -> io:format("Station with such name or coordinates doesn't exist"), {};
    true -> [Key] = FiltredList,
      MensurationsNumber = length(maps:get(Key, Monitor)),
      DayNumber = length(getUniqueDays(maps:get(Key, Monitor))),
      MensurationsNumber / DayNumber
  end.

%% zwraca liczbę stacji na których nie została przekroczona norma dla danego parametru
getDailyUnderLimit(Date, Type, Limit, Monitor) ->
  %%  StationsAndMensurations = [{{Name, Coord}, NumberOfMensurations / number of days}, ...]
  StationsAndMensurations = [{X, Y} || X <- maps:keys(Monitor), %% X - station name
    Y <- maps:get(X, Monitor),
    Date =:= element(1, element(1, Y)),
    Limit > element(3, Y),
    Type =:= element(2, Y)],
  length(StationsAndMensurations).

