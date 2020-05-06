%%%-------------------------------------------------------------------
%%% @author grzegorz
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. kwi 2020 22:10
%%%-------------------------------------------------------------------
-module(pollution_gen_server_test).
-author("grzegorz").

%% API
-include_lib("eunit/include/eunit.hrl").

prepare() ->
  pollution_server:start(),
  pollution_server:addStation("Gilowice",{50, 45}),
  pollution_server:addStation("Wola",{30, 143}),
  pollution_server:addValue("Wola", calendar:local_time(), "PM2,5", 70),
  pollution_server:addValue("Gilowice", {{2020, 3, 29}, 18}, "PM2,5", 70),
  pollution_server:addValue("Gilowice", {{2020, 3, 29}, 18}, "PM10", 98),
  pollution_server:addValue("Gilowice", {{2020, 3, 30}, 18}, "PM10", 115),
  pollution_server:addValue("Wola", {{2020, 4, 1}, 18}, "PM10", 110),
  pollution_server:addValue("Gilowice", {{2020, 4, 1}, 18}, "PM10", 50),
  pollution_server:addValue("Gilowice", {{2020, 4, 2}, 18}, "PM10", 100),
  pollution_server:addValue("Gilowice", calendar:local_time(), "PM2,5", 113),
  pollution_server:addValue("Gilowice", calendar:local_time(), "PM10", 59).

prepareWithoutOneValue() ->
  pollution_server:start(),
  pollution_server:addStation("Gilowice",{50, 45}),
  pollution_server:addStation("Wola",{30, 143}),
  pollution_server:addValue("Wola", calendar:local_time(), "PM2,5", 70),
  pollution_server:addValue("Gilowice", {{2020, 3, 29}, 18}, "PM2,5", 70),
  pollution_server:addValue("Gilowice", {{2020, 3, 29}, 18}, "PM10", 98),
  pollution_server:addValue("Gilowice", {{2020, 3, 30}, 18}, "PM10", 115),
  pollution_server:addValue("Wola", {{2020, 4, 1}, 18}, "PM10", 110),
  pollution_server:addValue("Gilowice", {{2020, 4, 1}, 18}, "PM10", 50),
  pollution_server:addValue("Gilowice", {{2020, 4, 2}, 18}, "PM10", 100),
  pollution_server:addValue("Gilowice", calendar:local_time(), "PM2,5", 113).

% próba dodania wartości w tym samym czasie i tego samego typu - zostanie zwrócona informacja o błędzie
% oraz taki sam monitor
add_the_same_value_test() ->
  prepare(),
  ?assertEqual("Such mensuration exist", pollution_server:addValue("Gilowice", calendar:local_time(), "PM2,5", 50)),
  pollution_server:stop().

%%% próba dodania stacji o nazwie która już istnieje - zostanie zwrócona informacja o błędzie
%%% oraz niezmieniony monitor
add_station_with_the_same_name_test() ->
  prepare(),
  ?assertEqual("Station with such name or coordinates exists", pollution_server:addStation("Wola",{14, 144})),
  pollution_server:stop().

%%% takiego rekordu nie ma, zwrócony zostanie niezmieniony monitor
remove_non_existing_value_test() ->
  prepare(),
  M1 = pollution_server:getMonitor(),
  pollution_server:stop(),
  prepare(),
  pollution_server:removeValue("Wola", calendar:local_time(), "PM10"),
  M2 = pollution_server:getMonitor(),
  ?assertEqual(M1, M2),
  pollution_server:stop().

% powinien zostać zwrócony monitor bez jednego rekordu
removeValue_test() ->
  prepare(),
  pollution_server:removeValue("Gilowice", calendar:local_time(), "PM10"),
  M1 = pollution_server:getMonitor(),
  pollution_server:stop(),
  prepareWithoutOneValue(),
  M2 = pollution_server:getMonitor(),
    pollution_server:stop(),
  ?assertEqual(M1, M2).

%%% powinno zostać zwrócone 113 - wartość z dzisiaj w "Gilowice" o typie "PM2,5"
getOneValue_test() ->
  prepare(),
  ?assertEqual(113, pollution_server:getOneValue("Gilowice", calendar:local_time(), "PM2,5")),
  pollution_server:stop().

% powinno zostać zwrócone 90.75, czyli średnia z 4 wartości PM10 w "Gilowice"
getStationMean_test() ->
  prepareWithoutOneValue(),
  ?assertEqual(90.75, pollution_server:getStationMean("Gilowice", "PM10")),
  pollution_server:stop().

% powinno zostać zwrócone 80, czyli średnia z pomiarów PM10 1 kwietnia w obu stacjach
getDailyMean_test() ->
  prepareWithoutOneValue(),
  ?assertEqual(80.0, pollution_server:getDailyMean({{2020, 4, 1}, 18}, "PM10")),
    pollution_server:stop().

% powinna zostać zwrócona 1,1 ("Wola" ma średnio jeden pomiar dziennie, "Gilowice" mają 6 pomiarów i 5 dni pomiarów -
% średnio 1,2 - zatem (1,2 + 1)/2 = 1,1
getDailyAverageDataCount_test() ->
  prepareWithoutOneValue(),
  ?assertEqual(1.1, pollution_server:getDailyAverageDataCount()),
  pollution_server:stop().

% jest jedna stacja z pomiarem wyższym od średniej w tym dniu ("Gilowice")
getDailyOverLimit_test() ->
  Date = element(1, calendar:local_time()),
  prepareWithoutOneValue(),
  ?assertEqual(1, pollution_server:getDailyOverLimit(Date, "PM2,5", 90)),
    pollution_server:stop().

% "Gilowice" mają 6 pomiarów i 5 dni pomiarów - srednio 1,2 pomiaru na dzień
getDailyAverageMensurationOfStation_test() ->
  prepareWithoutOneValue(),
  ?assertEqual(1.2, pollution_server:getDailyAverageMensurationOfStation("Gilowice")),
    pollution_server:stop().

% jest jedna stacja z pomiarem niższym od średniej w tym dniu ("Wola")
getDailyUnderLimit_test() ->
  Date = element(1, calendar:local_time()),
  prepareWithoutOneValue(),
  ?assertEqual(1, pollution_server:getDailyUnderLimit(Date, "PM2,5", 90)),
    pollution_server:stop().
