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
  pollution_gen_server:start(),
  pollution_gen_server:addStation("Gilowice",{50, 45}),
  pollution_gen_server:addStation("Wola",{30, 143}),
  pollution_gen_server:addValue("Wola", calendar:local_time(), "PM2,5", 70),
  pollution_gen_server:addValue("Gilowice", {{2020, 3, 29}, 18}, "PM2,5", 70),
  pollution_gen_server:addValue("Gilowice", {{2020, 3, 29}, 18}, "PM10", 98),
  pollution_gen_server:addValue("Gilowice", {{2020, 3, 30}, 18}, "PM10", 115),
  pollution_gen_server:addValue("Wola", {{2020, 4, 1}, 18}, "PM10", 110),
  pollution_gen_server:addValue("Gilowice", {{2020, 4, 1}, 18}, "PM10", 50),
  pollution_gen_server:addValue("Gilowice", {{2020, 4, 2}, 18}, "PM10", 100),
  pollution_gen_server:addValue("Gilowice", calendar:local_time(), "PM2,5", 113).

% próba dodania wartości w tym samym czasie i tego samego typu - zostanie zwrócona informacja o błędzie
% oraz taki sam monitor
add_the_same_value_test() ->
  prepare(),
  ?assertEqual("Such mensuration exist", pollution_gen_server:addValue("Gilowice", calendar:local_time(), "PM2,5", 50)).

%%% próba dodania stacji o nazwie która już istnieje - zostanie zwrócona informacja o błędzie
%%% oraz niezmieniony monitor
add_station_with_the_same_name_test() ->
  ?assertEqual("Station with such name or coordinates exists", pollution_gen_server:addStation("Wola",{14, 144})).


%%% takiego rekordu nie ma, zwrócony zostanie niezmieniony monitor
remove_non_existing_value_test() ->
  M1 = pollution_gen_server:getMonitor(),
  pollution_gen_server:removeValue("Wola", calendar:local_time(), "PM10"),
  M2 = pollution_gen_server:getMonitor(),
  ?assertEqual(M1, M2).

% powinien zostać zwrócony monitor bez jednego rekordu
removeValue_test() ->
  M1 = pollution_gen_server:getMonitor(),
  pollution_gen_server:addValue("Gilowice", calendar:local_time(), "PM7", 59),
  pollution_gen_server:removeValue("Gilowice", calendar:local_time(), "PM7"),
  M2 = pollution_gen_server:getMonitor(),
  ?assertEqual(M1, M2).

%%% powinno zostać zwrócone 113 - wartość z dzisiaj w "Gilowice" o typie "PM2,5"
getOneValue_test() ->
  ?assertEqual(113, pollution_gen_server:getOneValue("Gilowice", calendar:local_time(), "PM2,5")).

% powinno zostać zwrócone 90.75, czyli średnia z 4 wartości PM10 w "Gilowice"
getStationMean_test() ->
  ?assertEqual(90.75, pollution_gen_server:getStationMean("Gilowice", "PM10")).

% powinno zostać zwrócone 80, czyli średnia z pomiarów PM10 1 kwietnia w obu stacjach
getDailyMean_test() ->
  ?assertEqual(80.0, pollution_gen_server:getDailyMean({{2020, 4, 1}, 18}, "PM10")).

% powinna zostać zwrócona 1,1 ("Wola" ma średnio jeden pomiar dziennie, "Gilowice" mają 6 pomiarów i 5 dni pomiarów -
% średnio 1,2 - zatem (1,2 + 1)/2 = 1,1
getDailyAverageDataCount_test() ->
  ?assertEqual(1.1, pollution_gen_server:getDailyAverageDataCount()).

% jest jedna stacja z pomiarem wyższym od średniej w tym dniu ("Gilowice")
getDailyOverLimit_test() ->
  Date = element(1, calendar:local_time()),
  ?assertEqual(1, pollution_gen_server:getDailyOverLimit(Date, "PM2,5", 90)).

% "Gilowice" mają 6 pomiarów i 5 dni pomiarów - srednio 1,2 pomiaru na dzień
getDailyAverageMensurationOfStation_test() ->
  ?assertEqual(1.2, pollution_gen_server:getDailyAverageMensurationOfStation("Gilowice")).

% jest jedna stacja z pomiarem niższym od średniej w tym dniu ("Wola")
getDailyUnderLimit_test() ->
  Date = element(1, calendar:local_time()),
  ?assertEqual(1, pollution_gen_server:getDailyUnderLimit(Date, "PM2,5", 90)).