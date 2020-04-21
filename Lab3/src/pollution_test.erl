%%%-------------------------------------------------------------------
%%% @author grzegorz
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. kwi 2020 22:09
%%%-------------------------------------------------------------------
-module(pollution_test).
-author("grzegorz").

%% API
-include_lib("eunit/include/eunit.hrl").

prepare() ->
  pollution:addValue("Wola", calendar:local_time(), "PM2,5", 70,
    pollution:addValue("Gilowice", {{2020, 3, 29}, 18}, "PM2,5", 70,
      pollution:addValue("Gilowice", {{2020, 3, 29}, 18}, "PM10", 98,
        pollution:addValue("Gilowice", {{2020, 3, 30}, 18}, "PM10", 115,
          pollution:addValue("Wola", {{2020, 4, 1}, 18}, "PM10", 110,
            pollution:addValue("Gilowice", {{2020, 4, 1}, 18}, "PM10", 50,
              pollution:addValue("Gilowice", {{2020, 4, 2}, 18}, "PM10", 100,
                pollution:addValue("Gilowice", calendar:local_time(), "PM2,5", 113,
                  pollution:addValue("Gilowice", calendar:local_time(), "PM10", 59,
                    pollution:addStation("Gilowice",{50, 45},
                      pollution:addStation("Wola",{30, 143},
                        pollution:createMonitor()))))))))))).

prepareWithoutOneValue() ->
  pollution:addValue("Wola", calendar:local_time(), "PM2,5", 70,
    pollution:addValue("Gilowice", {{2020, 3, 29}, 18}, "PM2,5", 70,
      pollution:addValue("Gilowice", {{2020, 3, 29}, 18}, "PM10", 98,
        pollution:addValue("Gilowice", {{2020, 3, 30}, 18}, "PM10", 115,
          pollution:addValue("Wola", {{2020, 4, 1}, 18}, "PM10", 110,
            pollution:addValue("Gilowice", {{2020, 4, 1}, 18}, "PM10", 50,
              pollution:addValue("Gilowice", {{2020, 4, 2}, 18}, "PM10", 100,
                pollution:addValue("Gilowice", calendar:local_time(), "PM2,5", 113,
                  pollution:addStation("Gilowice",{50, 45},
                    pollution:addStation("Wola",{30, 143},
                      pollution:createMonitor())))))))))).

% próba dodania wartości w tym samym czasie i tego samego typu - zostanie zwrócona informacja o błędzie
% oraz taki sam monitor
add_the_same_value_test() ->
  ?assertException(throw,"Such mensuration exist", pollution:addValue("Gilowice", calendar:local_time(), "PM2,5", 50, prepare())).

%%% próba dodania stacji o nazwie która już istnieje - zostanie zwrócona informacja o błędzie
%%% oraz niezmieniony monitor
add_station_with_the_same_name_test() ->
  ?assertException(throw,"Station with such name or coordinates exists", pollution:addStation("Wola",{14, 144}, prepare())).

%%% takiego rekordu nie ma, zwrócony zostanie niezmieniony monitor
remove_non_existing_value_test() ->
  ?assertEqual(prepare(), pollution:removeValue("Wola", calendar:local_time(), "PM10", prepare())).

% powinien zostać zwrócony monitor bez jednego rekordu
removeValue_test() ->
  ?assertEqual(prepareWithoutOneValue(), pollution:removeValue("Gilowice", calendar:local_time(), "PM10", prepare())).

%%% powinno zostać zwrócone 113 - wartość z dzisiaj w "Gilowice" o typie "PM2,5"
getOneValue_test() ->
  ?assertEqual(113, pollution:getOneValue("Gilowice", calendar:local_time(), "PM2,5", prepare())).

% powinno zostać zwrócone 90.75, czyli średnia z 4 wartości PM10 w "Gilowice"
getStationMean_test() ->
  ?assertEqual(90.75, pollution:getStationMean("Gilowice", "PM10", prepareWithoutOneValue())).

% powinno zostać zwrócone 80, czyli średnia z pomiarów PM10 1 kwietnia w obu stacjach
getDailyMean_test() ->
  ?assertEqual(80.0, pollution:getDailyMean({{2020, 4, 1}, 18}, "PM10", prepareWithoutOneValue())).

% powinna zostać zwrócona 1,1 ("Wola" ma średnio jeden pomiar dziennie, "Gilowice" mają 6 pomiarów i 5 dni pomiarów -
% średnio 1,2 - zatem (1,2 + 1)/2 = 1,1
getDailyAverageDataCount_test() ->
  ?assertEqual(1.1, pollution:getDailyAverageDataCount(prepareWithoutOneValue())).

% jest jedna stacja z pomiarem wyższym od średniej w tym dniu ("Gilowice")
getDailyOverLimit_test() ->
  Date = element(1, calendar:local_time()),
  ?assertEqual(1, pollution:getDailyOverLimit(Date, "PM2,5", 90, prepareWithoutOneValue())).

% "Gilowice" mają 6 pomiarów i 5 dni pomiarów - srednio 1,2 pomiaru na dzień
getDailyAverageMensurationOfStation_test() ->
  ?assertEqual(1.2, pollution:getDailyAverageMensurationOfStation("Gilowice", prepareWithoutOneValue())).

% jest jedna stacja z pomiarem niższym od średniej w tym dniu ("Wola")
getDailyUnderLimit_test() ->
  Date = element(1, calendar:local_time()),
  ?assertEqual(1, pollution:getDailyUnderLimit(Date, "PM2,5", 90, prepareWithoutOneValue())).
