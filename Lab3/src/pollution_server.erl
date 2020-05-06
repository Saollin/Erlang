%%%-------------------------------------------------------------------
%%% @author grzegorz
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. kwi 2020 16:55
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("grzegorz").

%% API
-export([start/0, stop/0, init/0]).
-export([addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2]).
-export([getHourlyMean/3, getDailyAverageDataCount/0, getMaximumGradientStations/0, getDailyOverLimit/3]).
-export([getDailyAverageMensurationOfStation/1, getDailyUnderLimit/3, getMonitor/0]).

start() ->
  register(server, spawn (?MODULE, init, [])),
  call(start).

stop() ->
  call(stop).

init() ->
  loop(pollution:createMonitor()).

loop(Monitor) ->
  receive
    {request, Pid, stop} ->
      Pid ! {reply, "Server Stopped"},
      ok;
    {request, Pid, start} ->
      Pid ! {reply, "Server start"},
      loop(Monitor);
    {request, Pid, getMonitor} ->
      Pid ! {reply, Monitor},
      loop(Monitor);
    {request,  Pid, Arguments} ->
      countAndCatch(Monitor, Pid, Arguments)
  after
    10000 ->
      ok
  end.

call(Message) ->
  server ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  end.

countAndCatch(Monitor, Pid, {addStation, Name, Coord}) ->
  try pollution:addStation(Name, Coord, Monitor) of
      Val -> Pid ! {reply, Val},
      loop(Val)
  catch
    throw:X ->
      Pid ! {reply, X},
      loop(Monitor)
  end;
countAndCatch(Monitor, Pid, {addValue, Name, Date, Type, Value}) ->
  try pollution:addValue(Name, Date, Type, Value, Monitor) of
      Val -> Pid ! {reply, Val},
      loop(Val)
  catch
    throw:X ->
      Pid ! {reply, X},
      loop(Monitor)
  end;
countAndCatch(Monitor, Pid, {removeValue, Name, Date, Type})->
  try pollution:removeValue(Name, Date, Type, Monitor) of
      Val -> Pid ! {reply, Val},
      loop(Val)
  catch
    throw:X ->
      Pid ! {reply, X},
      loop(Monitor)
  end;
countAndCatch(Monitor, Pid, {getOneValue, Name, Date, Type})->
  try pollution:getOneValue(Name, Date, Type, Monitor) of
      Val -> Pid ! {reply, Val},
      loop(Monitor)
  catch
    throw:X ->
      Pid ! {reply, X},
      loop(Monitor)
  end;
countAndCatch(Monitor, Pid, {getStationMean, Name, Type})->
  try pollution:getStationMean(Name, Type, Monitor) of
      Val -> Pid ! {reply, Val},
      loop(Monitor)
  catch
    throw:X ->
      Pid ! {reply, X},
      loop(Monitor)
  end;
countAndCatch(Monitor, Pid, {getHourlyMean, Name, Hour, Type})->
  try pollution:getHourlyMean(Name, Hour, Type, Monitor) of
      Val -> Pid ! {reply, Val},
      loop(Monitor)
  catch
    throw:X ->
      Pid ! {reply, X},
      loop(Monitor)
  end;
countAndCatch(Monitor, Pid, {getDailyOverLimit, Date, Type, Limit})->
  try pollution:getDailyOverLimit(Date, Type, Limit, Monitor) of
      Val -> Pid ! {reply, Val},
      loop(Monitor)
  catch
    throw:X ->
      Pid ! {reply, X},
      loop(Monitor)
  end;
countAndCatch(Monitor, Pid, {getDailyAverageMensurationOfStation, Name})->
  try pollution:getDailyAverageMensurationOfStation(Name, Monitor) of
      Val -> Pid ! {reply, Val},
      loop(Monitor)
  catch
    throw:X ->
      Pid ! {reply, X},
      loop(Monitor)
  end;
countAndCatch(Monitor, Pid, {getMaximumGradientStations})->
  try pollution:getMaximumGradientStations(Monitor) of
    Val -> Pid ! {reply, Val},
      loop(Monitor)
  catch
    throw:X ->
      Pid ! {reply, X},
      loop(Monitor)
  end;
countAndCatch(Monitor, Pid, {getDailyMean, Date, Type})->
  Val = pollution:getDailyMean(Date, Type, Monitor),
  Pid ! {reply, Val},
  loop(Monitor);
countAndCatch(Monitor, Pid, {getDailyAverageDataCount})->
  Val = pollution:getDailyAverageDataCount(Monitor),
  Pid ! {reply, Val},
  loop(Monitor);
countAndCatch(Monitor, Pid, {getDailyUnderLimit, Date, Type, Limit})->
  Val = pollution:getDailyUnderLimit(Date, Type, Limit, Monitor),
  Pid ! {reply, Val},
  loop(Monitor).

getMonitor() -> call(getMonitor).

addStation(Name, Coord) -> call({addStation, Name, Coord}).

addValue(Name, Date, Type, Value) -> call({addValue, Name, Date, Type, Value}).

removeValue(Name, Date, Type) -> call({removeValue, Name, Date, Type}).

getOneValue(Name, Date, Type) -> call({getOneValue, Name, Date, Type}).

getStationMean(Name, Type) -> call({getStationMean, Name, Type}).

getHourlyMean(Name, Hour, Type) -> call({getHourlyMean, Name, Hour, Type}).

getDailyOverLimit(Date, Type, Limit) -> call({getDailyOverLimit, Date, Type, Limit}).

getMaximumGradientStations() -> call({getMaximumGradientStations}).

getDailyAverageMensurationOfStation(Name) -> call({getDailyAverageMensurationOfStation, Name}).

getDailyMean(Name, Type) -> call({getDailyMean, Name, Type}).

getDailyAverageDataCount() -> call({getDailyAverageDataCount}).

getDailyUnderLimit(Date, Type, Limit) -> call({getDailyUnderLimit, Date, Type, Limit}).
