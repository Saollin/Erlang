-module(pollution_gen_server).
-behaviour(gen_server).
-version('1.0').

-export([start/0, init/1, handle_call/3, handle_cast/2, terminate/2]).

-export([stop/0, addStation/2, addValue/4, removeValue/3, getOneValue/3,crash/0]).
-export([getStationMean/2, getHourlyMean/3, getDailyOverLimit/3, getMaximumGradientStations/0]).
-export([getDailyAverageMensurationOfStation/1, getDailyMean/2, getDailyAverageDataCount/0, getDailyUnderLimit/3]).
start() ->
	gen_server:start_link(
			{local, ?MODULE},
			?MODULE, 
			[], []).

init([]) ->
	io:format("~n=================  Server init  ==================~n~n"),
	%% on start of server backup.val has empty monitor
	{ok, ets:lookup_element(backup, val, 2)}.
                              
%% user interface
addStation(Name, Coord) ->
	gen_server:call(pollution_gen_server, {addStation, Name, Coord}).
addValue(Name, Date, Type, TypeValue) ->
	gen_server:call(pollution_gen_server, {addValue, Name, Date, Type, TypeValue}).
removeValue(Name, Date, Type) ->
	gen_server:call(pollution_gen_server, {removeValue, Name, Date, Type}).
getOneValue(Name, Date, Type) ->
	gen_server:call(pollution_gen_server, {getOneValue, Name, Date, Type}).
getStationMean(Name, Type) ->
	gen_server:call(pollution_gen_server, {getStationMean, Name, Type}).
getHourlyMean(Name, Hour, Type) ->
	gen_server:call(pollution_gen_server, {getHourlyMean, Name, Hour, Type}).
getDailyOverLimit(Date, Type, Limit) ->
	gen_server:call(pollution_gen_server, {getDailyOverLimit, Date, Type, Limit}).
getMaximumGradientStations() ->
	gen_server:call(pollution_gen_server, {getMaximumGradientStations}).
getDailyAverageMensurationOfStation(Name) ->
	gen_server:call(pollution_gen_server, {getDailyAverageMensurationOfStation, Name}).
getDailyMean(Name, Type) ->
	gen_server:call(pollution_gen_server, {getDailyMean, Name, Type}).
getDailyAverageDataCount() ->
	gen_server:call(pollution_gen_server, {getDailyAverageDataCount}).
getDailyUnderLimit(Date, Type, Limit) ->
	gen_server:call(pollution_gen_server, {getDailyUnderLimit, Date, Type, Limit}).
crash() ->
	gen_server:cast(pollution_gen_server, crash).
stop() ->
	gen_server:cast(pollution_gen_server, stop).

%% callbacks
handle_call({addStation, Name, Coord}, _From, Value) ->
	try pollution:addStation(Name, Coord, Value) of
		NewVal -> {reply, NewVal, NewVal}
	catch
		throw:X ->
			{reply, X, Value}
	end;
handle_call({addValue, Name, Date, Type, TypeValue}, _From, Value) ->
	try pollution:addValue(Name, Date, Type, TypeValue, Value) of
		NewVal -> {reply, NewVal, NewVal}
	catch
		throw:X ->
			{reply, X, Value}
	end;
handle_call({removeValue, Name, Date, Type}, _From, Value) ->
	try pollution:removeValue(Name, Date, Type, Value) of
		NewVal -> {reply, NewVal, NewVal}
	catch
		throw:X ->
			{reply, X, Value}
	end;
handle_call({getOneValue, Name, Date, Type}, _From, Value) ->
	try pollution:getOneValue(Name, Date, Type, Value) of
		GetVal -> {reply, GetVal, Value}
	catch
		throw:X ->
			{reply, X, Value}
	end;
handle_call({getStationMean, Name, Type}, _From, Value) ->
	try pollution:getStationMean(Name, Type, Value) of
		NewVal -> {reply, NewVal, NewVal}
	catch
		throw:X ->
			{reply, X, Value}
	end;
handle_call({getHourlyMean, Name, Hour, Type}, _From, Value) ->
	try pollution:getHourlyMean(Name, Hour, Type, Value) of
		NewVal -> {reply, NewVal, NewVal}
	catch
		throw:X ->
			{reply, X, Value}
	end;
handle_call({getDailyOverLimit, Date, Type, Limit}, _From, Value) ->
	try pollution:getDailyOverLimit(Date, Type, Limit, Value) of
		NewVal -> {reply, NewVal, NewVal}
	catch
		throw:X ->
			{reply, X, Value}
	end;
handle_call({getMaximumGradientStations}, _From, Value) ->
	try pollution:getMaximumGradientStations(Value) of
		NewVal -> {reply, NewVal, NewVal}
	catch
		throw:X ->
			{reply, X, Value}
	end;
handle_call({getDailyAverageMensurationOfStation, Name}, _From, Value) ->
	try pollution:getDailyAverageMensurationOfStation(Name, Value) of
		NewVal -> {reply, NewVal, NewVal}
	catch
		throw:X ->
			{reply, X, Value}
	end;
handle_call({getDailyMean, Name, Type}, _From, Value) ->
	try pollution:getDailyMean(Name, Type, Value) of
		NewVal -> {reply, NewVal, NewVal}
	catch
		throw:X ->
			{reply, X, Value}
	end;
handle_call({getDailyAverageDataCount}, _From, Value) ->
	try pollution:getDailyAverageDataCount(Value) of
		NewVal -> {reply, NewVal, NewVal}
	catch
		throw:X ->
			{reply, X, Value}
	end;
handle_call({getDailyUnderLimit, Date, Type, Limit}, _From, Value) ->
	try pollution:getDailyUnderLimit(Date, Type, Limit, Value) of
		NewVal -> {reply, NewVal, NewVal}
	catch
		throw:X ->
			{reply, X, Value}
	end.
handle_cast(crash, Value) ->
	1 / 0,
	{noreply, Value};
handle_cast(stop, Value) ->
	{stop, normal, Value}.

%% callbacks
terminate(normal, Value) ->
	ets:insert(backup, {val, pollution:createMonitor()}),
	io:format("~nServer: exit with monitor: ~p~n", [Value]),
	io:format("Server exit normal ~n~n");
terminate(Reason, Value) ->
	ets:insert(backup, {val, Value}),
	io:format("~nServer: exit with monitor: ~p~n", [Value]),
	io:format("Server: exit with reason: ~p~n~n", [Reason]),
	Reason.
