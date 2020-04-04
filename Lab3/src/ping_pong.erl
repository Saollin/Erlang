%%%-------------------------------------------------------------------
%%% @author grzegorz
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. kwi 2020 22:18
%%%-------------------------------------------------------------------
-module(ping_pong).
-author("grzegorz").

%% API
-export([start/0, stop/0, play/1]).
-export([pingFunction/1, pongFunction/1]).


start() ->
  register(ping, spawn (?MODULE, pingFunction, [0])),
  register(pong, spawn (?MODULE, pongFunction, [0])).

pingFunction(Value) ->
  receive
    Data when  Data > 0 ->
      NewValue = Value + Data,
      io:format("I sent ~w to Pong, I have received in total ~w ~n", [Data, NewValue]),
      timer:sleep(500),
      pong ! Data,
      ping_pong:pingFunction(NewValue);
    _ -> ok
  after
    20000 -> ok
  end.

pongFunction(Value) ->
  receive
    Data when  Data > 0 ->
      NewValue = Value + Data,
      io:format("I have received ~w from Ping, I have received in total ~w ~n", [Data, NewValue]),
      timer:sleep(500),
      Answer = Data - 1,
      ping ! Answer,
      ping_pong:pongFunction(NewValue);
    _ -> ok
  after
    20000 -> ok
  end.

stop()->
  ping ! 0,
  pong ! 0.

play(N) ->
  ping ! N.