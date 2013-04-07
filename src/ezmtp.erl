-module(ezmtp).

%% API.
-export([start/0]).

%% API.

start() ->
	ok = application:start(ranch),
	ok = application:start(sasl),
	ok = application:start(ezmtp).
