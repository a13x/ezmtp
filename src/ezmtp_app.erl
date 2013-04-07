%% @private
-module(ezmtp_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	{ok, _} = ranch:start_listener(ezmtp, 50,
		ranch_tcp, [{port, 9000}], ezmtp_protocol, []),
	ezmtp_sup:start_link().

stop(_State) ->
	ok.
