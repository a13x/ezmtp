%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(ezmtp_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
	{ok, {{one_for_one, 10, 10}, []}}.
