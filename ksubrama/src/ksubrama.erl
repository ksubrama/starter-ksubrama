%% Interface module to simplify launching the application from the command line
%% during development.

-module(ksubrama).
-export([start/0]).

start() ->
	{ok, _Started} = application:ensure_all_started(ksubrama).
