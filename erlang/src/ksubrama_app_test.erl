-module(ksubrama_app_test).
-include_lib("eunit/include/eunit.hrl").


startup_test() ->
	% TODO: There has to be an automatic way to launch all the dependent applications just like
	% the functionality that the repl provides.  Maybe?
	ok = application:start(cowlib),
	ok = application:start(ranch),
	ok = application:start(cowboy),
	ok = application:start(ksubrama),
	?assertNot(undefined == whereis(ksubrama_sup)).
