-module(ksubrama_app_test).
-include_lib("eunit/include/eunit.hrl").


startup_test() ->
	ok = application:start(ksubrama),
	?assertNot(undefined == whereis(ksubrama_sup)).
