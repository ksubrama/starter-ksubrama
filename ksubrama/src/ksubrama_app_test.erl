-module(ksubrama_app_test).
-include_lib("eunit/include/eunit.hrl").


%% Start our application and all its dependencies.
%% TODO: Maybe there is a "spec" based testing framework that can help log this stuff better?
startup_test() ->
	{ok, Started} = application:ensure_all_started(ksubrama),
	?assertNot(undefined == whereis(ksubrama_sup)),
	lists:foreach(fun(App) ->
			ok = application:stop(App)
		end, Started).
