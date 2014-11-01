-module(ksubrama_app_test).
-include_lib("eunit/include/eunit.hrl").


%% Start our application and all its dependencies.
%% TODO: Maybe there is a "spec" based testing framework that can help log this stuff better?
startup_test() ->
	{ok, Started} = application:ensure_all_started(ksubrama),
	?assertNot(undefined == whereis(ksubrama_sup)),
	% Can we reach the storage system.
	Pid = whereis(store),
	?assertNot(undefined == Pid),
	ok = store:create_user(Pid, {<<>>, <<>>, <<>>, []}),
	lists:foreach(fun(App) ->
			ok = application:stop(App)
		end, Started).

%% Has our nonsense hackery to get a json library suceeded?
jsx_test() ->
	Inputs = [
		[<<"This">>, <<"is">>, <<"a">>, <<"list">>],
		[{atom1, <<"Hello">>}, {atom2, [1,2,3]}, {<<"???">>, atom3}]
	],

	?debugMsg("Verifying if we have access to the jsx json libraries."),
	lists:foreach(
		fun(Input) ->
			?debugFmt("Original: ~p", [Input]),
			Encoded = jsx:encode(Input),
			?debugFmt("Encoded: ~p", [Encoded]),
			Decoded = jsx:decode(Encoded),
			?debugFmt("Decoded: ~p", [Decoded])
		end, Inputs).

