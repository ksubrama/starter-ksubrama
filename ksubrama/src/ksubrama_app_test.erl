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

%% Do these two work together?
ej_test() ->
	ValidJson = <<"{
    \"userid\": \"hello\",
    \"first_name\": \"Herb\",
    \"last_name\": \"Ello\",
    \"groups\": [
        \"plant\",
        \"springville\"
    ]
  }">>,
	?debugFmt("Original json: ~s", [ValidJson]),
	Decoded = {jsx:decode(ValidJson)},
	?debugFmt("Decoded json: ~p", [Decoded]),
	ok = ej:valid(com_handler:spec_for(user), Decoded),
	lists:foreach(
		fun(Field) ->
			Extracted = ej:get({Field}, Decoded),
			?debugFmt("~s = ~p~n", [Field, Extracted])
		end,
		[<<"userid">>, <<"first_name">>, <<"last_name">>, <<"groups">>]).

