-module(store_test).
-include_lib("eunit/include/eunit.hrl").

% Test method to start the storage system in a stand-alone manner.  We want to
% be able to kill it brutally without blowing ourselves up.
start_store() ->
	{ok, Pid} = gen_server:start(store, [], []),
	Pid.

store_start_stop_test() ->
	?debugMsg("Starting and killing the storage process."),
	Pid = start_store(),
	exit(Pid, kill).

store_empty_after_init_test() ->
	Pid = start_store(),
	TestStrs = [
		<<"Hello">>,
		<<"">>,
		<<"~!@#$%^&*()_">>
		% TODO: Find a unicode string - a weird one in those non-default code-planes
		% that ruins everything for everybody.
	],

	?debugMsg("There should be no users by default."),
	lists:foreach(
		fun(User) ->
			notfound = store:get_user(Pid, User),
			false = store:user_exists(Pid, User)
		end, TestStrs),

	?debugMsg("There should be no groups by default."),
	lists:foreach(
		fun(User) ->
			notfound = store:get_group(Pid, User),
			false = store:group_exists(Pid, User)
		end, TestStrs),

	exit(Pid, kill).

