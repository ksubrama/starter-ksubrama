-module(store_test).
-include_lib("eunit/include/eunit.hrl").

-define(NONEXISTENT,<<"nonexistent">>).


test_strs() ->
	[
		<<"Hello">>,
		<<"">>,
		<<"~!@#$%^&*()_">>
		% TODO: Find a unicode string - a weird one in those non-default code-planes
		% that ruins everything for everybody.
	].


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

	?debugMsg("There should be no users by default."),
	lists:foreach(
		fun(User) ->
			notfound = store:get_user(Pid, User),
			false = store:user_exists(Pid, User)
		end, test_strs()),

	?debugMsg("There should be no groups by default."),
	lists:foreach(
		fun(Group) ->
			notfound = store:get_group(Pid, Group),
			false = store:group_exists(Pid, Group)
		end, test_strs()),

	exit(Pid, kill).


store_groups_test() ->
	Pid = start_store(),

	?debugMsg("Can't create invalid groups."),
	lists:foreach(
		fun(Group) ->
			invalid_user = store:create_group(Pid, {Group, [Group]}),
			invalid_user = store:update_group(Pid, {Group, [Group]})
		end, test_strs()),

	?debugMsg("Can't delete non-existent groups."),
	lists:foreach(
		fun(Group) ->
			notfound = store:delete_group(Pid, Group)
		end, test_strs()),

	?debugMsg("Add empty groups."),
	lists:foreach(
		fun(Group) ->
			ok = store:create_group(Pid, {Group, []}),
			ok = store:update_group(Pid, {Group, []})
		end, test_strs()),

	?debugMsg("Can recreate said empty groups - they still don't exist."),
	lists:foreach(
		fun(Group) ->
			notfound = store:get_group(Pid, Group),
			ok = store:create_group(Pid, {Group, []})
		end, test_strs()),

	?debugMsg("Empty groups don't exist and can't be deleted."),
	lists:foreach(
		fun(Group) ->
			false = store:group_exists(Pid, Group),
			notfound = store:delete_group(Pid, Group)
		end, test_strs()),

	?debugMsg("Assuming users work, update groups with users."),
	lists:foreach(
		fun(Group) ->
			ok = store:create_user(Pid, {Group, Group, Group, [Group]})
		end, test_strs()),

	?debugMsg("Groups now exist and have members."),
	lists:foreach(
		fun(Group) ->
			{Group, [Group]} = store:get_group(Pid, Group),
			true = store:group_exists(Pid, Group)
		end, test_strs()),

	?debugMsg("Groups can't be recreated, but they can be updated."),
	lists:foreach(
		fun(Group) ->
			conflict = store:create_group(Pid, {Group, [Group]}),
			true = store:group_exists(Pid, Group)
		end, test_strs()),

	?debugMsg("Groups can't be updated/recreated to invalid values."),
	lists:foreach(
		fun(Group) ->
			invalid_user = store:create_group(Pid, {Group, [?NONEXISTENT]}),
			{Group, [Group]} = store:get_group(Pid, Group),
			true = store:group_exists(Pid, Group),
			invalid_user = store:update_group(Pid, {Group, [?NONEXISTENT]}),
			{Group, [Group]} = store:get_group(Pid, Group),
			true = store:group_exists(Pid, Group)
		end, test_strs()),

	?debugMsg("Groups can now be deleted but only once."),
	lists:foreach(
		fun(Group) ->
			ok = store:delete_group(Pid, Group),
			notfound = store:get_group(Pid, Group),
			false = store:group_exists(Pid, Group),
			notfound = store:delete_group(Pid, Group)
		end, test_strs()),

	?debugMsg("Deleted groups can be recreated."),
	lists:foreach(
		fun(Group) ->
			ok = store:create_group(Pid, {Group, [Group]}),
			true = store:group_exists(Pid, Group)
		end, test_strs()),

	exit(Pid, kill).


store_users_test() ->
	Pid = start_store(),


	exit(Pid, kill).

