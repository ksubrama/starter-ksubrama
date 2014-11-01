-module(store).

-behavior(gen_server).

-export([start_link/0]).
-export([create_user/2, update_user/2, get_user/2, user_exists/2, delete_user/2]).
-export([create_group/2, update_group/2, get_group/2, group_exists/2, delete_group/2]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% Implements the data storage interface.
%% If I knew more erlang, I'd hook up bindings to...  Dynamo? Cassandra?
%% some other cool key-value store?  Whatever.  Or I might just be sensible and
%% hook it up to just a flat file or to some SQL server like PostgreSQL or SQLLite
%% or something even more low-tech.  But since I don't actually know
%% Erlang that well, I'm just going to store stuff in ETS tables in one process
%% because why not?  It doesn't let me show of any of my knowledge is races/
%% thread safety, but it is the easiest solution (besides passing around gb_trees)
%% that I can see.
%%
%% Basic design:
%%
%% User table:
%% userid -> first name, last name, live
%% Holding only the normalized data.
%%
%% Group membership bags:
%% userid -> group
%% group -> userid
%%
%% These act akin to two covered indexes on a 2 columns userid,group id table.
%% Yes...  Yuck!  But I prefer relational models first before jumping in and
%% denormalizing stuff.  Even the extra "reverse" index is an optimization
%% because O(whatever) for 10 items doesn't matter at all.
%%
%% Now I could just put the userid -> group info along with the first dictionary
%% because that's one less thing to deal with and we're a process and we're
%% serializing things anyway.
%% But the first step of scaling this up would probably be a relational store or
%% some table on disk and I'd prefer having sensible, normalized data along with
%% places where they are explicitly denormalized instead of implicitly putting that
%% stuff wherever and worrying about data consistency later.
%%
%% TODO: Why is dialyzer not catching any issues with return types?  Why is it not
%% following through gen_server:call?

-type ks_user() :: {binary(), binary(), binary(), [binary()]}.
-type ks_group() :: {binary(), [binary()]}.
-export_type([ks_user/0, ks_group/0]).
-type ks_state() :: {ets:tid(), ets:tid(), ets:tid()}.
-type ks_iuser() :: {binary(), binary(), binary()}.


%% Public interface.

start_link() ->
	io:format("Starting storage subsystem.~n"),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% User CRUD.
-spec create_user (pid(), ks_user()) -> ok | conflict.
create_user(Pid, {UserId, FirstName, LastName, Groups}) ->
	gen_server:call(Pid, {update_user, create, {UserId, FirstName, LastName}, Groups}).

-spec update_user (pid(), ks_user()) -> ok | conflict.
update_user(Pid, {UserId, FirstName, LastName, Groups}) ->
	gen_server:call(Pid, {update_user, update, {UserId, FirstName, LastName}, Groups}).

-spec delete_user (pid(), binary()) -> ok | notfound.
delete_user(Pid, UserId) ->
	gen_server:call(Pid, {update_user, delete, {UserId, <<>>, <<>>}, []}).

-spec get_user (pid(), binary()) -> ks_user() | notfound.
get_user(Pid, UserId) ->
	case gen_server:call(Pid, {get_user, full, UserId}) of
		{{UserId, FirstName, LastName}, Groups} -> {UserId, FirstName, LastName, Groups};
		Else -> Else
	end.

-spec user_exists (pid(), binary()) -> boolean().
user_exists(Pid, UserId) ->
	gen_server:call(Pid, {get_user, check, UserId}).


%% Group CRUD.
-spec create_group (pid(), ks_group()) -> ok | conflict | invalid_user.
create_group(Pid, {Group, UserIds}) ->
	gen_server:call(Pid, {update_group, create, Group, UserIds}).

-spec update_group (pid(), ks_group()) -> ok | invalid_user.
update_group(Pid, {Group, UserIds}) ->
	gen_server:call(Pid, {update_group, update, Group, UserIds}).

-spec delete_group (pid(), binary()) -> ok | invalid_user | notfound.
delete_group(Pid, Group) ->
	gen_server:call(Pid, {update_group, delete, Group, []}).

-spec get_group (pid(), binary()) -> ks_group() | notfound.
get_group(Pid, Group) ->
	case gen_server:call(Pid, {get_group, full, Group}) of
		{UserIds} -> {Group, UserIds};
		Else -> Else
	end.

-spec group_exists (pid(), binary()) -> boolean().
group_exists(Pid, Group) ->
	gen_server:call(Pid, {get_group, check, Group}).


%% Internal methods and gen_server callbacks.

-define(MAP_GROUP_COL, 1).
-define(MAP_USER_COL, 2).
-define(USER_ALIVE_COL, 4).

%% Create linked ETS tables.  If we crash, they go away.
%% TODO:  Maybe have the supervisor control the tables?
-spec init_tables () -> ks_state().
init_tables() ->
	%% {userid, first name, last name, live}
	UserTable = ets:new(users, [set]),
	%% {group, userid}
	MapGroupToUser = ets:new(map_group_to_user, [bag]),
	%% {group, userid}
	MapUserToGroup = ets:new(map_user_to_group, [bag, {keypos, ?MAP_USER_COL}]),
	{UserTable, MapGroupToUser, MapUserToGroup}.

-spec init ([]) -> {ok, ks_state()}.
init([]) ->
	State = init_tables(),
	{ok, State}.


-spec maybe_lookup(ets:tid(), term(), pos_integer()) -> [term()].
maybe_lookup(Tab, Key, Pos) ->
	try
		ets:lookup_element(Tab, Key, Pos)
	catch
		error:badarg ->
			[]
	end.


-spec add_membership (ets:tid(), ets:tid(), [binary()], [binary()]) -> true.
add_membership(GtU, UtG, Groups, UserIds) ->
	Memberships = [{Group, UserId} || Group <- Groups, UserId <- UserIds],
	true = ets:insert(UtG, Memberships),
	true = ets:insert(GtU, Memberships).


-spec remove_membership (ets:tid(), ets:tid(), [binary()], [binary()]) -> true.
remove_membership(GtU, UtG, Groups, UserIds) ->
	Memberships = [{Group, UserId} || Group <- Groups, UserId <- UserIds],
	lists:foreach(
		fun({Group, UserId}) ->
			true = ets:delete_object(UtG, {Group, UserId}),
			true = ets:delete_object(GtU, {Group, UserId})
		end, Memberships),
	true.


%% Reset the membership attributes of a particular group to the specified user.
-spec set_group_membership (ets:tid(), ets:tid(), binary(), [binary()]) -> true.
set_group_membership(GtU, UtG, Group, UserIds) ->
	New = sets:from_list(UserIds),
	Old = sets:from_list(maybe_lookup(GtU, Group, ?MAP_USER_COL)),
	ToAdd = sets:to_list(sets:subtract(New, Old)),
	ToRemove = sets:to_list(sets:subtract(Old, New)),
	true = add_membership(GtU, UtG, [Group], ToAdd),
	true = remove_membership(GtU, UtG, [Group], ToRemove).


%% Reset the membership attributes of a particular user to the specified groups.
-spec set_user_membership (ets:tid(), ets:tid(), [binary()], binary()) -> true.
set_user_membership(GtU, UtG, Groups, UserId) ->
	New = sets:from_list(Groups),
	Old = sets:from_list(maybe_lookup(UtG, UserId, ?MAP_GROUP_COL)),
	ToAdd = sets:to_list(sets:subtract(New, Old)),
	ToRemove = sets:to_list(sets:subtract(Old, New)),
	true = add_membership(GtU, UtG, ToAdd, [UserId]),
	true = remove_membership(GtU, UtG, ToRemove, [UserId]).


-spec ks_update_user
	(create | update, ks_iuser(), [binary()], ks_state()) -> ok | conflict;
	(delete, ks_iuser(), [binary()], ks_state()) -> ok | notfound.
ks_update_user(Flavor, {UserId, FirstName, LastName}, Groups, {UserTable, GtU, UtG}) ->
	case {Flavor, maybe_lookup(UserTable, UserId, ?USER_ALIVE_COL)} of
		{create, []} ->
			% Insertions *only* accept pristine state.
			ets:insert_new(UserTable, {UserId, FirstName, LastName, alive}),
			add_membership(GtU, UtG, Groups, [UserId]), ok;
		{create, _} ->
			conflict;
		{update, dead}  ->
			conflict;
		{update, _} ->
			% Updating an existing but live user succeeds.
			ets:insert(UserTable, {UserId, FirstName, LastName, alive}),
			set_user_membership(GtU, UtG, Groups, UserId), ok;
		{delete, alive} ->
			ets:insert(UserTable, {UserId, FirstName, LastName, dead}),
			set_user_membership(GtU, UtG, Groups, UserId), ok;
		{delete, _} ->
			notfound
	end.


-spec ks_get_user
	(full, binary(), ks_state()) -> ks_user() | notfound;
	(check, binary(), ks_state()) -> boolean().
ks_get_user(full, UserId, {UserTable, _, UtG}) ->
	case ets:lookup(UserTable, UserId) of
		[{UserId, FirstName, LastName, alive}] ->
			Groups = maybe_lookup(UtG, UserId, ?MAP_GROUP_COL),
			{{UserId, FirstName, LastName}, Groups};
		_ ->
			notfound
	end;
ks_get_user(check, UserId, {UserTable, _, _}) ->
	% There has to be an easier way to convert a pattern match to a boolean.  The only
	% things I can come up with feel like hacks.
	case ets:match(UserTable, {UserId, '_', '_', alive}) of
		[_] -> true;
		_ -> false
	end.


-spec ks_update_group
	(create | update, binary(), [binary()], ks_state()) -> ok | conflict | invalid_user;
	(delete, binary(), [binary()], ks_state()) -> ok | notfound | invalid_user.
ks_update_group(Flavor, Group, UserIds, State = {_, GtU, UtG}) ->
	% A real storage system would do this "all" in parallel and decided whether to seek or
	% scan based on programmer guidance/statistics.
	case {
			lists:all(fun(UserId) -> ks_get_user(check, UserId, State) end, UserIds),
			ets:member(GtU, Group),
			Flavor} of
		{true, false, create} ->
			add_membership(GtU, UtG, [Group], UserIds), ok;
		{true, true, create} ->
			conflict;
		{true, _, update} ->
			set_group_membership(GtU, UtG, Group, UserIds), ok;
		{true, false, delete} ->
			notfound;
		{true, true, delete} ->
			set_group_membership(GtU, UtG, Group, UserIds), ok;
		{false, _, _} ->
			invalid_user
	end.


-spec ks_get_group
	(full, binary(), ks_state()) -> ks_group() | notfound;
	(check, binary(), ks_state()) -> boolean().
ks_get_group(full, Group, {_, GtU, _}) ->
	case maybe_lookup(GtU, Group, ?MAP_USER_COL) of
		[] ->
			notfound;
		UserIds ->
			{Group, UserIds}
	end;
ks_get_group(check, Group, {_, GtU, _}) ->
	ets:member(GtU, Group).


-spec handle_call
	({update_user, create | update | delete, ks_iuser(), [binary()]}, _, ks_state()) -> {reply, ok | conflict | notfound, ks_state()};
	({get_user, check | full, binary()}, _, ks_state()) -> {reply, ks_user() | notfound | boolean(), ks_state()};
	({update_group, create | update | delete, binary(), [binary()]}, _, ks_state()) -> {reply, ok | conflict | notfound | invalid_user, ks_state()};
	({get_group, check | full, binary()}, _, ks_state()) -> {reply, ks_group() | notfound | boolean(), ks_state()}.
	% TODO: Fix this when we figure out how to handle overlapping domains or how to represent differences.
	%(_, _, ks_state()) -> {stop, unknown_call, ks_state()}.
handle_call(Req, _From, State) ->
	% We're one process...  so it's all serialized anyway.  Just do the work,
	case Req of
		{update_user, Flavor, User, Groups} ->
			{reply, ks_update_user(Flavor, User, Groups, State), State};
		{get_user, Flavor, UserId} ->
			{reply, ks_get_user(Flavor, UserId, State), State};
		{update_group, Flavor, Group, UserIds} ->
			{reply, ks_update_group(Flavor, Group, UserIds, State), State};
		{get_group, Flavor, Group} ->
			{reply, ks_get_group(Flavor, Group, State), State};
		_ -> {stop, unknown_call, State}
	end.


-spec handle_cast(_, ks_state()) -> {stop, unknown_call, ks_state()}.
handle_cast(_Req, State) -> {stop, unknown_call, State}.

%% Trace and the ingore bad calls.
%% TODO: How does tracing work?
-spec handle_info(_, ks_state()) -> {noreply, ks_state()}.
handle_info(Info, State) ->
	io:format("Unknown call to storage process: ~p~n", [Info]),
	{noreply, State}.

%% No stop/terminate request accepted.  We have a "crash to exit" policy.
%% When designing storage systems, you want a "crash-to-exit" design because you
%% want to actually run the codepath that you're most scared about as often as
%% possible so that you validate that scenario and design it solidly.  If you
%% are crash resistant...  then why do you need clean shutdown?  Just call
%% kill to exit.
-spec terminate(_, ks_state()) -> _.
terminate(Reason, _State) ->
	io:format("Storage process terminating: ~p~n", [Reason]).


%% Not really worried about upgrades now.
%% TODO: Figure out what I need to do.
-spec code_change(_, ks_state(), _) -> {ok, ks_state()}.
code_change(_, State, _) ->
	{ok, State}.
