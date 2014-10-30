-module(store).

-behavior(gen_server)

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
%% userid -> first name, last name
%% Holding only the normalized data.
%%
%% Group table:
%% group
%% It's just a set to verify existence of the group.
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

start_link([]) ->
	{ok, _Pid} = gen_server:start_link({local, ?MODULE}, [], []).


create_user(Pid, UserId, FirstName, LastName, Groups) ->
	gen_server:call(Pid, {update_user, create, {UserId, FirstName, LastName}, Groups}).

update_user(Pid, UserId, FirstName, LastName, Groups) ->
	gen_server:call(Pid, {update_user, update, {UserId, FirstName, LastName}, Groups}).

get_user(Pid, UserId) ->
	get_server:call(Pid, {get_user, full, UserId}).

user_exists(Pid, UserId) ->
	get_server:call(Pid, {get_user, check, UserId}).

create_group(Pid, Group, UserIds) ->
	gen_server:call(Pid, {update_group, create, Group, UserIds}).

update_group(Pid, Group, UserIds) ->
	gen_server:call(Pid, {update_group, update, Group, UserIds}).

get_group(Pid, Group) ->
	get_server:call(Pid, {get_group, full, Group}).

group_exists(Pid, Group) ->
	get_server:call(Pid, {get_group, check, Group}).


%% Internal methods and gen_server callbacks.

%% Create linked ETS tables.  If we crash, they go away.
%% TODO:  Maybe have the supervisor control the tables?
-spec init_tables -> {ets:tid(), ets:tid(), ets:tid(), ets:tid()}
init_tables() ->
	%% {userid, first name, last name}
	UserTable = ets:new(users, [set]),
	%% {group}
	GroupTable = ets:new(groups, [set]),
	%% {group, userid}
	MapGroupToUser = ets:new(map_group_to_user, [bag]),
	%% {group, userid}
	MapUserToGroup = ets:new(map_user_to_group, [bag, {keypos, 2}]),
	{UserTable, GroupTable, MapGroupToUser, MapUserToGroup}.


init([]) ->
	State = init_tables(),
	{ok, State}.


% TODO string?  Binary string?  Check this.
-spec add_membership {ets:tid(), ets:tid(), [string()], [string()]}
add_membership(GtU, UtG, Groups, UserIds) ->
	Memberships = [{Group, UserId} || Group <- Groups, UserId <- UserIds],
	true = ets:insert(UtG, Memberships),
	true = ets:insert(GtU, Memberships).

internal_update_user(Flavor, User = {UserId, _}, Groups, {UserTable, GroupTable, GtU, UtG}) ->
	% A real storage system would do this "all" in parallel and decided whether to seek or
	% scan based on programmer guidance/statistics.
	if list:all(fun(Group) -> ets:member(GroupTable, Group) end, Groups)	->
			Good = case Flavor of
				create ->
					ets:insert_new(UserTable, User);
				update ->
					ets:insert(UserTable, User)
			end,
			if Good ->
					add_membership(GtU, UtG, [UserId], Groups),
					ok;
				true ->
					conflict
			end;
		true ->
			invalid_groups
	end.


internal_get_user(full, UserId, {UserTable, _, _, UtG}) ->
	case ets:lookup(UserTable, UserId) of
		[] ->
			notfound;
		[User] ->
			Groups = ets:Match(UtG, {"$1", UserId}),
			{User, Groups}
		% Crash if we get more than one user - that's not possible because it's a set
		% and we key off of userid.
	end;
internal_get_user(check, UserId, {UserTable, _, _, _}) ->
	if ets:member(UserTable, UserId) ->
				found;
		true ->
			notfound
	end.


internal_update_group(Flavor, Group, UserIds, {UserTable, GroupTable, GtU, UtG}) ->
	% A real storage system would do this "all" in parallel and decided whether to seek or
	% scan based on programmer guidance/statistics.
	if list:all(fun(UserId) -> ets:member(UserTable, UserId) end, UserIds)	->
			Good = case Flavor of
				create ->
					ets:insert_new(GroupTable, {Group});
				update ->
					ets:insert(GroupTable, {Group})
			end,
			if Good ->
					add_membership(GtU, UtG, UserIds, [Group]),
					ok;
				true ->
					conflict
			end;
		true ->
			invalid_users
	end.


internal_get_group(Flavor, Group, {_, GroupTable, GtU, _}) ->
	case {Flavor, ets:member(GroupTable, Group)} of
		{_, false} ->
			notfound;
		{check, true} ->
			found;
		{full, true} ->
			{ets:Match(GtU, {Group, "$1"})}
	end.


handle_call(Req, _From, State = {UserTable, GroupTable, GtU, UtG}) ->
	% We're one process...  so it's all serialized anyway.  Just do the work,
	Reply = case Req of
		{update_user, Flavor, User, Groups} ->
			internal_update_user(Flavor, User, Groups, State);
		{get_user, Flavor, UserId} ->
			internal_get_user(Flavor, UserId, State);
		{update_group, Flavor, Group, UserIds} ->
			internal_update_group(Flavor, Group, UserIds, State);
		{get_group, Group} ->
			internal_get_group(Flavor, Group, State)
	end,
	{reply, Reply, State}.


%% We don't handly any async calls into the memory subsystem right now.
%% handle_cast(Request, From, State)

%% Trace and the ingore bad calls.
%% TODO: How does tracing work?
handle_info(Info, State) ->
	io:format("Unknown call to storage process: ~p~n", [Info]),
	{noreply, State}.

%% No stop/terminate request accepted.  We have a "crash to exit" policy.
%% When designing storage systems, you want a "crash-to-exit" design because you
%% want to actually run the codepath that you're most scared about as often as
%% possible so that you validate that scenario and design it solidly.  If you
%% are crash resistant...  then why do you need clean shutdown?  Just call
%% kill to exit.
terminate(Reason, State) ->
	io:format("Storage process terminating: ~p~n", [Reason]).
