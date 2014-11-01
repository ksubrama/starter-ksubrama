-module(groups_handler).

-export([init/3, rest_init/2]).
-export([allowed_methods/2]).
-export([allow_missing_post/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([delete_resource/2]).
-export([resource_exists/2]).

-export([group_from_json/2]).
-export([group_to_json/2]).

%% Handler callbacks for the cowboy_rest state machine.
%% This module handles the /groups/[<groupname>] REST end-point.  Only JSON is
%% accepted and returned.
%%
%% GETs and POSTs all follow the following json template.
%% [ "<userid1>", ... ]
%%
%% Most of the stream of thought that applied to the users handler applies here
%% as well.  For named user groups, I'd probably not permanently reserve a group
%% name but would use an internal, non-reusable group id instead.  Depending on
%% the scope of visibility of the group id, I'd either use a uuid of some kind
%% to ensure non-guessability and obscure the temporal correlation of group
%% creation.  Otherwise, we could make it a monotonically increasing integer
%% scoped to whatever access visibility space that we care to limit it too.
%%
%% Further, I'd provide either a flag to provide the full user details instead
%% of just the member id during a GET or make that the default behavior.  It
%% would seem that the general use case is to either test group membership of
%% a single user (in which case it's easier to query the user) or to enumerate
%% the users in a group, which is most likely going to be followed up with
%% some display of user information or some follow up logic on each user in
%% said group.  Since we don't have any API to "batch-fetch" the details on a
%% bunch of users, it would be prudent to just supply the information here.
%% Of course, this is all raw speculation based on some hypothetical use case.
%% In real life, we should design our APIs based on usage intent and not expose
%% more functionality than we need to expose.


init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
	{Group, Req1} = cowboy_req:binding(group, Req),
	Pid = whereis(store),
	{ok, Req1, {Pid, Group}}.

allowed_methods(Req, State) ->
	{ [<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>],
		Req, State}.

%% We allow posts to a non-existent resource location instead of just the root
%% location.
allow_missing_post(Req, State) ->
	{true, Req, State}.

content_types_accepted(Req, State) ->
	{[
		{<<"application/json">>, group_from_json}
	], Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, group_to_json}
	], Req, State}.

delete_resource(Req, State = {Pid, Group}) ->
	io:format("Deleting ~p~n", [Group]),
	store:delete_group(Pid, Group),
	%% Well..  either the resource was deleted, or got deleted in between the
	%% check earlier and now.  No harm in claiming that the resource got deleted.
	{true, Req, State}.

resource_exists(Req, State = {Pid, Group}) ->
	{store:group_exists(Pid, Group), Req, State}.

group_from_json(Req, State = {Pid, Group}) ->
	{ok, Body, Req1} = cowboy_req:body(Req),
	{Method, Req2} = cowboy_req:method(Req1),
	io:format("[~s] against ~p~n", [Method, Group]),
	try
		jsx:is_json(Body) orelse throw({decode, Body}),
		UserIds = jsx:decode(Body),
		ok = ej:valid(com_handler:spec_for(group), UserIds),
		{Regex, _} = com_handler:regex_for(id),
		{match, _} = re:run(Group, Regex),

		io:format("Inserting/Updating: ~p~n", [Group]),
		ok = case Method of
			<<"POST">> ->
				store:create_group(Pid, {Group, UserIds});
			<<"PUT">> ->
				store:update_group(Pid, {Group, UserIds});
			_ -> throw({unknown_verb, Method})
		end,
		{true, Req2, State}
	catch
		error:Msg ->
			io:format("Error ~p during [~s]~n", [Msg, Method]),
			{false, Req2, State};
		throw:Msg ->
			io:format("Error ~p during [~s]~n", [Msg, Method]),
			{false, Req2, State}
	end.

group_to_json(Req, State = {Pid, Group}) ->
	io:format("Fetching ~p~n", [Group]),
	case store:get_group(Pid, Group) of
		notfound ->
			{ok, Req1} = cowboy_req:reply(404, Req),
			{halt, Req1, State};
		{Group, UserIds} ->
			ok = ej:valid(com_handler:spec_for(group), UserIds),
			Body = jsx:encode(UserIds),
			{Body, Req, State}
	end.
