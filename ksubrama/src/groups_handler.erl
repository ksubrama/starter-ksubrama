-module(groups_handler).

-export([init/3]).
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

delete_resource(Req, State) ->
	%% TODO: Fill in the logic.
	{false, Req, State}.

resource_exists(Req, State) ->
	%% TODO: Fill with logic.
	{true, Req, State}.

group_from_json(Req, State) ->
	%% TODO: Fill with logic.
	{true, Req, State}.

group_to_json(Req, State) ->
	%% TODO: Fill with logic.
	{<<"[]">>, Req, State}.
