-module(users_handler).

-export([init/3, rest_init/2]).
-export([allowed_methods/2]).
-export([allow_missing_post/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([delete_resource/2]).
-export([resource_exists/2]).

-export([user_from_json/2]).
-export([user_to_json/2]).

%% Handler callbacks for the cowboy_rest state machine.
%% This module handles the /users/[<userid>] REST end-point.  Only JSON is
%% accepted and returned.
%%
%% GETs and POSTs all follow the following json template.
%% {"first_name": "<First Name>",
%%  "last_name": "<Last Name>",
%%  "userid": "<userid>",
%%  "groups": ["<group1>", ...]}
%%
%% XXX: Here is a stream of thought on how I might change this simple CRUD pattern.
%%
%% If I were to design it, I would change the json template to hold a generation
%% counter/UUID (or use etag headers).
%% GETs should return the stamp of last modification.
%% POST would not exist (or would behave the same as PUT) since we always know
%% our resource locations.
%% PUT should accept a stamp in the content body and only modify the existing
%% record if the stamp matches the current version's (use a 0 stamp to ask for
%% a fresh creation).
%% PATCH can also work to update details.
%% DELETE would be implemented as I have right now - it would merely mark the
%% account as deactived/soft deleted.  Even if all actual resources related to
%% such an account were to be reclaimed, user ids would never be
%% returned to the general pool to avoid hazards involving id re-use.
%%
%% Also, we should also implement the is_authorized callback to check the cookies
%% provided to ensure that the request is both authentic by, for example, checking
%% if the auth tokens corresponding to a recent challenge are right and then
%% confirming if the authentic request is from a principal authorized to perform
%% the action - either by checking against an internal policy agent or by
%% validating an included proof of capability (usually by checking if the
%% request has been appropriately signed by/vouched for by a trusted provider of
%% some kind).
%%
%% Actually, we should do no such thing because that's crypto and we'd have to
%% be high and insane to even contemplate implementing that on our own - so we
%% should instead hook up the oauth libraries, hire some pen-testers, sacrifice a
%% chicken to the spirit of Bruce Schneier and develop and drinking problem.
%%
%% There are a couple other minor design concerns.  Both the user and group
%% objects would benefit from a "link" field with urls to the corresponding
%% resources by default although we could optionally elide this to optimize
%% bandwidth through an optional argument at each resource end-point.  I'd also
%% prefix the urls with /1 or /v1 or something similar to indicate the REST API
%% versioning prefix because it's cheap and the alternative (figuring out you
%% need it later) is gigantic pain in the ass.

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
	% Once I figure out logging, add a log at every rest_init to correctly write
	% down what the request is, the http verb is, strip out any PII and store it
	% for telemetry and auditing.
	% Also, formalize the state here.  I'm just passing a tuple around.  There needs
	% to be a proper "request context" that can trace the performance and activity
	% within the context of this request.
	{UserId, Req1} = cowboy_req:binding(userid, Req),
	% TODO: There is probably a better way of "knowing" the module you're supposed
	% to talk to but I don't care to go read a bunch of cowboy documentation to
	% figure out the right way to get this here.
	% Also...  if the memory subsystem fails, I'm a-ok with just "letting it crash"
	% but that means figuring out exactly how cowboy is going to handle that.  I hope
	% it just returns a 500 or, at worst, closes the connection and starts again.
	% Maybe??
	Pid = whereis(store),
	{ok, Req1, {Pid, UserId}}.

allowed_methods(Req, State) ->
	{ [<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>],
		Req, State}.

%% We allow posts to a non-existent resource location instead of just the root
%% location.
allow_missing_post(Req, State) ->
	{true, Req, State}.

content_types_accepted(Req, State) ->
	{[
		{<<"application/json">>, user_from_json}
	], Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, user_to_json}
	], Req, State}.

delete_resource(Req, State = {Pid, UserId}) ->
	io:format("Deleting ~p~n", [UserId]),
	store:delete_user(Pid, UserId),
	%% Well..  either the resource was deleted, or got deleted in between the
	%% check earlier and now.  No harm in claiming that the resource got deleted.
	{true, Req, State}.

resource_exists(Req, State = {Pid, UserId}) ->
	{store:user_exists(Pid, UserId), Req, State}.

user_from_json(Req, State = {Pid, UserId}) ->
	{ok, Body, Req1} = cowboy_req:body(Req),
	{Method, Req2} = cowboy_req:method(Req1),
	io:format("[~s] against ~p~n", [Method, UserId]),
	try
		jsx:is_json(Body) orelse throw({error, decode}),
		DecodedJson = jsx:decode(Body),
		ej:valid(com_handler:spec_for(user), DecodedJson) =:= ok orelse throw({error, json_spec}),
		FirstName = ej:get({"first_name"}, DecodedJson),
		LastName = ej:get({"last_name"}, DecodedJson),
		UserId_ = ej:get({"userid"}, DecodedJson),
		Groups = ej:get({"groups"}, DecodedJson),
		UserId_ =:= UserId orelse throw({error, userid_mismatch}),

		User = {UserId, FirstName, LastName, Groups},
		io:format("Inserting/Updating: ~p~n", [User]),
		Outcome = case Method of
			<<"POST">> ->
				store:create_user(Pid, User);
			<<"PUT">> ->
				store:update_user(Pid, User);
			_ -> throw({error, unknown_verb})
		end,
		Outcome =:= ok orelse throw({error, Outcome}),
		{true, Req2, State}
	catch
		error:Msg ->
			io:format("Error ~p during [~s]~n", [Msg, Method]),
			{false, Req2, State}
	end.

user_to_json(Req, State = {Pid, UserId}) ->
	io:format("Fetching ~p~n", [UserId]),
	case store:get_user(Pid, UserId) of
		notfound ->
			{ok, Req1} = cowboy_req:reply(404, Req),
			{halt, Req1, State};
		{UserId, FirstName, LastName, Groups} ->
			DecodedJson = [
				{<<"first_name">>, FirstName},
				{<<"last_name">>, LastName},
				{<<"userid">>, UserId},
				{<<"groups">>, Groups}
			],
			Body = jsx:encode(DecodedJson),
			{Body, Req, State}
	end.
