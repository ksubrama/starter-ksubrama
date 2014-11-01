-module(ksubrama_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	% TODO: Come back and add constraint handlers here if we need to
	% pre-validate our user/group names in some way.
	RouteList = [
		{"/users/:userid", users_handler, []},
		{"/groups/:group", groups_handler, []}
	],
	Dispatch = cowboy_router:compile([{'_', RouteList}]),
	% TODO: Learn what the default "max" values are and how cowboy offers
	% DOS protection.
	% TODO: Why do the examples want me to throw away the supervisor pid
	% returned here?  That seems strange...
	% TODO: Figure out how argument passing works here - maybe there are
	% deployment configuration options or something?  I haven't figured out how
	% command line arguments work here.
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]}
	]),
	ksubrama_sup:start_link().

stop(_State) ->
	ok.
