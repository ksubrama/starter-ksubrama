-module(com_handler).

-export([regex_for/1, spec_for/1]).

%% Common handler related constants, templates and functions.

%% Let's restrict this to a unix style user name/group name.
%% The storage back-end technically doesn't care and if we really cared,
%% we'd restrict it there but I don't feel like fidding with ampersands in
%% my JSON...  so here it is.
regex_for(id) ->
	{ok, Regex} = re:compile("^[a-z0-9]+$"),
	{Regex, <<"Unix style user names with a-z and 0-9 allowed">>};
%% Names...  they're just free form fields.  As long as you put something
%% there, it doesn't matter.
regex_for(name) ->
	{ok, Regex} = re:compile(".+"),
	{Regex, <<"Names cannot be empty">>}.

spec_for(user) ->
	{[
		{<<"userid">>, {string_match, regex_for(id)}},
		{<<"first_name">>, {string_match, regex_for(name)}},
		{<<"last_name">>, {string_match, regex_for(name)}},
		{<<"groups">>, spec_for(id_array)}
	]};
spec_for(group) ->
	spec_for(id_array);
spec_for(id_array) ->
	{array_map, {string_match, regex_for(id)}}.

