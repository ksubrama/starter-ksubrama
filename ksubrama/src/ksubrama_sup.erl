-module(ksubrama_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, brutal_kill, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link () -> {ok, pid()}.
start_link() ->
	{ok, _Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec init ([]) -> {ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()}, [supervisor:child_spec()]}}.
init([]) ->
	{
		ok,
		{
			{one_for_one, 5, 10},
		 	[?CHILD(storage, worker)]
		}
	}.

