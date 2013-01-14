
-module(wallaroo_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    Result = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    error_logger:info_msg("WALLAROO SUP start_link result ~p~n", [Result]),
    Result.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_) ->
    error_logger:info_msg("in WALLAROO SUP init callback ~n", []),
    APIServer = ?CHILD(wallaroo, worker),
    ConfigServer = ?CHILD(wallaroo_config, worker),
    Children = [APIServer, ConfigServer],
    RestartStrategy = {one_for_one, 1, 60},
    Result = {ok, {RestartStrategy, Children}},
    error_logger:info_msg("returning ~p from WALLAROO SUP init callback ~n", [Result]),
    Result.

