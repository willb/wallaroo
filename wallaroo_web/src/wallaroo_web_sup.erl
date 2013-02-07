%% @author William Benton <willb@redhat.com>
%% @copyright 2011 Red Hat, Inc., and William C. Benton

%% @doc Supervisor for the wallaroo_web application.

-module(wallaroo_web_sup).
-author('William Benton <willb@redhat.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,
    {ok, Dispatch} = file:consult(filename:join(
                         [filename:dirname(code:which(?MODULE)),
                          "..", "priv", "dispatch.conf"])),
    WebConfig = [
                 {ip, Ip},
                 {port, 8000},
                 {log_dir, "priv/log"},
                 {dispatch, Dispatch},
		 {error_handler, wallaroo_web_errors}],
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, dynamic},
    Wallaroo = {wallaroo_sup,
           {wallaroo_sup, start_link, []},
           permanent, 5000, supervisor, [wallaroo]},
    Processes = [Wallaroo, Web],
    application:set_env(webmachine, server_name, "Wallaroo " ++ wallaroo:version_string() ++ " (http://getwallaby.com/)"),
    {ok, { {one_for_one, 10, 10}, Processes} }.
