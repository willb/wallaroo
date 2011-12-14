%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the wallaroo_web application.

-module(wallaroo_web_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for wallaroo_web.
start(_Type, _StartArgs) ->
    wallaroo_web_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for wallaroo_web.
stop(_State) ->
    ok.
