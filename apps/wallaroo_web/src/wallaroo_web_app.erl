%% @author William C. Benton <willb@redhat.com>
%% @copyright 2012 Red Hat, Inc. and William C. Benton.

%% @doc Callbacks for the wallaroo_web application.

-module(wallaroo_web_app).
-author('William C. Benton <willb@redhat.com>').

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
