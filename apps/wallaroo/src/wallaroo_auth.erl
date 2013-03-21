% Authentication and authorization helpers for Wallaroo
% Copyright (c) 2013 Red Hat, Inc., and William C. Benton

-module(wallaroo_auth).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-record(authstate, {hashmod, hashopts, storage}).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/0]).

% API methods
-export([create_user/3, % name, pass, role 
	 delete_user/1, % name
	 authorized/3,  % name, pass, action
	 authorized/4,  % name, pass, secret, action
	 list_users/0
	]).

create_user(Name, Pass, Role) ->
    gen_server:call(?SERVER, {create_user, Name, Pass, Role}).

delete_user(Name) ->
    gen_server:call(?SERVER, {delete_user, Name}).

authorized(Name, Pass, Action) ->
    authorized(Name, Pass, <<>>, Action).

authorized(Name, Pass, Secret, Action) ->
    gen_server:call(?SERVER, {authorized, Name, Pass, Secret, Action}).

list_users() ->
    gen_server:call(?SERVER, {list_users}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

config_val(K, Default) ->
    case application:get_env(wallaroo, K) of
        {ok, Value} ->
            Value;
        _ ->
            Default
    end.

init(_) ->
    StoreMod = config_val(storage_module, wallaroo_store_ets),
    HashMod = config_val(password_hash_module, wallaroo_password),
    HashOpts = config_val(password_hash_options, []),
    {ok, #authstate{hashmod=HashMod, hashopts=HashOpts, storage=StoreMod}}.

%%% API functions

%%% gen_server callbacks
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_call({Call}, _From, #authstate{}=State) ->
    {reply, ok, State}.

handle_info(_X, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.
