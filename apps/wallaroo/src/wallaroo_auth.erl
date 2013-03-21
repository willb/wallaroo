% Authentication and authorization helpers for Wallaroo
% Copyright (c) 2013 Red Hat, Inc., and William C. Benton

-module(wallaroo_auth).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-record(authstate, {hashmod, hashopts, storage}).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/0]).

% API
-export([create_user/3,       % name, pass, role 
	 delete_user/2,       % secret, name
	 delete_user/3,       % admin_name, admin_pass, name
	 update_pass/3,       % name, oldpass, newpass
	 admin_update_pass/3, % secret, username, userpass
	 admin_update_pass/4, % adminname, adminpass, username, userpass
	 authorized/3,        % name, pass, action
	 authorized/2,        % secret, action
	 list_users/0
	]).

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


create_user(Name, Pass, Role) ->
    gen_server:call(?SERVER, {create_user, Name, Pass, Role}).

delete_user(AdminName, AdminPass, Name) ->
    gen_server:call(?SERVER, {delete_user, basic, {AdminName, AdminPass}, Name}).

delete_user(Secret, Name) ->
    gen_server:call(?SERVER, {delete_user, secret, Secret, Name}).

admin_update_pass(Secret, User, NewPass) ->
    gen_server:call(?SERVER, {admin_update_pass, secret, Secret, User, NewPass}).

update_pass(Name, Pass, NewPass) ->
    gen_server:call(?SERVER, {update_pass, basic, {Name, Pass}, NewPass}).

admin_update_pass(Name, Pass, User, NewPass) ->
    gen_server:call(?SERVER, {admin_update_pass, basic, {Name, Pass}, User, NewPass}).

authorized(Name, Pass, Action) ->
    authorized_by(basic, {Name, Pass}, Action).

authorized(Secret, Action) ->
    authorized_by(secret, Secret, Action).

authorized_by(Mechanism, Credential, Action) ->
    gen_server:call(?SERVER, {authorized_by, Mechanism, Credential, Action}).

list_users() ->
    gen_server:call(?SERVER, {list_users}).

%%% gen_server callbacks
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_call({create_user, Name, Pass, Role}, _From, #authstate{}=State) ->
    {Result, StatePrime} = internal_create_user(Name, Pass, Role, State),
    {reply, Result, StatePrime};
handle_call({delete_user, Mechanism, Creds, Name}, _From, #authstate{}=State) ->
    {Result, StatePrime} = internal_delete_user(Mechanism, Creds, Name, State),
    {reply, Result, StatePrime};
handle_call({admin_update_pass, Mechanism, Creds, User, NewPass}, _From, #authstate{}=State) ->
    {Result, StatePrime} = internal_admin_update_pass(Mechanism, Creds, User, NewPass, State),
    {reply, Result, StatePrime};
handle_call({update_pass, User, OldPass, NewPass}, _From, #authstate{}=State) ->
    {Result, StatePrime} = internal_update_pass(User, OldPass, NewPass, State),
    {reply, Result, StatePrime};
handle_call({authorized_by, Mechanism, Creds, Action}, _From, #authstate{}=State) ->
    {Result, StatePrime} = internal_authorized_by(Mechanism, Creds, Action, State),
    {reply, Result, StatePrime};
handle_call({list_users}, _From, #authstate{}=State) ->
    {Result, StatePrime} = internal_list_users(State),
    {reply, Result, StatePrime}.

internal_create_user(_,_,_,State) ->
    {false, State}.

internal_delete_user(_,_,_,State) ->
    {false, State}.

internal_admin_update_pass(_,_,_,_,State) ->
    {false, State}.

internal_update_pass(_,_,_,State) ->
    {false, State}.

internal_authorized_by(_,_,_,State) ->
    {false, State}.

internal_list_users(State) ->
    {[], State}.

handle_info(_X, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.
