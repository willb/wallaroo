% Authentication and authorization helpers for Wallaroo
% Copyright (c) 2013 Red Hat, Inc., and William C. Benton

-module(wallaroo_auth).

-behaviour(gen_server).


% -define(debug, true).
-include("dlog.hrl").

-define(SERVER, ?MODULE).
-record(authstate, {hashmod, hashopts, storage}).

% XXX put this in a header
-define(IS_ROLE(X), (X=:=read orelse X=:=write orelse X=:=admin orelse X=:=none)).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/0]).

% API
-export([create_user/5,       % admin_name, admin_pass, name, pass, role 
	 create_user/4,       % secret, name, pass, role 
	 delete_user/2,       % secret, name
	 delete_user/3,       % admin_name, admin_pass, name
	 update_pass/3,       % name, oldpass, newpass
	 modify_user/3,       % secret, username, options
	 modify_user/4,       % adminname, adminpass, username, options
	 authorized/3,        % name, pass, action
	 authorized/2,        % secret, action
	 user_exists/1,
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


create_user(AdminName, AdminPass, Name, Pass, Role) when is_binary(Name), is_binary(Pass), ?IS_ROLE(Role) ->
    gen_server:call(?SERVER, {create_user, basic, {AdminName, AdminPass}, Name, Pass, Role}).

create_user(Secret, Name, Pass, Role) when is_binary(Name), is_binary(Pass), ?IS_ROLE(Role) ->
    gen_server:call(?SERVER, {create_user, secret, Secret, Name, Pass, Role}).

delete_user(AdminName, AdminPass, Name) ->
    gen_server:call(?SERVER, {delete_user, basic, {AdminName, AdminPass}, Name}).

delete_user(Secret, Name) ->
    gen_server:call(?SERVER, {delete_user, secret, Secret, Name}).

modify_user(Secret, User, Options) ->
    true = valid_modify_options(Options),
    gen_server:call(?SERVER, {modify_user, secret, Secret, User, Options}).

modify_user(Name, Pass, User, Options) ->
    true = valid_modify_options(Options),
    gen_server:call(?SERVER, {modify_user, basic, {Name, Pass}, User, Options}).

valid_modify_options(Ls) ->
    valid_modify_options(Ls, []).

valid_modify_options([], _) -> 
    true;
valid_modify_options([{What,Val}|Rest], Ls) 
  when 
      (What =:= pass andalso is_binary(Val)) orelse 
      (What =:= role andalso ?IS_ROLE(Val)) ->
    not lists:member(What, Ls) andalso valid_modify_options(Rest, [What|Ls]);
valid_modify_options(_,_) ->
    false.

update_pass(Name, Pass, NewPass) ->
    gen_server:call(?SERVER, {update_pass, basic, {Name, Pass}, NewPass}).

user_exists(Name) ->
    gen_server:call(?SERVER, {user_exists, Name}).

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

handle_call({user_exists, Name}, _From, State) ->
    {reply, user_exists(Name, State), State};
handle_call({create_user, Mechanism, Creds, Name, Pass, Role}, _From, #authstate{}=State) ->
    {Result, StatePrime} = internal_create_user(Mechanism, Creds, Name, Pass, Role, State),
    {reply, Result, StatePrime};
handle_call({delete_user, Mechanism, Creds, Name}, _From, #authstate{}=State) ->
    {Result, StatePrime} = internal_delete_user(Mechanism, Creds, Name, State),
    {reply, Result, StatePrime};
handle_call({modify_user, Mechanism, Creds, User, Options}, _From, #authstate{}=State) ->
    {Result, StatePrime} = internal_modify_user(Mechanism, Creds, User, Options, State),
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

internal_create_user(Mech, Creds, Name, Pass, Role, State) ->
    case user_exists(Name, State) of
	true -> authorize_and_do(Mech, Creds, admin, fun(_,_) -> {error, user_exists} end, ignored, State);
	_ -> authorize_and_do(Mech, Creds, admin, fun do_create_user/2, {Name, Pass, Role, []}, State)
    end.

user_exists(Name, #authstate{storage=StoreMod}) ->
    Res = StoreMod:find_user(Name),
    not is_atom(Res).

do_create_user({Name, Pass, Role, Meta}, #authstate{hashmod=Mod, hashopts=Opts, storage=StoreMod}) ->
    UObj = wallaroo_user:new(Name, Pass, Role, Meta, {Mod, Opts}),
    StoreMod:store_user(Name, UObj).
    
internal_delete_user(Mech,Creds,Name,State) ->
    case user_exists(Name, State) of
	false -> authorize_and_do(Mech, Creds, admin, fun(_,_) -> {error, no_such_user} end, ignored, State);
	_ -> authorize_and_do(Mech, Creds, admin, fun do_delete_user/2, Name, State)
    end.

do_delete_user(Name, #authstate{storage=StoreMod}) ->
    StoreMod:delete_user(Name).

internal_modify_user(Mech,Creds,User,Options,State) ->
    case user_exists(User, State) of
	false -> authorize_and_do(Mech, Creds, admin, fun(_,_) -> {error, no_such_user} end, ignored, State);
	_ -> authorize_and_do(Mech, Creds, admin, fun do_modify_user/2, {User, Options}, State)
    end.

do_modify_user({User, Options}, #authstate{hashmod=Mod, hashopts=Opts, storage=StoreMod}) ->
    UObj = lists:foldl(modification_factory(Mod, Opts), User, Options),
    StoreMod:store_user(wallaroo_user:get_name(UObj), UObj).

modification_factory(HashMod, HashOpts) ->
    fun({pass, Password}, UO) ->
	    wallaroo_user:set_pass(UO, Password, {HashMod, HashOpts});
       ({role, Role}, UO) ->
	    wallaroo_user:set_role(UO, Role)
    end.

internal_update_pass(User,Pass,NewPass,State) ->
    authorize_and_do(basic, {User, Pass}, none, fun do_update_pass/2, {User, NewPass}, State).
    
do_update_pass({User, Pass}, #authstate{hashmod=Mod, hashopts=Opts, storage=StoreMod}) ->
    StoreMod:store_user(User, wallaroo_user:set_pass(StoreMod:find_user(User), Pass, {Mod, Opts})).

internal_authorized_by(Mechanism,Creds,Action,State) ->
    authorize_and_do(Mechanism, Creds, Action, fun(ok,_) -> true end, ok, State, false).

internal_list_users(#authstate{storage=StoreMod}=State) ->
    {[U || {U, _} <- StoreMod:users()], State}.

authorize_and_do(Mechanism, Creds, Action, Callback, Arg, State) ->
    authorize_and_do(Mechanism, Creds, Action, Callback, Arg, State, {error, not_authorized}).

authorize_and_do(Mechanism, Creds, Action, Callback, Arg, State, FailureVal) ->
    Authorized = ?D_VAL(authorized_by_empty_userlist(State)) orelse
	?D_VAL(authorized_by_creds(Mechanism, Creds, Action, State)),
    invoke_callback(Authorized, Callback, Arg, State, FailureVal).

authorized_by_empty_userlist(#authstate{storage=StoreMod}) ->
    StoreMod:users() =:= [].

authorized_by_creds(basic, {User, Pass}, Action, #authstate{storage=StoreMod}) ->
    UObj = StoreMod:find_user(User),
    UObj =/= find_failed andalso wallaroo_password:compare(Pass, wallaroo_user:get_hash(UObj)) andalso wallaroo_user:allowed_to(UObj, Action);
authorized_by_creds(secret, Secret, _, _) ->
    Secret =/= [] andalso Secret =:= config_val(auth_secret, []).

invoke_callback(true, Callback, Arg, State, _) ->
    {Callback(Arg, State), State};
invoke_callback(false, _, _, State, FailureVal) ->
    {FailureVal, State}.

handle_info(_X, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.
