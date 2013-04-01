% User implementation for Wallaroo
% Copyright (c) 2013 Red Hat, Inc., and William C. Benton

-module(wallaroo_user).
-export([new/4, new/5, get_name/1, get_hash/1, get_role/1, get_meta/2, set_name/2, set_pass/2, set_pass/3, set_role/2, set_meta/3, role/1, allowed_to/2, store/2]).

-define(USER_TUPLE_TAG, wallaroo_user).
-define(READ_ROLE, wallaroo_role_read).
-define(WRITE_ROLE, wallaroo_role_write).
-define(ADMIN_ROLE, wallaroo_role_admin).
-define(NO_ROLE, wallaroo_role_none).
-define(IS_ROLE(X), (X=:=read orelse X=:=write orelse X=:=admin orelse X=:=none)).

role(read) ->
    ?READ_ROLE;
role(write) ->
    ?WRITE_ROLE;
role(admin) ->
    ?ADMIN_ROLE;
role(none) ->
    ?NO_ROLE.

new(Name, Pass, Role, Meta) when is_binary(Name), is_binary(Pass), ?IS_ROLE(Role) ->
    new(Name, Pass, Role, Meta, {wallaroo_password, []}).

new(Name, Pass, Role, Meta, {PassMod, Options}) when is_binary(Name), is_binary(Pass), ?IS_ROLE(Role) ->
    OrderedMeta = orddict:from_list(Meta),
    PassHash = wallaroo_password:hash(PassMod, Pass, Options),
    {?USER_TUPLE_TAG, {Name, PassHash, Role, OrderedMeta}}.

store({?USER_TUPLE_TAG, {Name, _PassHash, _Role, _OrderedMeta}}=UserStruct, StoreMod) ->
    StoreMod:store_user(Name, UserStruct).

get_name({?USER_TUPLE_TAG, {Name, _PassHash, _Role, _OrderedMeta}}) ->
    Name.

get_hash({?USER_TUPLE_TAG, {_Name, PassHash, _Role, _OrderedMeta}}) ->
    PassHash.

get_role({?USER_TUPLE_TAG, {_Name, _PassHash, Role, _OrderedMeta}}) ->
    Role.

get_meta({?USER_TUPLE_TAG, {_Name, _PassHash, _Role, OrderedMeta}}, Key) ->
    orddict:find(Key, OrderedMeta).

set_name({?USER_TUPLE_TAG, {Name, PassHash, Role, OrderedMeta}}, NewName) ->
    {?USER_TUPLE_TAG, {NewName, PassHash, Role, OrderedMeta}}.

set_pass({?USER_TUPLE_TAG, {_Name, _PassHash, _Role, _OrderedMeta}}=User, NewPass) ->
    set_pass(User, NewPass, {wallaroo_password, []}).

set_pass({?USER_TUPLE_TAG, {Name, _, Role, OrderedMeta}}, NewPass, {PassMod, Options}) ->
    PassHash = wallaroo_password:hash(PassMod, NewPass, Options),
    {?USER_TUPLE_TAG, {Name, PassHash, Role, OrderedMeta}}.

set_role({?USER_TUPLE_TAG, {Name, PassHash, Role, OrderedMeta}}, NewRole) ->
    {?USER_TUPLE_TAG, {Name, PassHash, NewRole, OrderedMeta}}.

set_meta({?USER_TUPLE_TAG, {Name, PassHash, Role, OrderedMeta}}, Key, Val) ->
    {?USER_TUPLE_TAG, {Name, PassHash, Role, orddict:store(Key, Val, OrderedMeta)}}.

allowed_to({?USER_TUPLE_TAG, {_, _, Role, _}}, Action) when ?IS_ROLE(Action) ->
    role_includes(Role, role(Action)).

role_includes(?NO_ROLE, _) ->
    false;
role_includes(?ADMIN_ROLE, _) ->
    true;
role_includes(?WRITE_ROLE, ?ADMIN_ROLE) ->
    false;
role_includes(?WRITE_ROLE, _) ->
    true;
role_includes(?READ_ROLE, ?ADMIN_ROLE) ->
    false;
role_includes(?READ_ROLE, ?WRITE_ROLE) ->
    false;
role_includes(?READ_ROLE, _) ->
    true;
role_includes(X, Y) when ?IS_ROLE(X) ->
    role_includes(role(X), Y);
role_includes(X, Y) when ?IS_ROLE(Y) ->
    role_includes(X, role(Y)).







