% ETS implementation of wallaroo_storage
% Copyright (c) 2011 Red Hat, Inc., and William C. Benton

-module(wallaroo_store_ets).
-export([init/1,start/1,cleanup/1,delete_object/1,delete_commit/1,delete_tag/1,delete_branch/1,delete_meta/2,find_object/1,find_commit/1,find_tag/1,find_meta/1,find_meta/2,find_branch/1,store_object/2,store_meta/3,store_commit/2,store_tag/2,store_branch/2, objects/0, tags/0, commits/0, branches/0, meta/0, users/0, find_user/1, delete_user/1, store_user/2]).
-behaviour(wallaroo_storage).

-define(OBJECT_TABLE, wallaroo_objects).
-define(COMMIT_TABLE, wallaroo_commits).
-define(TAG_TABLE, wallaroo_tags).
-define(BRANCH_TABLE, wallaroo_branches).
-define(META_TABLE, wallaroo_metadata).
-define(USER_TABLE, wallaroo_users).
-define(TABLES, [?OBJECT_TABLE, ?COMMIT_TABLE, ?TAG_TABLE, ?BRANCH_TABLE, ?META_TABLE, ?USER_TABLE]).

init(_) ->
    lists:map(fun(X)->create_table(X) end, ?TABLES), ok.

create_table(T) ->
    case ets:info(T) of
	undefined ->
	    ets:new(T, [public, named_table]),
	    ok;
	_ ->
	    ok
    end.

cleanup(_) ->
    lists:map(fun(X) -> ets:delete(X) end, ?TABLES), ok.

start(_) ->
    ok.

find_object(Hash) ->
    generic_find(?OBJECT_TABLE, Hash).

find_commit(Hash) ->
    generic_find(?COMMIT_TABLE, Hash).

find_tag(Hash) ->
    generic_find(?TAG_TABLE, Hash).

find_branch(Hash) ->
    generic_find(?BRANCH_TABLE, Hash).

find_user(Name) ->
    generic_find(?USER_TABLE, Name).

find_meta(Domain, Key) ->
    generic_find(?META_TABLE, {Domain, Key}).

find_meta(Domain) ->
    [{Key, Val} || [Key, Val] <- ets:match(?META_TABLE, {{Domain, '$1'}, '$2'})].

generic_find(Table, Hash) ->
    case ets:match(Table, {Hash, '$1'}) of
	[[X]] ->
	    X;
	[[_X]|_Xs] ->
	    too_many;
        _ ->
	    find_failed
    end.

store_object(Key, Value) ->
    generic_store(?OBJECT_TABLE, Key, Value).

store_commit(Key, Value) ->
    generic_store(?COMMIT_TABLE, Key, Value).

store_tag(Key, Value) ->
    generic_store(?TAG_TABLE, Key, Value, true).

store_branch(Key, Value) ->
    generic_store(?BRANCH_TABLE, Key, Value, true).

store_user(Key, Value) ->
    generic_store(?USER_TABLE, Key, Value, true).

store_meta(Domain, Key, Value) ->
    generic_store(?META_TABLE, {Domain, Key}, Value, true).

generic_store(Table, Key, Value) ->
    generic_store(Table, Key, Value, false).

generic_store(Table, Key, Value, Overwrite) ->
    case {ets:lookup(Table, Key), Overwrite} of
	{[], _} ->
	    ets:insert(Table, {Key, Value}),
	    ok;
	{_, true} ->
	    ets:insert(Table, {Key, Value}),
	    ok;
	{_, false} ->
	    ok
    end.

delete_tag(Key) ->
    generic_delete(?TAG_TABLE, Key).

delete_object(Key) ->
    generic_delete(?OBJECT_TABLE, Key).

delete_commit(Key) ->
    generic_delete(?COMMIT_TABLE, Key).

delete_branch(Key) ->
    generic_delete(?BRANCH_TABLE, Key).

delete_user(Key) ->
    generic_delete(?USER_TABLE, Key).

delete_meta(Domain, Key) ->
    generic_delete(?META_TABLE, {Domain, Key}).

generic_delete(Table, Key) ->
    ets:delete(Table, Key).

commits() ->
    ets:tab2list(?COMMIT_TABLE).

objects() ->
    ets:tab2list(?OBJECT_TABLE).

tags() ->
    ets:tab2list(?TAG_TABLE).

branches() ->
    ets:tab2list(?BRANCH_TABLE).

meta() ->
    ets:tab2list(?META_TABLE).

users() ->
    ets:tab2list(?USER_TABLE).
