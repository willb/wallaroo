% ETS implementation of wallaroo_storage
% Copyright (c) 2011 Red Hat, Inc., and William C. Benton

-module(wallaroo_store_ets).
-export([init/1,start/1,cleanup/1,find_object/1,find_commit/1,find_tag/1,store_object/2,store_commit/2,store_tag/2, objects/0, tags/0, commits/0]).
-behaviour(wallaroo_storage).

-define(OBJECT_TABLE, wallaroo_objects).
-define(COMMIT_TABLE, wallaroo_commits).
-define(TAG_TABLE, wallaroo_tags).

init(_) ->
    lists:map(fun(X)->create_table(X) end, [?OBJECT_TABLE, ?COMMIT_TABLE, ?TAG_TABLE]), ok.

create_table(T) ->
    case ets:info(T) of
	undefined ->
	    ets:new(T, [public, named_table]),
	    ok;
	_ ->
	    ok
    end.

cleanup(_) ->
    lists:map(fun(X) -> ets:delete(X) end, [?OBJECT_TABLE, ?COMMIT_TABLE, ?TAG_TABLE]), ok.

start(_) ->
    ok.

find_object(Hash) ->
    generic_find(?OBJECT_TABLE, Hash).

find_commit(Hash) ->
    generic_find(?COMMIT_TABLE, Hash).

find_tag(Hash) ->
    generic_find(?TAG_TABLE, Hash).

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

commits() ->
    ok.

objects() ->
    ok.

tags() ->
    ok.
