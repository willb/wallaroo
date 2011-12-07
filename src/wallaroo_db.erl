% High-level DB functionality for Wallaroo
% Copyright (c) 2011 Red Hat, Inc., and William C. Benton

-module(wallaroo_db).
-compile(export_all).

hash_and_store(Object, StoreMod) ->
    hash_and_store(Object, StoreMod, store_object).

hash_and_store(Object, StoreMod, Func) ->
    SHA = wallaroo_hash:as_bitstring(Object),
    StoreMod:Func(SHA, Object),
    {SHA, Object}.

store(Object) ->
    ok.

get(Object) ->
    {ok, nil}.
