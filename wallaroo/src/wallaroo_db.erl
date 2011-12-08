% High-level DB functionality for Wallaroo
% Copyright (c) 2011 Red Hat, Inc., and William C. Benton

-module(wallaroo_db).
-compile(export_all).

identity(Term) ->
    wallaroo_hash:as_bitstring(Term).

hash_and_store(Term, StoreMod) ->
    hash_and_store(Term, StoreMod, store_object).

hash_and_store(Term, StoreMod, Func) ->
    SHA = identity(Term),
    StoreMod:Func(SHA, Term),
    {SHA, Term}.
