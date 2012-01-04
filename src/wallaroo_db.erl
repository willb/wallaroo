% High-level DB functionality for Wallaroo
% Copyright (c) 2011 Red Hat, Inc., and William C. Benton

-module(wallaroo_db).
-export([identity/1, hash_and_store/2, hash_and_store/3]).
-export_type([store_result/0]).

-type store_result() :: {wallaroo_hash:bin(), any()}.

-spec identity(any()) -> wallaroo_hash:bin().			
identity(Term) ->
    wallaroo_hash:as_bitstring(Term).

-spec hash_and_store(any(), atom()) -> store_result().
hash_and_store(Term, StoreMod) ->
    hash_and_store(Term, StoreMod, store_object).

-spec hash_and_store(any(), atom(), atom()) -> store_result().
hash_and_store(Term, StoreMod, Func) ->
    SHA = identity(Term),
    StoreMod:Func(SHA, Term),
    {SHA, Term}.
