% commit representation for Wallaroo
% Copyright (c) 2011 Red Hat, Inc., and William C. Benton

-module(wallaroo_commit).
-compile(export_all).

empty() ->
    {wallaroo_commit, empty}.

new(Parent, Tree, Changes) when is_list(Changes), is_bitstring(Parent) ->
    {wallaroo_commit, {Parent, Tree, Changes}}.

store({wallaroo_commit, {_P,_T,_C}}=Commit, StoreFunc) ->
    {SHA, _} = wallaroo_tree:hash_and_store(Commit, StoreFunc),
    SHA.
