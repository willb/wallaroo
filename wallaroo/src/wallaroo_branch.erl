% Branch implementation for Wallaroo
% Copyright (c) 2012 Red Hat, Inc., and William C. Benton

-module(wallaroo_branch).
-compile(export_all).

-define(BRANCH_TUPLE_TAG, wallaroo_branch).

new(Commit, Annotation, Meta) when is_binary(Commit) ->
    OrderedMeta = orddict:from_list(Meta),
    {?BRANCH_TUPLE_TAG, {Commit, Annotation, OrderedMeta}}.

store(Name, {?BRANCH_TUPLE_TAG, {_C, _A, _M}}=Branch, StoreMod) ->
    StoreMod:store_branch(Name, Branch).

get_meta({?BRANCH_TUPLE_TAG, {_, _, Meta}}, Key) ->
    orddict:find(Key, Meta).

get_annotation({?BRANCH_TUPLE_TAG, {_, Annotation, _}}) ->
    Annotation.

get_commit({?BRANCH_TUPLE_TAG, {Commit, _, _}}) ->
    Commit.
