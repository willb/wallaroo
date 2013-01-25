% Tag implementation for Wallaroo
% Copyright (c) 2011 Red Hat, Inc., and William C. Benton

-module(wallaroo_tag).
-export([new/3, store/3, get_meta/2, get_annotation/1, get_commit/1, store_without_validating/3, store/4]).

-define(TAG_TUPLE_TAG, wallaroo_tag).

new(Commit, Annotation, Meta) when is_binary(Commit) ->
    OrderedMeta = orddict:from_list(Meta),
    {?TAG_TUPLE_TAG, {Commit, Annotation, OrderedMeta}}.

store(Name, {?TAG_TUPLE_TAG, {_C, _A, _M}}=Tag, StoreMod) ->
    store(Name, Tag, StoreMod, none).

store(Name, {?TAG_TUPLE_TAG, {CommitHash, _A, _M}}=Tag, StoreMod, Validator) when is_function(Validator, 2) ->
    Commit = StoreMod:find_commit(CommitHash),
    error_logger:warning_msg("wallaroo_tag:store/4:  Name=~p, Tag=~p, Commit=~p~n", [Name, Tag, Commit]),
    Tree = wallaroo_commit:get_tree(Commit, StoreMod),
    case Validator(Tree, StoreMod) of
	ok -> 
	    StoreMod:store_tag(Name, Tag), ok;
        {fail, _Why}=F -> F
    end;
store(Name, {?TAG_TUPLE_TAG, {_C, _A, _M}}=Tag, StoreMod, none) ->
    StoreMod:store_tag(Name, Tag).

store_without_validating(Name, {?TAG_TUPLE_TAG, {_C, _A, _M}}=Tag, StoreMod) ->
    store(Name, Tag, StoreMod, none).

get_meta({?TAG_TUPLE_TAG, {_, _, Meta}}, Key) ->
    orddict:find(Key, Meta).

get_annotation({?TAG_TUPLE_TAG, {_, Annotation, _}}) ->
    Annotation.

get_commit({?TAG_TUPLE_TAG, {Commit, _, _}}) ->
    Commit.
