% tree interface for Wallaroo
% Copyright (c) 2011 Red Hat, Inc., and William C. Benton

-module(wallaroo_tree).

empty() ->
    gb_trees:empty().

put(Key, Val, Tree) ->
    case gb_trees:lookup(Key, Tree) of
	none ->
	    gb_trees:insert(Key, Val, Tree);
	{value, _} ->
	    gb_trees:replace(Key, Val, Tree)
	end.

get(Key, Tree) ->
    gb_trees:lookup(Key, Tree).

has(Key, Tree) ->
    gb_trees:is_defined(Key, Tree).
