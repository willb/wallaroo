% tree interface for Wallaroo
% Copyright (c) 2011 Red Hat, Inc., and William C. Benton

-module(wallaroo_tree).
-compile(export_all).

empty() ->
    gb_trees:empty().

put(Key, Val, Tree) ->
    case gb_trees:lookup(Key, Tree) of
	none ->
	    gb_trees:insert(Key, Val, Tree);
	{value, _} ->
	    gb_trees:update(Key, Val, Tree)
	end.

hash_and_store(Object, StoreFunc) ->
    SHA = wallaroo_hash:as_bitstring(Object),
    StoreFunc(SHA, Object),
    {SHA, Object}.


put_path([P], BS, Tree, StoreFunc, _FindFunc) when is_binary(BS) ->
    NewTree = put(P, BS, Tree),
    hash_and_store(NewTree, StoreFunc);	    
put_path([P], Object, Tree, StoreFunc, _FindFunc) ->
    {ObjectHash, Object} = hash_and_store(Object, StoreFunc),
    put_path([P], ObjectHash, Tree, StoreFunc, _FindFunc);
put_path([P|Rest], Object, Tree, StoreFunc, FindFunc) ->
    case get(P, Tree) of
	none ->
	    [Last|Tser] = lists:reverse(Rest),
	    {LeafHash, _Leaf} = put_path([Last], Object, gb_trees:empty(), StoreFunc, FindFunc),
	    FoldFun = fun(Element, AccHash) -> {Hash, _NST} = put_path([Element], AccHash, gb_trees:empty(), StoreFunc, FindFunc), Hash end,
	    NewBranch = lists:foldl(FoldFun, LeafHash, Tser),
	    put_path([P], NewBranch, Tree, StoreFunc, FindFunc);
	{value, ElementHash} ->
	    Subtree = FindFunc(ElementHash),
	    {SubtreeHash, _NewSubtree} = put_path(Rest, Object, Subtree, StoreFunc, FindFunc),
	    NewTree = put(P, SubtreeHash, Tree),
	    hash_and_store(NewTree, StoreFunc)
    end.

whatis(X) ->
    test_store(bogus, X).

get(Key, Tree) ->
    gb_trees:lookup(Key, Tree).

has(Key, Tree) ->
    gb_trees:is_defined(Key, Tree).

test_init() ->
    case ets:info(test_wallaroo_tree) of
	undefined ->
	    ets:new(test_wallaroo_tree, [public, named_table]),
	    ok;
	_ ->
	    ok
    end.

test_store(Hash, Object) ->
    case ets:lookup(test_wallaroo_tree, Hash) of
	[] ->
	    ets:insert(test_wallaroo_tree, {Hash, Object}),
	    ok;
	_ ->
	    ok
    end.

test_find(Hash) ->
    case ets:match(test_wallaroo_tree, {Hash, '$1'}) of
	[[X]] ->
	    X;
	[[_X]|_Xs] ->
	    too_many;
        X ->
	    whatis(X),
	    find_failed
    end.

do_dbg() ->
    dbg:start(),
    dbg:tracer(),
    dbg:tpl(wallaroo_tree, '_', []),
    dbg:p(all, c).

do_test() ->
    test_init(),
    Store = fun wallaroo_tree:test_store/2,
    Find = fun wallaroo_tree:test_find/1,
    {_H1, Res} = wallaroo_tree:put_path([a,b,c,d,e], 37, wallaroo_tree:empty(), Store, Find),
    {_H2, Res2} = wallaroo_tree:put_path([a,b,d,e], 42, Res, Store, Find),
    {_H3, _Res3} = wallaroo_tree:put_path([a,c,d,e], 18, Res2, Store, Find).
