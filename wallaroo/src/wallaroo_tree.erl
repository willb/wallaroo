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

get(Key, Tree) ->
    gb_trees:lookup(Key, Tree).

has(Key, Tree) ->
    gb_trees:is_defined(Key, Tree).


hash_and_store(Object, StoreFunc) ->
    SHA = wallaroo_hash:as_bitstring(Object),
    StoreFunc(SHA, Object),
    {SHA, Object}.


get_path([], BS, FindFunc) when is_binary(BS) ->
    FindFunc(BS);
get_path([], Obj, _) ->
    Obj;
get_path([P|Rest], Tree, FindFunc) ->
    case get(P, Tree) of
	none ->
	    none;
	{value, ElementHash} ->
	    Branch = FindFunc(ElementHash),
	    get_path(Rest, Branch, FindFunc)
    end.
    

put_tree(Tree, StoreFunc) ->
    hash_and_store(Tree, StoreFunc).

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


-ifdef(TEST).
-define(ETS_STORE_BACKEND, true).
-include_lib("eunit/include/eunit.hrl").

test_setup() ->
    ets_db_init().

first_fixture() ->
    SF = fun wallaroo_tree:ets_db_store/2,
    FF = fun wallaroo_tree:ets_db_find/1,
    {_T0H, T0} = wallaroo_tree:put_path([a,b,c,d,0], "a/b/c/d/0 for all", wallaroo_tree:empty(), SF, FF),
    {_T1H, T1} = wallaroo_tree:put_path([a,b,c,d,e], "a/b/c/d/e for T1", T0, SF, FF),
    {_T2H, T2} = wallaroo_tree:put_path([a,b,c,d,e], "a/b/c/d/e for T2", T1, SF, FF),
    {_T3H, T3} = wallaroo_tree:put_path([a,b,x], "a/b/x for T3", T2, SF, FF),
    {_T4H, T4} = wallaroo_tree:put_path([a,b,y], "a/b/y for T4", T3, SF, FF),
    SF(t0, T0),
    SF(t1, T1),
    SF(t2, T2),
    SF(t3, T3),
    SF(t4, T4).
    

test_teardown() ->
    ets:delete(test_wallaroo_tree).

find_fixture_tree(Id) ->
    wallaroo_tree:ets_db_find(Id).

simple_test_() ->
    {inorder,
     {setup,
      fun() -> test_setup(), first_fixture() end,
      fun(_) -> test_teardown() end,
      [?_assertEqual("a/b/c/d/0 for all", 
		    wallaroo_tree:get_path([a,b,c,d,0], find_fixture_tree(t0), fun wallaroo_tree:ets_db_find/1)),
      ?_assertEqual("a/b/c/d/0 for all", 
		    wallaroo_tree:get_path([a,b,c,d,0], find_fixture_tree(t1), fun wallaroo_tree:ets_db_find/1)),
      ?_assertEqual("a/b/c/d/0 for all", 
		    wallaroo_tree:get_path([a,b,c,d,0], find_fixture_tree(t2), fun wallaroo_tree:ets_db_find/1)),
      ?_assertEqual("a/b/c/d/0 for all", 
		    wallaroo_tree:get_path([a,b,c,d,0], find_fixture_tree(t3), fun wallaroo_tree:ets_db_find/1)),
      ?_assertEqual("a/b/c/d/0 for all", 
		    wallaroo_tree:get_path([a,b,c,d,0], find_fixture_tree(t4), fun wallaroo_tree:ets_db_find/1)),
      ?_assertEqual("a/b/c/d/e for T1", 
		    wallaroo_tree:get_path([a,b,c,d,e], find_fixture_tree(t1), fun wallaroo_tree:ets_db_find/1)),
      ?_assertEqual("a/b/c/d/e for T2", 
		    wallaroo_tree:get_path([a,b,c,d,e], find_fixture_tree(t2), fun wallaroo_tree:ets_db_find/1)),
      ?_assertEqual("a/b/c/d/e for T2", 
		    wallaroo_tree:get_path([a,b,c,d,e], find_fixture_tree(t3), fun wallaroo_tree:ets_db_find/1)),
      ?_assertEqual("a/b/c/d/e for T2", 
		    wallaroo_tree:get_path([a,b,c,d,e], find_fixture_tree(t4), fun wallaroo_tree:ets_db_find/1)),
      ?_assertEqual("a/b/x for T3", 
		    wallaroo_tree:get_path([a,b,x], find_fixture_tree(t3), fun wallaroo_tree:ets_db_find/1)),
      ?_assertEqual("a/b/x for T3", 
		    wallaroo_tree:get_path([a,b,x], find_fixture_tree(t4), fun wallaroo_tree:ets_db_find/1)),
      ?_assertEqual("a/b/y for T4", 
		    wallaroo_tree:get_path([a,b,y], find_fixture_tree(t4), fun wallaroo_tree:ets_db_find/1)),
      ?_assertNot("a/b/y for T4" =:=
		       wallaroo_tree:get_path([a,b,y], find_fixture_tree(t1), fun wallaroo_tree:ets_db_find/1))]}}.
		    

	      

-endif.

-ifdef(DEBUG).
-define(ETS_STORE_BACKEND, true).

do_dbg_from_shell() ->
    dbg:start(),
    dbg:tracer(),
    dbg:tpl(wallaroo_tree, '_', []),
    dbg:p(all, c).

do_test_from_shell() ->
    ets_db_init(),
    Store = fun wallaroo_tree:ets_db_store/2,
    Find = fun wallaroo_tree:ets_db_find/1,
    {_H1, Res} = wallaroo_tree:put_path([a,b,c,d,e], 37, wallaroo_tree:empty(), Store, Find),
    {_H2, Res2} = wallaroo_tree:put_path([a,b,d,e], 42, Res, Store, Find),
    {_H3, _Res3} = wallaroo_tree:put_path([a,c,d,e], 18, Res2, Store, Find).

-endif.


-ifdef(ETS_STORE_BACKEND).
ets_db_init() ->
    case ets:info(test_wallaroo_tree) of
	undefined ->
	    ets:new(test_wallaroo_tree, [public, named_table]),
	    ok;
	_ ->
	    ok
    end.

ets_db_store(Hash, Object) ->
    case ets:lookup(test_wallaroo_tree, Hash) of
	[] ->
	    ets:insert(test_wallaroo_tree, {Hash, Object}),
	    ok;
	_ ->
	    ok
    end.

ets_db_find(Hash) ->
    case ets:match(test_wallaroo_tree, {Hash, '$1'}) of
	[[X]] ->
	    X;
	[[_X]|_Xs] ->
	    too_many;
        _ ->
	    find_failed
    end.

ets_db_keys() ->
    ets_db_keys(as_binary).

ets_db_keys(as_binary) ->
    Matches = ets:match(test_wallaroo_tree, {'$1', '_'}),
    [Hash || [Hash] <- Matches];
ets_db_keys(as_string) ->
    Matches = ets:match(test_wallaroo_tree, {'$1', '_'}),
    [wallaroo_hash:as_string(Hash) || [Hash] <- Matches].

-endif.
