% tree interface for Wallaroo
% Copyright (c) 2011 Red Hat, Inc., and William C. Benton

-module(wallaroo_tree).
-compile(export_all).

-type tree() :: gb_tree().
-type find_result() :: 'none' | {'value', _}.
-export_type([tree/0]).

-spec empty() -> tree().
empty() ->
    gb_trees:empty().

-spec store(_,_,tree()) -> tree().
store(Key, Val, Tree) ->
    case gb_trees:lookup(Key, Tree) of
	none ->
	    gb_trees:insert(Key, Val, Tree);
	{value, _} ->
	    gb_trees:update(Key, Val, Tree)
	end.

-spec find(_, tree()) -> find_result().
find(Key, Tree) ->
    gb_trees:lookup(Key, Tree).

-spec has(_, tree()) -> boolean().
has(Key, Tree) ->
    gb_trees:is_defined(Key, Tree).

-spec get_path([any()], _, atom()) -> any().
get_path([], BS, StoreMod) when is_binary(BS) ->
    StoreMod:find_object(BS);
get_path([], Obj, _) ->
    Obj;
get_path([P|Rest], Tree, StoreMod) ->
    case find(P, Tree) of
	none ->
	    none;
	{value, ElementHash} ->
	    Branch = StoreMod:find_object(ElementHash),
	    get_path(Rest, Branch, StoreMod)
    end.
    

put_tree(Tree, StoreMod) ->
    wallaroo_db:hash_and_store(Tree, StoreMod).

put_path([P], BS, Tree, StoreMod) when is_binary(BS) ->
    NewTree = store(P, BS, Tree),
    wallaroo_db:hash_and_store(NewTree, StoreMod);	    
put_path([P], Object, Tree, StoreMod) ->
    {ObjectHash, Object} = wallaroo_db:hash_and_store(Object, StoreMod),
    put_path([P], ObjectHash, Tree, StoreMod);
put_path([P|Rest], Object, Tree, StoreMod) ->
    case find(P, Tree) of
	none ->
	    [Last|Tser] = lists:reverse(Rest),
	    {LeafHash, _Leaf} = put_path([Last], Object, gb_trees:empty(), StoreMod),
	    FoldFun = fun(Element, AccHash) -> {Hash, _NST} = put_path([Element], AccHash, gb_trees:empty(), StoreMod), Hash end,
	    NewBranch = lists:foldl(FoldFun, LeafHash, Tser),
	    put_path([P], NewBranch, Tree, StoreMod);
	{value, ElementHash} ->
	    Subtree = StoreMod:find_object(ElementHash),
	    {SubtreeHash, _NewSubtree} = put_path(Rest, Object, Subtree, StoreMod),
	    NewTree = store(P, SubtreeHash, Tree),
	    wallaroo_db:hash_and_store(NewTree, StoreMod)
    end.

del_path([P], Tree, StoreMod) ->
    NewTree = gb_trees:balance(gb_trees:delete(P, Tree)),
    wallaroo_db:hash_and_store(NewTree, StoreMod);
del_path([P|Rest], Tree, StoreMod) ->
    {value, SubtreeHash} = find(P, Tree),
    Subtree = StoreMod:find_object(SubtreeHash),
    {NSTHash, _} = del_path(Rest, Subtree, StoreMod),
    NewTree = gb_trees:balance(store(P, NSTHash, Tree)),
    wallaroo_db:hash_and_store(NewTree, StoreMod).


children(Tree) ->
    lists:map(fun(Key) -> {_, Value} = find(Key, Tree), {Key, Value} end, gb_trees:keys(Tree)).


diff(T1, T2) ->
    Keys = ordsets:union(gb_trees:keys(T1), gb_trees:keys(T2)),
    [{Key, V1, V2} || {Key, V1, V2} <- [{K, gb_trees:lookup(K, T1), gb_trees:lookup(K, T2)} || K <- Keys], V1 =/= V2].

-ifdef(TEST).
-define(ETS_STORE_BACKEND, true).
-include_lib("eunit/include/eunit.hrl").

test_setup() ->
    wallaroo_store_ets:init([]).

first_fixture() ->
    SM = wallaroo_store_ets,
    {_T0H, T0} = wallaroo_tree:put_path([a,b,c,d,0], "a/b/c/d/0 for all", wallaroo_tree:empty(), SM),
    {_T1H, T1} = wallaroo_tree:put_path([a,b,c,d,e], "a/b/c/d/e for T1", T0, SM),
    {_T2H, T2} = wallaroo_tree:put_path([a,b,c,d,e], "a/b/c/d/e for T2", T1, SM),
    {_T3H, T3} = wallaroo_tree:put_path([a,b,x], "a/b/x for T3", T2, SM),
    {_T4H, T4} = wallaroo_tree:put_path([a,b,y], "a/b/y for T4", T3, SM),
    SM:store_object(t0, T0),
    SM:store_object(t1, T1),
    SM:store_object(t2, T2),
    SM:store_object(t3, T3),
    SM:store_object(t4, T4).
    

test_teardown() ->
    wallaroo_store_ets:cleanup([]).

find_fixture_tree(Id) ->
    wallaroo_store_ets:find_object(Id).

simple_test_() ->
    {inorder,
     {setup,
      fun() -> test_setup(), first_fixture() end,
      fun(_) -> test_teardown() end,
      [?_assertEqual("a/b/c/d/0 for all", 
		    wallaroo_tree:get_path([a,b,c,d,0], find_fixture_tree(t0), wallaroo_store_ets)),
      ?_assertEqual("a/b/c/d/0 for all", 
		    wallaroo_tree:get_path([a,b,c,d,0], find_fixture_tree(t1), wallaroo_store_ets)),
      ?_assertEqual("a/b/c/d/0 for all", 
		    wallaroo_tree:get_path([a,b,c,d,0], find_fixture_tree(t2), wallaroo_store_ets)),
      ?_assertEqual("a/b/c/d/0 for all", 
		    wallaroo_tree:get_path([a,b,c,d,0], find_fixture_tree(t3), wallaroo_store_ets)),
      ?_assertEqual("a/b/c/d/0 for all", 
		    wallaroo_tree:get_path([a,b,c,d,0], find_fixture_tree(t4), wallaroo_store_ets)),
      ?_assertEqual("a/b/c/d/e for T1", 
		    wallaroo_tree:get_path([a,b,c,d,e], find_fixture_tree(t1), wallaroo_store_ets)),
      ?_assertEqual("a/b/c/d/e for T2", 
		    wallaroo_tree:get_path([a,b,c,d,e], find_fixture_tree(t2), wallaroo_store_ets)),
      ?_assertEqual("a/b/c/d/e for T2", 
		    wallaroo_tree:get_path([a,b,c,d,e], find_fixture_tree(t3), wallaroo_store_ets)),
      ?_assertEqual("a/b/c/d/e for T2", 
		    wallaroo_tree:get_path([a,b,c,d,e], find_fixture_tree(t4), wallaroo_store_ets)),
      ?_assertEqual("a/b/x for T3", 
		    wallaroo_tree:get_path([a,b,x], find_fixture_tree(t3), wallaroo_store_ets)),
      ?_assertEqual("a/b/x for T3", 
		    wallaroo_tree:get_path([a,b,x], find_fixture_tree(t4), wallaroo_store_ets)),
      ?_assertEqual("a/b/y for T4", 
		    wallaroo_tree:get_path([a,b,y], find_fixture_tree(t4), wallaroo_store_ets)),
      ?_assertNot("a/b/y for T4" =:=
		       wallaroo_tree:get_path([a,b,y], find_fixture_tree(t1), wallaroo_store_ets))]}}.
		    

	      

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
    SM = wallaroo_store_ets,
    {_H1, Res} = wallaroo_tree:put_path([a,b,c,d,e], 37, wallaroo_tree:empty(), SM),
    {_H2, Res2} = wallaroo_tree:put_path([a,b,d,e], 42, Res, SM),
    {_H3, _Res3} = wallaroo_tree:put_path([a,c,d,e], 18, Res2, SM).

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
