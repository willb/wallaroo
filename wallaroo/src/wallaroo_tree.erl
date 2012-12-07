-module(wallaroo_tree).
% XXX:  should store/3 and friends be exported?
-export([empty/0, store/3, find/2, has/2, get_path/3, put_path/4, del_path/3, put_tree/2, diff/2, resolve/3, children/2]).

-define(DEBUG, yes).

-ifdef(DEBUG).
-export([do_test_from_shell/0, do_dbg_from_shell/0]).
-endif.

-define(TAG_OBJECT, wObj).
-define(TAG_ACCESSIBLE_TREE, wAT).
-define(TAG_BUCKETED_TREE, wBT).
-define(MAX_TREE_SIZE, 512).

-type rawtree() :: gb_tree().
% tag, depth, tree
-type bucketed_tree() :: {?TAG_BUCKETED_TREE, integer(), rawtree()}.
-type accessible_tree() :: {?TAG_ACCESSIBLE_TREE, rawtree()}.
-type object() :: {?TAG_OBJECT, any()}.
-type tree() :: bucketed_tree() | accessible_tree().
-type find_result() :: 'none' | {'value', _}.
-record(res_none, {tree :: tree(), 
		   depth = 0 :: integer(),
		   rest :: list(),
		   subkey = 'empty' :: 'empty' | integer()}).
-record(res_some, {tree :: tree(), 
		   depth :: integer(), 
		   keyhash :: bitstring(), 
		   kv :: {any(), any()}}).
-type resolve_elt() :: #res_none{} | #res_some{}.
-type resolution() :: [resolve_elt()].
-export_type([tree/0, find_result/0]).

%% @doc returns an empty (accessible) tree
-spec empty() -> accessible_tree().
empty() ->
    {?TAG_ACCESSIBLE_TREE, gb_trees:empty()}.

wrap_object({?TAG_OBJECT, _}=O) ->
    O;
wrap_object(O) ->
    {?TAG_OBJECT, O}.

unwrap_object({?TAG_OBJECT, O}) ->
    O;
unwrap_object(O) ->
    O.

%% @doc stores an entry in a Tree with key Key and value Val
-spec store(_,_,tree()) -> tree().
store(Key, Val, {?TAG_ACCESSIBLE_TREE=Tag, Tree}) ->
    case gb_trees:lookup(Key, Tree) of
	none ->
	    {Tag, gb_trees:insert(Key, Val, Tree)};
	{value, _} ->
	    {Tag, gb_trees:update(Key, Val, Tree)}
    end;
store(Subkey, Val, {?TAG_BUCKETED_TREE=Tag, Depth, Tree}) ->
    case gb_trees:lookup(Subkey, Tree) of
	none ->
	    {Tag, Depth, gb_trees:insert(Subkey, Val, Tree)};
	{value,_} ->
	    {Tag, Depth, gb_trees:update(Subkey, Val, Tree)}
    end.


%% When you split, take the kth part of the hash and use that as a key
kth_part(K, Bin) when is_bitstring(Bin) and is_integer(K) ->
    SkipSize = K * 6,
    <<_Skip:SkipSize, Take:6, _Rest/bitstring>> = Bin,
    Take.

% Returns a raw gb_tree of trees mapping from key_hash_part -> key -> value
map_pairs([_|_]=Pairs, Depth) when is_integer(Depth) ->
    FoldFun = fun({Key, Val}, Acc) ->
		      HashPart = kth_part(Depth, wallaroo_db:identity(Key)),
		      case gb_trees:lookup(HashPart, Acc) of
			  none ->
			      Subtree = gb_trees:insert(Key, Val, gb_trees:empty()),
			      gb_trees:insert(HashPart, Subtree, Acc);
			  {value, ExistingSubtree} ->
			      Subtree = gb_trees:insert(Key, Val, ExistingSubtree),
			      gb_trees:update(HashPart, Subtree, Acc)
		      end
	      end,
    lists:foldl(FoldFun, gb_trees:empty(), Pairs).

%% @doc splits and balances an accessible tree into buckets if necessary; requires a storage module
%% @return a tree that should be hashed and stored
split({?TAG_ACCESSIBLE_TREE=Tag, Tree}, Depth, StoreMod)  ->
    case gb_trees:size(Tree) =< ?MAX_TREE_SIZE of
	true -> 
	    {Tag, Tree};
	false ->
	    RawTree = map_pairs(gb_trees:to_list(Tree), Depth),
	    StoreFunc = fun(_HashPart, Subtree) -> element(1, wallaroo_db:hash_and_store({?TAG_ACCESSIBLE_TREE, Subtree}, StoreMod)) end,
	    NewTree = gb_trees:map(StoreFunc, RawTree),
	    {?TAG_BUCKETED_TREE, Depth, NewTree}
    end.

%% @doc finds the value stored in Tree under Key
-spec find(_, tree()) -> find_result().
find(Key, {?TAG_ACCESSIBLE_TREE, Tree}) ->
    gb_trees:lookup(Key, Tree);
find(Key, {?TAG_BUCKETED_TREE, _Depth, Tree}) ->
    gb_trees:lookup(Key, Tree).

%% @doc returns true if Tree contains an entry for Key
-spec has(_, tree()) -> boolean().
has(Key, {?TAG_ACCESSIBLE_TREE, Tree}) ->
    gb_trees:is_defined(Key, Tree).

%% XXX: first any is really tree() | bitstring() | object(); need a new type for this
-spec resolve(list(), any(), atom()) -> resolution().
resolve(Path, Tree, StoreMod) ->
    resolve_it(Path, Tree, StoreMod, 0, nil, []).

%% XXX: first any is really tree() | bitstring() | object(); second any is really 'nil' | bitstring()
-spec resolve_it(list(), any(), atom(), integer(), any(), resolution()) -> resolution().
resolve_it([], _, _, _, _, Acc) ->
    Acc;
resolve_it([P|Rest]=Path, {?TAG_ACCESSIBLE_TREE, _}=Tree, StoreMod, Depth, KeyHash, Acc) ->
    case find(P, Tree) of
	none ->
	    [#res_none{tree=Tree, depth=Depth, rest=Path}|Acc];
	{value, ElementHash} ->
	    Branch = StoreMod:find_object(ElementHash),
	    resolve_it(Rest, Branch, StoreMod, 0, nil, [#res_some{kv={P, Branch}, tree=Tree, depth=Depth, keyhash=KeyHash}|Acc])
    end;
resolve_it([P|_]=Path, {?TAG_BUCKETED_TREE, _, _}=Tree, StoreMod, Depth, nil, Acc) ->
    KeyHash = wallaroo_db:identity(P),
    resolve_it(Path, Tree, StoreMod, Depth, KeyHash, Acc);
resolve_it(Path, {?TAG_BUCKETED_TREE, Depth, _}=Tree, StoreMod, ProvidedDepth, KeyHash, Acc) ->
    Subkey = kth_part(Depth, KeyHash),
    case find(Subkey, Tree) of
	none ->
	    [#res_none{tree=Tree, depth=Depth, subkey=Subkey, rest=Path}|Acc];
	{value, ElementHash} ->
	    Subtree = StoreMod:find_object(ElementHash),
	    NewDepth = case {rist_final, Subtree} of
			   {rist_final, {?TAG_BUCKETED_TREE, D, _}} ->
			       D;
			   {rist_final, {?TAG_ACCESSIBLE_TREE, _}} ->
			       ProvidedDepth + 1
		       end,
	    Entry = #res_some{kv={Subkey, Subtree}, tree=Tree, depth=NewDepth, keyhash=KeyHash},
	    resolve_it(Path, Subtree, StoreMod, NewDepth, KeyHash, [Entry|Acc])
    end.


-spec put_tree(tree(), atom()) -> binary().
put_tree(Tree, StoreMod) ->
    element(1, wallaroo_db:hash_and_store(Tree, StoreMod)).

%% @doc returns the result of looking up a path
-spec get_path([binary()|atom()], _, atom()) -> find_result().
get_path(Path, Tree, StoreMod) ->
    case resolve(Path, Tree, StoreMod) of
	[#res_none{}|_] ->
	    none;
	[#res_some{kv={_,V}}|_] ->
	    {value, unwrap_object(V)}
    end.

%% @doc stores Val in Tree at PathPart, assuming that no component of PathPart is already in Tree; returns hash for updated Tree
fold_absent([Key], {?TAG_OBJECT, _}=Val, {?TAG_BUCKETED_TREE, _, _}=Tree, _, StoreMod) ->
    VH = element(1, wallaroo_db:hash_and_store(Val, StoreMod)),
    wallaroo_db:hash_and_store(store(Key, VH, Tree), StoreMod);
fold_absent([Key], {?TAG_OBJECT, _}=Val, {?TAG_ACCESSIBLE_TREE, _}=Tree, Depth, StoreMod) ->
    VH = element(1, wallaroo_db:hash_and_store(Val, StoreMod)),
    wallaroo_db:hash_and_store(split(store(Key, VH, Tree), Depth + 1, StoreMod), StoreMod);
fold_absent([Key|PathPart], {?TAG_OBJECT, _}=Val, Tree, Depth, StoreMod) ->
    [Last|Tser] = lists:reverse(PathPart),
    ValHash = element(1, wallaroo_db:hash_and_store(Val, StoreMod)),
    Leaf = wallaroo_db:hash_and_store(store(Last, ValHash, empty()), StoreMod),
    FoldFun = fun(Element, {AccHash,_}) ->
		      T = store(Element, AccHash, empty()),
		      wallaroo_db:hash_and_store(T, StoreMod)
	      end,
    {NBH,_NBT} = lists:foldl(FoldFun, Leaf, Tser),
    case Tree of 
	{?TAG_BUCKETED_TREE, _, _} ->
	    wallaroo_db:hash_and_store(store(Key, NBH, Tree), StoreMod);
	{?TAG_ACCESSIBLE_TREE, _} ->
	    wallaroo_db:hash_and_store(split(store(Key, NBH, Tree), Depth + 1, StoreMod), StoreMod)
    end.

fold_present(ResolvedPath, {H, _}=Val, StoreMod) when is_bitstring(H) ->
    KTs = [{K, T} || #res_some{kv={K, _}, tree=T} <- ResolvedPath],
    FF = fun({Ky, Tr}, {AccHash, _AccObj}) ->
		 wallaroo_db:hash_and_store(store(Ky, AccHash, Tr), StoreMod)
	 end,
    lists:foldl(FF, Val, KTs).

%% @doc stores a value at a given path
-spec put_path([binary()|atom()], object() | binary(), tree(), atom()) -> {binary(), any()}.
put_path(Path, {?TAG_OBJECT, _}=Object, Tree, StoreMod) when is_list(Path) ->
    ResolvedPath = resolve(Path, Tree, StoreMod),
    case ResolvedPath of
	[#res_none{tree=Subtree, subkey=empty, depth=Depth, rest=AbsentPath}|Rest] ->	    
	    NBHash = fold_absent(AbsentPath, Object, Subtree, Depth, StoreMod),
	    fold_present(Rest, NBHash, StoreMod);
	[#res_none{tree=Subtree, subkey=SK, depth=Depth, rest=AbsentPath}|Rest] ->	    
	    NBHash = fold_absent([SK|AbsentPath], Object, Subtree, Depth, StoreMod),
	    fold_present(Rest, NBHash, StoreMod);
	[#res_some{}|_] -> 
	    fold_present(ResolvedPath, wallaroo_db:hash_and_store(Object, StoreMod), StoreMod)
    end;
put_path(Path, Object, Tree, StoreMod) when is_list(Path) ->
    put_path(Path, wrap_object(Object), Tree, StoreMod).


del_path(_Path, _Tree, _StoreMod) ->
    throw(not_implemented).

-spec children(tree(), module()) -> [{binary(), any()}].
children(Root, StoreMod) ->
    children(Root, StoreMod, [[]], []).

children([], _, [], Acc) ->
    Acc;
children({?TAG_ACCESSIBLE_TREE, Root}, StoreMod, [Top|Rest], Acc) ->
    % [{K,StoreMod:find_object(V)} || {K, V} <- gb_trees:to_list(Root)] ++ Acc,
    NewAcc = 
	lists:foldl(fun({K,V}, A) -> [{K,unwrap_object(StoreMod:find_object(V))}|A] end,
		    Acc,
		    gb_trees:to_list(Root)),
    children(Top, StoreMod, Rest, NewAcc);
children({?TAG_BUCKETED_TREE, _, Root}, StoreMod, Stack, Acc) ->
    [NewRoot|NewStack] = lists:foldl(fun({_K,V}, A) -> [StoreMod:find_object(V)|A] end,
				     Stack,
				     gb_trees:to_list(Root)),
    children(NewRoot, StoreMod, NewStack, Acc).

%% XXX: for non-leaf atrees and transitive children, use a single fold that adds to both stack and acc



diff(T1, T2) ->
    Keys = ordsets:union(gb_trees:keys(T1), gb_trees:keys(T2)),
    [{Key, V1, V2} || {Key, V1, V2} <- [{K, gb_trees:lookup(K, T1), gb_trees:lookup(K, T2)} || K <- Keys], V1 =/= V2].

-ifdef(TEST).
-define(ETS_STORE_BACKEND, true).
-define(BIG_TEST_SIZE, 8192).
-include_lib("eunit/include/eunit.hrl").

-ifdef(DEBUG).
dbg_setup() ->
    dbg:start(),
    dbg:tracer(),
    dbg:tpl(wallaroo_tree, '_', []),
    dbg:p(all, c).
-else.
dbg_setup() ->
    ok.
-endif.


test_setup() ->
%    dbg_setup(),
    wallaroo_store_ets:init([]).

first_fixture() ->
    SM = wallaroo_store_ets,
    {_T0H, T0} = wallaroo_tree:put_path([a,b,c,d,0], "a/b/c/d/0 for all", empty(), SM),
    {_T1H, T1} = wallaroo_tree:put_path([a,b,c,d,e], "a/b/c/d/e for T1", T0, SM),
    {_T2H, T2} = wallaroo_tree:put_path([a,b,c,d,e], "a/b/c/d/e for T2", T1, SM),
    {_T3H, T3} = wallaroo_tree:put_path([a,b,x], "a/b/x for T3", T2, SM),
    {_T4H, T4} = wallaroo_tree:put_path([a,b,y], "a/b/y for T4", T3, SM),
    SM:store_object(t0, T0),
    SM:store_object(t1, T1),
    SM:store_object(t2, T2),
    SM:store_object(t3, T3),
    SM:store_object(t4, T4).

second_fixture() -> 
    dbg:start(),
    dbg:tracer(),
    dbg:tpl(wallaroo_tree, second_fixture, []),
    dbg:p(all, c),
    SM = wallaroo_store_ets,
    SM:store_object(mt, wallaroo_tree:empty()),
    LS = [{list_to_atom("element_" ++ integer_to_list(X)), X} || X <- lists:seq(1,?BIG_TEST_SIZE)],
    FoldFun = fun({Leaf,Value}, {_H,T}) -> wallaroo_tree:put_path([x,y,z,Leaf], Value, T, SM) end,
    {_,SFT} = lists:foldl(FoldFun, {ignored, wallaroo_tree:empty()}, LS),
    SM:store_object(sft, SFT).

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

splitting_test_() ->
    {inorder,
     {setup,
      fun() -> test_setup(), second_fixture() end,
      fun(_) -> test_teardown() end,
      [?_assertEqual(X, wallaroo_tree:get_path([x,y,z,list_to_atom("element_" ++ integer_to_list(X))], find_fixture_tree(sft), wallaroo_store_ets)) || X <- lists:seq(1,?BIG_TEST_SIZE)]}}.
	      

-endif.

-ifdef(DEBUG).
-ifndef(ETS_STORE_BACKEND).
-define(ETS_STORE_BACKEND, true).
-endif.

do_dbg_from_shell() ->
    dbg:start(),
    dbg:tracer(),
    dbg:tpl(wallaroo_tree, '_', []),
    dbg:p(all, c).

do_test_from_shell() ->
    wallaroo_store_ets:init([]),
    SM = wallaroo_store_ets,
    {_H1, Res} = wallaroo_tree:put_path([a,b,c,d,e], wrap_object(37), wallaroo_tree:empty(), SM),
    {_H2, Res2} = wallaroo_tree:put_path([a,b,d,e], wrap_object(42), Res, SM),
    {_H3, Res3} = wallaroo_tree:put_path([a,c,d,e], wrap_object(18), Res2, SM),
    {Res, Res2, Res3}.

-endif.


%% -ifdef(ETS_STORE_BACKEND).
%% ets_db_init() ->
%%     case ets:info(test_wallaroo_tree) of
%% 	undefined ->
%% 	    ets:new(test_wallaroo_tree, [public, named_table]),
%% 	    ok;
%% 	_ ->
%% 	    ok
%%     end.

%% ets_db_store(Hash, Object) ->
%%     case ets:lookup(test_wallaroo_tree, Hash) of
%% 	[] ->
%% 	    ets:insert(test_wallaroo_tree, {Hash, Object}),
%% 	    ok;
%% 	_ ->
%% 	    ok
%%     end.

%% ets_db_find(Hash) ->
%%     case ets:match(test_wallaroo_tree, {Hash, '$1'}) of
%% 	[[X]] ->
%% 	    X;
%% 	[[_X]|_Xs] ->
%% 	    too_many;
%%         _ ->
%% 	    find_failed
%%     end.

%% ets_db_keys() ->
%%     ets_db_keys(as_binary).

%% ets_db_keys(as_binary) ->
%%     Matches = ets:match(test_wallaroo_tree, {'$1', '_'}),
%%     [Hash || [Hash] <- Matches];
%% ets_db_keys(as_string) ->
%%     Matches = ets:match(test_wallaroo_tree, {'$1', '_'}),
%%     [wallaroo_hash:as_string(Hash) || [Hash] <- Matches].

%% -endif.
