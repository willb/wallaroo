-module(wallaroo_bktree).
% XXX:  should store/3 and friends be exported?
-export([empty/0, store/3, find/2, has/2, get_path/3, put_path/4, del_path/3, put_tree/2, children/1, diff/2]).


-define(TAG_OBJ, wObj).
-define(TAG_ACCESSIBLE_TREE, wAT).
-define(TAG_BUCKETED_TREE, wBT).
-define(MAX_TREE_SIZE, 1024).

-type rawtree() :: gb_tree().
% tag, depth, tree
-type bucketed_tree() :: {?TAG_BUCKETED_TREE, int(), rawtree()}.
-type accessible_tree() :: {?TAG_ACCESSIBLE_TREE, rawtree()}.
-type object() :: {?TAG_OBJECT, any()}.
-type tree() :: bucketed_tree() | accessible_tree().
-type find_result() :: 'none' | {'value', _}.
-export_type([tree/0, find_result/0]).

%% @doc returns an empty (accessible) tree
-spec empty() -> tree().
empty() ->
    {?TAG_ACCESSIBLE_TREE, gb_trees:empty()}.

%% @doc stores an entry in Tree with key Key and value Val
-spec store(_,_,tree()) -> tree().
store(Key, Val, {?TAG_ACCESSIBLE_TREE=Tag, Tree}) ->
    case gb_trees:lookup(Key, Tree) of
	none ->
	    {Tag, gb_trees:insert(Key, Val, Tree)};
	{value, _} ->
	    {Tag, gb_trees:update(Key, Val, Tree)}
	end.


%% When you split, take the kth byte of the hash and use that as a key
kth_part(K, Bin) when is_bitstring(Bin) && is_integer(K) ->
    SkipSize = K * 8,
    <<Skip:SkipSize, Take:8, Rest/binary>> = Bin,
    Take.

% Returns a raw gb_tree of trees mapping from key_hash_part -> key -> value
map_pairs([_|_]=Pairs, Depth) when is_integer(Depth) ->
    FoldFun = fun({Key, Val}, Acc) ->
		      HashPart = kth_part(Depth, wallaroo_db:identity(Key)),
		      case gb_trees:lookup(Key, HashPart) of
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
split({?TAG_ACCESSIBLE_TREE=Tag, {Sz,_}=Tree}, _, _) when Sz <= ?MAX_TREE_SIZE ->
    {Tag, Tree};
split({?TAG_ACCESSIBLE_TREE=Tag, Tree}, Depth, StoreMod) ->
    RawTree = map_pairs(gb_trees:to_list(Tree), Depth),
    StoreFunc = fun(HashPart, Subtree) -> wallaroo_db:hash_and_store(Subtree, StoreMod) end,
    NewTree = gb_trees:map(StoreFunc, RawTree),
    {?TAG_BUCKETED_TREE, Depth, NewTree}.


    
