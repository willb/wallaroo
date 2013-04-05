% commit representation for Wallaroo
% Copyright (c) 2011 Red Hat, Inc., and William C. Benton

-module(wallaroo_commit).
-export([empty/0, new/4, store/2, get_meta/2, get_changes/1, get_tree_hash/1, get_tree/2, reachable/2, reachable/3]).

-define(COMMIT_TUPLE_TAG, wallaroo_commit).
-define(EMPTY_COMMIT, {?COMMIT_TUPLE_TAG, empty}).

-export_type([parents/0, tree_id/0, changes/0, meta/0, commit/0, commit_id/0]).

-type parents() :: [wallaroo_hash:bin()].
-type tree_id() :: wallaroo_hash:bin().
-type changes() :: [any()].
-type meta() :: [{atom(), any()}].

-type commit() :: ?EMPTY_COMMIT | {?COMMIT_TUPLE_TAG, {parents(), tree_id(), changes(), meta()}}.
-type commit_id() :: wallaroo_hash:bin().

-spec empty() -> commit().
empty() ->
    ?EMPTY_COMMIT.

-spec new(parents(), tree_id(), changes(), meta()) -> commit().
new([_|_]=Parents, Tree, Changes, Meta) 
  when is_list(Changes), is_list(Parents), is_list(Meta) ->
    % error_logger:warning_msg("Parents = ~p~nTree = ~p~nChanges = ~p~nMeta = ~p~n", [Parents, Tree, Changes, Meta]),
    OrderedParents = lists:sort(Parents),
    OrderedMeta = orddict:from_list(Meta),
    {?COMMIT_TUPLE_TAG, {OrderedParents, Tree, Changes, OrderedMeta}}.

-spec store(commit(), module()) -> commit_id().
store({?COMMIT_TUPLE_TAG, {_P,_T,_C,_M}}=Commit, StoreMod) ->
    {SHA, _} = wallaroo_db:hash_and_store(Commit, StoreMod, store_commit),
    SHA;
store(?EMPTY_COMMIT=Commit, StoreMod) ->
    {SHA, _} = wallaroo_db:hash_and_store(Commit, StoreMod, store_commit),
    SHA.

-spec get_meta(commit(), atom()) -> any().
get_meta({?COMMIT_TUPLE_TAG, {_, _, _, Meta}}, Key) ->
    orddict:find(Key, Meta);
get_meta(?EMPTY_COMMIT, _) ->
    error.

-spec get_parents(commit()) -> {ok, parents()} | error.
get_parents({?COMMIT_TUPLE_TAG, {Parents, _, _, _}}) ->
    {ok, Parents};
get_parents(?EMPTY_COMMIT) ->
    {ok, []}.

-spec get_changes(commit()) -> {ok, changes()} | error.
get_changes({?COMMIT_TUPLE_TAG, {_, _, Changes, _}}) ->
    {ok, Changes};
get_changes(?EMPTY_COMMIT) ->
    error.

-spec get_tree_hash(commit()) -> {ok, tree_id()} | error.
get_tree_hash({?COMMIT_TUPLE_TAG, {_, Tree, _, _}}) ->
    {ok, Tree};
get_tree_hash(?EMPTY_COMMIT) ->
    error.

-spec get_tree(commit(), module()) -> wallaroo_tree:tree() | error.
get_tree({?COMMIT_TUPLE_TAG, {_, Tree, _, _}}, StoreMod) ->
    StoreMod:find_object(Tree);
get_tree(?EMPTY_COMMIT, _) ->
    error.

-spec reachable(commit() | commit_id(), module()) -> [commit_id()].
reachable(From, StoreMod) ->
    reachable(From, StoreMod, as_list).

-spec reachable(commit() | commit_id(), module(), as_list) -> [commit_id()]
      ; (commit() | commit_id(), module(), as_set) -> gb_set().
reachable({?COMMIT_TUPLE_TAG, _}=FC, StoreMod, How) ->
    reachable(wallaroo_db:identity(FC), StoreMod, How);
reachable(From, StoreMod, as_list) ->  
    gb_sets:to_list(reachable(From, StoreMod, as_set));
reachable(From, StoreMod, as_set) ->
    reachable_helper([From], StoreMod, gb_sets:empty()).

reachable_helper([], _, Visited) ->
    Visited;
reachable_helper([Hash|Hs], StoreMod, Visited) ->
    case gb_sets:is_member(Hash, Visited) of 
	true ->
	    reachable_helper(Hs, StoreMod, Visited);
	false -> 
	    Commit = StoreMod:find_commit(Hash), 
	    NewVisited = gb_sets:add(Hash, Visited),
	    {ok, Parents} = get_parents(Commit),
	    reachable_helper(append_unvisited(Parents, Hs, Visited), StoreMod, NewVisited)
    end.

append_unvisited([], Acc, _) ->
    Acc;
append_unvisited([H|Hs], Acc, Visited) ->
    case gb_sets:is_member(H, Visited) of
	true ->
	    append_unvisited(Hs, Acc, Visited);
	false ->
	    append_unvisited(Hs, [H|Acc], Visited)
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

test_setup() ->
    wallaroo_store_ets:init([]).

fixture() ->
    SM = wallaroo_store_ets,
    EmptySHA = store(empty(), SM),
    FirstSHA = store(new([EmptySHA], first, [], []), SM),
    SecondSHA = store(new([EmptySHA], second, [], []), SM),
    ThirdSHA = store(new([SecondSHA], third, [], []), SM),
    FourthSHA = store(new([FirstSHA, SecondSHA], fourth, [], []), SM),
    FifthSHA = store(new([FirstSHA, SecondSHA, EmptySHA], fifth, [], []), SM),
    SM:store_object(es, EmptySHA), 
    SM:store_object(fis, FirstSHA),
    SM:store_object(ss, SecondSHA),
    SM:store_object(ts, ThirdSHA),
    SM:store_object(fos, FourthSHA),
    SM:store_object(ffs, FifthSHA).

basic_test_() ->
    {inorder,
     {setup,
      fun() -> test_setup(), fixture() end,
      fun(_) -> ok end,
      [?_assertEqual(1, length(reachable(wallaroo_store_ets:find_object(es), wallaroo_store_ets, as_list))),
       ?_assertEqual(2, length(reachable(wallaroo_store_ets:find_object(fis), wallaroo_store_ets, as_list))),
       ?_assertEqual(2, length(reachable(wallaroo_store_ets:find_object(ss), wallaroo_store_ets, as_list))),
       ?_assertEqual(3, length(reachable(wallaroo_store_ets:find_object(ts), wallaroo_store_ets, as_list))),
       ?_assertEqual(4, length(reachable(wallaroo_store_ets:find_object(fos), wallaroo_store_ets, as_list))),
       ?_assertEqual(4, length(reachable(wallaroo_store_ets:find_object(ffs), wallaroo_store_ets, as_list)))
]}}.
	     	      
-endif.
