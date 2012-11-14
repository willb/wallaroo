-module(tree_eval).
-export([tree_size/1, overheads/1, powerof2_overheads/2]).

overheads([_|_]=Sizes) ->
    [tree_size(X) || X <- Sizes].

powerof2_overheads(Small, Large) ->
    overheads([trunc(math:pow(2,Y)) || Y <- lists:seq(Small,Large)]).

tree_size(Count) -> 
    [N|_] = Nodes = ["node-" ++ integer_to_list(N) ++ ".local.domain.com" || N <- lists:seq(1,Count)],
    ExemplarHash = wallaroo_hash:as_bitstring(N),
    NodePairs = [{Node, ExemplarHash} || Node <- Nodes],
    Set = gb_trees:from_orddict(NodePairs),
    Sz = size(term_to_binary(Set)),
    {Count, length(lists:flatten(Nodes)) + 20 * length(Nodes), Sz}.
