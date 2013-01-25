%% @author William C. Benton <willb@redhat.com>
%% @copyright 2012 Red Hat, Inc. and William C. Benton
%% @doc Wallaby-specific validators for Wallaroo

-module(wallaby_validators).
-export([make_activate_validators/2, make_activate_validators/3]).

validator_all_vertices_exist(Entities, Relationships) ->
    fun(_T, _SM) ->
	    G = digraph:new([private]),
	    _ = [{E, digraph:add_vertex(G, E)} || E <- Entities],
	    Edges = [{Edge, digraph:add_edge(G, From, To, Kind)} || Edge={Kind, From={_,_}, To={_,_}} <- Relationships],
	    ErrorEdges = [{Edge, Error} || {Edge, {error, Error}} <- Edges],
	    case ErrorEdges of
		[] ->
		    ok;
		[_|_] ->
		    {fail, {all_vertices_exist, {bad_edges, ErrorEdges}}}
	    end
    end.	    

validator_no_circular_includes(Entities, Relationships) ->
    fun(_T, _SM) ->
	    G = digraph:new([private]),
	    [digraph:add_vertex(G, E) || E = {'feature', _x} <- Entities],
	    [digraph:add_edge(G, F1, F2, L) || {L='includes', F1={feature, _}, F2={feature, _}} <- Relationships],
	    case digraph_utils:is_acyclic(G) of
		true ->
		    ok;
		false ->
		    {fail, {no_circular_includes, {cycles, digraph_utils:strong_components(G)}}}
	    end
    end.

validator_no_circular_feature_depends(Entities, Relationships) ->
    fun(_T, _SM) ->
	    G = digraph:new([private]),
	    [digraph:add_vertex(G, E) || E = {'feature', _x} <- Entities],
	    [digraph:add_edge(G, F1, F2, L) || {L='depends', F1={feature, _}, F2={feature, _}} <- Relationships],
	    case digraph_utils:is_acyclic(G) of
		true ->
		    ok;
		false ->
		    {fail, {no_circular_feature_depends, {cycles, digraph_utils:strong_components(G)}}}
	    end
    end.

validator_no_circular_parameter_depends(Entities, Relationships) ->
    fun(_T, _SM) ->
	    G = digraph:new([private]),
	    [digraph:add_vertex(G, E) || E = {'parameter', _x} <- Entities],
	    [digraph:add_edge(G, P1, P2, L) || {L='depends_on', P1={parameter, _}, P2={parameter, _}} <- Relationships],
	    case digraph_utils:is_acyclic(G) of
		true ->
		    ok;
		false ->
		    {fail, {no_circular_parameter_depends, {cycles, digraph_utils:strong_components(G)}}}
	    end
    end.

immediately_reachable(Graph, StartNode) ->
    ordsets:from_list([To || {_Edge,_From,To,_Label} <- [digraph:edge(Graph,E) || E <- digraph:out_edges(Graph, StartNode)]]).

transitively_reachable(Graph, StartNode) ->
    ordsets:from_list(digraph_utils:reachable_neighbours([StartNode], Graph)).

transitively_reachable(Graph, StartNode, Filter) ->
    [Node || Node <- transitively_reachable(Graph, StartNode), Filter(Node)].

validator_no_immed_conflicts_with_transitive_includes_or_deps(Entities, Relationships) ->
    fun(_T, _SM) ->
	    CG = digraph:new([private]),
	    DIG = digraph:new([private]),
	    Features = [F || F = {'feature', _X} <- Entities],
	    _ = [{digraph:add_vertex(CG, E), digraph:add_vertex(DIG, E)} || E <- Features],
	    _ = [digraph:add_edge(DIG, F1, F2, L) || {L, F1={feature, _}, F2={feature, _}} <- Relationships, L == 'depends_on' orelse L == 'includes'],
	    _ = [digraph:add_edge(CG, F1, F2, L) || {L='conflicts_with', F1={feature, _}, F2={feature, _}} <- Relationships],
	    DepsVsConflicts = 
		[{F, TransitiveDepsAndIncludes, ImmediateConflicts} 
		 || F <- Features, 
		    ordsets:intersection(TransitiveDepsAndIncludes=transitively_reachable(DIG, F), 
					 ImmediateConflicts=immediately_reachable(CG, F)) =/= ordsets:new()
		],
	    case DepsVsConflicts of
		[] -> ok;
		_ -> {fail, {no_immed_conflicts_with_transitive_includes_or_deps, DepsVsConflicts}}
	    end
    end.

validator_deps_and_conflicts_satisfied(Entities, Relationships) ->
    fun(_T, _SM) ->
	    FDepGraph = digraph:new([protected]),
	    PDepGraph = digraph:new([protected]),
	    PConGraph = digraph:new([protected]),
	    NodeGraph = digraph:new([protected]),
	    FConGraph = digraph:new([protected]),
	    lists:foreach(fun(Entity={Kind, _}) when Kind =:= 'feature' ->
				  digraph:add_vertex(FDepGraph, Entity),
				  digraph:add_vertex(FConGraph, Entity),
				  digraph:add_vertex(NodeGraph, Entity);
			     (Entity={Kind, _}) when Kind =:= 'parameter' ->
				  digraph:add_vertex(PDepGraph, Entity),
				  digraph:add_vertex(PConGraph, Entity),
				  digraph:add_vertex(NodeGraph, Entity);			
			     (Entity={Kind, _}) when Kind =:= 'group' orelse Kind =:= 'node' ->
				  digraph:add_vertex(NodeGraph, Entity);
			     (_) -> ok
			  end,
			  Entities),
	    %% XXX: this fun ain't got no alibi
	    lists:foreach(fun({L, E1={Kind, _}, E2={Kind, _}}) when L =:= 'depends_on' andalso Kind =:= 'feature'->
				  digraph:add_edge(FDepGraph, E1, E2, L);
			     ({L, E1={Kind, _}, E2={Kind, _}}) when L =:= 'conflicts_with' andalso Kind =:= 'feature'->
				  digraph:add_edge(FConGraph, E1, E2, L);
			     ({L, E1={Kind, _}, E2={Kind, _}}) when L =:= 'depends_on' andalso Kind =:= 'parameter'->
				  digraph:add_edge(PDepGraph, E1, E2, L);
			     ({L, E1={Kind, _}, E2={Kind, _}}) when L =:= 'conflicts_with' andalso Kind =:= 'parameter'->
				  digraph:add_edge(PConGraph, E1, E2, L);
			     ({L, E1, E2}) when L =:= 'installs' orelse L =:= 'sets_param' orelse L =:= 'member_of' ->
				  digraph:add_edge(NodeGraph, E1, E2, L);
			     (_) -> ok
			  end,
			  Relationships),
	    Nodes = 
		[Node || Node={'node', _} <- digraph:vertices(NodeGraph)],

	    FeatureReqsSatisfied = requirements_checker_maker(Nodes, NodeGraph, FDepGraph, FConGraph, fun({'feature', _}) -> true; (_) -> false end, feature),
            ParamReqsSatisfied = requirements_checker_maker(Nodes, NodeGraph, PDepGraph, PConGraph, fun({'parameter', _}) -> true; (_) -> false end, parameter),
	    Worker = wallaroo_validators:pcompose([FeatureReqsSatisfied, ParamReqsSatisfied]),
	    Worker(_T, _SM)
    end.

requirements_checker_maker(Nodes, NodeGraph, DepGraph, ConflictGraph, EntityMatcher, What) ->
    fun(_T, _SM) ->
	    NodeMap = 
		[{Node, ordsets:from_list(transitively_reachable(NodeGraph, Node, EntityMatcher))} ||
		    Node <- Nodes],
	    
	    Entities =
		lists:foldl(fun({_, Es}, Acc) -> ordsets:union(Es, Acc) end, [], NodeMap),
	    
	    % XXX: there is a way to generalize this even more, of course
	    DepsSatisfied = 
		fun(_, _) -> 
			Deps = 
			    gb_trees:from_orddict([{Entity, ordsets:from_list(transitively_reachable(DepGraph, Entity))} || Entity <- Entities]),
			MissingDepMap = lists:foldl(fun({Node, Es}, Acc) ->
							    Result = {Node, 
								      [{Entity, DepDiff} || 
									  Entity <- Es, 
									  (DepDiff = ordsets:subtract(gb_trees:get(Deps, Entity), Es)) =/= []]},
							    [Result|Acc]
						    end,
						    [],
						    NodeMap),
			case [{Node, Ls} || {Node, Ls} <- MissingDepMap, Ls =/= []] of
			    [] ->
				ok;
			    MissingDeps when is_list(MissingDeps) ->
				{fail, {missing_dependencies, What, for_nodes, MissingDeps}}
			end
		end,
	    
	    ConflictsRespected = 
		fun(_, _) -> 
			Cnfs = 
			    gb_trees:from_orddict([{Entity, ordsets:from_list(immediately_reachable(ConflictGraph, Entity))} || Entity <- Entities]),
			BadCnfMap = lists:foldl(fun({Node, Es}, Acc) ->
							    Result = {Node, 
								      [{Entity, ViolatedCnfs} || 
									  Entity <- Es, 
									  (ViolatedCnfs = ordsets:intersection(gb_trees:get(Cnfs, Entity), Es)) =/= []]},
							    [Result|Acc]
						    end,
						    [],
						    NodeMap),
			case [{Node, Ls} || {Node, Ls} <- BadCnfMap, Ls =/= []] of
			    [] ->
				ok;
			    ViolatedCnfs when is_list(ViolatedCnfs) ->
				{fail, {violated_conflicts, What, for_nodes, ViolatedCnfs}}
			end
		end,
	    
	    Worker = wallaroo_validators:pcompose([DepsSatisfied, ConflictsRespected]),
	    Worker(_T, _SM)
    end.


make_activate_validators(Tree, StoreMod) ->
    make_activate_validators(all, Tree, StoreMod).

make_activate_validators({E,R}, _Tree, _StoreMod) ->
    [validator_all_vertices_exist(E,R),
     validator_no_circular_includes(E,R),
     validator_no_immed_conflicts_with_transitive_includes_or_deps(E,R),
     validator_no_circular_feature_depends(E,R),
     validator_no_circular_parameter_depends(E,R),
     validator_deps_and_conflicts_satisfied(E,R)
     %% TODO:  must-change params
    ];
make_activate_validators(DirtyNodes, Tree, StoreMod) when is_list(DirtyNodes) ->
    make_activate_validators(wallaby_graph:extract_graph(Tree, StoreMod, DirtyNodes), ignored, ignored);
make_activate_validators(all, Tree, StoreMod) ->
    make_activate_validators(wallaby_graph:extract_graph(Tree, StoreMod), ignored, ignored).


     
