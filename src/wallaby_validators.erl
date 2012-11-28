%% @author William C. Benton <willb@redhat.com>
%% @copyright 2012 Red Hat, Inc. and William C. Benton
%% @doc Wallaby-specific validators for Wallaroo

-module(wallaby_validators).
-export([extract_graph/2,sg_union/2, make_activate_validators/2, make_activate_validators/3]).

-type node_entity() :: {'node', string()}.
-type group_entity() :: {'group', string()}.
-type feature_entity() :: {'feature', string()}.
-type parameter_entity() :: {'parameter', string()}.
-type subsystem_entity() :: {'subsystem', string()}.
-type entity() :: node_entity() | group_entity() | feature_entity() | parameter_entity() | subsystem_entity().

-type simple_relationship(Verb, Noun) :: {Verb, Noun, Noun}.
-type two_kinded_relationship(Verb, N1, N2) :: {Verb, N1, N2}.

-type relationship() :: two_kinded_relationship('member_of', node_entity(), group_entity()) | 
			two_kinded_relationship('installs', group_entity(), feature_entity()) | 
			two_kinded_relationship('sets_param', group_entity(), parameter_entity()) |
			two_kinded_relationship('sets_param', feature_entity(), parameter_entity()) |
			simple_relationship('includes', feature_entity()) |
			simple_relationship('depends_on', feature_entity()) |
			simple_relationship('conflicts_with', feature_entity()) |
			simple_relationship('depends_on', parameter_entity()) |
			simple_relationship('conflicts_with', parameter_entity()) |
			two_kinded_relationship('is_interested_in', subsystem_entity(), parameter_entity()).

-define(NODEPATH, <<"nodes">>).
-define(GROUPPATH, <<"groups">>).
-define(FEATUREPATH, <<"features">>).
-define(PARAMETERPATH, <<"parameters">>).
-define(SUBSYSTEMPATH, <<"subsystems">>).

-type simple_graph() :: {[entity()], [relationship()]}.
-type sg_validity() :: 'ok' | [{error, any()}].

-export_type([simple_graph/0]).

-spec extract_graph(wallaroo_tree:tree(), module()) -> simple_graph().
extract_graph(Tree, StoreMod) ->
    % XXX:  these, like all raw tree accesses, should be factored out to an internal API
    {NodeEntities, NodeRelationships} = extract_node_entities(get_children([?NODEPATH], Tree, StoreMod)),
    {GroupEntities, GroupRelationships} = extract_group_entities(get_children([?GROUPPATH], Tree, StoreMod)),
    {FeatureEntities, FeatureRelationships} = extract_feature_entities(get_children([?FEATUREPATH], Tree, StoreMod)),
    {ParamEntities, ParamRelationships} = extract_parameter_entities(get_children([?PARAMETERPATH], Tree, StoreMod)),
    {SubsysEntities, SubsysRelationships} = extract_subsystem_entities(get_children([?SUBSYSTEMPATH], Tree, StoreMod)),
    Entities = lists:foldl(fun ordsets:union/2, [], [NodeEntities, GroupEntities, FeatureEntities, ParamEntities, SubsysEntities]),
    Relationships = lists:foldl(fun ordsets:union/2, [], [NodeRelationships, GroupRelationships, FeatureRelationships, ParamRelationships, SubsysRelationships]),
    {Entities, Relationships}.

objects_from_names(Tree, Kind, Names, StoreMod) ->
    [Obj || 
	{_,Obj} <- [wallaroo_tree:get_path([Kind, Name], Tree, StoreMod) ||
		       Name <- Names]].

saturate_features([], {Entities, Relationships, _}, _Tree, _StoreMod) ->
    {Entities, Relationships};
saturate_features(Wl, {E, R, S}, Tree, StoreMod) ->
    {Ne, Nr} = extract_feature_entities(objects_from_names(Tree, ?FEATUREPATH, Wl, StoreMod)),
    Included = [F || {'includes', _, {'feature', F}} <- Nr],
    Acc = {ordsets:union(Ne,E),
	   ordsets:union(Nr,R),
	   Ns = ordsets:union(Wl, S)},
    saturate_features(ordsets:subtract(Included, Ns), Acc, Tree, StoreMod).

extract_graph(Tree, StoreMod, DirtyNodes) ->
    NodeObjects = objects_from_names(Tree, ?NODEPATH, DirtyNodes, StoreMod),
    {NodeEntities, NodeRelationships} = 
	extract_node_entities(NodeObjects),
    GroupNames = ordsets:from_list([Group || {'member_of', _, {'group', Group}} <- NodeRelationships]),
    {GroupEntities, GroupRelationships} =
	extract_group_entities(objects_from_names(Tree, ?GROUPPATH, GroupNames, StoreMod)),
    GroupParams = ordsets:from_list([Param || {'installs', _, {'parameter', Param}} <- GroupRelationships]),
    GroupFeatures = ordsets:from_list([F || {'installs', _, {'feature', F}} <- GroupRelationships]),
    {FeatureEntities, FeatureRelationships} =
	saturate_features(GroupFeatures, {[], [], []}, Tree, StoreMod),
    FeatureParams = ordsets:from_list([Param || {'installs', _, {'parameter', Param}} <- FeatureRelationships]),
    {ParamEntities, ParamRelationships} =
	extract_parameter_entities(ordsets:union(GroupParams, FeatureParams)),
    % XXX: Subsystems aren't used by validation yet, so they don't appear here
    Entities = lists:foldl(fun ordsets:union/2, [], [NodeEntities, GroupEntities, FeatureEntities, ParamEntities]),
    Relationships = lists:foldl(fun ordsets:union/2, [], [NodeRelationships, GroupRelationships, FeatureRelationships, ParamRelationships]),
    {Entities, Relationships}.

get_children(Path, Tree, StoreMod) when is_list(Path) ->
    {_, Root} = wallaroo_tree:get_path(Path, Tree, StoreMod),
    [Obj || {_, Obj} <- wallaroo_tree:children(Root, StoreMod)].

-spec extract_node_entities([wallaby_node:wnode()]) -> simple_graph().
extract_node_entities(NodeObjects) when is_list(NodeObjects) ->
    {Es,Rs} = lists:foldl(fun(Node, {Nodes, Relationships}) ->
				  Name = {'node', wallaby_node:name(Node)},
				  Memberships = [{'member_of', Name, {'group', G}} || G <- wallaby_node:all_memberships(Node)],
				  {[Name|Nodes], Memberships++Relationships}
			  end, {[], []}, NodeObjects),
    {ordsets:from_list(Es), ordsets:from_list(Rs)}.

extract_group_entities(GroupObjects) when is_list(GroupObjects) ->
    {Es,Rs} = lists:foldl(fun(Group, {Groups, Relationships}) ->
				  Name = {'group', wallaby_group:name(Group)},
				  InstalledFeatures = [{'installs', Name, {'feature', F}} || F <- wallaby_group:features(Group)],
				  InstalledParams = [{'installs', Name, {'parameter', P}} || {P, _} <- wallaby_group:parameters(Group)],
				  {[Name|Groups], InstalledFeatures++InstalledParams++Relationships}
			  end, {[], []}, GroupObjects),
    {ordsets:from_list(Es), ordsets:from_list(Rs)}.

extract_feature_entities(FeatureObjects) when is_list(FeatureObjects) ->
    {Es,Rs} = lists:foldl(fun(Feature, {Features, Relationships}) ->
				  Name = {'feature', wallaby_feature:name(Feature)},
				  Incs = [{'includes', Name, {feature, F}} || F <- wallaby_feature:includes(Feature)],
				  Deps = [{'depends_on', Name, {feature, F}} || F <- wallaby_feature:depends(Feature)],
				  Cnfs = [{'conflicts_with', Name, {feature, F}} || F <- wallaby_feature:depends(Feature)],
				  InstalledParams = [{'installs', Name, {'parameter', P}} || {P, _} <- wallaby_feature:parameters(Feature)],
				  {[Name|Features], Incs++Deps++Cnfs++InstalledParams++Relationships}
			  end, {[], []}, FeatureObjects),
    {ordsets:from_list(Es), ordsets:from_list(Rs)}.

extract_parameter_entities(ParameterObjects) when is_list(ParameterObjects) ->
    {Es,Rs} = lists:foldl(fun(Parameter, {Parameters, Relationships}) ->
				  Name = {'parameter', wallaby_parameter:name(Parameter)},
				  Deps = [{'depends_on', Name, {parameter, F}} || F <- wallaby_parameter:depends(Parameter)],
				  Cnfs = [{'conflicts_with', Name, {parameter, F}} || F <- wallaby_parameter:depends(Parameter)],
				  {[Name|Parameters], Deps++Cnfs++Relationships}
			  end, {[], []}, ParameterObjects),
    {ordsets:from_list(Es), ordsets:from_list(Rs)}.

extract_subsystem_entities(SubsystemObjects) when is_list(SubsystemObjects) ->
    {Es,Rs} = lists:foldl(fun(Subsystem, {Subsystems, Relationships}) ->
				  Name = {'subsystem', wallaby_subsystem:name(Subsystem)},
				  Prms = [{'is_interested_in', Name, {parameter, P}} || P <- wallaby_subsystem:parameters(Subsystem)],
				  {[Name|Subsystems], Prms++Relationships}
			  end, {[], []}, SubsystemObjects),
    {ordsets:from_list(Es), ordsets:from_list(Rs)}.

-spec sg_union(simple_graph(), simple_graph()) -> simple_graph().
sg_union({G1E, G1R}, {G2E, G2R}) ->
    Entities = ordsets:union(G1E, G2E),
    Relationships = ordsets:union(G1R, G2R),
    {Entities, Relationships}.

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
	    [digraph:add_edge(G, P1, P2, L) || {L='parameter', P1={parameter, _}, P2={parameter, _}} <- Relationships],
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

make_activate_validators(Tree, StoreMod) ->
    make_activate_validators(all, Tree, StoreMod).

make_activate_validators({E,R}, _Tree, _StoreMod) ->
    [validator_all_vertices_exist(E,R),
     validator_no_circular_includes(E,R),
     validator_no_immed_conflicts_with_transitive_includes_or_deps(E,R),
     validator_no_circular_feature_depends(E,R),
     validator_no_circular_parameter_depends(E,R)];
make_activate_validators([_|_]=DirtyNodes, Tree, StoreMod) ->
    make_activate_validators(extract_graph(Tree, StoreMod, DirtyNodes), ignored, ignored);
make_activate_validators(all, Tree, StoreMod) ->
    make_activate_validators(extract_graph(Tree, StoreMod), ignored, ignored).
    
     
