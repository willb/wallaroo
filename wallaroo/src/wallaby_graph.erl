%% @author William C. Benton <willb@redhat.com>
%% @copyright 2012 Red Hat, Inc. and William C. Benton
%% @doc Wallaby entity graph

-module(wallaby_graph).
-export([extract_graph/2,extract_graph/3,sg_union/2,get_children/3]).

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
			two_kinded_relationship('param_value', group_entity(), {parameter_entity(), binary()}) |
			two_kinded_relationship('param_value', feature_entity(), {parameter_entity(), binary()}) |
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

-export_type([simple_graph/0]).

-spec extract_graph(wallaroo_tree:tree(), module()) -> simple_graph().
extract_graph(Tree, StoreMod) ->
    % XXX:  these, like all raw tree accesses, should be factored out to an internal API
    {NodeEntities, NodeRelationships} = extract_node_entities(get_children([?NODEPATH], Tree, StoreMod)),
    {GroupEntities, GroupRelationships} = extract_group_entities(get_children([?GROUPPATH], Tree, StoreMod)),
    {FeatureEntities, FeatureRelationships} = extract_feature_entities(get_children([?FEATUREPATH], Tree, StoreMod)),
    {ParamEntities, ParamRelationships} = extract_parameter_entities(get_children([?PARAMETERPATH], Tree, StoreMod)),
    {SubsysEntities, SubsysRelationships} = extract_subsystem_entities(get_children([?SUBSYSTEMPATH], Tree, StoreMod)),
    Entities = lists:foldl(fun ordsets:union/2, [], [NodeEntities, GroupEntities, FeatureEntities, ParamEntities, SubsysEntities, [{'group', <<"+++DEFAULT">>}, {'group', <<"+++SKEL">>}]]),
    Relationships = lists:foldl(fun ordsets:union/2, [], [NodeRelationships, GroupRelationships, FeatureRelationships, ParamRelationships, SubsysRelationships]),
    {Entities, Relationships}.

objects_from_names(Tree, Kind, Names, StoreMod) ->
    [Obj || 
	{value, Obj} <- [wallaroo_tree:get_path([Kind, Name], Tree, StoreMod) ||
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
    GroupParams = ordsets:from_list([Param || {'sets_param', _, {'parameter', Param}} <- GroupRelationships]),
    GroupFeatures = ordsets:from_list([F || {'installs', _, {'feature', F}} <- GroupRelationships]),
    {FeatureEntities, FeatureRelationships} =
	saturate_features(GroupFeatures, {[], [], []}, Tree, StoreMod),
    FeatureParams = ordsets:from_list([Param || {'sets_param', _, {'parameter', Param}} <- FeatureRelationships]),
    {ParamEntities, ParamRelationships} =
	extract_parameter_entities(ordsets:union(GroupParams, FeatureParams)),
    % XXX: Subsystems aren't used by validation yet, so they don't appear here
    Entities = lists:foldl(fun ordsets:union/2, [], [NodeEntities, GroupEntities, FeatureEntities, ParamEntities, [{'group', <<"+++DEFAULT">>}, {'group', <<"+++SKEL">>}]]),
    Relationships = lists:foldl(fun ordsets:union/2, [], [NodeRelationships, GroupRelationships, FeatureRelationships, ParamRelationships]),
    {Entities, Relationships}.

get_children(Path, Tree, StoreMod) when is_list(Path) ->
    Root = wallaroo_tree:get_path(Path, Tree, StoreMod),
    % error_logger:warning_msg("wallaby_validators:get_children:  Path=~p, Tree=~p, Root=~p~n", [Path, Tree, Root]),
    case Root of
	none ->
	    [];
	{value, ST} ->
	    [Obj || {_, Obj} <- wallaroo_tree:children(ST, StoreMod)]
    end.

-spec extract_node_entities([wallaby_node:wnode()]) -> simple_graph().
extract_node_entities(NodeObjects) when is_list(NodeObjects) ->
    {Es,Rs} = lists:foldl(fun(Node, {Nodes, Relationships}) ->
				  Name = {'node', wallaby_node:name(Node)},
				  Memberships = [{'member_of', Name, {'group', G}} || G <- wallaby_node:all_memberships(Node)],
				  {[Name|[{'group', wallaby_node:identity_group(Node)}|Nodes]], Memberships++Relationships}
			  end, {[], []}, NodeObjects),
    {ordsets:from_list(Es), ordsets:from_list(Rs)}.

extract_group_entities(GroupObjects) when is_list(GroupObjects) ->
    lists:foldl(fun(Group, {Entities, Relationships}) ->
			Name = {'group', wallaby_group:name(Group)},
			InstalledFeatures = ordsets:from_list([{'installs', Name, {'feature', F}} || F <- wallaby_group:features(Group)]),
			PVPairs = ordsets:from_list([{{'parameter', P}, V} || {P, V} <- wallaby_group:parameters(Group)]),
			ParamValues = ordsets:from_list([{'param_value', Name, PV} || PV <- PVPairs]),
			InstalledParams = ordsets:from_list([{'sets_param', Name, P} || {P, _} <- PVPairs]),
			{ordsets:union([[Name],PVPairs, Entities]), ordsets:union([InstalledFeatures,InstalledParams,ParamValues,Relationships])}
			 end, {[], []}, GroupObjects).

extract_feature_entities(FeatureObjects) when is_list(FeatureObjects) ->
    lists:foldl(fun(Feature, {Entities, Relationships}) ->
			Name = {'feature', wallaby_feature:name(Feature)},
			Incs = ordsets:from_list([{'includes', Name, {feature, F}} || F <- wallaby_feature:includes(Feature)]),
			Deps = ordsets:from_list([{'depends_on', Name, {feature, F}} || F <- wallaby_feature:depends(Feature)]),
			Cnfs = ordsets:from_list([{'conflicts_with', Name, {feature, F}} || F <- wallaby_feature:depends(Feature)]),
			PVPairs = ordsets:from_list([{{'parameter', P}, V} || {P, V} <- wallaby_feature:parameters(Feature)]),
			ParamValues = ordsets:from_list([{'param_value', Name, PV} || PV <- PVPairs]),
			InstalledParams = [{'sets_param', Name, P} || {P, _} <- PVPairs],
			{ordsets:union([[Name], PVPairs, Entities]), ordsets:union([Incs,Deps,Cnfs,InstalledParams,ParamValues,Relationships])}
			 end, {[], []}, FeatureObjects).

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
