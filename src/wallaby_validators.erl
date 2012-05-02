%% @author William C. Benton <willb@redhat.com>
%% @copyright 2012 Red Hat, Inc. and William C. Benton
%% @doc Wallaby-specific validators for Wallaroo

-module(wallaby_validators).
-export([extract_graph/2,sg_union/2]).

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

-type simple_graph() :: {[entity()], [relationship()]}.
-type sg_validity() :: 'ok' | [{error, any()}].

-spec extract_graph(wallaroo_tree:tree(), module()) -> simple_graph().
extract_graph(Tree, StoreMod) ->
    % XXX:  these, like all raw tree accesses, should be factored out to an internal API
    {NodeEntities, NodeRelationships} = extract_node_entities(get_children(["nodes"], Tree, StoreMod)),
    {GroupEntities, GroupRelationships} = extract_group_entities(get_children(["groups"], Tree, StoreMod)),
    {FeatureEntities, FeatureRelationships} = extract_feature_entities(get_children(["features"], Tree, StoreMod)),
    {ParamEntities, ParamRelationships} = extract_parameter_entities(get_children(["parameters"], Tree, StoreMod)),
    {SubsysEntities, SubsysRelationships} = extract_subsystem_entities(get_children(["subsystems"], Tree, StoreMod)),
    Entities = lists:foldl(fun ordsets:union/2, [], [NodeEntities, GroupEntities, FeatureEntities, ParamEntities, SubsysEntities]),
    Relationships = lists:foldl(fun ordsets:union/2, [], [NodeRelationships, GroupRelationships, FeatureRelationships, ParamRelationships, SubsysRelationships]),
    {Entities, Relationships}.

get_children([_|_]=Path, Tree, StoreMod) ->
    Root = wallaroo_tree:get_path(Path, Tree, StoreMod),
    [Obj || {_, Obj} <- wallaroo_tree:children(Root)].

extract_node_entities([_|_]=NodeObjects) ->
    {Es,Rs} = lists:foldl(fun(Node, {[_|_]=Nodes, [_|_]=Relationships}) ->
				  Name = {'node', wallaby_node:name(Node)},
				  Memberships = [{'member_of', Name, {'group', G}} || G <- wallaby_node:memberships(Node)],
				  {[Name|Nodes], Memberships++Relationships}
			  end, {[], []}, NodeObjects),
    {ordsets:from_list(Es), ordsets:from_list(Rs)}.

extract_group_entities([_|_]=GroupObjects) ->
    {Es,Rs} = lists:foldl(fun(Group, {[_|_]=Groups, [_|_]=Relationships}) ->
				  Name = {'group', wallaby_group:name(Group)},
				  InstalledFeatures = [{'installs', Name, {'feature', F}} || F <- wallaby_group:features(Group)],
				  InstalledParams = [{'installs', Name, {'parameter', P}} || {P, _} <- wallaby_group:parameters(Group)],
				  {[Name|Groups], InstalledFeatures++InstalledParams++Relationships}
			  end, {[], []}, GroupObjects),
    {ordsets:from_list(Es), ordsets:from_list(Rs)}.

extract_feature_entities([_|_]=FeatureObjects) ->
    {Es,Rs} = lists:foldl(fun(Feature, {[_|_]=Features, [_|_]=Relationships}) ->
				  Name = {'feature', wallaby_feature:name(Feature)},
				  Incs = [{'installs', Name, {feature, F}} || F <- wallaby_feature:includes(Feature)],
				  Deps = [{'depends_on', Name, {feature, F}} || F <- wallaby_feature:depends(Feature)],
				  Cnfs = [{'conflicts_with', Name, {feature, F}} || F <- wallaby_feature:depends(Feature)],
				  InstalledParams = [{'installs', Name, {'parameter', P}} || {P, _} <- wallaby_feature:parameters(Feature)],
				  {[Name|Features], Incs++Deps++Cnfs++InstalledParams++Relationships}
			  end, {[], []}, FeatureObjects),
    {ordsets:from_list(Es), ordsets:from_list(Rs)}.

extract_parameter_entities([_|_]=ParameterObjects) ->
    {Es,Rs} = lists:foldl(fun(Parameter, {[_|_]=Parameters, [_|_]=Relationships}) ->
				  Name = {'parameter', wallaby_parameter:name(Parameter)},
				  Deps = [{'depends_on', Name, {parameter, F}} || F <- wallaby_parameter:depends(Parameter)],
				  Cnfs = [{'conflicts_with', Name, {parameter, F}} || F <- wallaby_parameter:depends(Parameter)],
				  {[Name|Parameters], Deps++Cnfs++Relationships}
			  end, {[], []}, ParameterObjects),
    {ordsets:from_list(Es), ordsets:from_list(Rs)}.

extract_subsystem_entities([_|_]=SubsystemObjects) ->
    {Es,Rs} = lists:foldl(fun(Subsystem, {[_|_]=Subsystems, [_|_]=Relationships}) ->
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

-spec sg_validate_mentioned_nodes(simple_graph()) -> sg_validity().
sg_validate_mentioned_nodes({Ge,[_|_]=Gr}) ->
    MentionedNodes = lists:foldl(fun({_,X,Y}, Entities) ->
					    ordsets:add_element(X, ordsets:add_element(Y, Entities))
				    end, ordsets:new(), Gr),
    case ordsets:subtract(MentionedNodes, Ge) of
	[] ->
	    ok;
	[_|_]=MissingNodes -> 
	    [{error, {missing_node, Node}} || Node <- MissingNodes]
    end.
			       
