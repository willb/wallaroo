%% @author William C. Benton <willb@redhat.com>
%% @copyright 2012 Red Hat, Inc. and William C. Benton
%% @doc Wallaby-specific validators for Wallaroo

-module(wallaby_validators).

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

-spec extract_graph(wallaroo_tree:tree(), module()) -> simple_graph().
extract_graph(Tree, StoreMod) ->
    {NodeEntities, NodeRelationships} = extract_node_entities(get_children(["nodes"], Tree, StoreMod)),
    {GroupEntities, GroupRelationships} = extract_group_entities(get_children(["groups"], Tree, StoreMod)),
    {FeatureEntities, FeatureRelationships} = extract_feature_entities(get_children(["features"], Tree, StoreMod)),
    {ParamEntities, ParamRelationships} = extract_parameter_entities(get_children(["params"], Tree, StoreMod)),
    GroupRoot = wallaroo_tree:get_path(["nodes"], Tree, StoreMod),

    GroupObjects = wallaroo_tree:get_path(["nodes"], Tree, StoreMod),
    {[], []}.

get_children([_|_]=Path, Tree, StoreMod) ->
    % XXX:  this, like all raw tree accesses, should be factored out to an internal API
    Root = wallaroo_tree:get_path(Path, Tree, StoreMod),
    [Obj || {_, Obj} <- wallaroo_tree:children(Root)].

extract_node_entities([_|_]=NodeObjects) ->
    {Es,Rs} = lists:foldl(fun(Node, {[_|_]=Nodes, [_|_]=Relationships}) ->
				  Name = {'node', wallaby_node:name(Node)},
				  Memberships = [{'member_of', Name, Grp} || Grp <- wallaby_node:memberships(Node)],
				  {Name|Nodes, Memberships++Relationships}
			  end, {[], []}, NodeObjects),
    {ordsets:from_list(Es), ordsets:from_list(Rs)}.

-spec sg_union(simple_graph(), simple_graph()) -> simple_graph().
sg_union({G1E, G1R}, {G2E, G2R}) ->
    Entities = ordsets:union(G1E, G2E),
    Relationships = ordsets:union(G1R, G2R),
    {Entities, Relationships}.
