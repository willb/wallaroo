%% @author William C. Benton <willb@redhat.com>
%% @copyright 2011 Red Hat, Inc. and William C. Benton
%% @doc Wallaroo node resource

-module(wallaroo_node_resource).
-export([init/1, to_json/2, resource_exists/2]). 
-export([from_json/2]).
-export([allowed_methods/2, content_types_provided/2, content_types_accepted/2, finish_request/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(Ctx) ->
    {{trace, "priv"}, Ctx}.

allowed_methods(ReqData, Ctx) ->
    {['HEAD', 'GET', 'POST', 'PUT'], ReqData, Ctx}.

resource_exists(ReqData, Ctx) ->
    wallaroo_web_common:generic_entity_exists(ReqData, Ctx, fun(Name, Commit) -> wallaroo:get_entity(Name, node, Commit) end).

content_types_accepted(ReqData, Ctx) ->
    {[{"application/json", from_json}], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
    {[{"application/json", to_json}], ReqData, Ctx}.

finish_request(ReqData, Ctx) ->
    {true, ReqData, Ctx}.

to_json(ReqData, Ctx) ->
    wallaroo_web_common:generic_to_json(ReqData, Ctx, fun(Commit) -> wallaroo:list_entities(node, Commit) end, fun(Name, Commit) -> wallaroo:get_entity(Name, node, Commit) end).

from_json(ReqData, Ctx) ->
    wallaroo_web_common:generic_from_json(ReqData, Ctx, fun(Nm) -> wallaby_node:new(Nm, true) end, node, "nodes", fun validate/2).

validate({wallaby_node, _}=Node, none) ->
    case wallaby_node:memberships(Node) of
	[] -> ok;
	Ls ->
	    {error, [{nonexistent_groups, Ls}]}
    end;
validate({wallaby_node, _}=Node, Commit) ->
    Groups = wallaby_node:memberships(Node),
    SpecialGroups = [Group || Group <- Groups, special_group(Group)],
    NonexistentGroups = [Group || Group <- Groups, wallaroo:get_entity(Group, group, Commit) =:= none, not special_group(Group)],
    case {node_validate,SpecialGroups, NonexistentGroups} of
	{node_validate,[],[]} ->
	    ok;
	{node_validate, [], Ls} ->
	    {error, [{nonexistent_groups, Ls}]};
	{node_validate, Ls, []} ->
	    {error, [{special_groups, Ls}]};
	{node_validate, Ls, Ls2} ->
	    {error, [{special_groups, Ls},{nonexistent_groups, Ls2}]}
    end.

special_group(<<"+++SKEL">>) ->
    false;
special_group(<<"+++", _/binary>>) ->
    true;
special_group(_) ->
    false.

