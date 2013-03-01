%% @author William C. Benton <willb@redhat.com>
%% @copyright 2011 Red Hat, Inc. and William C. Benton
%% @doc Wallaroo group resource

-module(wallaroo_group_resource).
-export([init/1, to_json/2, resource_exists/2]). 
-export([from_json/2]).
-export([allowed_methods/2, content_types_provided/2, content_types_accepted/2, finish_request/2, delete_resource/2, delete_completed/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("dlog.hrl").

init(Args) ->
    wallaroo_web_common:generic_init(Args).

allowed_methods(ReqData, Ctx) ->
    {['HEAD', 'GET', 'POST', 'PUT', 'DELETE'], ReqData, Ctx}.

resource_exists(ReqData, Ctx) ->
    wallaroo_web_common:generic_entity_exists(ReqData, Ctx, fun(Name, Commit) -> 
								    ?D_VAL(wallaroo:get_entity(Name, group, Commit))
							    end).

content_types_accepted(ReqData, Ctx) ->
    {[{"application/json", from_json}], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
    {[{"application/json", to_json}], ReqData, Ctx}.

finish_request(ReqData, Ctx) ->
    {true, ReqData, Ctx}.

delete_resource(ReqData, Ctx) ->
    wallaroo_web_common:generic_delete_entity(ReqData, Ctx, group, "groups").

delete_completed(ReqData, Ctx) ->
    {true, ReqData, Ctx}.

to_json(ReqData, Ctx) ->
    wallaroo_web_common:generic_to_json(ReqData, Ctx, fun(Commit) -> wallaroo:list_entities(group, Commit) end, fun(Name, Commit) -> wallaroo:get_entity(Name, group, Commit) end).

from_json(ReqData, Ctx) ->
    wallaroo_web_common:generic_from_json(ReqData, Ctx, fun(Nm) -> wallaby_group:new(Nm) end, group, "groups", fun validate/2).

%%% XXX: this doesn't do proactive graph validation yet -- but it could (and should)
validate({wallaby_group, _}=Group, none) ->
    ?D_LOG("validating ~p for no commit~n", [Group]),
    Features = {nonexistent_features, wallaby_group:features(Group)},
    Parameters = {nonexistent_parameters, [P || {P, _} <- wallaby_group:parameters(Group)]},
    case [Fail || Fail={_, Ls} <- [Features, Parameters], Ls =/= []] of
	[] -> ok;
	Ls ->
	    {error, Ls}
    end;
validate({wallaby_group, _}=Group, Commit) ->
    ?D_LOG("validating ~p for commit~p~n", [Group, Commit]),
    BadFeatures = {nonexistent_features, {array, [F || F <- wallaby_group:features(Group), wallaroo:get_entity(F, feature, Commit) =:= none]}},
    BadParameters = {nonexistent_parameters, {array, [P || {P, _} <- wallaby_group:parameters(Group), wallaroo:get_entity(P, parameter, Commit) =:= none]}},
    case [Fail || Fail={_, Ls} <- [BadFeatures, BadParameters], Ls =/= {array, []}] of
	[] -> ok;
	Ls ->
	    {error, {struct, Ls}}
    end.
