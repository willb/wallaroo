%% @author William C. Benton <willb@redhat.com>
%% @copyright 2011 Red Hat, Inc. and William C. Benton
%% @doc Wallaroo feature resource

-module(wallaroo_feature_resource).
-export([init/1, to_json/2, resource_exists/2]). 
-export([from_json/2]).
-export([allowed_methods/2, content_types_provided/2, content_types_accepted/2, finish_request/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(Args) ->
    wallaroo_web_common:generic_init(Args).

allowed_methods(ReqData, Ctx) ->
    {['HEAD', 'GET', 'POST', 'PUT'], ReqData, Ctx}.

resource_exists(ReqData, Ctx) ->
    wallaroo_web_common:generic_entity_exists(ReqData, Ctx, fun(Name, Commit) -> wallaroo:get_entity(Name, feature, Commit) end).

content_types_accepted(ReqData, Ctx) ->
    {[{"application/json", from_json}], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
    {[{"application/json", to_json}], ReqData, Ctx}.

finish_request(ReqData, Ctx) ->
    {true, ReqData, Ctx}.

to_json(ReqData, Ctx) ->
    wallaroo_web_common:generic_to_json(ReqData, Ctx, fun(Commit) -> wallaroo:list_entities(feature, Commit) end, fun(Name, Commit) -> wallaroo:get_entity(Name, feature, Commit) end).

from_json(ReqData, Ctx) ->
    wallaroo_web_common:generic_from_json(ReqData, Ctx, fun(Nm) -> wallaby_feature:new(Nm) end, feature, "features", fun validate/2).


%%% XXX: this doesn't do proactive graph validation yet -- but it could (and should)
validate({wallaby_feature, _}=Feature, none) ->
    Includes = {nonexistent_includes, wallaby_feature:includes(Feature)},
    Depends = {nonexistent_depends, wallaby_feature:depends(Feature)},
    Conflicts = {nonexistent_conflicts, wallaby_feature:conflicts(Feature)},
    Parameters = {nonexistent_parameters, [P || {P, _} <- wallaby_feature:parameters(Feature)]},
    case [Fail || Fail={_, Ls} <- [Includes, Depends, Conflicts, Parameters], Ls =/= []] of
	[] -> ok;
	Ls ->
	    {error, Ls}
    end;
validate({wallaby_feature, _}=Feature, Commit) ->
    BadIncludes = {nonexistent_includes, [F || F <- wallaby_feature:includes(Feature), wallaroo:get_entity(F, feature, Commit) =:= none]},
    BadDepends = {nonexistent_depends, [F || F <- wallaby_feature:depends(Feature), wallaroo:get_entity(F, feature, Commit) =:= none]},
    BadConflicts = {nonexistent_conflicts, [F || F <- wallaby_feature:conflicts(Feature), wallaroo:get_entity(F, feature, Commit) =:= none]}, 
    BadParameters = {nonexistent_parameters, [P || {P, _} <- wallaby_feature:parameters(Feature), wallaroo:get_entity(P, parameter, Commit) =:= none]},
    case [Fail || Fail={_, Ls} <- [BadIncludes, BadDepends, BadConflicts, BadParameters], Ls =/= []] of
	[] -> ok;
	Ls ->
	    {error, Ls}
    end.
