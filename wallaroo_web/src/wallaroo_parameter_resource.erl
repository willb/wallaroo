%% @author William C. Benton <willb@redhat.com>
%% @copyright 2011 Red Hat, Inc. and William C. Benton
%% @doc Wallaroo parameter resource

-module(wallaroo_parameter_resource).
-export([init/1, to_json/2, resource_exists/2]). 
-export([from_json/2]).
-export([allowed_methods/2, content_types_provided/2, content_types_accepted/2, finish_request/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(Args) ->
    wallaroo_web_common:generic_init(Args).

allowed_methods(ReqData, Ctx) ->
    {['HEAD', 'GET', 'POST', 'PUT'], ReqData, Ctx}.

resource_exists(ReqData, Ctx) ->
    wallaroo_web_common:generic_entity_exists(ReqData, Ctx, fun(Name, Commit) -> wallaroo:get_entity(Name, parameter, Commit) end).

content_types_accepted(ReqData, Ctx) ->
    {[{"application/json", from_json}], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
    {[{"application/json", to_json}], ReqData, Ctx}.

finish_request(ReqData, Ctx) ->
    {true, ReqData, Ctx}.

to_json(ReqData, Ctx) ->
    wallaroo_web_common:generic_to_json(ReqData, Ctx, fun(Commit) -> wallaroo:list_entities(parameter, Commit) end, fun(Name, Commit) -> wallaroo:get_entity(Name, parameter, Commit) end).

from_json(ReqData, Ctx) ->
    wallaroo_web_common:generic_from_json(ReqData, Ctx, fun(Nm) -> wallaby_parameter:new(Nm) end, parameter, "parameters", fun validate/2).


%%% XXX: this doesn't do proactive graph validation yet -- but it could (and should)
validate({wallaby_parameter, _}=Parameter, none) ->
    Depends = {nonexistent_depends, {array, wallaby_parameter:depends(Parameter)}},
    Conflicts = {nonexistent_conflicts, {array, wallaby_parameter:conflicts(Parameter)}},
    case [Fail || Fail={_, Ls} <- [Depends, Conflicts], Ls =/= {array, []}] of
	[] -> ok;
	Ls ->
	    error_logger:warning_msg("param validation failed; errors are ~p~n", [Ls]),
	    {error, Ls}
    end;
validate({wallaby_parameter, _}=Parameter, Commit) ->
    BadDepends = {nonexistent_depends, {array, [F || F <- wallaby_parameter:depends(Parameter), wallaroo:get_entity(F, parameter, Commit) =:= none]}},
    BadConflicts = {nonexistent_conflicts, {array, [F || F <- wallaby_parameter:conflicts(Parameter), wallaroo:get_entity(F, parameter, Commit) =:= none]}}, 
    case [Fail || Fail={_, Ls} <- [BadDepends, BadConflicts], Ls =/= {array, []}] of
	[] -> ok;
	Ls ->
	    error_logger:warning_msg("param validation failed for ~p at ~p; errors are ~p~n", [Parameter, Commit, Ls]),
	    {error, {struct, Ls}}
    end.
