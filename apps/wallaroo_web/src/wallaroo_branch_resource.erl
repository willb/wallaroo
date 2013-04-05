%% @author William C. Benton <willb@redhat.com>
%% @copyright 2012 Red Hat, Inc. and William C. Benton
%% @doc Wallaroo branch resource

-module(wallaroo_branch_resource).
-export([init/1, to_json/2, resource_exists/2]). 
-export([from_json/2]).
-export([is_authorized/2, allowed_methods/2, content_types_provided/2, content_types_accepted/2, finish_request/2, delete_resource/2, delete_completed/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("wallaroo_web_auth.hrl").

init(Args) ->
    wallaroo_web_common:generic_init(Args).

allowed_methods(ReqData, Ctx) ->
    {['HEAD', 'GET', 'POST', 'PUT'], ReqData, Ctx}.

is_authorized(ReqData, Ctx) ->
    ?STANDARD_AUTH(ReqData, Ctx).

resource_exists(ReqData, Ctx) ->
    wallaroo_web_common:generic_entity_exists_nc(ReqData, Ctx, fun(Name) -> wallaroo:get_branch(Name) end, branch).

content_types_accepted(ReqData, Ctx) ->
    {[{"application/json", from_json}], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
    {[{"application/json", to_json}], ReqData, Ctx}.

finish_request(ReqData, Ctx) ->
    {true, ReqData, Ctx}.

to_json(ReqData, Ctx) ->
    wallaroo_web_common:generic_to_json(ReqData, Ctx, fun(_) -> [Name || {Name, _} <- wallaroo:list_branches()] end, fun(Name) -> wallaroo:get_branch(Name) end, false).
			      
from_json(ReqData, Ctx) ->
    wallaroo_web_common:generic_from_json(ReqData, Ctx, fun(_) -> ok end, branch, "branches").

delete_resource(ReqData, Ctx) ->
    wallaroo_web_common:generic_delete_nc(ReqData, Ctx, fun wallaroo:delete_branch/1, "branches").

delete_completed(ReqData, Ctx) ->
    {true, ReqData, Ctx}.
