%% @author William C. Benton <willb@redhat.com>
%% @copyright 2011 Red Hat, Inc. and William C. Benton
%% @doc Wallaroo tag resource

-module(wallaroo_tag_resource).
-export([init/1, to_json/2, resource_exists/2]). 
-export([from_json/2]).
-export([allowed_methods/2, content_types_provided/2, content_types_accepted/2, finish_request/2, delete_resource/2, delete_completed/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(Args) ->
    wallaroo_web_common:generic_init(Args).

allowed_methods(ReqData, Ctx) ->
    {['HEAD', 'GET', 'POST', 'PUT', 'DELETE'], ReqData, Ctx}.

resource_exists(ReqData, Ctx) ->
    wallaroo_web_common:generic_entity_exists_nc(ReqData, Ctx, fun(Name) -> wallaroo:get_tag(Name) end, tag).

content_types_accepted(ReqData, Ctx) ->
    {[{"application/json", from_json}], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
    {[{"application/json", to_json}], ReqData, Ctx}.

finish_request(ReqData, Ctx) ->
    {true, ReqData, Ctx}.

to_json(ReqData, Ctx) ->
    wallaroo_web_common:generic_to_json(ReqData, Ctx, fun(_) -> [Name || {Name, _} <- wallaroo:list_tags()] end, fun(Name) -> wallaroo:get_tag(Name) end, false).
			      
from_json(ReqData, Ctx) ->
    wallaroo_web_common:generic_from_json(ReqData, Ctx, fun(_) -> ok end, tag, "tags").

delete_resource(ReqData, Ctx) ->
    wallaroo_web_common:generic_delete_nc(ReqData, Ctx, fun wallaroo:delete_tag/1, "tags").

delete_completed(ReqData, Ctx) ->
    {true, ReqData, Ctx}.
