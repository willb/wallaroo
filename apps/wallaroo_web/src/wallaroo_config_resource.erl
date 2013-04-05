%% @author William C. Benton <willb@redhat.com>
%% @copyright 2013 Red Hat, Inc. and William C. Benton
%% @doc Wallaroo configuration resource

-module(wallaroo_config_resource).
-export([init/1, to_json/2, resource_exists/2]). 
-export([is_authorized/2, allowed_methods/2, content_types_provided/2, content_types_accepted/2, finish_request/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("wallaroo_web_auth.hrl").

init(Args) ->
    wallaroo_web_common:generic_init(Args).

allowed_methods(ReqData, Ctx) ->
    {['HEAD', 'GET'], ReqData, Ctx}.

is_authorized(ReqData, Ctx) ->
    ?STANDARD_AUTH(ReqData, Ctx).

resource_exists(ReqData, Ctx) ->
    case wallaroo_web_common:config_for(Ctx) of
	undefined ->
	    {false, Ctx};
	Kind ->
	    wallaroo_web_common:generic_entity_exists(ReqData, Ctx, 
						      fun(Name, Commit) -> 
							      case wallaby_config:has(Kind, Name, Commit) of 
								  false -> none;
								  {error, E} -> none;
								  X -> X
							      end
						      end)
    end.

content_types_accepted(ReqData, Ctx) ->
    {[{"application/json", from_json}], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
    {[{"application/json", to_json}], ReqData, Ctx}.

finish_request(ReqData, Ctx) ->
    {true, ReqData, Ctx}.

to_json(ReqData, Ctx) ->
    Kind = wallaroo_web_common:config_for(Ctx),
    wallaroo_web_common:generic_to_json(ReqData, Ctx, nonsense_function, 
					fun(Name, Commit) -> 
						RawConf = wallaby_config:for(Kind, Name, Commit),
						Config = case Kind of
							     node ->
								 orddict:store(<<"WALLABY_CONFIG_VERSION">>, list_to_binary(wallaroo_hash:stringize(Commit)), RawConf);
							     _ ->
								 RawConf
							 end,
						{value, {struct, Config}} end).
