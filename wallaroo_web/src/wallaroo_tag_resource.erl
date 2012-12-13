%% @author William C. Benton <willb@redhat.com>
%% @copyright 2011 Red Hat, Inc. and William C. Benton
%% @doc Wallaroo tag resource

-module(wallaroo_tag_resource).
-export([init/1, to_json/2, resource_exists/2]). 
-export([from_json/2]).
-export([process_post/2, process_put/2]).
-export([allowed_methods/2, content_types_provided/2, content_types_accepted/2, finish_request/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(Ctx) ->
    {{trace, "priv"}, Ctx}.

allowed_methods(ReqData, Ctx) ->
    {['HEAD', 'GET', 'POST', 'PUT'], ReqData, Ctx}.

resource_exists(ReqData, [{show_all}]=Ctx) ->
    {true, ReqData, Ctx};
resource_exists(ReqData, Ctx) ->
    TagName = wrq:path_info(name, ReqData),
    case wallaroo:get_tag(TagName) of
	find_failed ->
	    {false, ReqData, Ctx};
	Tag ->
	    {true, ReqData, [{Tag}|Ctx]}
    end.

content_types_accepted(ReqData, Ctx) ->
    {[{"application/json", from_json}], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
    {[{"application/json", to_json}], ReqData, Ctx}.

finish_request(ReqData, Ctx) ->
    {true, ReqData, Ctx}.

process_post(ReqData, Ctx) ->
    {true, ReqData, Ctx}.

process_put(ReqData, Ctx) ->
    {true, ReqData, Ctx}.

stringize_sha(<<CommitNum:160/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~40.16.0b", [CommitNum])).

tag_to_struct({N, T}) ->
    {struct, [{"name", N}, {"commit", stringize_sha(wallaroo_tag:get_commit(T))}]}.

to_json(ReqData, [{show_all}]=Ctx) ->
    Payload = mochijson:encode({array, [tag_to_struct({N, T}) || {N, T} <- wallaroo:list_tags()]}),
    {Payload, ReqData, Ctx};
to_json(ReqData, [{Tag}|_]=Ctx) ->
    Payload = mochijson:encode(tag_to_struct(Tag)),
    {Payload, ReqData, Ctx}.

from_json(ReqData, Ctx) ->
    Data = [{list_to_atom(K), V} || {K, V} <- (mochijson:decoder([{object_hook, fun hook/1}]))(wrq:req_body(ReqData))],
    case wrq:path_info(name, ReqData) of
        undefined ->
	    case orddict:find(name, Data) of
		error ->
		    {false, ReqData, Ctx};
		{ok, _} ->
		    from_json_helper(Data, ReqData, Ctx)
	    end;
        Name ->
	    NamedData = orddict:store(name, Name, Data),
	    from_json_helper(NamedData, ReqData, Ctx)
    end.

from_json_helper(Data, ReqData, Ctx) ->
    Name = orddict:fetch(name, Data),
    SHA = orddict:fetch(commit, Data),
    case {wtr_fjh, wallaroo:put_tag(Name, SHA)} of
	{wtr_fjh, {fail, Failure}} ->
	    ResponseBody = wrq:append_to_response(mochijson:binary_encode([Failure]), ReqData),
	    {{halt, 400}, ResponseBody, NewCtx};
	_ ->
	    {true, ReqData, Ctx}
    end.
	
hook({struct, [_|_]=UnhookedDict}) ->
    Dict = [{K,hook(V)} || {K,V} <- UnhookedDict],
    orddict:from_list(Dict);
hook({array, [_|_]=Ls}) ->
    [hook(Elt) || Elt <- Ls];
hook(X) -> X.
