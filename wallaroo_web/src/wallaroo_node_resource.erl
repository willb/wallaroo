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

resource_exists(ReqData, [{show_all}]=Ctx) ->
    {true, ReqData, Ctx};
resource_exists(ReqData, Ctx) ->
    Commit = get_starting_commit(ReqData),
    case wrq:path_info(name, ReqData) of
	undefined ->
	    {false, ReqData, Ctx};
	[_|_]=Node ->
	    case Commit of 
		none ->
		    {false, ReqData, Ctx};
		_ ->
		    case wallaroo:get_node(Node, Commit) of
			none ->
			    {false, ReqData, Ctx};
			_ ->
			    {true, ReqData, [{Node, Commit}|Ctx]}
		    end
	    end
    end.

content_types_accepted(ReqData, Ctx) ->
    {[{"application/json", from_json}], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
    {[{"application/json", to_json}], ReqData, Ctx}.

finish_request(ReqData, Ctx) ->
    {true, ReqData, Ctx}.

to_json(ReqData, [{show_all}]=Ctx) ->
    Commit = get_starting_commit(ReqData),
    Payload = case Commit of 
		  none -> mochijson:encode({array, []});
		  _ -> mochijson:encode({array, wallaroo:list_nodes(Commit)})
	      end,
    {Payload, ReqData, Ctx};
to_json(ReqData, [{Node, Commit}|_]=Ctx) ->
    find_node(Commit, Node, ReqData, Ctx).

find_node(Commit, Node, ReqData, Ctx) ->
    case Commit of
	none ->
	    {{halt, 404}, ReqData, Ctx};
	_ ->
	    case wallaroo:get_node(Node, Commit) of 
		none ->
		    {{halt, 404}, ReqData, Ctx};
		Result ->
		    dump_node(Result, ReqData, Ctx)
	    end
    end.

dump_node({wallaby_node, Node}, ReqData, Ctx) ->
    FixedNode = {struct, orddict:store(memberships, {array, orddict:fetch(memberships, Node)}, Node)},
    {mochijson:encode(FixedNode), ReqData, Ctx}.

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
    Commit = get_starting_commit(ReqData),
    NodeName = orddict:fetch(name, Data),
    {wallaby_node, Defaults} = wallaby_node:new(NodeName, false),
    Dict = orddict:merge(fun(_,V,_) -> V end, Data, Defaults),
    NewCommit = case Commit of
		    none ->
			wallaroo:put_node(NodeName, {wallaby_node, Dict});
		    Whence ->
			wallaroo:put_node(NodeName, {wallaby_node, Dict}, Whence)
		end,
    error_logger:info_msg("NewCommit is ~p~n", [NewCommit]),
    <<CommitNum:160/big-unsigned-integer>> = NewCommit,
    NewCommitStr = lists:flatten(io_lib:format("~40.16.0b", [CommitNum])),
    NewLocation = io_lib:format("/nodes/~s?commit=~s", [NodeName, NewCommitStr]),
    error_logger:info_msg("NewLocation is ~p~n", [NewLocation]),
    Redir = wrq:do_redirect(true, wrq:set_resp_header("location", NewLocation, ReqData)),
    {true, Redir, Ctx}.

get_starting_commit(ReqData) ->
    Tag=wrq:get_qs_value("tag", "", ReqData),
    Commit=wrq:get_qs_value("commit", "", ReqData),
    case {Tag, Commit} of
    	{[], []} ->
	    none;
	{_, []} ->
	    case wallaroo:get_tag(Tag) of
		{wallaroo_tag, _}=TTerm ->
		    wallaroo_tag:get_commit(TTerm);
		find_failed ->
		    none
	    end;
	_ ->
	    Commit
    end.
	
hook({struct, [_|_]=UnhookedDict}) ->
    Dict = [{K,hook(V)} || {K,V} <- UnhookedDict],
    orddict:from_list(Dict);
hook({array, [_|_]=Ls}) ->
    [hook(Elt) || Elt <- Ls];
hook(X) -> X.
