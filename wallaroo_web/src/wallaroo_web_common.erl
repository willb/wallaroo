-module(wallaroo_web_common).
-export([generic_entity_exists/3, get_starting_commit/1, generic_find/5, generic_find/6, dump_json/3, generic_to_json/4, generic_from_json/5, generic_from_json/6]).

generic_entity_exists(ReqData,  [{show_all}]=Ctx, _) ->
    {true, ReqData, Ctx};
generic_entity_exists(ReqData, Ctx, LookupFun) ->
    Commit = get_starting_commit(ReqData),
    case wrq:path_info(name, ReqData) of
	undefined ->
	    {false, ReqData, Ctx};
	[_|_]=Node ->
	    case Commit of 
		none ->
		    {false, ReqData, Ctx};
		_ ->
		    case LookupFun(Node, Commit) of
			none ->
			    {false, ReqData, Ctx};
			_ ->
			    {true, ReqData, [{Node, Commit}|Ctx]}
		    end
	    end
    end.
generic_find(Commit, FindFunc, Name, ReqData, Ctx) ->
    generic_find(Commit, FindFunc, fun dump_json/3, Name, ReqData, Ctx).

generic_find(Commit, FindFunc, DumpFunc, Name, ReqData, Ctx) ->
    case Commit of
	none ->
	    {{halt, 404}, ReqData, Ctx};
	_ ->
	    case FindFunc(Name, Commit) of 
		none ->
		    {{halt, 404}, ReqData, Ctx};
		Result ->
		    DumpFunc(Result, ReqData, Ctx)
	    end
    end.

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

generic_to_json(ReqData, [{show_all}]=Ctx, ListFun, _GetFun) ->
    Commit = wallaroo_web_common:get_starting_commit(ReqData),
    {Payload, _, _} = case Commit of 
		  none -> dump_json([], ReqData, Ctx);
		  _ -> dump_json(ListFun(Commit), ReqData, Ctx)
	      end,
    {Payload, ReqData, Ctx};
generic_to_json(ReqData, [{Name, Commit}]=Ctx, _ListFun, GetFun) ->
    wallaroo_web_common:generic_find(Commit, GetFun, Name, ReqData, Ctx).


dump_json(Entities, ReqData, Ctx) when is_list(Entities) ->
    error_logger:warning_msg("Entities are ~p~n", [Entities]),
    {mochijson:binary_encode({array, [fix_json(Entity) || Entity <- Entities]}), ReqData, Ctx};
dump_json({_Kind, _Dict}=Entity, ReqData, Ctx) ->
    error_logger:warning_msg("Entity is ~p~n", [Entity]),
    {mochijson:binary_encode(fix_json(Entity)), ReqData, Ctx}.

jsonify_entry({LVK, Ls}) when LVK =:= memberships; LVK =:= features; LVK =:= features;  LVK =:= includes;  LVK =:= depends;  LVK =:= conflicts; LVK =:= ss_params ->
    {LVK, {array, Ls}};
jsonify_entry({DVK, Ls}) when DVK =:= parameters ->
    {DVK, {struct, Ls}};
jsonify_entry(X) ->
    X.

-spec fix_json({atom(), orddict:orddict()}) -> {struct, orddict:orddict()}.
fix_json({_Kind, EntityDict}) ->
    {struct, [jsonify_entry(Entry) || Entry <- EntityDict]}.

generic_from_json(ReqData, Ctx, NewFunc, PutKind, PathPart) ->
    generic_from_json(ReqData, Ctx, NewFunc, PutKind, PathPart, fun(_,_) -> ok end).

generic_from_json(ReqData, Ctx, NewFunc, PutKind, PathPart, ValidFunc) ->
    error_logger:warning_msg("Ctx is is ~p~n", [Ctx]),
    Body = wrq:req_body(ReqData),
    error_logger:warning_msg("Body is is ~p~n", [Body]),
    Data = [{list_to_atom(binary_to_list(K)), V} || {K, V} <- (mochijson:binary_decoder([{object_hook, fun peel/1}]))(wrq:req_body(ReqData))],
    error_logger:warning_msg("Data is is ~p~n", [Data]),
    case wrq:path_info(name, ReqData) of
        undefined ->
	    case orddict:find(name, Data) of
		error ->
		    {false, ReqData, Ctx};
		{ok, _} ->
		    from_json_helper(Data, ReqData, Ctx, NewFunc, PutKind, PathPart, ValidFunc)
	    end;
        Name ->
	    NamedData = orddict:store(name, Name, Data),
	    from_json_helper(NamedData, ReqData, Ctx, NewFunc, PutKind, PathPart, ValidFunc)
    end.

from_json_helper(Data, ReqData, Ctx, NewFunc, PutKind, PathPart, ValidFunc) ->
    Commit = wallaroo_web_common:get_starting_commit(ReqData),
    Name = orddict:fetch(name, Data),
    {Kind, Defaults} = NewFunc(Name),
    Dict = orddict:merge(fun(_,V,_) -> V end, Data, Defaults),
    case ValidFunc(Entity={Kind, Dict}, Commit) of
	ok ->
	    NewCommit = case Commit of
			    none ->
				wallaroo:put_entity(Name, PutKind, Entity);
			    Whence ->
				wallaroo:put_entity(Name, PutKind, Entity, Whence)
			end,
	    error_logger:info_msg("NewCommit is ~p~n", [NewCommit]),
	    <<CommitNum:160/big-unsigned-integer>> = NewCommit,
	    NewCommitStr = lists:flatten(io_lib:format("~40.16.0b", [CommitNum])),
	    NewLocation = io_lib:format("/~s/~s?commit=~s", [PathPart, Name, NewCommitStr]),
	    error_logger:info_msg("NewLocation is ~p~n", [NewLocation]),
	    Redir = wrq:do_redirect(true, wrq:set_resp_header("Location", NewLocation, ReqData)),
	    {true, Redir, Ctx};
	{error, Why} ->
	    ResponseBody = wrq:append_to_response_body(mochijson:binary_encode(Why), ReqData),
	    {{halt, 400}, ResponseBody, Ctx}
    end.

peel({struct, [_|_]=UnpeeledDict}) ->
    Dict = [{K,peel(V)} || {K,V} <- UnpeeledDict],
    orddict:from_list(Dict);
peel({array, [_|_]=Ls}) ->
    [peel(Elt) || Elt <- Ls];
peel(X) -> X.
