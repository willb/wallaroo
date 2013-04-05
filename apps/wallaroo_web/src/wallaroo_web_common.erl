-module(wallaroo_web_common).
-export([generic_init/1, generic_entity_exists/3, generic_entity_exists_nc/4, get_starting_commit/2, generic_find/5, generic_find/6,  generic_find_nc/4, generic_find_nc/5, dump_json/3, generic_to_json/4, generic_to_json/5, generic_from_json/5,generic_from_json_raw/5, generic_from_json/6, config_for/1, meta_for/1, fixup_meta_ctx/2, generic_delete_entity/4, generic_delete_nc/4, generic_auth/4, generic_auth/3, auth_user/1]).
-export([known_meta_atoms/0]).

-record(ww_ctx, {show_all=false, name, commit, branch, via, head, config_for, meta_domain, meta_key=all, auth_user=none}).
% -define(DO_TRACE, {trace, "priv"}).
-define(DO_TRACE, ok).

% -define(debug, true).
-include("dlog.hrl").

config_for(#ww_ctx{config_for=Kind}) ->
    Kind.

meta_for(#ww_ctx{meta_domain=Domain, meta_key=Key}) ->
    {Domain, Key}.

auth_user(#ww_ctx{auth_user=X}) when is_atom(X) ->
    <<>>;
auth_user(#ww_ctx{auth_user=S}) when is_binary(S) ->
    S.

meta_name(#ww_ctx{meta_domain=Domain, meta_key=Key}) ->
    iolist_to_binary(io_lib:format("~p/~p", [Domain, Key])).

%% returns a list of atoms we will recognize in tag and branch metadata
known_meta_atoms() ->
    [validated].

generic_init([{meta_domain, Domain}, {meta_key, Key}]) ->
    {?DO_TRACE, #ww_ctx{meta_domain=Domain, meta_key=Key}};
generic_init([{config_for, Kind}]) ->
    {?DO_TRACE, #ww_ctx{config_for=Kind}};
generic_init([{show_all}]) ->
    {?DO_TRACE, #ww_ctx{show_all=true}};
generic_init(_) ->
    {?DO_TRACE, #ww_ctx{}}.

generic_entity_exists(ReqData,  #ww_ctx{show_all=true}=Ctx, _) ->
    {true, ReqData, Ctx};
generic_entity_exists(ReqData, Ctx, LookupFun) ->
    {Commit, NewCtx} = get_starting_commit(ReqData, Ctx),
    case wrq:path_info(name, ReqData) of
	undefined ->
	    {false, ReqData, NewCtx};
	[_|_]=Name ->
	    BName = list_to_binary(mochiweb_util:unquote(Name)),
	    case Commit of 
		none ->
		    {false, ReqData, NewCtx};
		_ ->
		    case ?D_VAL(LookupFun(BName, Commit)) of
			Fail when Fail =:= find_failed orelse Fail =:= none ->
			    {false, ReqData, NewCtx};
			_ ->
			    {true, ReqData, NewCtx#ww_ctx{name=BName,commit=Commit}}
		    end
	    end
    end.

fixup_meta_ctx(ReqData, #ww_ctx{meta_domain=domain}=Ctx) ->
    case wrq:path_info(domain, ReqData) of
	undefined ->
	    Ctx;
	[_|_]=LDom ->
	    BDom = list_to_binary(mochiweb_util:unquote(LDom)),
	    BKey = case wrq:path_info(key, ReqData) of
		       undefined -> all;
		       [_|_]=LKey ->
			   list_to_binary(mochiweb_util:unquote(LKey))
		   end,
	    Ctx#ww_ctx{meta_domain=BDom,meta_key=BKey}
    end.

generic_entity_exists_nc(ReqData,  #ww_ctx{show_all=true}=Ctx, _, _) ->
    {true, ReqData, Ctx};
generic_entity_exists_nc(ReqData, #ww_ctx{meta_domain=D, meta_key=K}=Ctx, LookupFun, What=meta)
  when is_binary(D), is_binary(K) orelse K =:= all ->
    case LookupFun(ignored) of
	Fail when Fail =:= find_failed orelse Fail =:= none ->
	    {false, ReqData, Ctx};
	Head ->
	    {true, ReqData, Ctx#ww_ctx{head={What, Head}, name=meta_name(Ctx)}}
    end;
generic_entity_exists_nc(ReqData, Ctx, LookupFun, What) ->
    EntityName = ensure_str_format(mochiweb_util:unquote(wrq:path_info(name, ReqData)), binary),
    case LookupFun(EntityName) of
	Fail when Fail =:= find_failed orelse Fail =:= none ->
	    {false, ReqData, Ctx};
	Head ->
	    {true, ReqData, Ctx#ww_ctx{head={What, Head}, name=EntityName}}
    end.

extract_creds(B64Creds) ->
    [U|P] = binary:split(base64:mime_decode(B64Creds), <<":">>),
    {U, ensure_str_format(P, binary)}.

extract_basic_header(ReqData) ->
    case wrq:get_req_header("authorization", ReqData) of
	"Basic "++B64 ->
	    extract_creds(B64);
	_ ->
	    none
    end.

extract_secret_header(ReqData) ->
    case wrq:get_req_header("x-wallaroo-secret", ReqData) of
	Str when is_list(Str) ->
	    ensure_str_format(Str, binary);
	_ ->
	    none
    end.

authorized(Secret, Creds, Role) ->
    authorized(Secret, Creds, Role, none).

authorized(Secret, _, Role, _) when is_binary(Secret) ->
    ?D_VAL(authorized_1),
    ?D_VAL({secret, wallaroo_auth:authorized(Secret, Role)});
authorized(_, {User, Pass}, Role, none) ->
    ?D_VAL({authorized_2, {User, Pass}}),
    ?D_VAL(authorized(none, {User, Pass}, Role, User));
authorized(_, {User, Pass}, Role, User) ->
    ?D_VAL(authorized_3),
    ?D_VAL({User, wallaroo_auth:authorized(User, Pass, Role)});
authorized(Anything, none, Role, none) ->
    ?D_VAL(authorized(Anything, none, Role, <<>>));
authorized(_, none, Role, <<>>) ->
    ?D_VAL(authorized_4),
    ?D_VAL({none, wallaroo_auth:authorized(<<>>, <<>>, Role)});
authorized(_, _, _, _) ->
    ?D_VAL(authorized_5),
    {none, false}.

auth_result({How, true}, ReqData, Ctx) ->
    {true, ReqData, Ctx#ww_ctx{auth_user=How}};
auth_result({_, false}, ReqData, Ctx) ->
    {"Basic realm=wallaroo", ReqData, Ctx}.

generic_auth(ReqData, Ctx, VerbRoles, DefaultRole) when is_list(VerbRoles) ->
    ?D_VAL(generic_auth_4_clause_1),
    Verb = wrq:method(ReqData),
    Role = ?D_VAL(orddict_default_fetch(Verb, VerbRoles, DefaultRole)),
    generic_auth(ReqData, Ctx, Role).

generic_auth(ReqData, Ctx, VerbRoles) when is_list(VerbRoles) ->
    ?D_VAL(generic_auth_3_clause_1),
    generic_auth(ReqData, Ctx, VerbRoles, admin);
generic_auth(ReqData, Ctx, Role) ->
    ?D_VAL(generic_auth_3_clause_2),
    Secret = ?D_VAL(extract_secret_header(ReqData)),
    Basic = ?D_VAL(extract_basic_header(ReqData)),
    Result = ?D_VAL(authorized(Secret, Basic, Role)),
    auth_result(Result, ReqData, Ctx).

generic_find(Commit, FindFunc, Name, ReqData, Ctx) ->
    generic_find(Commit, FindFunc, fun dump_json/3, Name, ReqData, Ctx).

generic_find(Commit, FindFunc, DumpFunc, Name, ReqData, Ctx) ->
    %error_logger:warning_msg("in generic_find and name is ~p~n", [Name]),
    case Commit of
	none ->
	    {{halt, 404}, ReqData, Ctx};
	_ ->
	    case FindFunc(Name, Commit) of 
		none ->
		    {{halt, 404}, ReqData, Ctx};
		{value, Result} ->
		    DumpFunc(Result, ReqData, Ctx)
	    end
    end.


generic_find_nc(FindFunc, Name, ReqData, Ctx) ->
    generic_find_nc(FindFunc, fun dump_json/3, Name, ReqData, Ctx).

generic_find_nc(FindFunc, DumpFunc, Name, ReqData, Ctx) ->
    ?D_LOG("in generic_find_nc and name is ~p~n", [Name]),
    case FindFunc(Name) of 
	Fail when Fail =:= none orelse Fail =:= find_failed ->
	    {{halt, 404}, ReqData, Ctx};
	Result ->
	    ?D_VAL(Result),
	    DumpFunc(Result, ReqData, Ctx)
    end.

get_starting_commit(ReqData, Ctx) ->
    Tag=list_to_binary(mochiweb_util:unquote(wrq:get_qs_value("tag", "", ReqData))),
    Commit=mochiweb_util:unquote(wrq:get_qs_value("commit", "", ReqData)),
    Branch=list_to_binary(mochiweb_util:unquote(wrq:get_qs_value("branch", "", ReqData))),
    ?D_LOG("wallaroo_web_common:get_starting_commit/2 Tag=~p, Commit=~p, Branch=~p~n", [Tag, Commit, Branch]),
    case {get_starting_commit, Branch, Tag, Commit} of
    	{get_starting_commit, <<>>, <<>>, []} ->
	    {none, Ctx};
	{get_starting_commit, _, <<>>, []} ->
	    case wallaroo:get_branch(Branch) of
		{wallaroo_branch, _}=TTerm ->
		    {wallaroo_branch:get_commit(TTerm), Ctx#ww_ctx{via={branch, Branch}}};
		find_failed ->
		    {none, Ctx}
	    end;
	{get_starting_commit, <<>>, _, []} ->
	    case wallaroo:get_tag(Tag) of
		{wallaroo_tag, _}=TTerm ->
		    {wallaroo_tag:get_commit(TTerm), Ctx#ww_ctx{via={tag, Tag}}};
		find_failed ->
		    {none, Ctx}
	    end;
	_ ->
	    {Commit, Ctx#ww_ctx{via=commit}}
    end.

generic_to_json(ReqData, Ctx, ListFun, GetFun) ->
    generic_to_json(ReqData, Ctx, ListFun, GetFun, true).

generic_to_json(ReqData, #ww_ctx{show_all=true}=Ctx, ListFun, _GetFun, CommitMatters) ->
    {Commit, NewCtx} = wallaroo_web_common:get_starting_commit(ReqData, Ctx),
    {Payload, _, _} = case {generic_to_json, Commit, CommitMatters} of 
		  {generic_to_json, none, true} -> dump_json([], ReqData, NewCtx);
		  _ -> dump_json(?D_VAL(ListFun(Commit)), ReqData, NewCtx)
	      end,
    {Payload, ReqData, NewCtx};
generic_to_json(ReqData, #ww_ctx{name=Name, commit=Commit}=Ctx, _ListFun, GetFun, true) ->
    wallaroo_web_common:generic_find(Commit, GetFun, Name, ReqData, Ctx);
generic_to_json(ReqData, #ww_ctx{name=Name}=Ctx, _ListFun, GetFun, false) ->
    wallaroo_web_common:generic_find_nc(GetFun, Name, ReqData, Ctx).

stringize_sha(<<CommitNum:160/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~40.16.0b", [CommitNum])).

dump_json(Entities, ReqData, Ctx) when is_list(Entities) ->
    ?D_LOG("Entities are ~p~n", [Entities]),
    {mochijson:binary_encode({array, [fix_json(Entity) || Entity <- Entities]}), ReqData, Ctx};
dump_json({_Kind, _Dict}=Entity, ReqData, Ctx) ->
    ?D_LOG("Entity is ~p~n", [Entity]),
    {mochijson:binary_encode(fix_json(Entity)), ReqData, Ctx}.

jsonify_entry({LVK, Ls}) when LVK =:= memberships; LVK =:= features; LVK =:= features;  LVK =:= includes;  LVK =:= depends;  LVK =:= conflicts; LVK =:= ss_params ->
    {LVK, {array, Ls}};
jsonify_entry({DVK, Ls}) when DVK =:= parameters ->
    {DVK, {struct, Ls}};
jsonify_entry({commit, <<_:160/big-unsigned-integer>>=SHA}) ->
    {commit, stringize_sha(SHA)};
jsonify_entry({last_updated_version, <<_:160/big-unsigned-integer>>=SHA}) ->
    {last_updated_version, stringize_sha(SHA)};
jsonify_entry({BTV, Ls}) when is_list(Ls), BTV =:= meta orelse BTV =:= annotation ->
    {BTV, {struct, Ls}};
jsonify_entry(X) ->
    X.

-spec fix_json({atom(), any()|orddict:orddict()}) -> {struct, orddict:orddict()}.
fix_json({Head, {SHA, Anno, Meta}}) when Head =:= wallaroo_tag orelse Head =:= wallaroo_branch ->
    {struct, [{commit, ensure_str_format(wallaroo_hash:stringize(SHA), binary)}, {annotation, Anno}, {meta, {struct, [jsonify_entry(Entry) || Entry <- Meta]}}]};
fix_json({wallaby_subsystem, EntityDict}) ->
    {struct, [jsonify_entry({K,V}) || {K,V} <- EntityDict, K =/= parameters] ++ [{parameters,{array, V}} || {parameters, V} <- EntityDict] };
fix_json({struct, _}=S) ->
    ?D_VAL(S);
fix_json({Kind, _}=Tup) when is_binary(Kind) ->
    ?D_VAL({struct, [Tup]});
fix_json({_Kind, EntityDict}) ->
    ?D_VAL({struct, [jsonify_entry(Entry) || Entry <- EntityDict]});
fix_json(BStr) when is_binary(BStr) ->
    BStr;
fix_json(Str) when is_list(Str) ->
    error_logger:warning_msg("called fix_json(\"~s\") with list instead of binary~n", [Str]),
    list_to_binary(Str);
fix_json(X) ->
    ?D_VAL(X).

generic_delete_nc(ReqData, #ww_ctx{show_all=true}=Ctx, _DelFun, _PathPart) ->
    {false, ReqData, Ctx};
generic_delete_nc(ReqData, #ww_ctx{name=Name}=Ctx, DelFun, PathPart) ->
    case DelFun(Name) of 
	{error, E} ->
	    ResponseBody = wrq:append_to_response_body(mochijson:binary_encode({struct, [{error, E}]}), ReqData),
	    {{halt, 400}, ResponseBody, Ctx};
	ok ->
	    NewLocation = io_lib:format("/~s", [PathPart]),
	    ?D_LOG("NewLocation is ~p~n", [NewLocation]),
	    Redir = wrq:do_redirect(true, wrq:set_resp_header("Location", NewLocation, ReqData)),
	    {true, Redir, Ctx}
    end.
	    

generic_delete_entity(ReqData, #ww_ctx{show_all=true}=Ctx, _Kind, _PathPart) ->
    {false, ReqData, Ctx};
generic_delete_entity(ReqData, #ww_ctx{name=Name, commit=Commit}=Ctx, Kind, PathPart) ->
    case wallaroo:delete_entity(Name, Kind, Commit) of
	{error, E} ->
	    ResponseBody = wrq:append_to_response_body(mochijson:binary_encode({struct, [{error, E}]}), ReqData),
	    {{halt, 400}, ResponseBody, Ctx};
	SHA ->
	     <<CommitNum:160/big-unsigned-integer>> = SHA,
	    NewCommitStr = lists:flatten(io_lib:format("~40.16.0b", [CommitNum])),
	    NewLocation = io_lib:format("/~s?commit=~s", [PathPart, NewCommitStr]),
	    ?D_LOG("NewLocation is ~p~n", [NewLocation]),
	    Redir = wrq:do_redirect(true, wrq:set_resp_header("Location", NewLocation, ReqData)),
	    {true, Redir, Ctx}
    end.

generic_from_json(ReqData, Ctx, NewFunc, PutKind, PathPart) ->
    generic_from_json(ReqData, Ctx, NewFunc, PutKind, PathPart, fun(_,_) -> ok end).

generic_from_json_raw(ReqData, Ctx, NewFunc, PutKind, PathPart) ->
    generic_from_json(ReqData, Ctx, NewFunc, PutKind, PathPart, fun(_,_) -> ok end, no).

generic_from_json(ReqData, Ctx, NewFunc, PutKind, PathPart, ValidFunc) ->
    generic_from_json(ReqData, Ctx, NewFunc, PutKind, PathPart, ValidFunc, yes).

generic_from_json(ReqData, Ctx, NewFunc, PutKind, PathPart, ValidFunc, Peel) ->
    ?D_LOG("Ctx is is ~p~n", [Ctx]),
    ?D_LOG("Body is is ~p~n", [wrq:req_body(ReqData)]),
    Data = case Peel of 
	       yes -> [{list_to_atom(binary_to_list(K)), V} || {K, V} <- (mochijson:binary_decoder([{object_hook, fun peel/1}]))(wrq:req_body(ReqData))];
	       no -> (mochijson:binary_decoder([]))(wrq:req_body(ReqData))
	   end,
    ?D_LOG("Data is is ~p~n", [Data]),
    case {PutKind, wrq:path_info(name, ReqData)} of
        {meta, _} ->
	    from_json_helper(Data, ReqData, Ctx, NewFunc, PutKind, PathPart, ValidFunc);
	{_, undefined} ->
	    case ?D_VAL(orddict:find(name, Data)) of
		error ->
		    {false, ReqData, Ctx};
		{ok, Name} ->
		    NamedData = orddict:store(name, list_to_binary(mochiweb_util:unquote(Name)), Data),
		    from_json_helper(NamedData, ReqData, Ctx, NewFunc, PutKind, PathPart, ValidFunc)
	    end;
        {_, Name} when is_binary(Name) orelse is_list(Name) ->
	    NamedData = orddict:store(name, list_to_binary(mochiweb_util:unquote(Name)), Data),
	    from_json_helper(NamedData, ReqData, Ctx, NewFunc, PutKind, PathPart, ValidFunc)
    end.

orddict_default_fetch(Key, Dict, Default) ->
    case orddict:find(Key, Dict) of
	{ok, V} ->
	    V;
	error ->
	    Default
    end.

ensure_str_format(Str, list) when is_list(Str) ->
    Str;
ensure_str_format(Str, binary) when is_list(Str) ->
    list_to_binary(Str);
ensure_str_format(Bin, list) when is_binary(Bin) ->
    binary_to_list(Bin);
ensure_str_format(Bin, binary) when is_binary(Bin) ->
    Bin.

atomize_pair({K, V}) when is_list(K) orelse is_binary(K) ->
    BK = ensure_str_format(K, binary),
    case catch binary_to_existing_atom(BK, latin1) of
	AK when is_atom(AK) ->
	    {AK, V};
	_ ->
	    {BK, V}
    end;
atomize_pair({K, V}) ->
    {K, V}.

atomize_meta(Dict) ->
    [atomize_pair(Pair) || Pair <- Dict].

vfail_to_json({fail, {all_vertices_exist, {bad_edges, Vs}}}) ->
    {struct, [{failure_kind, all_vertices_exist}, {bad_edges, {array, Vs}}]};
vfail_to_json({fail, {CyclicFail, {cycles, Cs}}}) ->
    {struct, [{failure_kind, CyclicFail}, {cycles, {array, Cs}}]};
vfail_to_json({fail, {no_immed_conflicts_with_transitive_includes_or_deps, DCs}}) ->
    DvC_struct = [{struct, [{feature, F}, 
			    {transitive_deps_and_includes, {array, DIs}}, 
			    {immediate_conflicts, {array, Cs}}]} 
		  || {F, DIs, Cs} <- DCs],
    {struct, [{failure_kind, no_immed_conflicts_with_transitive_includes_or_deps}, {features, {array, DvC_struct}}]};
vfail_to_json({fail, {DepOrConFail, What, for_nodes, Entities}}) ->
    {struct, [{failure_kind, DepOrConFail}, {entity_kind, What}, {entities, {array, Entities}}]};
vfail_to_json({fail, {multiple_failures, Fails}}) ->
    {struct, [{failure_kind, multiple_failures},
	      {failures, {array, [vfail_to_json({fail, Fail}) || Fail <- Fails]}}]};
vfail_to_json({fail, Fail}) when is_tuple(Fail), size(Fail) >= 2 ->
    error_logger:warning_msg("vfail_to_json/1 handling {fail, ~p}~n", [Fail]),
    [Kind|Details] = tuple_to_list(Fail),
    {struct, [{failure_kind, Kind},
	      {failure_string, iolist_to_binary(io_lib:format("~p", [list_to_tuple(Details)]))}]};
vfail_to_json(X) ->
    error_logger:warning_msg("vfail_to_json/1 handling unexpected term ~p~n", [X]),
    {struct, [{failure_kind, unknown},
	      {failure_term, iolist_to_binary(io_lib:format("~p", [X]))}]}.

from_json_helper(Data, ReqData, #ww_ctx{meta_domain=D, meta_key=K}=Ctx, _NewFunc, meta, PathPart, _ValidFunc) ->
    case ?D_VAL(wallaroo:put_meta(D, K, Data)) of
	{fail, _}=Failure ->
	    ResponseBody = 
		wrq:append_to_response_body(mochijson:binary_encode(vfail_to_json(Failure)), ReqData),
	    {{halt, 400}, ResponseBody, Ctx};
	_ ->
	    NewLocation = io_lib:format("/~s/~s/~s", [PathPart, mochiweb_util:quote_plus(D), mochiweb_util:quote_plus(K)]),
	    ?D_LOG("NewLocation is ~p~n", [NewLocation]),
	    Redir = wrq:do_redirect(true, wrq:set_resp_header("Location", NewLocation, ReqData)),
	    {true, Redir, Ctx}
    end;
from_json_helper(Data, ReqData, Ctx, _NewFunc, tag, PathPart, _ValidFunc) ->
    Name = ensure_str_format(orddict:fetch(name, Data), binary),
    SHA = ensure_str_format(orddict:fetch(commit, Data), list),
    Meta = atomize_meta(orddict_default_fetch(meta, Data, [])),
    Annotation = orddict_default_fetch(annotation, Data, []),
    ?D_LOG("about to convert JSON to a tag: Name=~p, SHA=~p, Meta=~p, Annotation=~p", [Name, SHA, Meta, Annotation]),
    case {tag_fjh, wallaroo:put_tag(Name, SHA, Annotation, Meta)} of
	{tag_fjh, {fail, _}=Failure} ->
	    ResponseBody = 
		wrq:append_to_response_body(mochijson:binary_encode(vfail_to_json(Failure)), ReqData),
	    {{halt, 400}, ResponseBody, Ctx};
	_ ->
	    NewLocation = io_lib:format("/~s/~s", [PathPart, mochiweb_util:quote_plus(Name)]),
	    ?D_LOG("NewLocation is ~p~n", [NewLocation]),
	    Redir = wrq:do_redirect(true, wrq:set_resp_header("Location", NewLocation, ReqData)),
	    {true, Redir, Ctx}
    end;
from_json_helper(Data, ReqData, Ctx, _NewFunc, branch, PathPart, _ValidFunc) ->
    Name = ensure_str_format(orddict:fetch(name, Data), binary),
    SHA = ensure_str_format(orddict:fetch(commit, Data), list),
    Meta = atomize_meta(orddict_default_fetch(meta, Data, [])),
    Annotation = orddict_default_fetch(annotation, Data, []),
    wallaroo:put_branch(Name, SHA, Annotation, Meta),
    NewLocation = io_lib:format("/~s/~s", [PathPart, mochiweb_util:quote_plus(Name)]),
    ?D_LOG("NewLocation is ~p~n", [NewLocation]),
    Redir = wrq:do_redirect(true, wrq:set_resp_header("Location", NewLocation, ReqData)),
    {true, Redir, Ctx};
from_json_helper(Data, ReqData, Ctx, NewFunc, PutKind, PathPart, ValidFunc) ->
    {Commit, NewCtx} = ?D_VAL(wallaroo_web_common:get_starting_commit(ReqData, Ctx)),
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
	    ?D_LOG("NewCommit is ~p~n", [NewCommit]),
	    case NewCtx of
		#ww_ctx{via={branch, Branch}} ->
		    wallaroo:put_branch(Branch, NewCommit);
		_ ->
		    ok
	    end,
	    <<CommitNum:160/big-unsigned-integer>> = NewCommit,
	    NewCommitStr = lists:flatten(io_lib:format("~40.16.0b", [CommitNum])),
	    NewLocation = io_lib:format("/~s/~s?commit=~s", [PathPart, mochiweb_util:quote_plus(Name), NewCommitStr]),
	    ?D_LOG("NewLocation is ~p~n", [NewLocation]),
	    Redir = wrq:do_redirect(true, wrq:set_resp_header("Location", NewLocation, ReqData)),
	    {true, Redir, NewCtx};
	{error, Why} ->
	    ResponseBody = wrq:append_to_response_body(mochijson:binary_encode(Why), ReqData),
	    {{halt, 400}, ResponseBody, NewCtx}
    end.

peel({struct, UnpeeledDict}) when is_list(UnpeeledDict) ->
    Dict = [{K,peel(V)} || {K,V} <- UnpeeledDict],
    orddict:from_list(Dict);
peel({array, Ls}) when is_list(Ls) ->
    [peel(Elt) || Elt <- Ls];
peel(X) -> X.
