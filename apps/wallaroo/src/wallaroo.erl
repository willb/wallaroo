%% @author William C. Benton <willb@redhat.com>
%% @copyright 2011 Red Hat, Inc. and William C. Benton
%% @doc Wallaroo internal API.  Unsupported and subject to change!

-module(wallaroo).

-behaviour(gen_server).

-export([start_link/0, get_entity/2, get_entity/3, get_tag/1, get_branch/1, put_entity/3, put_entity/4, put_tag/2, put_tag/4, put_branch/2, put_branch/4, get_meta/2, get_meta/1, put_meta/3, list_meta/0, list_entities/1, list_entities/2, list_tags/0, list_branches/0, version/0, version_string/0, delete_tag/1, delete_branch/1, delete_entity/3, delete_meta/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(VALID_ENTITY_KIND(Kind), Kind=:='node' orelse Kind=:='feature' orelse Kind =:= 'subsystem' orelse Kind=:='group' orelse Kind=:='parameter').
-include("version.hrl").
-include("dlog.hrl").

start_link() ->
    error_logger:info_msg("entering WALLAROO start_link/0 ~n", []),
    %% XXX: initialize non-ETS storage here
    wallaroo_store_ets:init([]),
    Result = try 
		 gen_server:start_link({local, ?SERVER}, ?MODULE, [], [])
	     catch
		 Kind:Reason -> error_logger:info_msg("failure! ~p:~p~n", [Kind, Reason]), {failure_with, {Kind, Reason}}
	     end,
    error_logger:info_msg("returning ~p from WALLAROO start_link/0 ~n", [Result]),
    Result.

init([]) ->
    init([{storemod, wallaroo_store_ets}]);
init([{storemod, StoreMod}]) ->
    case StoreMod:find_tag(<<"empty">>) of
	find_failed ->
	    setup_empty_tag(StoreMod);
	_ ->
	    pass
    end,
    {ok, {StoreMod}}.


%%% API functions

version() ->
    ?VERSION.

version_string() ->
    VList = [proplists:get_value(Key, version()) || Key <- [major, minor, patch, build]],
    Format = case proplists:get_value(build, version()) of
		 "" ->
		     "~B.~B.~B~s";
		 _ ->
		     "~B.~B.~B-~s"
	     end,
    binary_to_list(iolist_to_binary(io_lib:format(Format, VList))).

delete_meta(Domain, Key) ->
    gen_server:call(?SERVER, {delete_meta, Domain, Key}).

delete_tag(Tag) ->
    gen_server:call(?SERVER, {delete_tag, Tag}).
    
delete_branch(Branch) ->
    gen_server:call(?SERVER, {delete_branch, Branch}).
    
delete_entity(Name, Kind, Commit) ->
    gen_server:call(?SERVER, {delete_entity, Name, Kind, wallaroo_hash:canonicalize(Commit)}).

list_entities(Kind) when ?VALID_ENTITY_KIND(Kind) ->
    case get_tag(<<"current">>) of
	find_failed ->
	    [];
	Tag ->
	    Commit = wallaroo_tag:get_commit(Tag),
	    list_entities(Kind, Commit)
    end.

list_entities(Kind, Commit) when ?VALID_ENTITY_KIND(Kind) ->
    gen_server:call(?SERVER, {list, Kind, wallaroo_hash:canonicalize(Commit)}).

list_tags() ->
    gen_server:call(?SERVER, {list_tags}).

list_branches() ->
    gen_server:call(?SERVER, {list_branches}).

list_meta() ->
    gen_server:call(?SERVER, {list_meta}).

get_entity(Name, Kind) when is_binary(Name) andalso is_atom(kind) ->
    case get_tag(<<"current">>) of
	find_failed ->
	    find_failed;
	Tag ->
	    Commit = wallaroo_tag:get_commit(Tag),
	    get_entity(Name, Kind, Commit)
    end.

get_entity(Name, Kind, Commit) ->
    gen_server:call(?SERVER, {get, Kind, Name, wallaroo_hash:canonicalize(Commit)}).

get_meta(Domain, all) ->
    get_meta(Domain);
get_meta(Domain, Key) when is_binary (Domain), is_binary(Key) ->
    gen_server:call(?SERVER, {get_meta, Domain, Key}).

get_meta(Domain) when is_binary (Domain) ->
    gen_server:call(?SERVER, {get_meta, Domain}).

put_meta(Domain, Key, Value) when is_binary(Domain), is_binary(Key) ->
    gen_server:call(?SERVER, {put_meta, Domain, Key, Value}).    

get_tag(Name) ->
    gen_server:call(?SERVER,  {get_tag, Name}).

get_branch(Name) ->
    gen_server:call(?SERVER,  {get_branch, Name}).

put_branch(Name, Commit) ->
    put_branch(Name, Commit, [], []).

put_branch(Name, C, Anno, Meta) ->
    Commit = wallaroo_hash:canonicalize(C),
    gen_server:call(?SERVER,  {put_branch, Name, Commit, Anno, Meta}).

put_tag(Name, C) ->
    put_tag(Name, C, [], []).

put_tag(Name, C, Anno, Meta) ->
    Commit = wallaroo_hash:canonicalize(C),
    gen_server:call(?SERVER,  {put_tag, Name, Commit, Anno, Meta}).

value_check(node, {wallaby_node, _}) ->
    ok;
value_check(group, {wallaby_group, _}) ->
    ok;
value_check(feature, {wallaby_feature, _}) ->
    ok;
value_check(parameter, {wallaby_parameter, _}) ->
    ok;
value_check(subsystem, {wallaby_subsystem, _}) ->
    ok.


put_entity(Name, node, Value) ->
    value_check(node, Value),
    IdGroup = wallaby_node:identity_group(Value),
    Whence = wallaroo_hash:canonicalize(put_entity(IdGroup, group, wallaby_group:new(IdGroup))),
    gen_server:call(?SERVER,  {put, node, Name, Value, Whence});
put_entity(Name, Kind, Value) when ?VALID_ENTITY_KIND(Kind) ->
    value_check(Kind, Value),
    gen_server:call(?SERVER,  {put, Kind, Name, Value}).


put_entity(Name, node, Value, SuppliedCommit) ->
    value_check(node, Value),
    IdGroup = wallaby_node:identity_group(Value),
    C = wallaroo_hash:canonicalize(SuppliedCommit),
    Whence = wallaroo_hash:canonicalize(put_entity(IdGroup, group, wallaby_group:new(IdGroup), C)),
    gen_server:call(?SERVER,  {put, node, Name, Value, Whence});
put_entity(Name, Kind, Value, SuppliedCommit) when ?VALID_ENTITY_KIND(Kind) ->
    value_check(Kind, Value),
    C = wallaroo_hash:canonicalize(SuppliedCommit),
    gen_server:call(?SERVER,  {put, Kind, Name, Value, C}).

%%% gen_server callbacks

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_call({list_tags}, _From, {StoreMod}=State) ->
    {reply, StoreMod:tags(), State};
handle_call({list_branches}, _From, {StoreMod}=State) ->
    {reply, StoreMod:branches(), State};
handle_call({list, Kind, StartingCommit}, _From, {StoreMod}=State) ->
    CommitObj = get_commit(StartingCommit, StoreMod),
%    error_logger:warning_msg("list/2 StartingCommit=~p, CommitObj=~p~n", [StartingCommit, CommitObj]),
    Tree = wallaroo_commit:get_tree(CommitObj, StoreMod),
    case wallaroo_tree:get_path([xlate_what(Kind)], Tree, StoreMod) of
	{value, Entities} ->
	    {reply, [E || {E, _} <- wallaroo_tree:children(Entities, StoreMod)], State};
	none ->
	    {reply, [], State}
    end;
handle_call({get, What, Name, StartingCommit}, _From, {StoreMod}=State) when ?VALID_ENTITY_KIND(What) ->
    CommitObj = get_commit(StartingCommit, StoreMod),
    case CommitObj of
	find_failed ->
	    {reply, find_failed, State};
	_ ->
	    Tree = wallaroo_commit:get_tree(CommitObj, StoreMod),
	    GetResult = get_path(What, Name, Tree, StoreMod),
						% error_logger:warning_msg("GETRESULT:  ~p~n", [GetResult]),
	    {reply, add_last_updated(StartingCommit, GetResult), State}
    end;
handle_call({put, What, Name, Value, StartingCommit}, _From, {StoreMod}=State) when ?VALID_ENTITY_KIND(What) ->
    CommitObj = ensure_special_groups_exist(StartingCommit, get_commit(StartingCommit, StoreMod), StoreMod),
    Tree = wallaroo_commit:get_tree(CommitObj, StoreMod),
    {reply, put_path(What, Name, Value, Tree, StoreMod, StartingCommit), State};
handle_call({put, What, Name, Value}, _From, {StoreMod}=State) when ?VALID_ENTITY_KIND(What) ->
    Parent = setup_basic_commit(StoreMod),
    Tree = wallaroo_commit:get_tree(StoreMod:find_commit(Parent), StoreMod),
    {reply, put_path(What, Name, Value, Tree, StoreMod, Parent), State};
handle_call({get_branch, Name}, _From, {StoreMod}=State) ->
    Obj = StoreMod:find_branch(Name),
    {reply, Obj, State};
handle_call({get_tag, Name}, _From, {StoreMod}=State) ->
    TagObj = StoreMod:find_tag(Name),
    {reply, TagObj, State};
handle_call({put_branch, Name, Commit, Anno, Meta}, _From, {StoreMod}=State) ->
    Obj = StoreMod:store_branch(Name, wallaroo_branch:new(Commit, Anno, Meta)),
    {reply, Obj, State};
handle_call({put_tag, Name, Commit, Anno, Meta}, _From, {StoreMod}=State) ->
    CommitObj = get_commit(Commit, StoreMod),
    ?D_LOG("put_tag/4 Name=~p, Commit=~p, CommitObj=~p~n", [Name, Commit, CommitObj]),
    Tree = wallaroo_commit:get_tree(CommitObj, StoreMod),
    V = case orddict:find(validated, Meta) of
	    {ok, true} ->
		wallaroo_validators:pcompose(wallaby_validators:make_activate_validators(Tree, StoreMod));
	    _ ->
		fun(_,_) -> ok end
	end,
    case wallaby_vcache:for_commit(Commit, V) of
	ok ->
	    ?D_LOG("put_tag SUCCESS with Name=~p; Commit=~p, CommitObj=~p, Tree=~p~n", [Name, Commit, CommitObj, Tree]),
	    TagObj = StoreMod:store_tag(Name, wallaroo_tag:new(Commit, Anno, Meta)),
	    {reply, TagObj, State};
	{fail, _}=F ->
	    error_logger:warning_msg("put_tag FAILURE because ~p~n", [F]),
	    {reply, F, State}
    end;
handle_call({put_meta, Domain, Key, Value}, _From, {StoreMod}=State) ->
    StoreMod:store_meta(Domain, Key, Value),
    {reply, ok, State};
handle_call({get_meta, Domain}, _From, {StoreMod}=State) ->
    {reply, StoreMod:find_meta(Domain), State};
handle_call({get_meta, Domain, Key}, _From, {StoreMod}=State) ->
    {reply, StoreMod:find_meta(Domain, Key), State};
handle_call({list_meta}, _From, {StoreMod}=State) ->
    {reply, StoreMod:meta(), State};
handle_call({delete_tag, Tag}, _From, {StoreMod}=State) ->
    case delete_check(Tag, tag) of
	{error, _}=E ->
	    {reply, E, State};
	ok ->
	    StoreMod:delete_tag(Tag),
	    {reply, ok, State}
    end;
handle_call({delete_branch, Branch}, _From, {StoreMod}=State) ->
    StoreMod:delete_branch(Branch),
    {reply, ok, State};
handle_call({delete_meta, Domain, Key}, _From, {StoreMod}=State) ->
    StoreMod:delete_meta(Domain, Key),
    {reply, ok, State};
handle_call({delete_entity, Name, Kind, Commit}, _From, {StoreMod}=State) ->
    {reply, delete_helper(Name, Kind, Commit, StoreMod), State}.

delete_check(<<"empty">>, tag) ->
    {error, cant_delete_special_tags};
delete_check(<<"+++", _/binary>>, group) ->
    {error, cant_delete_special_groups};
delete_check(_,_) ->
    ok.

delete_helper(Name, Kind, Commit, StoreMod) ->
    case delete_check(Name, Kind) of
	{error, _}=E ->
	    E;
	ok ->
	    Tree = wallaroo_commit:get_tree(get_commit(Commit, StoreMod), StoreMod),
	    OtherEntities = immediately_affected_entities(Name, Kind, Tree, StoreMod),
	    {_, TmpTree} = 
		lists:foldl(fun({Path, 'DELETED'}, {_, TreeAcc}) ->
				    wallaroo_tree:del_path(Path, TreeAcc, StoreMod);
			       ({Path, Obj}, {_H, TreeAcc}) ->
				    wallaroo_tree:put_path(Path, Obj, TreeAcc, StoreMod)
			    end, 
			    {unchanged, Tree},
			    OtherEntities),
	    {NewHash, _NewTree} = 
		wallaroo_tree:del_path([xlate_what(Kind), Name], TmpTree, StoreMod),		    
	    wallaroo_commit:store(wallaroo_commit:new([Commit], NewHash, [], [{deletes,<<(xlate_what(Kind))/binary, 47, Name/binary>>}]), StoreMod)
    end.

immediately_affected_entities(Name, node, Tree, StoreMod) ->
    % removing a node doesn't affect anything but its identity group
    case wallaroo_tree:get_path([xlate_what(node), Name], Tree, StoreMod) of
	{value, Nd} ->
	    [{[xlate_what(group), wallaby_node:identity_group(Nd)], 'DELETED'}];
	_ ->
	    []
    end;
immediately_affected_entities(Name, group, Tree, StoreMod) ->
    % removing a group affects nodes that are a member of that group
    case wallaroo_tree:get_path([xlate_what(node)], Tree, StoreMod) of
	{value, Entities} ->
	    [{[xlate_what(node), N], wallaby_node:set_memberships(Nobj, lists:delete(Name, wallaby_node:memberships(Nobj)))} || 
		{N, Nobj} <- wallaroo_tree:children(Entities, StoreMod),
		lists:member(Name, wallaby_node:memberships(Nobj))];
	none ->
	    error_logger:warning_msg("No nodes for tree with hash ~p~n", [wallaroo_db:identity(Tree)]),
	    []
    end;
immediately_affected_entities(Name, feature, Tree, StoreMod) ->
    % removing a feature affects groups that installed that feature 
    % and features that depend upon or conflict with that feature
    Groups = 
	case wallaroo_tree:get_path([xlate_what(group)], Tree, StoreMod) of
	    {value, GEntities} ->
		[{[xlate_what(group), G], wallaby_group:set_features(Gobj, lists:delete(Name, wallaby_group:features(Gobj)))} || 
		    {G, Gobj} <- wallaroo_tree:children(GEntities, StoreMod),
		    lists:member(Name, wallaby_group:features(Gobj))];
	    none ->
		error_logger:warning_msg("No groups for tree with hash ~p~n", [wallaroo_db:identity(Tree)]),
		[]
	end,
    Features = 
	case wallaroo_tree:get_path([xlate_what(feature)], Tree, StoreMod) of
	    {value, FEntities} ->
		[{[xlate_what(feature), F], FobjPrime} ||
		    {F, Fobj} <- wallaroo_tree:children(FEntities, StoreMod),
		    (FobjPrime = elim_feature(Name, Fobj)) =/= Fobj];
	    none ->
		error_logger:warning_msg("No groups for tree with hash ~p~n", [wallaroo_db:identity(Tree)]),
		[]
	end,
    Groups ++ Features;
immediately_affected_entities(Name, subsystem, Tree, StoreMod) ->
    [];
immediately_affected_entities(Name, parameter, Tree, StoreMod) ->
    % removing a parameter affects groups or features that installed that parameter 
    % and subsystems that are interested in that parameter
    lists:flatten([case wallaroo_tree:get_path([xlate_what(Kind)], Tree, StoreMod) of
		       {value, Entities} ->
			   [{[xlate_what(Kind), E], ObjPrime} ||
			       {E, Obj} <- wallaroo_tree:children(Entities, StoreMod),
			       (ObjPrime = elim_param(Name, Obj)) =/= Obj];
		       none ->
			   error_logger:warning_msg("No ~ps for tree with hash ~p~n", [Kind, wallaroo_db:identity(Tree)]),
			   []
		   end || Kind <- [group, feature, subsystem]]).

% eliminates the feature named DelF from the feature object Fobj
-spec elim_feature(binary(), wallaby_feature:feature()) -> wallaby_feature:feature().
elim_feature(DelF, {wallaby_feature, _}=Fobj) ->
    lists:foldl(fun ({Get, Set}, Feature) ->
			Set(Feature, lists:delete(DelF, Get(Feature)))
		end, 
		Fobj,
		[{fun wallaby_feature:includes/1, fun wallaby_feature:set_includes/2},
		 {fun wallaby_feature:depends/1, fun wallaby_feature:set_depends/2},
		 {fun wallaby_feature:conflicts/1, fun wallaby_feature:set_conflicts/2}]).

% eliminates references to the parameter named by DelP from the entity given by Obj
elim_param(DelP, {wallaby_parameter, _}=Obj) ->
    lists:foldl(fun ({Get, Set}, Parameter) ->
			Set(Parameter, lists:delete(DelP, Get(Parameter)))
		end, 
		Obj,
		[{fun wallaby_parameter:conflicts/1, fun wallaby_parameter:set_conflicts/2},
		 {fun wallaby_feature:depends/1, fun wallaby_feature:set_depends/2}]);
elim_param(DelP, {wallaby_feature, _}=Obj) ->
    wallaby_feature:set_parameters(Obj, [{P,V} || {P,V} <- wallaby_feature:parameters(Obj),
						  P =/= DelP]);
elim_param(DelP, {wallaby_group, _}=Obj) ->
    wallaby_group:set_parameters(Obj, [{P,V} || {P,V} <- wallaby_group:parameters(Obj),
						P =/= DelP]);
elim_param(DelP, {wallaby_subsystem, _}=Obj) ->
    wallaby_subsystem:set_parameters(Obj, [P || P <- wallaby_subsystem:parameters(Obj),
						P =/= DelP]).

handle_info(_X, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%% Helpers

add_last_updated(Commit, {value, {wallaby_node, Dict}}) ->
    {value, {wallaby_node, orddict:store(last_updated_version, list_to_binary(wallaroo_hash:stringize(Commit)), Dict)}};
add_last_updated(_, _=Val) ->
    Val.

xlate_what('node') ->
    <<"nodes">>;
xlate_what('group') ->
    <<"groups">>;
xlate_what('feature') ->
    <<"features">>;
xlate_what('parameter') ->
    <<"parameters">>;
xlate_what('subsystem') ->
    <<"subsystems">>;
xlate_what(X) when is_atom(X) ->
    list_to_binary(atom_to_list(X) ++ "s").

create_what(group) ->
    fun(Name) -> wallaby_group:new(Name) end;
create_what(node) ->
    fun(Name) -> wallaby_node:new(Name, true) end;
create_what(feature) ->
    fun(Name) -> wallaby_feature:new(Name) end;
create_what(parameter) ->
    fun(Name) -> wallaby_parameter:new(Name) end;
create_what(subsystem) ->
    fun(Name) -> wallaby_subsystem:new(Name) end.

ensure_entities_exist({Hash, Tree}, Kind, Entities, StartingCommit, StoreMod) ->
    ensure_entities_exist({Hash, Tree}, Kind, Entities, StartingCommit, create_what(Kind), StoreMod).
ensure_entities_exist({Hash, Tree}, Kind, Entities, StartingCommit, CreateFun, StoreMod) ->
    {UpdatedTreeHash, _} = lists:foldl(fun(Entity, {H, T}) ->
					       Path = [xlate_what(Kind), Entity],
					       case wallaroo_tree:get_path(Path, T, StoreMod) of
						   none ->
						       wallaroo_tree:put_path(Path, CreateFun(Entity), T, StoreMod);
						   {_, _} ->
						       {H, T}
					       end
				       end, {Hash, Tree}, Entities),
    case UpdatedTreeHash of
	Hash ->
	    StartingCommit;
	_ ->
	    wallaroo_commit:store(wallaroo_commit:new([StartingCommit], UpdatedTreeHash, [], [{automatically_generated_by,ensure_entities_exist}]), StoreMod)
    end.

ensure_special_groups_exist(Commit, CommitObj, StoreMod) ->
    Hash = wallaroo_commit:get_tree_hash(CommitObj),
    Tree = wallaroo_commit:get_tree(CommitObj, StoreMod),
    UpdatedCommit = ensure_entities_exist({Hash, Tree}, group, [<<"+++DEFAULT">>, <<"+++SKEL">>], Commit, StoreMod),
    StoreMod:find_commit(UpdatedCommit).

setup_basic_commit(StoreMod) ->
    EmptyCommitSHA = wallaroo_commit:store(wallaroo_commit:empty(), StoreMod),
    {EmptyTreeHash, EmptyTree} = wallaroo_db:hash_and_store(wallaroo_tree:empty(), StoreMod),
    ensure_entities_exist({EmptyTreeHash, EmptyTree}, group, [<<"+++SKEL">>, <<"+++DEFAULT">>], EmptyCommitSHA, StoreMod).

setup_empty_tag(StoreMod) ->
    SHA = setup_basic_commit(StoreMod),
    wallaroo_tag:store_without_validating(<<"empty">>, wallaroo_tag:new(SHA, [], []), StoreMod).

get_path(What, Name, Tree, StoreMod) when is_atom(What) ->
    get_path(xlate_what(What), Name, Tree, StoreMod);
get_path(What, Name, Tree, StoreMod) when is_binary(What) ->
    Path = [What, Name],
    wallaroo_tree:get_path(Path, Tree, StoreMod).

put_path(What, Name, Value, Tree, StoreMod, Commit) when is_atom(What) ->
    put_path(xlate_what(What), Name, Value, Tree, StoreMod, Commit);
put_path(What, Name, Value, Tree, StoreMod, empty) when is_binary(What) ->
    Commit = wallaroo_commit:store(wallaroo_commit:empty(), StoreMod),
    put_path(What, Name, Value, Tree, StoreMod, Commit);
put_path(What, Name, Value, Tree, StoreMod, ParentCommit) when is_binary(What) ->
    {NewTree, _} = wallaroo_tree:put_path([What, Name], Value, Tree, StoreMod),
    Commit = wallaroo_commit:new([ParentCommit], NewTree, [], []),
    wallaroo_commit:store(Commit, StoreMod).

get_commit(SHA, StoreMod) ->
    StoreMod:find_commit(SHA).

