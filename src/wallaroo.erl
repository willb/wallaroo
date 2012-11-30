%% @author William C. Benton <willb@redhat.com>
%% @copyright 2011 Red Hat, Inc. and William C. Benton
%% @doc Wallaroo internal API.  Unsupported and subject to change!

-module(wallaroo).

-behaviour(gen_server).

-export([start_link/0, get_entity/2, get_entity/3, get_tag/1, put_entity/3, put_entity/4, put_tag/2, list_entities/1, list_entities/2, list_tags/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(VALID_ENTITY_KIND(Kind), Kind=:='node' orelse Kind=:='feature' orelse Kind =:= 'subsystem' orelse Kind=:='group' orelse Kind=:='parameter').

start_link() ->
    error_logger:info_msg("entering WALLAROO start_link/0 ~n", []),
    wallaroo_store_ets:init([]),
    Result = try 
		 gen_server:start_link({local, ?SERVER}, ?MODULE, [], [])
	     catch
		 Kind:Reason -> error_logger:info_msg("failure! ~p:~p~n", [Kind, Reason]), {failure_with, {Kind, Reason}}
	     end,
    error_logger:info_msg("returning ~p from WALLAROO start_link/0 ~n", [Result]),
    Result.

init([]) ->
    {ok, {wallaroo_store_ets}}.

%%% API functions

list_entities(Kind) when ?VALID_ENTITY_KIND(Kind) ->
    case get_tag(<<"current">>) of
	find_failed ->
	    [];
	Tag ->
	    Commit = wallaroo_tag:get_commit(Tag),
	    list_entities(Kind, Commit)
    end.

list_entities(Kind, Commit) when ?VALID_ENTITY_KIND(Kind) ->
    gen_server:call(?SERVER, {list, Kind, canonicalize_hash(Commit)}).

list_tags() ->
    gen_server:call(?SERVER, {list_tags}).

get_entity(Name, Kind) when is_binary(Name) andalso is_atom(kind) ->
    case get_tag(<<"current">>) of
	find_failed ->
	    find_failed;
	Tag ->
	    Commit = wallaroo_tag:get_commit(Tag),
	    get_entity(Name, Kind, Commit)
    end.

get_entity(Name, Kind, Commit) ->
    gen_server:call(?SERVER, {get, Kind, Name, canonicalize_hash(Commit)}).

get_tag(Name) ->
    gen_server:call(?SERVER,  {get_tag, Name}).

put_tag(Name, C) ->
    Commit = canonicalize_hash(C),
    gen_server:call(?SERVER,  {put_tag, Name, Commit}).

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

put_entity(Name, Kind, Value) when ?VALID_ENTITY_KIND(Kind) ->
    value_check(Kind, Value),
    gen_server:call(?SERVER,  {put, Kind, Name, Value}).

put_entity(Name, Kind, Value, SuppliedCommit) when ?VALID_ENTITY_KIND(Kind) ->
    value_check(Kind, Value),
    C = canonicalize_hash(SuppliedCommit),
    gen_server:call(?SERVER,  {put, Kind, Name, Value, C}).

%%% gen_server callbacks

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_call({list_tags}, _From, {StoreMod}=State) ->
    {reply, StoreMod:tags(), State};
handle_call({list, Kind, StartingCommit}, _From, {StoreMod}=State) ->
    CommitObj = get_commit(StartingCommit, StoreMod),
    Tree = wallaroo_commit:get_tree(CommitObj, StoreMod),
    case wallaroo_tree:get_path([xlate_what(Kind)], Tree, StoreMod) of
	{value, Entities} ->
	    {reply, [E || {E, _} <- wallaroo_tree:children(Entities, StoreMod)], State};
	none ->
	    {reply, [], State}
    end;
handle_call({get, What, Name, StartingCommit}, _From, {StoreMod}=State) when ?VALID_ENTITY_KIND(What) ->
    CommitObj = get_commit(StartingCommit, StoreMod),
    Tree = wallaroo_commit:get_tree(CommitObj, StoreMod),
    {reply, get_path(What, Name, Tree, StoreMod), State};
handle_call({put, What, Name, Value, StartingCommit}, _From, {StoreMod}=State) when ?VALID_ENTITY_KIND(What) ->
    CommitObj = get_commit(StartingCommit, StoreMod),
    Tree = wallaroo_commit:get_tree(CommitObj, StoreMod),
    {reply, put_path(What, Name, Value, Tree, StoreMod, StartingCommit), State};
handle_call({put, What, Name, Value}, _From, {StoreMod}=State) when ?VALID_ENTITY_KIND(What) ->
    Tree = wallaroo_tree:empty(),
    {reply, put_path(What, Name, Value, Tree, StoreMod, empty), State};
handle_call({get_tag, Name}, _From, {StoreMod}=State) ->
    TagObj = StoreMod:find_tag(Name),
    {reply, TagObj, State};
handle_call({put_tag, Name, Commit}, _From, {StoreMod}=State) ->
    TagObj = StoreMod:store_tag(Name, wallaroo_tag:new(Commit, [], [])),
    {reply, TagObj, State}.

handle_info(_X, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%% Helpers

canonicalize_hash(String) when is_list(String) ->
    wallaroo_hash:hash_to_bitstring(String);
canonicalize_hash(BS) when is_binary(BS) ->
    BS.

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

