%% @author William C. Benton <willb@redhat.com>
%% @copyright 2011 Red Hat, Inc. and William C. Benton
%% @doc Wallaroo API

-module(wallaroo).

-behaviour(gen_server).

-export([start_link/0, get_node/2, get_node/1, get_group/2, get_tag/1, get_group/1, put_node/2, put_node/3, put_group/2, put_group/3, put_tag/2, list_nodes/0, list_groups/0, list_tags/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

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

list_nodes() ->
    [].
list_groups() ->
    [].
list_tags() ->
    [].

get_node(Name) ->
    case get_tag("current") of
	find_failed ->
	    find_failed;
	Tag ->
	    Commit = wallaroo_tag:get_commit(Tag),
	    get_node(Name, Commit)
    end.

get_node(Name, Commit) ->
    gen_server:call(wallaroo,  {get, node, Name, canonicalize_hash(Commit)}).

get_group(Name) ->
    case get_tag("current") of
	find_failed ->
	    find_failed;
	Tag ->
	    Commit = wallaroo_commit:get_tag(Tag),
	    get_group(Name, Commit)
    end.

get_group(Name, Commit) ->
    gen_server:call(wallaroo,  {get, group, Name, canonicalize_hash(Commit)}).

get_tag(Name) ->
    gen_server:call(wallaroo,  {get_tag, Name}).

put_tag(Name, C) ->
    Commit = canonicalize_hash(C),
    gen_server:call(wallaroo,  {put_tag, Name, Commit}).

put_group(Name, {wallaby_group, [_|_]}=Group) ->
    gen_server:call(wallaroo,  {put, group, Name, Group}).

put_node(Name, {wallaby_node, [_|_]}=Node) ->
    gen_server:call(wallaroo,  {put, node, Name, Node}).

put_group(Name, {wallaby_group, [_|_]}=Group, SC) ->
    C = canonicalize_hash(SC),
    gen_server:call(wallaroo,  {put, group, Name, Group, C}).

put_node(Name, {wallaby_node, [_|_]}=Node, SC) ->
    C = canonicalize_hash(SC),
    gen_server:call(wallaroo,  {put, node, Name, Node, C}).


%%% gen_server callbacks

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_call({get, What, Name, StartingCommit}, _From, {StoreMod}=State) when What=:=group; What=:=node ->
    CommitObj = get_commit(StartingCommit, StoreMod),
    Tree = wallaroo_commit:get_tree(CommitObj, StoreMod),
    {reply, get_path(What, Name, Tree, StoreMod), State};
handle_call({put, What, Name, Value, StartingCommit}, _From, {StoreMod}=State) when What=:=group; What=:=node ->
    CommitObj = get_commit(StartingCommit, StoreMod),
    Tree = wallaroo_commit:get_tree(CommitObj, StoreMod),
    {reply, put_path(What, Name, Value, Tree, StoreMod, StartingCommit), State};
handle_call({put, What, Name, Value}, _From, {StoreMod}=State) when What=:=group; What=:=node ->
    Tree = wallaroo_tree:empty(),
    {reply, put_path(What, Name, Value, Tree, StoreMod, empty), State};
handle_call({get_tag, Name}, _From, {StoreMod}=State) ->
    TagObj = StoreMod:find_tag(Name),
    {reply, TagObj, State};
handle_call({put_tag, Name, Commit}, _From, {StoreMod}=State) ->
    TagObj = StoreMod:store_tag(Name, Commit),
    {reply, TagObj, State}.

handle_info(_X, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%% Helpers

canonicalize_hash(String) when is_list(String) ->
    wallaroo_hash:as_bitstring(String);
canonicalize_hash(BS) when is_binary(BS) ->
    BS.


get_path(What, Name, Tree, StoreMod) ->
    Path = [atom_to_list(What) ++ "s", Name],
    wallaroo_tree:get_path(Path, Tree, StoreMod).

put_path(What, Name, Value, Tree, StoreMod, empty) ->
    Commit = wallaroo_commit:store(wallaroo_commit:empty(), StoreMod),
    put_path(What, Name, Value, Tree, StoreMod, Commit);
put_path(What, Name, Value, Tree, StoreMod, ParentCommit) ->
    Path = [atom_to_list(What) ++ "s", Name],
    {NewTree, _} = wallaroo_tree:put_path(Path, Value, Tree, StoreMod),
    Commit = wallaroo_commit:new([ParentCommit], NewTree, [], []),
    wallaroo_commit:store(Commit, StoreMod).

get_commit(SHA, StoreMod) ->
    Result = StoreMod:find_commit(SHA),
    error_logger:info_msg("wallaroo:get_commit/2 -> ~p ~n", [Result]),
    Result.

