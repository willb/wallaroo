%% @author William C. Benton <willb@redhat.com>
%% @copyright 2011 Red Hat, Inc. and William C. Benton
%% @doc Wallaroo API

-module(wallaroo).

-behaviour(gen_server).

-export([start_link/0, get_node/2, get_node/1, get_group/2, get_tag/1, get_group/1, put_node/2, put_node/3, put_group/2, put_group/3, put_tag/2, list_nodes/0, list_groups/0, list_tags/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
    wallaroo_store_ets:init(),
    gen_server:start_link(?MODULE, [], []).

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
    wallaroo ! {get, node, Name, canonicalize_hash(Commit)}.

get_group(Name) ->
    case get_tag("current") of
	find_failed ->
	    find_failed;
	Tag ->
	    Commit = wallaroo_commit:get_tag(Tag),
	    get_group(Name, Commit)
    end.

get_group(Name, Commit) ->
    wallaroo ! {get, group, Name, canonicalize_hash(Commit)}.

get_tag(Name) ->
    wallaroo ! {get_tag, Name}.

put_tag(Name, C) ->
    Commit = canonicalize_hash(C),
    wallaroo ! {put_tag, Name, Commit}.

put_group(Name, {wallaby_group, [_|_]}=Group) ->
    wallaroo ! {put, group, Name, Group}.

put_node(Name, {wallaby_node, [_|_]}=Node) ->
    wallaroo ! {put, node, Name, Node}.

put_group(Name, {wallaby_group, [_|_]}=Group, SC) ->
    C = canonicalize_hash(SC),
    wallaroo ! {put, group, Name, Group, C}.

put_node(Name, {wallaby_node, [_|_]}=Node, SC) ->
    C = canonicalize_hash(SC),
    wallaroo ! {put, node, Name, Node, C}.


%%% gen_server callbacks

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_call({get, What, Name, StartingCommit}, _From, {StoreMod}=State) when What=:=group; What=:=node ->
    CommitObj = get_commit(StartingCommit, StoreMod),
    Tree = wallaroo_commit:get_tree(CommitObj),
    {reply, get_path(What, Name, Tree, StoreMod), State};
handle_call({put, What, Name, Value, StartingCommit}, _From, {StoreMod}=State) when What=:=group; What=:=node ->
    CommitObj = get_commit(StartingCommit, StoreMod),
    Tree = wallaroo_commit:get_tree(CommitObj),
    {reply, put_path(What, Name, Value, Tree, StoreMod), State};
handle_call({put, What, Name, Value}, _From, {StoreMod}=State) when What=:=group; What=:=node ->
    Tree = wallaroo_tree:empty(),
    {reply, put_path(What, Name, Value, Tree, StoreMod), State};
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
    Path = [What ++ "s", Name],
    wallaroo_tree:get_path(Path, Tree, StoreMod).

put_path(What, Name, Value, Tree, StoreMod) ->
    Path = [What ++ "s", Name],
    wallaroo_tree:put_path(Path, Value, Tree, StoreMod).

get_commit(SHA, StoreMod) ->
    StoreMod:find_commit(SHA).

