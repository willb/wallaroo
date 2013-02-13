%% @author William C. Benton <willb@redhat.com>
%% @copyright 2012 Red Hat, Inc. and William C. Benton
%% @doc Wallaby configuration-generation support

-module(wallaby_config).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/0]).

-export([has/3, for/3, cache_dump/0, reset/0]).

-record(cstate, {re, table, storage}).

-define(SERVER, ?MODULE).

-include("dlog.hrl").

start_link() ->
    Result = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    error_logger:info_msg("wallaby_config:start_link result ~p~n", [Result]),
    Result.

init(Options) when is_list(Options) ->
    {ok, RE} = re:compile(list_to_binary("(?-mix:^(?:(>=|&&=|\\?=|\\|\\|=)\\s*)+(.*?)\\s*$)")),
    Table = ets:new(config, []),
    StoreMod = case orddict:find(storage_module, Options) of
		   {ok, Mod} ->
		       Mod;
		   error ->
		       wallaroo_store_ets
	       end,
    {ok, #cstate{re=RE, table=Table, storage=StoreMod}};
init(_) ->
    init([]).

%%% API functions

has(Kind, Name, Commit) ->
    gen_server:call(?SERVER, {has_config, Kind, Name, Commit}).

for(Kind, Name, Commit) ->
    gen_server:call(?SERVER, {config_for, Kind, Name, Commit}).

cache_dump() ->
    gen_server:call(?SERVER, {cache_dump}).

reset() ->
    gen_server:call(?SERVER, {reset}).

%%% gen_server callbacks
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_call({cache_dump}, _From, #cstate{table=C}=State) ->
    {reply, ets:tab2list(C), State};
handle_call({reset}, _From, #cstate{table=C}=State) ->
    {reply, ets:delete_all_objects(C), State};
handle_call({has_config, Kind, Name, Commit}, _From, #cstate{storage=StoreMod}=State) ->
    Default = case Kind of
		  node ->
		      true;
		  _ ->
		      false
	      end,
    case generic_lookup(Kind, Name, Commit, State, Default) of
	{value, Result} ->
	    {reply, Result, State};
	Lookup ->
	    {reply, Lookup, State}
    end;
handle_call({config_for, Kind, Name, Commit}, _From, #cstate{re=RE, table=Cache, storage=StoreMod}=State) ->
    Default = case Kind of 
		  node ->
		      {wallaby_lw_config, Commit, [<<"+++SKEL">>, <<"+++DEFAULT">>]};
		  _ ->
		      []
	      end,
    ?D_LOG("{config_for, ~p, ~p, ~p} --> ~p", [Kind, Name, Commit, generic_lookup(Kind, Name, Commit, State, Default)]),
    {value, Config} = ?D_VAL(generic_lookup(Kind, Name, Commit, State, Default)),
    {reply, Config, State}.

handle_info(_X, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%% helpers

generic_lookup(Kind, Name, Commit, #cstate{storage=StoreMod}=State, Default) ->
    CommitObj = StoreMod:find_commit(wallaroo_hash:canonicalize(Commit)),
    case CommitObj of
	{wallaroo_commit, _} ->
	    generic_find(Kind, Name, Commit, CommitObj, State, Default);
	_ ->
	    {error, {bad_commit, Commit, CommitObj}}
    end.


%% XXX: refactor dupes plz
transitively_reachable(Graph, StartNode) ->
    ordsets:from_list(digraph_utils:reachable_neighbours([StartNode], Graph)).

transitively_reachable(Graph, StartNode, Filter) ->
    [Node || Node <- transitively_reachable(Graph, StartNode), Filter(Node)].

interesting_install_edge({Kind, _, {'group', _}}) when Kind =:= 'member_of' ->
    true;
interesting_install_edge({Kind, _, {'feature', _}}) when Kind =:= 'includes' orelse Kind =:= 'installs' ->
    true;
interesting_install_edge(_) ->
    false.

interesting_install_vertex({'feature', _}) ->
    true;
interesting_install_vertex({'node', _}) ->
    true;
interesting_install_vertex({'group', _}) -> 
    true;
interesting_install_vertex(_) -> 
    false.

%% interesting_value_edge({'param_value', _, _}) ->
%%     true;
%% interesting_value_edge(X) ->
%%     interesting_install_edge(X).

calc_configs(Commit, CommitObj, #cstate{re=RE, table=Cache, storage=StoreMod}=State) ->
    case seen(Commit, State) of 
	true ->
	    ok;
	false ->
	    Tree = wallaroo_commit:get_tree(CommitObj, StoreMod),
	    {Entities, Relationships} = wallaby_graph:extract_graph(Tree, StoreMod),
	    Installs = digraph:new([private]),
	    [digraph:add_vertex(Installs, Ent) || Ent <- Entities, interesting_install_vertex(Ent)],
	    [digraph:add_edge(Installs, E1, E2, Kind) || F={Kind, E1, E2} <- Relationships, interesting_install_edge(F)],
	    Order = lists:reverse(digraph_utils:topsort(Installs)),
	    [calc_one_config(Entity, Tree, Commit, State) || Entity <- Order],
	    see(Commit, State)
    end.

path_for_kind(feature) ->
    <<"features">>;
path_for_kind(group) ->
    <<"groups">>;
path_for_kind(node) ->
    <<"nodes">>;
path_for_kind(parameter) ->
    <<"parameters">>;
path_for_kind(X) when is_atom(X) ->
    list_to_binary(atom_to_list(X) ++ "s").

%% XXX:  should store these in lightweight format
calc_one_config({Kind, Name}, Tree, Commit, #cstate{re=RE, table=Cache, storage=StoreMod}=State) 
  when Kind =:= 'feature' ->
    %% get feature object from tree
    {value, EntityObj} = wallaroo_tree:get_path([path_for_kind(Kind), Name], Tree, StoreMod),
    %% apply included feature configs to empty config; these should already be in the cache
    BaseConfig = lists:foldl(apply_factory(true, State), [], [cache_fetch(Kind, Included, Commit, State) || Included <- lists:reverse(wallaby_feature:includes(EntityObj))]),
    %% resolve "defaulted" parameters
    MyConfig = orddict:map(fun(Param, 0) ->
				   {value, PObj} = wallaroo_tree:get_path([path_for_kind(parameter), Param], Tree, StoreMod),
				   wallaby_parameter:default_val(PObj);
			      (_, V) ->
				   V
			   end, wallaby_feature:parameters(EntityObj)),
    %% apply params to generated config and store in cache
    cache_store(Kind, Name, Commit, apply_to(BaseConfig, MyConfig, true, State), State);
calc_one_config({Kind, Name}, Tree, Commit, #cstate{re=RE, table=Cache, storage=StoreMod}=State) 
  when Kind =:= 'group' ->
    ?D_LOG("wallaby_config:calc_one_config/4:  {~p,~p}, ~p, ~p~n", [Kind, Name, Tree, Commit]),
    %% get group object from tree
    {value, EntityObj} = wallaroo_tree:get_path([path_for_kind(Kind), Name], Tree, StoreMod),
    %% apply installed feature configs to empty config; these should already be in the cache
    BaseConfig = lists:foldl(apply_factory(true, State), [], [cache_fetch(feature, Installed, Commit, State) || Installed <- lists:reverse(wallaby_group:features(EntityObj))]),
    %% apply my parameters to the base config
    cache_store(Kind, Name, Commit, apply_to(BaseConfig, wallaby_group:parameters(EntityObj), true, State), State); 
calc_one_config({Kind, Name}, Tree, Commit, #cstate{re=RE, table=Cache, storage=StoreMod}=State) 
  when Kind =:= 'node' ->
    %% get node object from tree
    {value, EntityObj} = wallaroo_tree:get_path([path_for_kind(Kind), Name], Tree, StoreMod),
    Config = {wallaby_lw_config, Commit, wallaby_node:all_memberships(EntityObj)},
    cache_store(Kind, Name, Commit, Config, State).
    
reconstitute_config({wallaby_lw_config, Commit, Memberships}, State) ->
    lists:foldl(apply_factory(false, State), [], [cache_fetch(group, Membership, Commit, State) || Membership <- lists:reverse(Memberships)]);
reconstitute_config(Val, _) ->
    Val.

join_factory(Bin) when is_binary(Bin) ->
    fun(Old, New) ->
	    <<New/binary, Bin/binary, Old/binary>>
    end.

combine_factory(<<">=">>) ->
    join_factory(<<", ">>);
combine_factory(<<"&&=">>) ->
    join_factory(<<" && ">>);
combine_factory(<<"&=">>) ->
    join_factory(<<" && ">>);
combine_factory(<<"||=">>) ->
    join_factory(<<" || ">>);
combine_factory(<<"|=">>) ->
    join_factory(<<" || ">>);
combine_factory(<<"?=">>) ->
    fun(Old, New) -> Old end.

apply_val_factory(RE, SSPrepend) ->
    fun(_, Old, New) ->
	 StrippedOld = case re:run(Old, RE, [{capture, all_but_first, binary}]) of
			   {match, [OPP, OV]} ->
			       OV;
			   nomatch ->
			       Old
		       end,
	 case re:run(New, RE, [{capture, all_but_first, binary}]) of
	     {match, [Prepend, Val]} ->
		 F = combine_factory(Prepend),
		 case SSPrepend of
		     true ->
			 NewVal = F(StrippedOld, Val),
			 <<Prepend/binary, 32, NewVal/binary>>;
		     false ->
			 F(StrippedOld, Val)
		 end;
	     nomatch ->
		 New
	 end
    end.

strip_prefix(Val, RE, SSP) ->
    case re:run(Val, RE, [{capture, all_but_first, binary}]) of
	{match, [P,V]} ->
	    case SSP of
		true ->
		    <<P/binary, 32, V/binary>>;
		false ->
		    <<V/binary>>
	    end;
	nomatch ->
	    Val
    end.

strip_prefixes(Ls, RE, SSP) ->
    [{K, strip_prefix(V, RE, SSP)} || {K,V} <- Ls].

apply_to(BaseConfig, NewConfig, SSPrepend) ->
    {ok, RE} = re:compile(list_to_binary("(?-mix:^(?:(>=|&&=|\\?=|\\|\\|=)\\s*)+(.*?)\\s*$)")),
    apply_to(BaseConfig, NewConfig, SSPrepend, #cstate{re=RE}).

apply_to(BaseConfig, NewConfig, SSPrepend, #cstate{re=RE}=State) ->
    strip_prefixes(orddict:merge(apply_val_factory(RE, SSPrepend), BaseConfig, NewConfig), RE, SSPrepend).

apply_factory(SSPrepend, State) ->
    fun(BaseConfig, NewConfig) ->
	    apply_to(BaseConfig, NewConfig, SSPrepend, State)
    end.

cache_store(Kind, Name, Commit, Config, #cstate{table=Cache}) ->
    ets:insert(Cache, {{Kind, Name, Commit}, Config}),
    Config.

cache_find(Kind, Name, Commit, #cstate{table=Cache}) ->
    ?D_LOG("cache_find/4 Kind=~p, Name=~p, Commit=~p~n", [Kind, Name, Commit]),
    case ets:match(Cache, {{Kind, Name, Commit}, '$1'}) of
	[[Config]] ->
	    {value, Config};
	 _ ->
	    find_failed
    end.

see(Commit, #cstate{table=Cache}) ->
    ets:insert(Cache, {Commit, seen}).

seen(Commit, #cstate{table=Cache}) ->
    case ets:match(Cache, {Commit, seen}) of
	[[{Commit, seen}|_]] ->
	    true;
	_ ->
	    false
    end.

%% XXX:  after changing how cache_fetch works, there's no reason to keep both it and _find
cache_fetch(Kind, Name, Commit, State) ->
    cache_fetch(Kind, Name, Commit, State, []).

cache_fetch(Kind, Name, Commit, State, Default) when not is_function(Default) ->
    cache_fetch(Kind, Name, Commit, State, fun(_,_) -> Default end);
cache_fetch(Kind, Name, Commit, State, Default) ->
    case cache_find(Kind, Name, Commit, State) of
	{value, Result} ->
	    Result;
	find_failed ->
	    Default(Commit, State)
    end.

generic_find(Kind, Name, Commit, CommitObj, State) ->
    generic_find(Kind, Name, Commit, CommitObj, State, []).

generic_find(Kind, Name, Commit, CommitObj, State, Default) ->
    case cache_find(Kind, Name, Commit, State) of
	{value, Config} ->
	    ?D_LOG("Config is ~p~n", [Config]),
	    {value, reconstitute_config(Config, State)};
	 find_failed ->
	    ?D_LOG("calculating configs for Commit=~p~n", [Commit]),
	    calc_configs(Commit, CommitObj, State),
	    {value, reconstitute_config(cache_fetch(Kind, Name, Commit, State, Default), State)}
    end.
