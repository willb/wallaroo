%% @author William C. Benton <willb@redhat.com>
%% @copyright 2012 Red Hat, Inc. and William C. Benton
%% @doc Wallaby configuration-generation support

-module(wallaby_config).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([for/3]).

-compile([export_all]).

-record(cstate, {re, table, storage}).

-define(SERVER, ?MODULE).

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

for(Kind, Name, Commit) ->
    gen_server:call(?SERVER, {config_for, Kind, Name, Commit}).

%%% gen_server callbacks
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_call({config_for, Kind, Name, Commit}, _From, #cstate{re=RE, table=Cache, storage=StoreMod}=State) ->
    
    {reply, [], State}.

handle_info(_X, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%% helpers


%% XXX: refactor me plz
transitively_reachable(Graph, StartNode) ->
    ordsets:from_list(digraph_utils:reachable_neighbours([StartNode], Graph)).

transitively_reachable(Graph, StartNode, Filter) ->
    [Node || Node <- transitively_reachable(Graph, StartNode), Filter(Node)].

interesting_install_edge({Kind, _, {'feature', _}}) when Kind =:= 'includes' orelse Kind =:= 'installs' ->
    true;
interesting_install_edge(_) ->
    false.

interesting_install_vertex({'feature', _}) ->
    true;
interesting_install_vertex({'group', _}) -> 
    true;
interesting_install_vertex(_) -> 
    false.

%% interesting_value_edge({'param_value', _, _}) ->
%%     true;
%% interesting_value_edge(X) ->
%%     interesting_install_edge(X).

calc_configs(Commit, #cstate{re=RE, table=Cache, storage=StoreMod}=State) ->
    Tree = wallaby_commit:get_tree(Commit, StoreMod),
    {Entities, Relationships} = wallaby_graph:extract_graph(Tree, StoreMod),
    Installs = digraph:new([private]),
    [digraph:add_vertex(Installs, Ent) || Ent <- Entities, interesting_install_vertex(Ent)],
    [digraph:add_edge(Installs, E1, E2, Kind) || F={Kind, E1, E2} <- Relationships, interesting_install_edge(F)],
    Order = lists:reverse(digraph_utils:topsort(Installs)),
    ok.

calc_one_config({'feature', Name}, Tree, Commit, #cstate{re=RE, table=Cache, storage=StoreMod}) ->
    ok.

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
	 case re:run(New, RE, [{capture, all_but_first, binary}]) of
	     {match, [Prepend, Val]} ->
		 F = combine_factory(Prepend),
		 case SSPrepend of
		     true ->
			 NewVal = F(Old, Val),
			 <<Prepend/binary, 32, NewVal/binary>>;
		     false ->
			 F(Old, Val)
		 end;
	     nomatch ->
		 New
	 end
    end.
		     
apply_to(BaseConfig, NewConfig, SSPrepend) ->
    {ok, RE} = re:compile(list_to_binary("(?-mix:^(?:(>=|&&=|\\?=|\\|\\|=)\\s*)+(.*?)\\s*$)")),
    apply_to(BaseConfig, NewConfig, SSPrepend, #cstate{re=RE}).

apply_to(BaseConfig, NewConfig, SSPrepend, #cstate{re=RE}=State) ->
    orddict:merge(apply_val_factory(RE, SSPrepend), BaseConfig, NewConfig).

cache_store(Kind, Name, Commit, Config, #cstate{table=Cache}) ->
    ets:insert(Cache, {{Kind, Name, Commit}, Config}).

cache_find(Kind, Name, Commit, #cstate{table=Cache}) ->
    ok.

generic_find(Kind, Name, Commit, #cstate{table=Cache}) ->
    case ets:match(Cache, {{Kind, Name, Commit}, '$1'}) of
	[[Config]] ->
	    Config;
	 _ ->
	    find_failed
    end.
