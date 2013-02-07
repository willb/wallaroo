%% @author William C. Benton <willb@redhat.com>
%% @copyright 2013 Red Hat, Inc. and William C. Benton
%% @doc Wallaby validation-result cache

-module(wallaby_vcache).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/0]).

-export([for_commit/1, for_commit/2, cache_dump/0, reset/0]).

-record(vcstate, {table, storage}).

-define(SERVER, ?MODULE).

start_link() ->
    Result = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    error_logger:info_msg("wallaby_vcache:start_link result ~p~n", [Result]),
    Result.

init(Options) when is_list(Options) ->
    Table = init_table(),
    % XXX: use application env here
    StoreMod = case orddict:find(storage_module, Options) of
		   {ok, Mod} ->
		       Mod;
		   error ->
		       wallaroo_store_ets
	       end,
    {ok, #vcstate{table=Table, storage=StoreMod}};
init(_) ->
    init([]).

init_table() -> ets:new(vcache, []).

%%% API functions

for_commit(Commit) ->
    for_commit(Commit, vfun()).
for_commit(Commit, VFun) ->
    gen_server:call(?SERVER, {for_commit, Commit, VFun}).

cache_dump() ->
    gen_server:call(?SERVER, {cache_dump}).

reset() ->
    gen_server:call(?SERVER, {reset}).

%%% gen_server callbacks
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_call({reset}, _From, #vcstate{table=C}=OldState) ->
    ets:delete(C),
    State = OldState#vcstate{table=init_table()},
    {reply, ok, State};
handle_call({cache_dump}, _From, #vcstate{table=C}=State) ->
    {reply, ets:tab2list(C), State};
handle_call({for_commit, Commit, VFun}, _From, State) ->
    Result = generic_lookup(Commit, State, VFun),
    case Result of
	{value, V} ->
	    {reply, V, State};
	_ ->
	    {reply, Result, State}
    end.

handle_info(_X, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%% helpers

%%% NB:  the general flow for finding a validation is this:
%%% 1.  determine if the commit is valid or not (if not, fail fast)
%%% 2.  see if the validation result is in the cache:
%%%      -- if so, return it
%%%      -- if not, calculate it, store it, and return it

generic_lookup(Commit, #vcstate{storage=StoreMod}=State, Default) ->
    CommitObj = StoreMod:find_commit(CCommit=wallaroo_hash:canonicalize(Commit)),
    case CommitObj of
	{wallaroo_commit, _} ->
	    find_or_calc(CCommit, CommitObj, State, Default);
	_ ->
	    {error, {bad_commit, Commit, CommitObj}}
    end.

vfun() ->
    fun(Tree, StoreMod) ->
	    V = wallaroo_validators:pcompose(wallaby_validators:make_activate_validators(Tree, StoreMod)),
	    V(Tree, StoreMod)
    end.

find_or_calc(Commit, CommitObj, State, Default) when not is_function(Default) ->
    find_or_calc(Commit, CommitObj, State, fun(_, _) -> Default end);
find_or_calc(Commit, CommitObj, #vcstate{storage=StoreMod}=State, V) ->
    case cache_find(Commit, State) of
	{value, Result} ->
	    {value, Result};
	 find_failed ->
	    Tree = wallaroo_commit:get_tree(CommitObj, StoreMod),
	    cache_store(Commit, V(Tree, StoreMod), State)
    end.

cache_store(Commit, Result, #vcstate{table=Cache}) ->
    ets:insert(Cache, {wallaroo_hash:canonicalize(Commit), Result}),
    Result.

cache_find(Commit, #vcstate{table=Cache}) ->
    case ets:match(Cache, {Commit, '$1'}) of
	[[Result]] ->
	    {value, Result};
	 _ ->
	    find_failed
    end.
