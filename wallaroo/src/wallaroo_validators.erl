% (Abstract) tree validators for Wallaroo commits and tags
% Copyright (c) 2011 Red Hat, Inc., and William C. Benton

-module(wallaroo_validators).
-export([succeed/2, compose/1, pcompose/1]).

succeed(_Tree, _StoreMod) ->
    ok.

compose(Validators) when is_list(Validators) ->
    Srotadilav = lists:reverse(Validators),
    lists:foldl(fun compose_once/2, fun succeed/2, Srotadilav).

compose_once(Fun, AccFun) when is_function(Fun, 2), is_function(AccFun, 2) ->
    fun(Tree, StoreMod) ->
	    case Fun(Tree, StoreMod) of
		ok ->
		    AccFun(Tree, StoreMod);
		{fail, _Why}=F ->
		    F
	    end
    end.

pcompose(Validators) when is_list(Validators) ->
    fun(Tree, StoreMod) ->
	    Self = self(),
	    EachFun = fun(V) -> spawn(fun() -> pcompose_worker(Self, V, Tree, StoreMod) end) end,
	    Pids = lists:map(EachFun, Validators),
	    %% FIXME:  probably not the best representation for very large validator lists
	    pcompose_loop(ordsets:from_list(Pids))
    end.

pcompose_loop([]) ->
    ok;
pcompose_loop(Pids) ->
    receive
	{result_for, Pid, is, ok} ->
	    NewPidSet = ordsets:del_element(Pid, Pids),
	    pcompose_loop(NewPidSet);
	{result_for, _, is, {fail, _}=Failure} ->
	    lists:map(fun(P) -> exit(P, kill) end, Pids),
	    Failure;
	X ->
	    lists:map(fun(P) -> exit(P, kill) end, Pids),
	    {fail, {other_result, X}}
    end.

pcompose_worker(Parent, Validator, Tree, StoreMod) ->
    Result = try
		 Validator(Tree, StoreMod)
	     catch
		 Kind:Reason -> {fail, {Kind, Reason}}
	     end,
    Parent ! {result_for, self(), is, Result}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

always(_,_) ->
    ok.

never(_,_) ->
    {fail, because}.

sometimes(X,_) ->
    case X of
	nowork ->
	    {fail, "nowork specified"};
	_ -> ok
    end.

bizarro_sometimes(X,_) ->
    case X of
	work ->
	    {fail, "work specified"};
	_ -> ok
    end.

basic_test_() ->
    {inorder,
     {setup,
      fun() -> ok end,
      fun(_) -> ok end,
      [?_assertEqual(ok, (compose([]))(work, bogus_mod)),
       ?_assertEqual({fail, because}, (compose([fun always/2, fun never/2]))(work, bogus_mod)),
       ?_assertEqual({fail, because}, (compose([fun always/2, fun never/2, fun always/2]))(work, bogus_mod)),
       ?_assertEqual({fail, because}, (compose([fun always/2, fun never/2, fun bizarro_sometimes/2]))(work, bogus_mod)),
       ?_assertEqual(ok, (compose([fun always/2, fun sometimes/2]))(work, bogus_mod)),
       ?_assertEqual(ok, (compose([(compose([fun always/2, fun sometimes/2])),(compose([fun always/2, fun sometimes/2]))]))(work, bogus_mod)),
       ?_assertEqual(ok, (compose([fun always/2, fun sometimes/2, fun always/2]))(work, bogus_mod)),
       ?_assertEqual({fail, "work specified"}, (compose([fun always/2, fun bizarro_sometimes/2]))(work, bogus_mod)),
       ?_assertEqual({fail, "nowork specified"}, (compose([fun always/2, fun bizarro_sometimes/2, fun sometimes/2]))(nowork, bogus_mod)),
       ?_assertEqual(ok, (compose([fun always/2, fun sometimes/2, fun bizarro_sometimes/2]))(maybe_work, bogus_mod))]}}.


pcompose_test_() ->
    {inorder,
     {setup,
      fun() -> ok end,
      fun(_) -> ok end,
      [?_assertEqual(ok, (pcompose([]))(work, bogus_mod)),
       ?_assertEqual({fail, because}, (pcompose([fun always/2, fun never/2]))(work, bogus_mod)),
       ?_assertEqual({fail, because}, (pcompose([fun always/2, fun never/2, fun always/2]))(work, bogus_mod)),
       ?_assertMatch({fail, _}, (pcompose([fun always/2, fun never/2, fun bizarro_sometimes/2]))(work, bogus_mod)),
       ?_assertMatch({fail, _}, (pcompose([fun always/2, fun (_,_) -> throw(some_other_failure) end]))(work, bogus_mod)),
       ?_assertEqual(ok, (pcompose([fun always/2, fun sometimes/2]))(work, bogus_mod)),
       ?_assertEqual(ok, (pcompose([(compose([fun always/2, fun sometimes/2])),(compose([fun always/2, fun sometimes/2]))]))(work, bogus_mod)),
       ?_assertEqual(ok, (pcompose([fun always/2, fun sometimes/2, fun always/2]))(work, bogus_mod)),
       ?_assertMatch({fail, [_|_]}, (pcompose([fun always/2, fun bizarro_sometimes/2]))(work, bogus_mod)),
       ?_assertMatch({fail, [_|_]}, (pcompose([fun always/2, fun bizarro_sometimes/2, fun sometimes/2]))(nowork, bogus_mod)),
       ?_assertEqual(ok, (pcompose([fun always/2, fun sometimes/2, fun bizarro_sometimes/2]))(maybe_work, bogus_mod))
      ]}}.

-endif.
