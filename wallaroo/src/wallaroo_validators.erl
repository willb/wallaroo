% (Abstract) tree validators for Wallaroo commits and tags
% Copyright (c) 2011 Red Hat, Inc., and William C. Benton

-module(wallaroo_validators).
-export([succeed/2, compose/1, pcompose/1, pcompose/2]).

-type validator_result() :: ('ok' | {'fail', _}).
-type validator() :: fun((wallaroo_tree:tree(), module()) -> validator_result()).
-export_type([validator_result/0, validator/0]).

succeed(_Tree, _StoreMod) ->
    ok.

-spec compose([validator()]) -> validator().		     
compose(Validators) when is_list(Validators) ->
    Srotadilav = lists:reverse(Validators),
    lists:foldl(fun compose_once/2, fun succeed/2, Srotadilav).

-spec compose_once(validator(), validator()) -> validator().		     
compose_once(Fun, AccFun) when is_function(Fun, 2), is_function(AccFun, 2) ->
    fun(Tree, StoreMod) ->
	    case Fun(Tree, StoreMod) of
		ok ->
		    AccFun(Tree, StoreMod);
		{fail, _Why}=F ->
		    F
	    end
    end.

-spec pcompose([validator()]) -> validator().		     
pcompose(Validators) when is_list(Validators) ->
    pcompose(Validators, []).

-spec pcompose([validator()], [atom()]) -> validator().		     
pcompose(Validators, Options) when is_list(Validators) andalso is_list(Options) ->
    Failfast = lists:member('failfast', Options),
    fun(Tree, StoreMod) ->
	    Self = self(),
	    EachFun = fun(V) -> spawn(fun() -> pcompose_worker(Self, V, Tree, StoreMod) end) end,
	    Pids = lists:map(EachFun, Validators),
	    %% FIXME:  probably not the best representation for very large validator lists
	    pcompose_loop(ordsets:from_list(Pids), Failfast)
    end.

-spec pcompose_loop([pid()]) -> validator_result().
pcompose_loop(Pids) ->
    pcompose_loop(Pids, false).

-spec pcompose_loop([pid()], boolean()) -> validator_result().
pcompose_loop(Pids, Failfast) ->
    pcompose_loop(Pids, [], Failfast).

-spec pcompose_loop([pid()], [validator_result()], boolean()) -> validator_result().
pcompose_loop([], [], _) ->
    ok;
pcompose_loop([], [Failure], _) ->
    Failure;
pcompose_loop([], Failures, _) ->
    {fail, 
     {multiple_failures, 
      lists:foldl(fun({fail, {multiple_failures, Ls}}, Acc) ->
			  Ls ++ Acc;
		     ({fail, F}, Acc) ->
			  [F | Acc]
		  end,
		  [],
		  Failures)}};
pcompose_loop(Pids, Failures, Failfast) ->
    receive
	{result_for, Pid, ok} ->
	    handle_success(Pid, Pids, Failures, Failfast);
	{result_for, Pid, {fail, _}=Failure} ->
	    handle_failure(Pid, Pids, Failure, Failures, Failfast);
	X ->
	    handle_failure(unknown, Pids, {fail, {other_validator_result, X}}, Failures, Failfast)	    
    end.

handle_success(Pid, Pids, Failures, Failfast) ->
    pcompose_loop(ordsets:del_element(Pid, Pids), Failures, Failfast).

-spec handle_failure(pid()|atom(), [pid()], validator_result(), [validator_result()], boolean()) -> validator_result().
			    
handle_failure(_Pid, Pids, Failure, _Failures, true) ->
    _ = [exit(P, kill) || P <- Pids],
    Failure;
handle_failure(Pid, Pids, Failure, Failures, false) ->
    pcompose_loop(ordsets:del_element(Pid, Pids), [Failure|Failures], false).

pcompose_worker(Parent, Validator, Tree, StoreMod) when is_function(Validator,2) ->
    Result = try
		 Validator(Tree, StoreMod)
	     catch
		 Kind:Reason -> {fail, {Kind, Reason}}
	     end,
    Parent ! {result_for, self(), Result}.

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
