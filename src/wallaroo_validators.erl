% (Abstract) tree validators for Wallaroo commits and tags
% Copyright (c) 2011 Red Hat, Inc., and William C. Benton

-module(wallaroo_validators).
-export([succeed/2, compose/1]).

succeed(_Tree, _StoreMod) ->
    ok.

compose([]) ->    
    fun(T,S) -> succeed(T,S) end;
compose([V|Vs]) when is_function(V,2) -> 
    K = compose(Vs),
    fun(Tree, StoreMod) ->
	    case V(Tree, StoreMod) of
		ok ->
		    K(Tree, StoreMod);
		{fail, _Why}=F ->
		    F
	    end
    end.

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
       ?_assertEqual(ok, (compose([fun always/2, fun sometimes/2, fun always/2]))(work, bogus_mod)),
       ?_assertEqual({fail, "work specified"}, (compose([fun always/2, fun bizarro_sometimes/2]))(work, bogus_mod)),
       ?_assertEqual({fail, "nowork specified"}, (compose([fun always/2, fun bizarro_sometimes/2, fun sometimes/2]))(nowork, bogus_mod)),
       ?_assertEqual(ok, (compose([fun always/2, fun sometimes/2, fun bizarro_sometimes/2]))(maybe_work, bogus_mod))]}}.

-endif.
