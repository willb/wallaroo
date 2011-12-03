% hedge against future external term format changes, for Wallaroo
% Copyright (c) 2011 Red Hat, Inc., and William C. Benton

-module(wallaroo_binary).
-export([from_term/1, bc_from_term/1]).

% XXX: the external term format and term_to_binary should be stable
% across Erlang versions, i.e., some term T will always be encoded as
% exactly the same string of bits (not merely that T1 = term_to_binary(T),
% binary_to_term(T1) will produce T for any version of binary_to_term 
% and term_to_binary.  The term format hasn't changed in a *long* time.
%
% However, we have no guarantee that this will always be the case:
% Erlang releases are supposed to be forwards-compatible for two major
% releases, and backwards-compatibility should only be broken at a new
% major release.  In the unlikely event that the external term format
% changes in the future, we will have to reimplement the R13 version
% of term_to_binary here for our own use.  Because we use this function 
% instead of a raw call to the term_to_binary NIF, we will be able to do 
% this in the face of live updates.

from_term(T) ->
    term_to_binary(T, [{minor_version,1}]).


% backwards-compatible from_term
bc_from_term(T) ->
    <<131, (bc_from_term_internal(T))/binary>>.
bc_from_term_internal(Num) when is_integer(Num) andalso Num < 256 andalso Num >= 0 ->
    <<97, Num:8>>;
bc_from_term_internal(Num) when is_integer(Num) andalso Num < (1 bsl 31) andalso Num >= (-1 bsl 31) ->
    <<98, Num:32/big-signed-integer>>;
bc_from_term_internal(BigInt) when is_integer(BigInt) ->
    ABI = abs(BigInt),
    Bin = binary:encode_unsigned(ABI),
    Size = size(Bin),
    Sign = if BigInt =:= ABI -> 0; true -> 1 end,
    if Size < 256 ->
	    <<110, Size:8, Sign:8, Bin/binary>>;
       true ->
	    <<111, Size:32, Sign:8, Bin/binary>>
    end;
bc_from_term_internal(Float) when is_float(Float) ->
    <<70, Float/big-float>>;
bc_from_term_internal(Atom) when is_atom(Atom) ->
    Ls = atom_to_list(Atom),
    Bin = list_to_binary(Ls),
    Size = length(Ls),
    <<100, Size:16, Bin/binary>>;
bc_from_term_internal(Tup) when is_tuple(Tup) andalso tuple_size(Tup) < 256 ->
    Tag = 104,
    Arity = tuple_size(Tup),
    Elements = << <<Elt/binary>> || Elt <- lists:map(fun bc_from_term_internal/1, tuple_to_list(Tup)) >>,
    <<Tag:8, Arity:8, Elements/binary>>;
bc_from_term_internal(Tup) when is_tuple(Tup) andalso tuple_size(Tup) >= 256 ->
    Tag = 105,
    Arity = tuple_size(Tup),
    Elements = << <<Elt/binary>> || Elt <- lists:map(fun bc_from_term_internal/1, tuple_to_list(Tup)) >>,
    <<Tag:8, Arity:32, Elements/binary>>;
bc_from_term_internal([]) ->
    <<106>>;
bc_from_term_internal(Ls) when is_list(Ls) ->
    bc_from_list(Ls);
bc_from_term_internal(Bin) when is_binary(Bin) ->
    Tag = 109, Size = size(Bin),
    <<Tag:8, Size:32, Bin/binary>>.

bc_from_list(Ls) ->
    {Ct, Chars, Bin, IsStr, Tl} = bc_from_list_int(Ls, {0, <<>>, <<>>, true}),
    if IsStr andalso (Ct < 65536) ->
	    <<107, Ct:16, Chars/binary>>;
       true ->
	    EncodedTl = bc_from_term_internal(Tl),
	    <<108, Ct:32, Bin/binary, EncodedTl/binary>>
    end.

bc_from_list_int([H|T], {Ct, Chars, Bin, true} ) when is_integer(H), H >= 0, H < 256 ->
    bc_from_list_int(T, {Ct+1, <<Chars/binary, H:8>>, <<Bin/binary, (bc_from_term_internal(H))/binary>>, true});
bc_from_list_int([H|T], {Ct, Chars, Bin, _} ) ->
    bc_from_list_int(T, {Ct+1, Chars, <<Bin/binary, (bc_from_term_internal(H))/binary>>, false});
bc_from_list_int(X, {Ct, Chars, Bin, IsStr}) ->
    {Ct, Chars, Bin, IsStr, X}.

% test code

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

simple_test_() ->
    {inorder,
     {setup,
      fun() -> ok end,
      fun(_) -> ok end,
      [?_assertEqual(<<131,100,0,3,97,98,99>>, bc_from_term(abc)),
       ?_assertEqual(<<131,97,0>>, bc_from_term(0)),
       ?_assertEqual(<<131,97,1>>, bc_from_term(1)),
       ?_assertEqual(<<131,97,255>>, bc_from_term(255)),
       ?_assertEqual(<<131,98,0,0,1,0>>, bc_from_term(256)),
       ?_assertEqual(<<131,98,0,0,4,0>>, bc_from_term(1024)),
       ?_assertEqual(<<131,70,63,217,153,153,153,153,153,154>>, bc_from_term(0.4)),
       ?_assertEqual(<<131,98,255,255,255,236>>, bc_from_term(-20)),
       ?_assertEqual(<<131,110,16,0,255,255,255,255,255,255,255,255,255,255,255,255,
		       255,255,255,255>>, bc_from_term(340282366920938463463374607431768211455)),
       ?_assertEqual(<<131,110,16,1,255,255,255,255,255,255,255,255,255,255,255,255,
		       255,255,255,255>>, bc_from_term(-340282366920938463463374607431768211455)),
       ?_assertEqual(<<131,104,3,100,0,3,97,98,99,100,0,4,98,108,97,104,100,0,4,97,
		       114,103,104>>, bc_from_term({abc,blah,argh})),
       ?_assertEqual(<<131,107,0,7,102,111,111,115,101,112,104>>, bc_from_term("fooseph")),
       ?_assertEqual(<<131,108,0,0,0,5,98,0,0,4,0,100,0,3,102,111,111,100,0,4,98,108,
		       97,104,100,0,4,97,114,103,104,104,2,100,0,5,121,105,107,101,
		       115,98,255,255,255,219,106>>, bc_from_term([1024,foo,blah,argh,{yikes,-37}])), 
       ?_assertEqual(<<131,109,0,0,0,4,192,168,0,1>>, bc_from_term(<<192,168,0,1>>)),
       ?_assertEqual(<<131,106>>, bc_from_term([]))]}}.

-endif.
