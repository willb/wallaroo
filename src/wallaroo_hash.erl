% hash stuff for Wallaroo
% Copyright (c) 2011 Red Hat, Inc., and William C. Benton

-module(wallaroo_hash).
-export([as_bitstring/1, as_string/1, as_num/1, hash_to_bitstring/1, hash_to_num/1]).

% XXX:  we need to verify that term_to_binary is stable across
% Erlang versions, i.e., that some term T will always be encoded
% as exactly the same string of bits (not merely that 
% binary_to_term(term_to_binary(T)) will produce T for any version 
% of binary_to_term and term_to_binary.
% 
% If not, we'll have to integrate or implement a stable serializer.
as_bitstring(Object) ->
    crypto:sha(term_to_binary(Object)).

as_string(Object) ->
    lists:flatten(io_lib:format("~40.16.0b", [as_num(Object)])).

as_num(Object) ->
    generic_to_num(Object, fun wallaroo_hash:as_bitstring/1).

generic_to_num(Object, F) ->
    <<SHA:160/big-unsigned-integer>> = F(Object),
    SHA.

hash_to_bitstring(S) ->
    << <<(hexdigit_to_int(C)):4>> || C <- S >>.

hash_to_num(S) ->
    generic_to_num(S, fun wallaroo_hash:hash_to_bitstring/1).

hexdigit_to_int(I) when I =< $9 andalso I >= $0 ->
    I - $0;
hexdigit_to_int(I) when I =< $f andalso I >= $a ->
    I - $a + 10;
hexdigit_to_int(I) when I =< $F andalso I >= $A ->
    I - $A + 10.
