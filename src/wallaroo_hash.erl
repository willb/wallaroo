% hash stuff for Wallaroo
% Copyright (c) 2011 Red Hat, Inc., and William C. Benton

-module(wallaroo_hash).
-export([as_bitstring/1, as_string/1, as_num/1, hash_to_bitstring/1, hash_to_num/1]).

as_bitstring(Object) ->
    crypto:md5(term_to_binary(Object)).

as_string(Object) ->
    lists:flatten(io_lib:format("~32.16.0b", [as_num(Object)])).

as_num(Object) ->
    <<SHA:128/big-unsigned-integer>> = as_bitstring(Object),
    SHA.

hash_to_bitstring(S) ->
    << <<(wallaroo_db:hexdigit_to_int(C)):4>> || C <- S >>.

hash_to_num(S) ->
    <<SHA:128/big-unsigned-integer>> = hash_to_bitstring(S),
    SHA.

hexdigit_to_int(I) when I =< $9 andalso I >= $0 ->
    I - $0;
hexdigit_to_int(I) when I =< $f andalso I >= $a ->
    I - $a + 10;
hexdigit_to_int(I) when I =< $F andalso I >= $A ->
    I - $A + 10.
