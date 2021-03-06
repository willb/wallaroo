% hash stuff for Wallaroo
% Copyright (c) 2011 Red Hat, Inc., and William C. Benton

-module(wallaroo_hash).
-export([as_bitstring/1, as_string/1, as_num/1, hash_to_bitstring/1, hash_to_num/1, stringize/1, canonicalize/1]).
-export_type([bin/0, hexdigit/0, hexstring/0]).

-type bin() :: <<_:160>>.
-type hexdigit() :: 48..57 | 65..70 | 97..102.
-type hexstring() :: [hexdigit(), ...].

-spec as_bitstring(any()) -> bin().
as_bitstring(Object) ->
    crypto:sha(wallaroo_binary:from_term(Object)).

-spec as_string(any()) -> hexstring().
as_string(Object) ->
    lists:flatten(io_lib:format("~40.16.0b", [as_num(Object)])).

as_num(Object) ->
    generic_to_num(Object, fun wallaroo_hash:as_bitstring/1).

generic_to_num(Object, F) ->
    <<SHA:160/big-unsigned-integer>> = F(Object),
    SHA.

-spec hash_to_bitstring(hexstring()) -> bin().
hash_to_bitstring(S) ->
    << <<(hexdigit_to_int(C)):4>> || C <- S >>.

hash_to_num(S) ->
    generic_to_num(S, fun wallaroo_hash:hash_to_bitstring/1).

bitstring_to_string(B) ->
    <<SHA:160/big-unsigned-integer>> = B,
    lists:flatten(io_lib:format("~40.16.0b", [SHA])).

hexdigit_to_int(I) when I =< $9 andalso I >= $0 ->
    I - $0;
hexdigit_to_int(I) when I =< $f andalso I >= $a ->
    I - $a + 10;
hexdigit_to_int(I) when I =< $F andalso I >= $A ->
    I - $A + 10.


canonicalize(String) when is_list(String) ->
    hash_to_bitstring(String);
canonicalize(BS) when is_binary(BS) ->
    BS.

stringize(B) when is_binary(B) ->
    <<SHA:160/big-unsigned-integer>> = B,
    lists:flatten(io_lib:format("~40.16.0b", [SHA]));
stringize(Str) when is_list(Str) ->
    Str.

