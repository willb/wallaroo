% Interface to be satisfied by all Wallaroo password
% hash backends and common utility functions
% Copyright (c) 2013 Red Hat, Inc., and William C. Benton

-module(wallaroo_password).
-export([behaviour_info/1, compare/2, hash/1, hash/2, hash/3, do_hash/3, random_salt/1]).

-export_type([key/0, salt/0, options/0, option/0, hash_result/0, hash_fun/0]).

-type key() :: binary().
-type hashedkey() :: binary().
-type salt() :: binary().
-type option() :: atom() | tuple().
-type options() :: [option()].
-type hash_result() :: {module(), hashedkey(), salt(), options()}.
-type hash_fun() :: fun((key(), salt(), options()) -> hash_result()).

-spec compare(key(), hash_result()) -> boolean().
compare(Pass, {Mod, _, Salt, Options}=Expected) ->
    Mod:do_hash(Pass, Salt, Options) =:= Expected.

hash(Pass) ->
    hash(wallaroo_password, Pass, []).

hash(Mod, Pass) ->
    hash(Mod, Pass, []).

hash(Mod, Pass, Options) ->
    Salt = random_salt(32),
    Mod:do_hash(Pass, Salt, Options).

-spec do_hash(key(), salt(), options()) -> hash_result().
do_hash(Pass, Salt, Options) ->
    Hash = wallaroo_hash:as_bitstring({Pass, Salt}),
    {?MODULE, Hash, Salt, Options}.

-spec random_salt(integer()) -> binary().
random_salt(Bytes) when is_integer(Bytes) ->
    try
	crypto:strong_rand_bytes(Bytes)
    catch 
	throw:low_entropy ->
	    crypto:rand_bytes(Bytes);
	error:undef ->
	    crypto:rand_bytes(Bytes)
    end.

behaviour_info(callbacks) ->
    [{do_hash, 3}];
behaviour_info(_Other) ->
    undefined.
