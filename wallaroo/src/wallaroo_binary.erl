% hedge against future external term format changes, for Wallaroo
% Copyright (c) 2011 Red Hat, Inc., and William C. Benton

-module(wallaroo_binary).
-export([from_term/1]).

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
    term_to_binary(T).
