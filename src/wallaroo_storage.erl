% Interface to be satisfied by all Wallaroo storage backends
% Copyright (c) 2011 Red Hat, Inc., and William C. Benton

-module(wallaroo_storage).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init,1},
     {start,1},
     {cleanup,1},
     {find_object,1},
     {find_commit,1},
     {find_tag,1},
     {store_object, 2},
     {store_commit, 2},
     {store_tag, 2},
     {objects,0},
     {tags,0},
     {commits,0}];
behaviour_info(_Other) ->
    undefined.
