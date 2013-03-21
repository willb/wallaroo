% Interface to be satisfied by all Wallaroo storage backends
% Copyright (c) 2011 Red Hat, Inc., and William C. Benton

-module(wallaroo_storage).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init,1},
     {start,1},
     {cleanup,1},
     {delete_object,1},
     {delete_commit,1},
     {delete_tag,1},
     {delete_branch,1},
     {delete_meta,2},
     {find_object,1},
     {find_commit,1},
     {find_meta, 1},
     {find_meta, 2},
     {find_tag,1},
     {store_object, 2},
     {store_commit, 2},
     {store_tag, 2},
     {store_meta, 3},
     {store_branch, 2},
     {find_user, 1},
     {delete_user, 1},
     {store_user, 2},
     {objects,0},
     {users,0},
     {tags,0},
     {commits,0},
     {branches,0},
     {meta,0}];
behaviour_info(_Other) ->
    undefined.
