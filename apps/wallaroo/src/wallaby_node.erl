% @doc Classic Wallaby node functionality, reimplemented for Wallaroo.
% The main important difference between this and classic Wallaby is that
% last_checkin and last_updated_version are no longer maintained within 
% the node structure.
% @copyright 2011-2012 Red Hat, Inc. and William C. Benton
% @author William Benton <willb@redhat.com>

-module(wallaby_node).
-export([new/2, name/1, provisioned/1, identity_group/1, memberships/1, all_memberships/1, set_memberships/2, make_provisioned/1]).
-export_type([wnode/0]).

-define(WALLABY_NODE_TAG, wallaby_node).

-type wnode() :: {?WALLABY_NODE_TAG, orddict:orddict()}.

% @doc Returns a new Wallaby node structure.
-spec new(binary(), boolean()) -> wnode().
new(Name, Provisioned) ->
    Memberships = case application:get_env(wallaroo, enable_skeleton_group) of
		      {ok, false} ->
			  [];
		      _ ->
			  [<<"+++SKEL">>]
		  end,
    Dict = orddict:from_list([{name, Name}, {memberships, Memberships}, {identity_group, idgroupname(Name)}, {provisioned, Provisioned}]),
    {?WALLABY_NODE_TAG, Dict}.

% @doc Returns the name of the given node.
-spec name(wnode()) -> binary().
name({?WALLABY_NODE_TAG, Dict}) -> orddict:fetch(name, Dict).

% @doc Returns whether or not the given node is provisioned.
-spec provisioned(wnode()) -> boolean().
provisioned({?WALLABY_NODE_TAG, Dict}) -> orddict:fetch(provisioned, Dict).

% @doc Returns the name of the given node's identity group.
-spec identity_group(wnode()) -> binary().
identity_group({?WALLABY_NODE_TAG, Dict}) -> orddict:fetch(identity_group, Dict).

% @doc Returns a list of the names of the groups that this node is a member of, 
% not including identity or default groups.
-spec memberships(wnode()) -> [binary()].
memberships({?WALLABY_NODE_TAG, Dict}) -> orddict:fetch(memberships, Dict).

% @doc Returns a list of the names of the groups that this node is a member of, 
% including identity or default groups.
-spec all_memberships(wnode()) -> [binary()].
all_memberships({?WALLABY_NODE_TAG, Dict}=Node) -> 
    [identity_group(Node) | orddict:fetch(memberships, Dict) ++ [<<"+++DEFAULT">>]].

% @doc Sets the membership list for this node; the supplied list is not checked
% to ensure that its constituents all refer to valid groups.  Returns a new term 
% representing this node with the changed memberships.
-spec set_memberships(wnode(), [binary()]) -> wnode().
set_memberships({?WALLABY_NODE_TAG, Dict}, Memberships) ->
    {?WALLABY_NODE_TAG, orddict:store(memberships, Memberships, Dict)}.

% @doc Returns a new term representing this node after it has been marked as
% explicitly provisioned.
-spec make_provisioned(wnode()) -> wnode().
make_provisioned({?WALLABY_NODE_TAG, Dict}) ->
    {?WALLABY_NODE_TAG, orddict:store(provisioned, true, Dict)}.

-spec idgroupname(binary()) -> binary().
idgroupname(Name) ->
    <<Hash:128/big-unsigned-integer>> = crypto:md5(Name),
    [MD5] = io_lib:format("~32.16.0b", [Hash]),
    list_to_binary("+++" ++ MD5).

