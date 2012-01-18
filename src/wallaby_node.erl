% @doc Classic Wallaby node functionality, reimplemented for Wallaroo.
% The main important difference between this and classic Wallaby is that
% last_checkin and last_updated_version are no longer maintained within 
% the node structure.
% @copyright 2011-2012 Red Hat, Inc. and William C. Benton
% @author William Benton <willb@redhat.com>

-module(wallaby_node).
-export([new/2, name/1, provisioned/1, identity_group/1, memberships/1, set_memberships/2, make_provisioned/2]).
-export_type([node/0]).

-define(WALLABY_NODE_TAG, wallaby_node).

-type node() :: {?WALLABY_NODE_TAG, orddict()}.

% @doc Returns a new Wallaby node structure.
-spec new(string(), boolean()) -> node().
new(Name, Provisioned) ->
    Dict = orddict:from_list([{name, Name}, {memberships, []}, {identity_group, "+++" + ""}, {provisioned, Provisioned}]),
    {?WALLABY_NODE_TAG, Dict}.

% @doc Returns the name of the given node.
-spec name(node()) -> string().
name({?WALLABY_NODE_TAG, Dict}) -> orddict:fetch(name, Dict).

% @doc Returns whether or not the given node is provisioned.
-spec provisioned(node()) -> boolean().
provisioned({?WALLABY_NODE_TAG, Dict}) -> orddict:fetch(provisioned, Dict).

% @doc Returns the name of the given node's identity group.
-spec identity_group(node()) -> string().
identity_group({?WALLABY_NODE_TAG, Dict}) -> orddict:fetch(identity_group, Dict).

% @doc Returns a list of the names of the groups that this node is a member of, 
% not including identity or default groups.
-spec memberships(node()) -> [string()].
memberships({?WALLABY_NODE_TAG, Dict}) -> orddict:fetch(memberships, Dict).

% @doc Sets the membership list for this node; the supplied list is not checked
% to ensure that its constituents all refer to valid groups.  Returns a new term 
% representing this node with the changed memberships.
-spec set_memberships(node(), [string()]) -> node().
set_memberships({?WALLABY_NODE_TAG, Dict}, Memberships) ->
    {?WALLABY_NODE_TAG, orddict:store(memberships, Memberships, Dict)}.

% @doc Returns a new term representing this node after it has been marked as
% explicitly provisioned.
-spec make_provisioned(node()) -> node().
make_provisioned({?WALLABY_NODE_TAG, Dict}) ->
    {?WALLABY_NODE_TAG, orddict:store(provisioned, true, Dict)}.
