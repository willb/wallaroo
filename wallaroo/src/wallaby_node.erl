% Classic Wallaby node functionality
% Copyright (c) 2011 Red Hat, Inc. and William C. Benton

-module(wallaby_node).
-compile(export_all).

new(Name, Provisioned) ->
    Dict = orddict:from_list([{name, Name}, {memberships, []}, {identity_group, nil}, {provisioned, Provisioned}, {last_checkin, 0}, {last_updated_version, 0}]),
    {wallaby_node, Dict}.

name({wallaby_node, Dict}) -> orddict:fetch(name, Dict).

provisioned({wallaby_node, Dict}) -> orddict:fetch(provisioned, Dict).

last_checkin({wallaby_node, Dict}) -> orddict:fetch(last_checkin, Dict).

last_updated_version({wallaby_node, Dict}) -> orddict:fetch(last_updated_version, Dict).

identity_group({wallaby_node, Dict}) -> orddict:fetch(identity_group, Dict).

memberships({wallaby_node, Dict}) -> orddict:fetch(memberships, Dict).

set_memberships({wallaby_node, Dict}, Memberships) ->
    {wallaby_node, orddict:store(memberships, Memberships, Dict)}.

make_provisioned({wallaby_node, Dict}) ->
    {wallaby_node, orddict:store(provisioned, true, Dict)}.
