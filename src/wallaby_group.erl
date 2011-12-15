% Classic Wallaby group functionality
% Copyright (c) 2011 Red Hat, Inc. and William C. Benton

-module(wallaby_group).
-compile(export_all).

-define(GROUP_TUPLE_TAG, wallaby_group).

new(Name) when is_binary(Name) ->
    Dict = orddict:from_list([{name, Name}, {features, []}, {parameters, nil}]),
    {?GROUP_TUPLE_TAG, Dict}.

name({?GROUP_TUPLE_TAG, Dict}) -> orddict:fetch(name, Dict).

features({?GROUP_TUPLE_TAG, Dict}) -> orddict:fetch(features, Dict).
parameters({?GROUP_TUPLE_TAG, Dict}) -> orddict:fetch(parameters, Dict).

set_features({?GROUP_TUPLE_TAG, Dict}, Fs) ->
    {?GROUP_TUPLE_TAG, orddict:store(features, Fs, Dict)}.
set_parameters({?GROUP_TUPLE_TAG, Dict}, Ps) ->
    {?GROUP_TUPLE_TAG, orddict:store(memberships, Ps, Dict)}.
