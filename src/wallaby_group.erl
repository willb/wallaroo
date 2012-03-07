% Classic Wallaby group functionality
% Copyright (c) 2011 Red Hat, Inc. and William C. Benton

-module(wallaby_group).
-export([new/1,name/1,features/1,parameters/1,set_features/2,set_parameters/2]).
-export_type([group/0]).

-define(GROUP_TUPLE_TAG, wallaby_group).

-type group() :: {?GROUP_TUPLE_TAG, orddict()}.

-spec new(binary()) -> group().
new(Name) when is_binary(Name) ->
    Dict = orddict:from_list([{name, Name}, {features, []}, {parameters, nil}]),
    {?GROUP_TUPLE_TAG, Dict}.

-spec name(group()) -> binary().
name({?GROUP_TUPLE_TAG, Dict}) -> orddict:fetch(name, Dict).

-spec features(group()) -> [binary()].
features({?GROUP_TUPLE_TAG, Dict}) -> orddict:fetch(features, Dict).

-spec parameters(group()) -> [binary()].
parameters({?GROUP_TUPLE_TAG, Dict}) -> orddict:fetch(parameters, Dict).

set_features({?GROUP_TUPLE_TAG, Dict}, Fs) ->
    {?GROUP_TUPLE_TAG, orddict:store(features, Fs, Dict)}.
set_parameters({?GROUP_TUPLE_TAG, Dict}, Ps) ->
    {?GROUP_TUPLE_TAG, orddict:store(memberships, Ps, Dict)}.
