% Classic Wallaby feature functionality
% Copyright (c) 2011 Red Hat, Inc. and William C. Benton

-module(wallaby_feature).
-export([new/1,name/1,features/1,parameters/1,set_features/2,set_parameters/2]).
-export_type([group/0]).

-define(FEATURE_TUPLE_TAG, wallaby_feature).

-type feature() :: {?FEATURE_TUPLE_TAG, orddict()}.

-spec new(binary()) -> feature().
new(Name) when is_binary(Name) ->
    Dict = orddict:from_list([{name, Name}, {includes, []}, {depends, []}, {conflicts, []}, {parameters, []}]),
    {?FEATURE_TUPLE_TAG, Dict}.

-spec name(feature()) -> binary().
name({?FEATURE_TUPLE_TAG, Dict}) -> orddict:fetch(name, Dict).

-spec features(feature()) -> [binary()].
includes({?FEATURE_TUPLE_TAG, Dict}) -> orddict:fetch(includes, Dict).

-spec conflicts(feature()) -> [binary()].
conflicts({?FEATURE_TUPLE_TAG, Dict}) -> orddict:fetch(conflicts, Dict).

-spec depends(feature()) -> [binary()].
depends({?FEATURE_TUPLE_TAG, Dict}) -> orddict:fetch(depends, Dict).

-spec parameters(feature()) -> [binary()].
parameters({?FEATURE_TUPLE_TAG, Dict}) -> orddict:fetch(parameters, Dict).

set_includes({?FEATURE_TUPLE_TAG, Dict}, Fs) ->
    {?FEATURE_TUPLE_TAG, orddict:store(includes, Fs, Dict)}.
set_depends({?FEATURE_TUPLE_TAG, Dict}, Fs) ->
    {?FEATURE_TUPLE_TAG, orddict:store(depends, orddict:from_list(Fs), Dict)}.
set_conflicts({?FEATURE_TUPLE_TAG, Dict}, Fs) ->
    {?FEATURE_TUPLE_TAG, orddict:store(conflicts, orddict:from_list(Fs), Dict)}.
set_parameters({?FEATURE_TUPLE_TAG, Dict}, Ps) ->
    {?FEATURE_TUPLE_TAG, orddict:store(parameters, Ps, Dict)}.
