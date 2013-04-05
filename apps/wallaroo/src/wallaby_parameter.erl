% Classic Wallaby parameter functionality
% Copyright (c) 2011 Red Hat, Inc. and William C. Benton

-module(wallaby_parameter).
-export([new/1,name/1,depends/1,conflicts/1,kind/1,description/1,default_val/1,must_change/1,visibility_level/1,requires_restart/1,set_conflicts/2,set_depends/2,set_default_val/2,set_description/2,set_must_change/2,set_kind/2,set_visibility_level/2,set_requires_restart/2]).
-export_type([parameter/0]).

-define(PARAMETER_TUPLE_TAG, wallaby_parameter).

-type parameter() :: {?PARAMETER_TUPLE_TAG, orddict:orddict()}.

-spec new(binary()) -> parameter().
new(Name) when is_binary(Name) ->
    Dict = orddict:from_list([{name, Name}, {description, nil}, {depends, []}, {conflicts, []}, {kind, nil}, {must_change, false}, {default_val, nil}, {visibility_level, 0}, {requires_restart, false}]),
    {?PARAMETER_TUPLE_TAG, Dict}.

-spec name(parameter()) -> binary().
name({?PARAMETER_TUPLE_TAG, Dict}) -> orddict:fetch(name, Dict).

-spec description(parameter()) -> binary().
description({?PARAMETER_TUPLE_TAG, Dict}) -> orddict:fetch(description, Dict).

-spec kind(parameter()) -> binary().
kind({?PARAMETER_TUPLE_TAG, Dict}) -> orddict:fetch(kind, Dict).

-spec conflicts(parameter()) -> [binary()].
conflicts({?PARAMETER_TUPLE_TAG, Dict}) -> orddict:fetch(conflicts, Dict).

-spec depends(parameter()) -> [binary()].
depends({?PARAMETER_TUPLE_TAG, Dict}) -> orddict:fetch(depends, Dict).

-spec must_change(parameter()) -> boolean().
must_change({?PARAMETER_TUPLE_TAG, Dict}) -> orddict:fetch(must_change, Dict).

-spec default_val(parameter()) -> any().
default_val({?PARAMETER_TUPLE_TAG, Dict}) -> orddict:fetch(default_val, Dict).

-spec visibility_level(parameter()) -> integer().
visibility_level({?PARAMETER_TUPLE_TAG, Dict}) -> orddict:fetch(visibility_level, Dict).

-spec requires_restart(parameter()) -> boolean().
requires_restart({?PARAMETER_TUPLE_TAG, Dict}) -> orddict:fetch(requires_restart, Dict).


set_description({?PARAMETER_TUPLE_TAG, Dict}, D) when is_binary(D) ->
    {?PARAMETER_TUPLE_TAG, orddict:store(description, D, Dict)}.
set_kind({?PARAMETER_TUPLE_TAG, Dict}, K) when is_binary(K) ->
    {?PARAMETER_TUPLE_TAG, orddict:store(kind, K, Dict)}.
set_depends({?PARAMETER_TUPLE_TAG, Dict}, Fs) ->
    {?PARAMETER_TUPLE_TAG, orddict:store(depends, ordset:from_list(Fs), Dict)}.
set_conflicts({?PARAMETER_TUPLE_TAG, Dict}, Fs) ->
    {?PARAMETER_TUPLE_TAG, orddict:store(conflicts, ordset:from_list(Fs), Dict)}.
set_must_change({?PARAMETER_TUPLE_TAG, Dict}, MC) when is_boolean(MC) ->
    {?PARAMETER_TUPLE_TAG, orddict:store(must_change, MC, Dict)}.
set_default_val({?PARAMETER_TUPLE_TAG, Dict}, DV) ->
    {?PARAMETER_TUPLE_TAG, orddict:store(default_val, DV, Dict)}.
set_visibility_level({?PARAMETER_TUPLE_TAG, Dict}, VL) ->
    {?PARAMETER_TUPLE_TAG, orddict:store(visibility_level, VL, Dict)}.
set_requires_restart({?PARAMETER_TUPLE_TAG, Dict}, RR) when is_boolean(RR) ->
    {?PARAMETER_TUPLE_TAG, orddict:store(requires_restart, RR, Dict)}.
