% Classic Wallaby subsystem functionality
% Copyright (c) 2011 Red Hat, Inc. and William C. Benton

-module(wallaby_subsystem).
-export([new/1,name/1,parameters/1,set_parameters/2]).
-export_type([subsystem/0]).

-define(SUBSYSTEM_TUPLE_TAG, wallaby_subsystem).

-type subsystem() :: {?SUBSYSTEM_TUPLE_TAG, orddict:orddict()}.

-spec new(binary()) -> subsystem().
new(Name) when is_binary(Name) ->
    Dict = orddict:from_list([{name, Name}, {parameters, []}]),
    {?SUBSYSTEM_TUPLE_TAG, Dict}.

-spec name(subsystem()) -> binary().
name({?SUBSYSTEM_TUPLE_TAG, Dict}) -> orddict:fetch(name, Dict).

-spec parameters(subsystem()) -> [orddict:orddict()].
parameters({?SUBSYSTEM_TUPLE_TAG, Dict}) -> orddict:fetch(parameters, Dict).

-spec set_parameters(subsystem(), [{binary(), binary()}]) -> subsystem().
set_parameters({?SUBSYSTEM_TUPLE_TAG, Dict}, Ps) ->
    {?SUBSYSTEM_TUPLE_TAG, orddict:store(parameters, orddict:from_list(Ps), Dict)}.
