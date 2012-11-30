%% @author William C. Benton <willb@redhat.com>
%% @copyright 2012 Red Hat, Inc. and William C. Benton
%% @doc Wallaby dirty list type and functions

-module(wallaby_dirtylist).
-export([empty/0, join/2, add/3]).

-type dl_entrykind() :: 'node' | 'group' | 'feature' | 'parameter' | 'subsystem'.
-type dl_entry() :: {dl_entrykind(), [string()]}.
-type dirtylist() :: 'all' | {wallaby_dl, [dl_entry()]}.

-spec empty() -> dirtylist().
empty() ->
    {wallaby_dl, []}.

-spec join(dirtylist(), dirtylist()) -> dirtylist().
join(all, _) ->
    all;
join(_, all) ->
    all;
join({wallaby_dl, _}=Result, {wallaby_dl, []}) ->
    Result;
join({wallaby_dl, []}, {wallaby_dl, _}=Result) ->
    Result;
join({wallaby_dl, LhLs}, {wallaby_dl, RhLs}) when is_list(LhLs) andalso is_list(RhLs) ->
    ResultLs = orddict:merge(fun(_, V1, V2) -> ordsets:union(V1, V2) end, LhLs, RhLs),
    {wallaby_dl, ResultLs}.

-spec add(dirtylist(), dl_entrykind(), string()) -> dirtylist().
add(all, _, _) ->
    all;
add({wallaby_dl, Dls}, Kind, Entry) ->
    case orddict:find(Kind, Dls) of
	{ok, Entries} ->
	    NewEntries = ordsets:add_element(Entry, Entries),
	    {wallaby_dl, orddict:store(Kind, NewEntries, Dls)};
	error ->
	    {wallaby_dl, orddict:store(Kind, [Entry], Dls)}
    end.


		
				      



