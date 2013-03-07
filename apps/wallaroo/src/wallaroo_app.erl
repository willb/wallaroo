-module(wallaroo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case wallaroo_sup:start_link() of
	{ok, _}=Ok ->
	    error_logger:info_msg("WALLAROO APP start_link success ~p~n", [Ok]),
	    Ok;
	Other ->
	    error_logger:info_msg("WALLAROO APP start_link failure ~p~n", [Other]),
	    {error, Other}
    end.

stop(_State) ->
    ok.
