%% @author William C. Benton <willb@redhat.com>
%% @copyright 2013 Red Hat, Inc. and William C. Benton.
%% (adapted from webmachine error handler)

-module(wallaroo_web_errors).

-export([render_error/3]).

render_error(Code, Req, Reason) ->
    case webmachine_request:has_response_body(Req) of
        {true,_} -> webmachine_request:response_body(Req);
        {false,_} -> specific_error_body(Code, webmachine_request:trim_state(Req), Reason)
    end.

generic_error_body(Code, Req, Reason) ->
    generic_error_body(Code, Req, Reason, <<>>).

generic_error_body(Code, Req, Reason, HumanText) ->
    {ok, ReqState} = webmachine_request:add_response_header("Content-Type", "application/json", Req),
    {Path, _} = webmachine_request:path(Req),
    Struct = {struct, 
	      Dict=([{http_code, Code}, {request_path, list_to_binary(Path)}]
	      ++ if HumanText =:= <<>> ->
			 [];
		    true ->
			 [{explanation, HumanText}]
		 end
	      ++ if Reason =:= {none, none, []} ->
			 [];
		    true ->
			 {reason, iolist_to_binary(io_lib:format("~p", [Reason]))}
		 end)
	     },
    case {Code, Reason} of
	{_, {exit, normal, _}} ->
	    ok;
	{X, _} when X >= 500 ->
	    error_logger:error_msg("error for path=~p; ~p~n", 
				   [Path, [{K,if is_binary(V) -> binary_to_list(V) ; true -> V end} || {K, V} <- Dict]]);
	_ ->
	    ok
    end,
    {iolist_to_binary(mochijson:binary_encode(Struct)), ReqState}.

specific_error_body(Code=404, Req, Reason) ->
    generic_error_body(Code, Req, Reason, <<"Resource not found">>);
specific_error_body(Code=500, Req, Reason) ->
    generic_error_body(Code, Req, Reason);
specific_error_body(Code=501, Req, Reason) ->
    {Method, _} = webmachine_request:method(Req),
    generic_error_body(Code, Req, Reason, iolist_to_binary(io_lib:format("Method '~p' not implemented", [Method])));
specific_error_body(Code=503, Req, Reason) ->
    generic_error_body(Code, Req, Reason, <<"Service unavailable">>);
specific_error_body(Code, Req, Reason) ->
    generic_error_body(Code, Req, Reason, list_to_binary(httpd_util:reason_phrase(Code))).
