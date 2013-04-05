% Debug logging macros
% Copyright (c) 2013 Red Hat, Inc., and William C. Benton

-ifdef(debug).
-define(D_LOG(F, A), error_logger:warning_msg("~s.~w: " ++ F, [ ?MODULE, ?LINE | A])).
-define(D_VAL(V), begin error_logger:warning_msg("~s.~w: ~s=~p~n", [ ?MODULE, ?LINE, ??V, V ]), V end).
-else.
-define(D_LOG(F, A), ok).
-define(D_VAL(V), V).
-endif.
