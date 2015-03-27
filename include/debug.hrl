-define(TRACE_DEBUG,	0).
-define(TRACE_INFO,		1).
-define(TRACE_WARN,		2).
-define(TRACE_CRITICAL,	3).
-define(TRACE_DEFAULT,	1).

-ifndef(TRACE_LEVEL).
-define(TRACE_LEVEL,?TRACE_INFO).
-endif.

-define(DEBUG, 0).
-define(LAGER, 1).
-ifdef(LAGER).
-define(trace(Str),		lager:trace("~p",[Str])).
-define(dbug(Str),		lager:debug("~p",[Str])).
-define(info(Str),		lager:info("~p",[Str])).
-define(error(Str),		lager:error("~p",[Str])).
-define(warn(Str),		lager:warning("~p",[Str])).
-define(critical(Str),	lager:critical("~p",[Str])).
-endif.