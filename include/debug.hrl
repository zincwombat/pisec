-define(TRACE_DEBUG,		0).
-define(TRACE_INFO,			1).
-define(TRACE_WARN,			2).
-define(TRACE_CRITICAL,		3).

-define(tracedebug,			put(trace_level,?TRACE_DEBUG)).
-define(traceinfo,			put(trace_level,?TRACE_INFO)).

-ifndef(TRACE_LEVEL).
-define(TRACE_LEVEL,?TRACE_INFO).
-endif.


-define(trace(Str),			syslogger:trace(?MODULE,{{line,?LINE},Str})).
-define(dbug(Str),			syslogger:debug(?MODULE,{{line,?LINE},Str})).
-define(info(Str),			syslogger:info(?MODULE,{{line,?LINE},Str})).
-define(warn(Str),			syslogger:warn(?MODULE,{{line,?LINE},Str})).
-define(critical(Str),		syslogger:critical(?MODULE,{{line,?LINE},Str})).
-define(tracelevel(Lvl),	syslogger:add(?MODULE,Lvl)).

-define(location,			{location,{module,?MODULE},{line,?LINE}}).
-define(error(E),			{error,{?location,{detail,E}}}).