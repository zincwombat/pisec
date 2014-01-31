-define(SIMULATOR_SCAN_INTERVAL,	50).
-define(DEFAULT_SCAN_INTERVAL,		50).
-define(MIN_PORT,			1).
-define(MAX_PORT,			8).
-define(IN_RANGE(X),			((X >= ?MIN_PORT) andalso (X =< ?MAX_PORT))).
-define(JSON(STATUS,PAYLOAD),		[{status,STATUS},
					 {content, "application/json", PAYLOAD}]).
-define(JSON(PAYLOAD),			[{content, "application/json", PAYLOAD}]).

-record(portstatus,	{ioport,pid,description,iostate,maskstate}).
-record(portmask,	{ioport,maskstate}).
-record(alarmconf,	{alarmstate,setmask,clearmask,simulator=false}).
-record(alarmstatus,	{alarmstate,portfilter,portstate=[]}).
-record(error,		{status,code,description,extended}).

-record(json,		{meta,data}).
-record(link,		{type,rel,href}).
