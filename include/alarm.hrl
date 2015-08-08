-define(APPNAME,						pisec).
-define(ALARMCONFIG,					"alarmconfig.erl").
-define(SIMULATOR_SCAN_INTERVAL,		50).
-define(DEFAULT_SCAN_INTERVAL,			50).
-define(DEFAULT_PORTHANDLER_HISTORY,	10).
-define(DEFAULT_ALARMHANDLER_HISTORY,	50).
-define(DEFAULT_ALARM_INIT_STATE,		'DISARMED').
-define(DEFAULT_WAIT_ARM_INTERVAL,		30000).
-define(MIN_PORT,						1).
-define(MAX_PORT,						8).
-define(IN_RANGE(X),					((X >= ?MIN_PORT) andalso (X =< ?MAX_PORT))).
-define(JSON(STATUS,PAYLOAD),			[{status,STATUS},
					 					 {content, "application/json", PAYLOAD}]).
-define(JSON(PAYLOAD),					[{content, "application/json", PAYLOAD}]).
-define(FLASH_SLOW,						1000).
-define(FLASH_NORMAL,					500).
-define(FLASH_FAST,						100).
-define(SIREN_ON_INTERVAL,				60000).	% 60 seconds
-define(SIREN_OFF_INTERVAL,				60000).	% 60 seconds
-define(BOOTSTRAP, 						"/home/pi/bootstrap.erl").

-record(portstatus,		{ioport,pid,description,iostate,maskstate,log}).
-record(portmask,		{ioport,maskstate}).
-record(alarmconf,		{alarmstate,setmask,clearmask,simulator=false}).
-record(alarmstatus,	{alarmstate,portfilter,portstate=[],log}).
-record(error,			{status,code,description,extended}).

-record(json,			{meta,data}).
-record(link,			{type,rel,href}).


-record(event, 			{port,sensorStatus,label,desc,assertLevel,type}).
-record(alarm, 			{port,label,desc}).
-record(control, 		{port,label,desc}).
-record(sensor, 		{type,label,desc,state}).
