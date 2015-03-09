-define(YAWS_PORT,				8888).
-define(YAWS_ID,				"pisec").
-define(YAWS_SERVERNAME, 		"embeddedpi").
-define(YAWS_LISTEN,			{0,0,0,0}).
%% -define(YAWS_DOCROOT,		"/home/pi/erlang/security/www/").
%% -define(YAWS_LOGDIR,		"/home/pi/erlang/security/www/logs/").
%% -define(YAWS_EBINDIRS,		["/home/pi/erlang/security/ebin/"]).
%% -define(YAWS_APPMODS,		[{"/",pisec,[["js"],["icons"],["static"],["css"]]}]).
-define(YAWS_DOCROOT,			"www/").
-define(YAWS_LOGDIR,			"www/logs/").
-define(YAWS_APPMODS,			[{"/",pisec,[["js"],["icons"],["static"],["css"]]}]).