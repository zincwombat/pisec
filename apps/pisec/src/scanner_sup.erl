-module(scanner_sup).

-behaviour(supervisor).
-include("debug.hrl").
-include("alarm.hrl").

%% API
-export([start/0,
	 start/1,
	 stop/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor

-define(CHILD(I,Type),{I,{I,start,[]},permanent,5000,Type,[I]}).
-define(CHILD(I,Type,Args),{I,{I,start,Args},permanent,5000,Type,[I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start()->
	supervisor:start_link({local,?MODULE},?MODULE,[]).

start(Args)->
	supervisor:start_link({local,?MODULE},?MODULE,Args).

stop()->
	exit(whereis(?MODULE),shutdown).

	

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init(Args) ->
	SetMask=
	case config:get(setmask) of
	undefined->
		config:set(setmask,0),
		0;
	S->
		S
	end,
	ClearMask=
	case config:get(clearmask) of
	undefined->
		config:set(clearmask,0),
		0;
	C->
		C	
	end,
	AC=#alarmconf{setmask=SetMask,clearmask=ClearMask},
	AlarmConf=
	case lists:keysearch(simulator,1,Args) of
		{value,{simulator,true}}->
			?warn("simulator mode"),
			AC#alarmconf{simulator=true};
		_->
			AC
	end,
	Scanner=?CHILD(scanner,worker,[AlarmConf]),
	Dispatcher=?CHILD(dispatcher_sup,supervisor),
	InitState=
	case config:get(initstate) of
	undefined->
		config:set(initstate,'DISARMED'),
		'DISARMED';
	AState->
		AState
	end,
	Alarm=?CHILD(alarm,worker,[InitState]),
	Cs=[Alarm,Scanner,Dispatcher],
	?info({starting,Cs}),
	{ok,{{one_for_all,5,10},Cs}}.

