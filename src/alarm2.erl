-module(alarm2).
-behaviour(gen_fsm).
-include("debug.hrl").
-include("alarm.hrl").

-define(ALARMING_INTERVAL,5000).	%% 5 seconds
-define(SYNC_INTERVAL,5000).		%% 5 seconds

-export([
	init/1,
	handle_event/3,
	handle_sync_event/4,
	handle_info/3,
	terminate/3,
	code_change/4
]).

-export([
	start/1,        %% start the FSM
	stop/0
]).

-export([
	notify/1,
	history/0,
	state/0
]).

-export([
	arm/0,
	disarm/0,
	ack/0,
	unack/0
]).

-export([
	'DISARMED'/2,
	'CLEAR'/2,
	'ACTIVE'/2,
	'ACK'/2,
	'SYNC'/2
]).

-define(START_OPTIONS,  []).
-define(SERVERNAME,     ?MODULE).


%% STATE DATA =================================================================

-record(state,{	
	tm_arming,
	tm_alerting,
	tm_sync,
	active_set,
	active_count,
	history
}).

start(_InitState)->
	gen_fsm:start_link({local,?MODULE},?MODULE,[],[]).

stop()->
	gen_fsm:send_all_state_event(?MODULE,{stop,normal}).

notify(Event=#event{})->
	gen_fsm:send_event(?MODULE,Event).

disarm()->
	gen_fsm:sync_send_all_state_event(?MODULE,disarm).

arm()->
	gen_fsm:sync_send_all_state_event(?MODULE,arm).

ack()->
	gen_fsm:sync_send_all_state_event(?MODULE,ack).

unack()->
	gen_fsm:sync_send_all_state_event(?MODULE,unack).

history()->
	gen_fsm:sync_send_all_state_event(?MODULE,history).

state()->
	gen_fsm:sync_send_all_state_event(?MODULE,state).


init(Args)->
	?info({starting,self()}),
	process_flag(trap_exit,true),
	HistorySize=config:get(alarm_handler_history_size,?DEFAULT_ALARMHANDLER_HISTORY),
	InitState=config:get(initstate,?DEFAULT_ALARM_INIT_STATE),
	config:set(initstate,InitState),
	Queue=aqueue:new(HistorySize),
    NewQueue=aqueue:logFsm(InitState,"init",'SYNC',Queue),
	StateData=#state{active_set=sets:new(),active_count=0,history=NewQueue},

	% we need to get the asserted states of all the alarms

	SensorStates=io_manager:getState(),

	?info({SensorStates,SensorStates}),


	{ok,InitState,StateData}.

handle_sync_event(Event=disarm,_From,StateName,StateData=#state{history=Queue}) when StateName /= 'DISARMED'->
	config:set(initstate,'DISARMED'),
	?info({state,'DISARMED'}),
    NewQueue=aqueue:logFsm(StateName,Event,'DISARM',Queue),
	{reply,{ok,'DISARMED'},'DISARMED',StateData#state{history=NewQueue}};

handle_sync_event(Event=arm,_From,StateName='DISARMED',StateData=#state{active_set=AA,history=Queue})->
	config:set(initstate,'ARMED'),
	NextState=
	case (sets:size(AA)==0) of
	true->
		'CLEAR';
	false->
		'ACTIVE'
	end,
    NewQueue=aqueue:logFsm(StateName,Event,NextState,Queue),
	{reply,{ok,NextState},NextState,StateData#state{history=NewQueue}};

handle_sync_event(Event=unack,_From,StateName='ACK',StateData=#state{active_set=AA,history=Queue})->
	NextState=
	case (sets:size(AA)==0) of
	true->
		'CLEAR';
	false->
		'ACTIVE'
	end,
    NewQueue=aqueue:logFsm(StateName,Event,NextState,Queue),
	{reply,{ok,NextState},NextState,StateData#state{history=NewQueue}};

handle_sync_event(Event=ack,_From,StateName='ACTIVE',StateData=#state{active_set=AA,history=Queue})->
	NextState=
	case (sets:size(AA)==0) of
	true->
		'CLEAR';
	false->
		'ACK'
	end,
    NewQueue=aqueue:logFsm(StateName,Event,NextState,Queue),
	{reply,{ok,NextState},NextState,StateData#state{history=NewQueue}};

handle_sync_event(Event=ack,_From,StateName='ACK',StateData=#state{active_set=AA,history=Queue})->
	NextState=
	case (sets:size(AA)==0) of
	true->
		'CLEAR';
	false->
		'ACK'
	end,
    NewQueue=aqueue:logFsm(StateName,Event,NextState,Queue),
	{reply,{ok,NextState},NextState,StateData#state{history=NewQueue}};

handle_sync_event(history,_From,State,StateData=#state{history=H})->
	{reply,aqueue:dump(H),State,StateData};

handle_sync_event(state,_From,State,StateData)->
	{reply,{ok,State},State,StateData};
		
handle_sync_event(Event,_From,StateName,StateData=#state{})->
	{reply,{error,{unhandled,Event}},StateName,StateData}.

handle_event(_Event={stop,Reason},_StateName,StateData=#state{})->
	{stop,Reason,StateData};

handle_event(_Event,StateName,StateData)->
	{next_state,StateName,StateData}.

handle_info(Event,State,StateData)->
	?info({ignored_info,Event}),
	{next_state,State,StateData}.

terminate(Reason,_StateName,_StateData)->
	{stop,Reason}.

code_change(_OldVsn,StateName,StateData,_Extra)->
	{ok,StateName,StateData}.	

%% STATE CALLBACKS

'DISARMED'(Event,StateData)->
	?info({{event,Event},{state,'DISARMED'}}),
	{next_state,'DISARMED',StateData}.

'SYNC'(Event,StateData)->
	?info({{event,Event},{state,'SYNC'}}),
	{next_state,'SYNC',StateData}.

'ACK'(Event,StateData)->
	?info({{event,Event},{state,'ACK'}}),
	{next_state,'ACK',StateData}.

'CLEAR'(Event,StateData)->
	?info({{event,Event},{state,'CLEAR'}}),
	{next_state,'CLEAR',StateData}.

'ACTIVE'(Event,StateData)->
	?info({{event,Event},{state,'ACTIVE'}}),
	{next_state,'ACTIVE',StateData}.

