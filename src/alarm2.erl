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
	state/0,
	alarmCount/0
]).

-export([
	ack/0,
	unack/0
]).

-export([
	'DISARMED'/2,
	'CLEAR'/2,
	'ACTIVE'/2,
	'ACK'/2,
	'WAIT_ARM'/2
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
	history,
	wait_timeout
}).

start(_InitState)->
	gen_fsm:start_link({local,?MODULE},?MODULE,[],[]).

stop()->
	gen_fsm:send_all_state_event(?MODULE,{stop,normal}).

notify(Event=#event{})->
	gen_fsm:send_all_state_event(?MODULE,Event).

ack()->
	gen_fsm:sync_send_all_state_event(?MODULE,ack).

unack()->
	gen_fsm:sync_send_all_state_event(?MODULE,unack).

history()->
	gen_fsm:sync_send_all_state_event(?MODULE,history).

alarmCount()->
	gen_fsm:sync_send_all_state_event(?MODULE,alarmCount).

state()->
	gen_fsm:sync_send_all_state_event(?MODULE,state).


init(Args)->
	?info({starting,self()}),
	process_flag(trap_exit,true),

	HistorySize=config:get(alarm_handler_history_size,?DEFAULT_ALARMHANDLER_HISTORY),

	Queue=aqueue:new(HistorySize),

	% we need to get the asserted states of all the alarms

	SensorStates=io_manager:getState(),

	?info({sensorStates,SensorStates}),

	% get the list of asserted sensors (not controls)

	Asserted=lists:filter(fun(Z)->isSensorAsserted(Z) end, SensorStates),
	ActiveSet=sets:from_list(lists:map(fun(Z)->eventToAlarm(Z) end,Asserted)),
	ActiveCount=sets:size(ActiveSet),

	WaitArmTimeout=config:get(timer_wait_arm,?DEFAULT_WAIT_ARM_INTERVAL),

	NextState=
	case isAlarmEnabled() of 
		false->
			'DISARMED';
		_->
			% set the arming timer
			TRef=erlang:start_timer(WaitArmTimeout,self(),tm_sync),
			'WAIT_ARM'
	end,

	NewQueue=aqueue:logFsm(NextState,"init",NextState,Queue),
	?info({initstate,NextState}),

	{ok,NextState,StateData=#state{	wait_timeout=WaitArmTimeout,
									active_count=ActiveCount,
									active_set=ActiveSet,
									history=NewQueue}}.


%% ============================================================================
%% SYNC STATE TRANSITIONS 
%% ============================================================================									

handle_sync_event(Event=unack,_From,StateName='ACK',
				  StateData=#state{history=Queue,active_count=0})->
	NextState='CLEAR',
    NewQueue=aqueue:logFsm(StateName,Event,NextState,Queue),
	{reply,{ok,NextState},NextState,StateData#state{history=NewQueue}};

handle_sync_event(Event=unack,_From,StateName='ACK',
				  StateData=#state{history=Queue})->
	NextState='ACTIVE',
    NewQueue=aqueue:logFsm(StateName,Event,NextState,Queue),
	{reply,{ok,NextState},NextState,StateData#state{history=NewQueue}};

handle_sync_event(Event=ack,_From,StateName='ACTIVE',
				  StateData=#state{history=Queue,active_count=0})->
	NextState='CLEAR',
    NewQueue=aqueue:logFsm(StateName,Event,NextState,Queue),
	{reply,{ok,NextState},NextState,StateData#state{history=NewQueue}};

handle_sync_event(Event=ack,_From,StateName='ACTIVE',
				  StateData=#state{history=Queue})->
	NextState='ACK',
    NewQueue=aqueue:logFsm(StateName,Event,NextState,Queue),
	{reply,{ok,NextState},NextState,StateData#state{history=NewQueue}};

handle_sync_event(history,_From,State,StateData=#state{history=H})->
	{reply,aqueue:dump(H),State,StateData};

handle_sync_event(state,_From,State,StateData)->
	{reply,{ok,State},State,StateData};

handle_sync_event(alarmCount,_From,State,StateData=#state{active_count=ActiveCount,
														  active_set=ActiveSet})->
	{reply,{ok,ActiveCount},State,StateData};
		
handle_sync_event(Event,_From,StateName,StateData=#state{})->
	{reply,{error,{unhandled,Event}},StateName,StateData}.


%% ============================================================================
%% HANDLE ALL STATE EVENT CALLBACKS
%% ============================================================================

handle_event(Event=#event{},StateName,StateData)->
	{NextState,NextStateData}=i_handle_event(Event,StateName,StateData),
	{next_state,NextState,NextStateData};

handle_event(_Event={stop,Reason},_StateName,StateData=#state{})->
	{stop,Reason,StateData};

handle_event(_Event,StateName,StateData)->
	{next_state,StateName,StateData}.

%% ============================================================================
%% HANDLE TIMERS etc
%% ============================================================================

handle_info(Event={timeout,_,tm_sync},'WAIT_ARM',StateData)->
	?info({event,Event}),

	SensorStates=io_manager:getState(),
	Asserted=lists:filter(fun(Z)->isSensorAsserted(Z) end, SensorStates),
	ActiveSet=sets:from_list(lists:map(fun(Z)->eventToAlarm(Z) end,Asserted)),
	ActiveCount=sets:size(ActiveSet),

	NextState=
	case ActiveCount of
		0->
			'CLEAR';
		_->
			'ACTIVE'
	end,

	NextStateData=#state{	active_count=ActiveCount,
							active_set=ActiveSet},

	?info({next_state,NextState}),

	{next_state,NextState,NextStateData};


handle_info(Event,State,StateData)->
	?info({ignored_info,Event}),
	{next_state,State,StateData}.

%% ============================================================================
%% OTHER
%% ============================================================================

terminate(Reason,_StateName,_StateData)->
	{stop,Reason}.

code_change(_OldVsn,StateName,StateData,_Extra)->
	{ok,StateName,StateData}.	

i_handle_event(Event=#event{type=sensor},StateName,StateData)->
	handle_alarm(Event,StateName,StateData);

i_handle_event(Event=#event{type=control},StateName,StateData)->
	handle_control(Event,StateName,StateData).

handle_alarm(Event=#event{sensorStatus=SensorStatus},
			 StateName,
			 StateData=#state{active_count=ActiveCount,
			 				  active_set=ActiveSet,
			 				  history=Queue}) when StateName /= 'DISARMED' ->
	?info({alarm_event,Event,StateName}),

	NewActiveSet=
	case SensorStatus of
		asserted->
			sets:add_element(eventToAlarm(Event),ActiveSet);
		_->
			sets:del_element(eventToAlarm(Event),ActiveSet)
	end,
	NewActiveCount=sets:size(NewActiveSet),

	NextState=
	case {StateName,ActiveCount} of
		{'DISARMED',_}->
			'DISARMED';
		{'ACK',0}->
			'CLEAR';
		{'CLEAR',0}->
			'CLEAR';
		{'CLEAR',_N}->
			'ACTIVE';
		_->
			StateName
	end,

	?info({next_state,NextState}),
	% TODO -- add history
	{NextState,StateData#state{	active_count=NewActiveCount,
								active_set=NewActiveSet}};

handle_alarm(Event,StateName,StateData)->
	{StateName,StateData}.

handle_control(Event=#event{sensorStatus=asserted,label=enable},'DISARMED',StateData)->
	?info({control_event,Event}),

	% we need to arm the system, first set timer
	TRef=erlang:start_timer(StateData#state.wait_timeout,self(),tm_sync),

	{'WAIT_ARM',StateData};

handle_control(Event=#event{sensorStatus=deAsserted,label=enable},State,StateData)->
	?info({control_event,Event}),
	{'DISARMED',StateData}.


%% ============================================================================
%% STATE CALLBACKS
%% ============================================================================

'DISARMED'(Event,StateData)->
	?info({{event,Event},{state,'DISARMED'}}),
	{next_state,'DISARMED',StateData}.

'WAIT_ARM'(Event,StateData)->
	?info({{event,Event},{state,'WAIT_ARM'}}),
	{next_state,'WAIT_ARM',StateData}.

'ACK'(Event,StateData)->
	?info({{event,Event},{state,'ACK'}}),
	{next_state,'ACK',StateData}.

'CLEAR'(Event,StateData)->
	?info({{event,Event},{state,'CLEAR'}}),
	{next_state,'CLEAR',StateData}.

'ACTIVE'(Event,StateData)->
	?info({{event,Event},{state,'ACTIVE'}}),
	{next_state,'ACTIVE',StateData}.


%% ============================================================================
%% Utility Routines
%% ============================================================================

isSensorAsserted(#event{type=sensor,sensorStatus=asserted})->
	true;

isSensorAsserted(_)->
	false.

eventToAlarm(#event{port=Port,label=Label,desc=Desc})->
	#alarm{port=Port,label=Label,desc=Desc}.

isAlarmEnabled()->
	case io_manager:getState(enable) of
		#event{sensorStatus=asserted}->
			true;
		_->
			false
	end.

