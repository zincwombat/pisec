-module(alarm2).
-behaviour(gen_fsm).
-include("debug.hrl").
-include("alarm.hrl").
-include("ports.hrl").

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

notify(Sensor=#sensor{})->
	gen_fsm:send_all_state_event(?MODULE,Sensor).

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

	SensorStates=input_manager:getState(),

	?info({sensorStates,SensorStates}),

	Outputs=config:get(outputs),

	% get the list of asserted sensors (not controls)

	Asserted=lists:filter(fun(Z)->isSensorAsserted(Z) end, SensorStates),
	ActiveSet=sets:from_list(lists:map(fun(Z)->{Z#sensor.label,Z#sensor.desc} end,Asserted)),
	ActiveCount=sets:size(ActiveSet),

	WaitArmTimeout=config:get(timer_wait_arm,?DEFAULT_WAIT_ARM_INTERVAL),

	NextState=
	case isAlarmEnabled() of 
		false->
			%% turn off alarm status LED
			'DISARMED';
		_->
			% set the arming timer
			TRef=erlang:start_timer(WaitArmTimeout,self(),tm_sync),
			%% fast flash the alarm status LED
			'WAIT_ARM'
	end,

	NewQueue=aqueue:logFsm(NextState,"init",NextState,Queue),
	?info({initstate,NextState}),

	alarmStatusLed(NextState),

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
	alarmStatusLed(NextState),
    NewQueue=aqueue:logFsm(StateName,Event,NextState,Queue),
    handle_statechange_notifications(StateName,NextState),
	{reply,{ok,NextState},NextState,StateData#state{history=NewQueue}};

handle_sync_event(Event=unack,_From,StateName='ACK',
				  StateData=#state{history=Queue})->
	NextState='ACTIVE',
    NewQueue=aqueue:logFsm(StateName,Event,NextState,Queue),
    handle_statechange_notifications(StateName,NextState),
	{reply,{ok,NextState},NextState,StateData#state{history=NewQueue}};

handle_sync_event(Event=ack,_From,StateName='ACTIVE',
				  StateData=#state{history=Queue,active_count=0})->
	NextState='CLEAR',
    NewQueue=aqueue:logFsm(StateName,Event,NextState,Queue),
    handle_statechange_notifications(StateName,NextState),
	{reply,{ok,NextState},NextState,StateData#state{history=NewQueue}};

handle_sync_event(Event=ack,_From,StateName='ACTIVE',
				  StateData=#state{history=Queue})->
	NextState='ACK',
    NewQueue=aqueue:logFsm(StateName,Event,NextState,Queue),
    handle_statechange_notifications(StateName,NextState),
	{reply,{ok,NextState},NextState,StateData#state{history=NewQueue}};

handle_sync_event(history,_From,State,StateData=#state{history=H})->
	{reply,aqueue:dump(H),State,StateData};

handle_sync_event(state,_From,State,StateData)->
	{reply,{state,State},State,StateData};

handle_sync_event(alarmCount,_From,State,StateData=#state{active_count=ActiveCount,
														  active_set=ActiveSet})->
	{reply,{ok,ActiveCount},State,StateData};
		
handle_sync_event(Event,_From,StateName,StateData=#state{})->
	{reply,{error,{unhandled,Event}},StateName,StateData}.


%% ============================================================================
%% HANDLE ALL STATE EVENT CALLBACKS
%% ============================================================================

handle_event(Sensor=#sensor{},StateName,StateData)->
	{NextState,NextStateData}=i_handle_event(Sensor,StateName,StateData),
	handle_statechange_notifications(StateName,NextState),
	{next_state,NextState,NextStateData};

handle_event(_Event={stop,Reason},_StateName,StateData=#state{})->
	{stop,Reason,StateData};

handle_event(_Event,StateName,StateData)->
	{next_state,StateName,StateData}.

%% ============================================================================
%% HANDLE TIMERS etc
%% ============================================================================

handle_info(Event={timeout,_,tm_sync},StateName='WAIT_ARM',StateData=#state{history=Queue})->
	?info({event,Event}),

	SensorStates=input_manager:getState(),
	Asserted=lists:filter(fun(Z)->isSensorAsserted(Z) end, SensorStates),
	ActiveSet=sets:from_list(lists:map(fun(Z)->{Z#sensor.label,Z#sensor.desc} end,Asserted)),
	ActiveCount=sets:size(ActiveSet),

	NextState=
	case ActiveCount of
		0->
			'CLEAR';
		_->
			'ACTIVE'
	end,

	NewQueue=aqueue:logFsm(StateName,Event,NextState,Queue),

	NextStateData=StateData#state{	active_count=ActiveCount,
									active_set=ActiveSet,
									history=NewQueue},

	?info({next_state,NextState}),
	handle_statechange_notifications(StateName,NextState),
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

i_handle_event(Sensor=#sensor{type=sensor},StateName,StateData)->
	handle_alarm(Sensor,StateName,StateData);

i_handle_event(Sensor=#sensor{type=control},StateName,StateData)->
	handle_control(Sensor,StateName,StateData).

handle_alarm(Sensor=#sensor{state=SensorStatus,desc=Desc},
			 StateName,
			 StateData=#state{active_count=ActiveCount,
			 				  active_set=ActiveSet,
			 				  history=Queue}) when StateName /= 'DISARMED' ->

	?info({alarm_event,Sensor,StateName}),

	LogMessage=io_lib:format("alarm: ~s is ~p",[Desc,SensorStatus]),

	NewActiveSet=
	case SensorStatus of
		asserted->
			addToSet(Sensor,ActiveSet);
		_->
			removeFromSet(Sensor,ActiveSet)
	end,
	NewActiveCount=sets:size(NewActiveSet),

	NextState=
	case {StateName,NewActiveCount} of
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

	NewQueue=aqueue:logFsm(StateName,LogMessage,NextState,Queue),

	{NextState,StateData#state{	active_count=NewActiveCount,
								active_set=NewActiveSet,
								history=NewQueue}};

handle_alarm(Sensor,StateName,StateData)->
	{StateName,StateData}.

handle_control(	Sensor=#sensor{state=asserted,label=enable},StateName='DISARMED',
				StateData=#state{history=Queue})->

	?info({control_event,Sensor}),

	LogMessage=io_lib:format("alarm enabled",[]),
	NextState='WAIT_ARM',
	NewQueue=aqueue:logFsm(StateName,LogMessage,NextState,Queue),
	TRef=erlang:start_timer(StateData#state.wait_timeout,self(),tm_sync),
	{NextState,StateData#state{history=NewQueue}};

handle_control(	Sensor=#sensor{state=deAsserted,label=enable},StateName,
				StateData=#state{history=Queue})->

	?info({control_event,Sensor}),

	LogMessage=io_lib:format("alarm disabled",[]),
	NextState='DISARMED',
	NewQueue=aqueue:logFsm(StateName,LogMessage,NextState,Queue),
	{NextState,StateData#state{history=NewQueue}}.

handle_statechange_notifications(State,State)->
	ignore;

handle_statechange_notifications(OldState,NewState)->
	alarmStatusLed(NewState),

	% needs convert State from atom to string ....

	Message="Alarm state change from: [" ++ 
	atom_to_list(OldState) ++ 
	"] to [" ++ 
	atom_to_list(NewState) ++
	 "]",

	twilio_manager:notify(Message),
		
	?info({stateChange, {from,OldState},{to,NewState}}),
	ok.

%% ============================================================================
%% STATE CALLBACKS
%% ============================================================================

'DISARMED'(Event,StateData)->
	{next_state,'DISARMED',StateData}.

'WAIT_ARM'(Event,StateData)->
	{next_state,'WAIT_ARM',StateData}.

'ACK'(Event,StateData)->
	{next_state,'ACK',StateData}.

'CLEAR'(Event,StateData)->
	{next_state,'CLEAR',StateData}.

'ACTIVE'(Event,StateData)->
	{next_state,'ACTIVE',StateData}.

%% ============================================================================
%% Utility Routines
%% ============================================================================

isSensorAsserted(#sensor{type=sensor,state=asserted})->
	true;

isSensorAsserted(_)->
	false.

isAlarmEnabled()->
	case input_manager:getState(enable) of
		#sensor{state=asserted}->
			true;
		_->
			false
	end.

addToSet(#sensor{label=Label,desc=Desc},Set)->
	sets:add_element({Label,Desc},Set).

removeFromSet(#sensor{label=Label,desc=Desc},Set)->
	sets:del_element({Label,Desc},Set).

alarmStatusLed('ACTIVE')->
	alarmStatusLed(on);

alarmStatusLed('WAIT_ARM')->
	alarmStatusLed({flash,?FLASH_FAST});

alarmStatusLed('DISARMED')->
	alarmStatusLed(off);

alarmStatusLed('CLEAR')->
	alarmStatusLed({flash,?FLASH_SLOW});

alarmStatusLed('ACTIVE')->
	alarmStatusLed(on);

alarmStatusLed('ACK')->
	alarmStatusLed({flash,?FLASH_NORMAL});

alarmStatusLed(Control)->
	Outputs=config:get(outputs),
	case lists:keyfind(alarm_status_led,2,Outputs) of
		{Port,_,_,_,_,_}->
			case Control of
				on->
					output_manager:set(Port);
				off->
					output_manager:clear(Port);
				{flash,Speed} when is_integer(Speed)->
					output_manager:flash(Port,Speed);
				Other->
					{error,{badarg,Other}}
			end;		

		false->
			{error,badarg}
	end.

