-module(alarm2).
-behaviour(gen_fsm).
-include("debug.hrl").
-include("alarm.hrl").
-include("ports.hrl").

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
	alarmCount/0,
	setNotifyStatus/1
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
	alarming_interval,
	alert_on_interval,
	alert_off_interval,
	notify_status
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

setNotifyStatus(Status)->
	gen_fsm:sync_send_all_state_event(?MODULE,{notify_status,Status}).

init(Args)->
	?info({starting,self()}),
	process_flag(trap_exit,true),

	% we need to get the asserted states of all the alarms

	SensorStates=input_manager:getState(),

	?info({sensorStates,SensorStates}),

	Outputs=config:get(outputs),

	% get the list of asserted sensors (not controls)

	Asserted=lists:filter(fun(Z)->isSensorAsserted(Z) end, SensorStates),
	ActiveSet=sets:from_list(lists:map(fun(Z)->{Z#sensor.label,Z#sensor.desc} end,Asserted)),
	ActiveCount=sets:size(ActiveSet),

	WaitArmInterval=config:get(timer_wait_arm,?DEFAULT_WAIT_ARM_INTERVAL),
	AlertOnInterval=config:get(timer_alert_on,?DEFAULT_SIREN_ON_INTERVAL),
	AlertOffInterval=config:get(timer_alert_off,?DEFAULT_SIREN_OFF_INTERVAL),
	NotifyStatus=config:get(twilio_notify,?DEFAULT_TWILIO_NOTIFY),

	NextState=
	case isAlarmEnabled() of 
		false->
			%% turn off alarm status LED
			'DISARMED';
		_->
			% set the arming timer
			TRef=erlang:start_timer(WaitArmInterval,self(),tm_sync),
			%% fast flash the alarm status LED
			'WAIT_ARM'
	end,

	logFsm(NextState,"init",NextState),
	?info({initstate,NextState}),

	alarmStatusLed(NextState),

	{ok,NextState,StateData=#state{	alarming_interval=WaitArmInterval,
									alert_on_interval=AlertOnInterval,
									alert_off_interval=AlertOffInterval,
									active_count=ActiveCount,
									active_set=ActiveSet,
									notify_status=NotifyStatus}}.

%% ============================================================================
%% SYNC STATE TRANSITIONS 
%% ============================================================================									

handle_sync_event(Event=unack,_From,StateName='ACK',
				  StateData=#state{active_count=0})->
	NextState='CLEAR',
	alarmStatusLed(NextState),
    logFsm(StateName,Event,NextState),
    NewStateData=handle_statechange_actions(StateName,NextState,StateData),
	{reply,{ok,NextState},NextState,NewStateData};

handle_sync_event(Event=unack,_From,StateName='ACK',
				  StateData=#state{})->
	NextState='ACTIVE',
    logFsm(StateName,Event,NextState),
    NewStateData=handle_statechange_actions(StateName,NextState,StateData),
	{reply,{ok,NextState},NextState,NewStateData};

handle_sync_event(Event=ack,_From,StateName='ACTIVE',
				  StateData=#state{active_count=0})->

	NextState='CLEAR',
    logFsm(StateName,Event,NextState),
    NewStateData=handle_statechange_actions(StateName,NextState,StateData),
	{reply,{ok,NextState},NextState,NewStateData};

handle_sync_event(Event=ack,_From,StateName='ACTIVE',
				  StateData)->
	NextState='ACK',
    logFsm(StateName,Event,NextState),
    NewStateData=handle_statechange_actions(StateName,NextState,StateData),
	{reply,{ok,NextState},NextState,NewStateData};

handle_sync_event(history,_From,State,StateData)->
	{reply,history_manager:getAll(),State,StateData};

handle_sync_event(state,_From,State,StateData)->
	{reply,{state,State},State,StateData};

handle_sync_event(alarmCount,_From,State,StateData=#state{active_count=ActiveCount,
														  active_set=ActiveSet})->
	{reply,{ok,ActiveCount},State,StateData};

handle_sync_event({notify_status,Status},_From,State,StateData) when is_boolean(Status)->
	config:set(twilio_notify,Status),
	case Status of
		true->
			Message="alarm state: " ++ atom_to_list(State),
			twilio_manager:notify(Message);
		_->
			ok
	end,
	{reply,ok,State,StateData#state{notify_status=Status}};
		
handle_sync_event(Event,_From,StateName,StateData=#state{})->
	{reply,{error,{unhandled,Event}},StateName,StateData}.

%% ============================================================================
%% HANDLE ALL STATE EVENT CALLBACKS
%% ============================================================================

handle_event(Sensor=#sensor{},StateName,StateData)->
	{NextState,NextStateData}=i_handle_event(Sensor,StateName,StateData),
	{next_state,NextState,handle_statechange_actions(StateName,NextState,NextStateData)};

handle_event(_Event={stop,Reason},_StateName,StateData=#state{})->
	{stop,Reason,StateData};

handle_event(_Event,StateName,StateData)->
	{next_state,StateName,StateData}.

%% ============================================================================
%% HANDLE TIMERS etc
%% ============================================================================

handle_info(Event={timeout,_,tm_sync},StateName='WAIT_ARM',StateData)->
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

	logFsm(StateName,"armed",NextState),

	NextStateData=StateData#state{	active_count=ActiveCount,
									active_set=ActiveSet},

	?info({next_state,NextState}),
	{next_state,NextState,handle_statechange_actions(StateName,NextState,NextStateData)};


handle_info(Event={timeout,TRef,tm_alert_on },State='ACTIVE',StateData=#state{tm_alerting=TRef})->
	?info({event,Event}),
	siren(on),
	NewTRef=erlang:start_timer(StateData#state.alert_off_interval,self(),tm_alert_off),
	{next_state,State,StateData#state{tm_alerting=NewTRef}};

handle_info(Event={timeout,TRef,tm_alert_off},State='ACTIVE',StateData=#state{tm_alerting=TRef})->
	?info({event,Event}),
	siren(off),
	NewTRef=erlang:start_timer(StateData#state.alert_on_interval,self(),tm_alert_on),
	{next_state,State,StateData#state{tm_alerting=NewTRef}};

handle_info(Event,State,StateData)->
	?info({ignored_info,Event}),
	{next_state,State,StateData}.

%% ============================================================================
%% OTHER
%% ============================================================================

terminate(Reason,_StateName,_StateData)->
	% TODO - turn off all the LEDs
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
			 				  active_set=ActiveSet}) when StateName /= 'DISARMED' ->

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

	?info({activeSetCount,NewActiveCount}),

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

	logFsm(StateName,LogMessage,NextState),

	{NextState,StateData#state{	active_count=NewActiveCount,
								active_set=NewActiveSet}};

handle_alarm(Sensor,StateName,StateData)->
	{StateName,StateData}.

handle_control(	Sensor=#sensor{state=asserted,label=enable},StateName='DISARMED',
				StateData)->

	?info({control_event,Sensor}),

	LogMessage=io_lib:format("alarm enabled",[]),
	NextState='WAIT_ARM',
	alarmStatusLed(NextState),
	logFsm(StateName,LogMessage,NextState),
	TRef=erlang:start_timer(StateData#state.alarming_interval,self(),tm_sync),
	{NextState,StateData};

handle_control(	Sensor=#sensor{state=deAsserted,label=enable},StateName,
				StateData)->

	?info({control_event,Sensor}),

	LogMessage=io_lib:format("alarm disabled",[]),
	NextState='DISARMED',
	alarmStatusLed(NextState),
	logFsm(StateName,LogMessage,NextState),
	{NextState,StateData}.

handle_statechange_actions(State,State,StateData)->
	StateData;

handle_statechange_actions(OldState,NewState,StateData=#state{tm_alerting=TRef})->
	alarmStatusLed(NewState),
	% needs convert State from atom to string ....

	Message="Alarm state change from: [" ++ 
	atom_to_list(OldState) ++ 
	"] to [" ++ 
	atom_to_list(NewState) ++
	 "]",

	NewStateData=
	case NewState of
		'ACTIVE'->
			NewTRef=erlang:start_timer(StateData#state.alert_on_interval,self(),tm_alert_off),
			?info({activated_alert_timer,NewTRef}),
			% activate siren, set tm_alert timer
			siren(on),
			StateData#state{tm_alerting=NewTRef};

		'ACK' when is_reference(TRef)->
			erlang:cancel_timer(StateData#state.tm_alerting),
			% deactivate siren, cancel tm_alert timer
			siren(off),
			StateData#state{tm_alerting=undefined};

		'CLEAR' when is_reference(TRef)->
			% deactivate siren, cancel tm_alert timer
			erlang:cancel_timer(StateData#state.tm_alerting),
			siren(off),
			StateData#state{tm_alerting=undefined};

		'WAIT_ARM' when is_reference(TRef)->
			siren(off),
			StateData#state{tm_alerting=undefined};

		'DISARMED' when is_reference(TRef)->
			erlang:cancel_timer(StateData#state.tm_alerting),
			siren(off),
			StateData#state{tm_alerting=undefined};

		_->
			StateData
	end,

	case StateData#state.notify_status of
		true->
			twilio_manager:notify(Message);
		_->
			ok
	end,

	?info({stateChange, {from,OldState},{to,NewState}}),
	NewStateData.

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

siren(on)->
	output_manager:set(api:getPort(siren));

siren(off)->
	output_manager:clear(api:getPort(siren)).

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
	?info({control,Control}),
	Outputs=config:get(outputs),
	case lists:keyfind(alarm_status_led,2,Outputs) of
		X={Port,_,_,_,_,_}->
			?info(X),
			case Control of
				on->
					output_manager:set(Port);
				off->
					output_manager:clear(Port);
				{flash,Speed} when is_integer(Speed)->
					output_manager:flash(Port,Speed);
				Other->
					?error({badarg,Other}),
					{error,{badarg,Other}}
			end;		

		false->
			?error(false),
			{error,badarg}
	end.

%% ============================================================================
%% History Logging Routines
%% ============================================================================

fsmFmtLogMessage(CurrentState,Event,NextState) when is_list(Event)->
        S=io_lib:format("[~s] :: ~s -> [~s]",[CurrentState,Event,NextState]),
        list_to_binary(S);

fsmFmtLogMessage(CurrentState,Event,NextState)->
        S=io_lib:format("[~s] :: ~p -> [~s]",[CurrentState,Event,NextState]),
        list_to_binary(S).

logFsm(CurrentState,Event,NextState)->
	MEvent=fsmFmtLogMessage(CurrentState,Event,NextState),
	log(MEvent).

log(Event)->
	history_manager:put({Event,iso8601:format(calendar:local_time())}).

