-module(alarm).
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
	stop/0,
	state/0,	%% return all alarms
	state/1,	%% return all alarms in given state
	portstate/1,	%% return the particular portstate
	masks/0,
	notify/2	%% client notifies of state change
]).

-export([
	arm/0,
	disarm/0,
	ack/0,
	unack/0,
	active/0,
	scan/0,
	sync/0
]).

-export([
		'DISARMED'/2,
		'CLEAR'/2,
		'ACTIVE'/2,
		'ACK'/2,
		'SYNC'/2
	]
).

-define(START_OPTIONS,  []).
-define(SERVERNAME,     ?MODULE).


%% STATE DATA =================================================================

-record(state,{	
		tm_arming,
		tm_alerting,
		tm_sync,
		active_alarms
		}).

start(InitState)->
	gen_fsm:start_link({local,?MODULE},?MODULE,InitState,[]).

stop()->
	gen_fsm:send_all_state_event(?MODULE,{stop,normal}).

notify(Port,PortState)->
	gen_fsm:send_event(?MODULE,{notify,{{port,Port},{state,PortState}}}).

disarm()->
	gen_fsm:sync_send_all_state_event(?MODULE,disarm).

arm()->
	gen_fsm:sync_send_all_state_event(?MODULE,arm).

ack()->
	gen_fsm:sync_send_all_state_event(?MODULE,ack).

unack()->
	gen_fsm:sync_send_all_state_event(?MODULE,unack).

sync()->
	scan().

scan()->
	gen_fsm:sync_send_all_state_event(?MODULE,scan).

state()->
	gen_fsm:sync_send_all_state_event(?MODULE,state).

state(AlarmState)->
	gen_fsm:sync_send_all_state_event(?MODULE,{state,AlarmState}).

portstate(Port)->
	gen_fsm:sync_send_all_state_event(?MODULE,{portstate,Port}).

masks()->
	gen_fsm:sync_send_all_state_event(?MODULE,masks).

active()->
	gen_fsm:sync_send_all_state_event(?MODULE,active).

init(InitState)->
	?tracelevel(?TRACE_LEVEL),
	?info({startup_state,InitState}),
	StateData=#state{active_alarms=sets:new()},
	process_flag(trap_exit,true),
	%%i_init(InitState,StateData).
	{NextState,NextStateData}=i_sync(InitState,StateData),
	{ok,NextState,NextStateData}.

%%=====================================================================================
%% STATE ENGINE for SYNC calls
%%=====================================================================================
i_sync(State,StateData)->
	case catch(dispatcher:getActive()) of
        E={'EXIT',_Reason}->
                ?warn({dispatcher_failed,E}),
                ?info({starting_timer,{timer,tm_sync},{interval,?ALARMING_INTERVAL}}),
                TRef=erlang:start_timer(?SYNC_INTERVAL,self(),{tm_sync,State}),
                {'SYNC',StateData#state{tm_sync=TRef}};

	[] when State=='ACTIVE'->
		{'CLEAR',StateData#state{active_alarms=sets:new()}};

	[] when State=='ACK'->
		{'CLEAR',StateData#state{active_alarms=sets:new()}};

	[] when State=='CLEAR'->
		{'CLEAR',StateData#state{active_alarms=sets:new()}};

        ActiveAlarms-> 
		ActiveSet=addToSet(ActiveAlarms),

		NextState2=
		case State of
		'ARMED'->
			'ACTIVE';

		'CLEAR'->
			'ACTIVE';

		Other->
			Other
		end,

		{NextState2,StateData#state{tm_sync=undefined,active_alarms=ActiveSet}}
        end.


i_scan(State,StateData)->
	i_sync(State,StateData).

%%	{NewState,NewStateData}=
%%	case dispatcher:getActive() of
%%        []->
%%                {'CLEAR',StateData#state{tm_arming=undefined,active_alarms=sets:new()}};
%%
%%        ActiveAlarms->
%%		ActiveSet=addToSet(ActiveAlarms),
%%
%%		NextState=
%%		case State of
%%			'CLEAR'->
%%				'ACTIVE';
%%
%%		Other->
%%			Other
%%		end,
%%                {NextState,StateData#state{tm_sync=undefined,active_alarms=ActiveSet}}
%%        end,
%%	{NewState,NewStateData}.

addToSet(L) when is_list(L)->
	lists:foldl(fun(Z,Set)->addToSet(Z,Set) end, sets:new(), L).

addToSet(#portstatus{ioport=Port},AS)->
	addToSet({port,Port},AS);

addToSet(P={port,_Port},AS)->
	sets:add_element(P,AS).
	
handle_sync_event(disarm,_From,StateName,StateData=#state{}) when StateName /= 'DISARMED'->
	config:set(initstate,'DISARMED'),
	?info({state,'DISARMED'}),
	{reply,{ok,'DISARMED'},'DISARMED',StateData};

handle_sync_event(arm,_From,'DISARMED',StateData=#state{active_alarms=AA})->
	config:set(initstate,'ARMED'),
	NextState=
	case sets:size(AA) of
	0->
		'CLEAR';
	_->
		'ACTIVE'
	end,
	{reply,{ok,NextState},NextState,StateData};

handle_sync_event(unack,_From,'ACK',StateData=#state{active_alarms=AA})->
	NextState=
	case sets:size(AA) of
	0->
		'CLEAR';
	_->
		'ACTIVE'
	end,
	{reply,{ok,NextState},NextState,StateData};

handle_sync_event(ack,_From,'ACTIVE',StateData=#state{active_alarms=AA})->
	NextState=
	case sets:size(AA) of
	0->
		'CLEAR';
	_->
		'ACK'
	end,
	{reply,{ok,NextState},NextState,StateData};

handle_sync_event(ack,_From,'ACK',StateData=#state{active_alarms=AA})->
	NextState=
	case sets:size(AA) of
	0->
		'CLEAR';
	_->
		'ACK'
	end,
	{reply,{ok,NextState},NextState,StateData};

handle_sync_event(scan,_From,State,StateData=#state{})->
	dispatcher:fsync(),
	{NextState,NewStateData}=i_scan(State,StateData),
	{reply,{ok,NextState},NextState,NewStateData};

handle_sync_event(state,_From,State,StateData=#state{})->
	PM=scanner:getMasks(),
	PS=lists:sort(dispatcher:pstate()),
	MS=buildPortStatusResponse(PM,PS),
	AS=#alarmstatus{alarmstate=atom_to_list(State),portfilter="ALL",portstate=MS},
	{reply,AS,State,StateData};

handle_sync_event({state,AlarmState},_From,State,StateData=#state{})->
	PM=scanner:getMasks(),
	PS=lists:sort(dispatcher:pstate()),
	MS=buildPortStatusResponse(PM,PS),
	MSS=lists:filter(fun(Z)->portFilter(AlarmState,Z) end,MS),
	AS=#alarmstatus{alarmstate=atom_to_list(State),portfilter=AlarmState,portstate=MSS},
	{reply,AS,State,StateData};

handle_sync_event({portstate,Port},_From,State,StateData) when ?IN_RANGE(Port)->
	PM=scanner:getMasks(Port),
	PS=dispatcher:pstate(Port),
	PortState=combine(PS,PM),
	{reply,PortState,State,StateData};

handle_sync_event(active,_From,State,StateData=#state{active_alarms=AA})->
	{reply,sets:to_list(AA),State,StateData};
		
handle_sync_event(Event,_From,StateName,StateData=#state{})->
	{reply,?error({unhandled,Event}),StateName,StateData}.

handle_event(_Event=disarm,StateName,StateData=#state{})->
	?info({alarmstate,{from,StateName},{to,'DISARMED'}}),
	{next_state,'DISARMED',StateData};

handle_event(_Event={stop,Reason},_StateName,StateData=#state{})->
	{stop,Reason,StateData};

handle_event(_Event,StateName,StateData)->
	{next_state,StateName,StateData}.

handle_info(_Event={timeout,_TRef,M={tm_sync,InitState}},'SYNC',StateData)->
        %% timer has fired, trigger a scan so that the system can
        %% (1) populate the active_alarm set
	%% (2) exercise the FSM to decide the next state
	?info({timer_fired,M}),
	{NextState,NewStateData}=i_sync(InitState,StateData),
	{next_state,NextState,NewStateData};

handle_info(Event,State,StateData)->
	?info({ignored_info,Event}),
	{next_state,State,StateData}.

terminate(Reason,_StateName,_StateData)->
	{stop,Reason}.

code_change(_OldVsn,StateName,StateData,_Extra)->
	{ok,StateName,StateData}.	

buildPortStatusResponse(PortMaskList,PortStatusList)->
	lists:zipwith(fun(X,Y)->combine(X,Y) end,PortStatusList,PortMaskList).

combine(PS=#portstatus{ioport=I},#portmask{ioport=I,maskstate=M})->
	PS#portstatus{maskstate=M}.

portFilter(Status,#portstatus{iostate=Status})->
	true;

portFilter(_,_)->
	false.

handle_alarm(N={notify,{P={port,_Port},{state,AlarmState}}},StateData=#state{active_alarms=AA})->
	NAA=
	case AlarmState of
	'ACTIVE'->
		sets:add_element(P,AA);
	'CLEAR'->
		sets:del_element(P,AA);
	_Other->
		?warn({unhandled,N}),
		AA
	end,
	StateData#state{active_alarms=NAA}.


%% STATE CALLBACKS

'DISARMED'(Event={notify,{{port,_Port},{state,_AlarmState}}},StateData)->
	?info({{event,Event},{state,'DISARMED'}}),
	NewStateData=handle_alarm(Event,StateData),
	{next_state,'DISARMED',NewStateData};

'DISARMED'(Event,StateData)->
	?info({{ignored_event,Event},{state,'DISARMED'}}),
	{next_state,'DISARMED',StateData}.

'SYNC'(Event,StateData)->
	?info({{ignored_event,Event},{state,'SYNC'}}),
	{next_state,'SYNC',StateData}.

'ACK'(Event={notify,{{port,_Port},{state,_AlarmState}}},StateData)->
	?info({{event,Event},{state,'ACK'}}),
	NewStateData=handle_alarm(Event,StateData),
	{next_state,'ACK',NewStateData};

'ACK'(Event,StateData)->
	?info({{event,Event},{state,'ACK'}}),
	{next_state,'ACK',StateData}.

'CLEAR'(Event={notify,{{port,_Port},{state,'ACTIVE'}}},StateData)->
	?info({{event,Event},{state,'CLEAR'}}),
	NewStateData=handle_alarm(Event,StateData),
	{next_state,'ACTIVE',NewStateData};

'CLEAR'(Event,StateData)->
	?info({{event,Event},{state,'CLEAR'}}),
	{next_state,'CLEAR',StateData}.

'ACTIVE'(Event={notify,{{port,_Port},{state,_AlarmState}}},StateData)->
	?info({{event,Event},{state,'ACTIVE'}}),
	NewStateData=handle_alarm(Event,StateData),
	{next_state,'ACTIVE',NewStateData};

'ACTIVE'(Event,StateData)->
	?info({{event,Event},{state,'ACTIVE'}}),
	{next_state,'ACTIVE',StateData}.

