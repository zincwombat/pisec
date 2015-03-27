-module(io_handler).
-behaviour(gen_fsm).
-include("debug.hrl").
-include("alarm.hrl").

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
	stop/1
]).

-export([
	cmd/2,
	notify/1,
	notify/2,
	pstate/1,
	sync/2
]).

-export([
	'WAIT_SYNC'/2,
	'ACTIVE'/2,
	'CLEAR'/2,
	'DISABLED'/2
]).

-define(START_OPTIONS,  []).
-define(SERVERNAME,     ?MODULE).

%% GLOBAL DEFINES =============================================================


%% STATE DATA =================================================================

-record(state,{	port,
		assertLevel,
		desc,
		history
		}).

start({Port,Desc,disabled,AssertLevel})->
	gen_fsm:start_link(?MODULE,[{Port,Desc,'DISABLED',AssertLevel,self()},[]],[]);

start({Port,Desc,enabled,AssertLevel})->
	gen_fsm:start_link(?MODULE,[{Port,Desc,'WAIT_SYNC',AssertLevel,self()},[]],[]).

stop(Port)->
	gen_fsm:send_all_state_event(Port,{stop,normal}).

notify({Level,Port})->
	notify(Level,Port).

notify(Level,Port)->
	?dbug({notify,{level,Level},{port,Port}}),
	gen_fsm:send_event(Port,{level,Level}).

sync(Level,Port)->
	gen_fsm:send_event(Port,{sync,Level}).

cmd(Port,Command) when is_pid(Port)->
	Reply=
	case Command of
		disable->
			disable(Port);
		enable->
			enable(Port);
		reset->
			reset(Port);
		set->
			set(Port);
		clear->
			clear(Port);
		pstate->
			pstate(Port);
		Unknown->
			{error,{badarg,Unknown}}
	end,
	Reply;

cmd(BadPort,_)->
	{error,{noprocess,BadPort}}.

disable(Port)->
	gen_fsm:send_all_state_event(Port,disable).

enable(Port)->
	gen_fsm:send_all_state_event(Port,enable).

set(Port)->
	gen_fsm:send_all_state_event(Port,set).

clear(Port)->
	gen_fsm:send_all_state_event(Port,clear).

reset(Port)->
	gen_fsm:send_all_state_event(Port,reset).

pstate(Port)->
	gen_fsm:sync_send_all_state_event(Port,pstate).

init([{Port,Desc,InitState,AssertLevel,_Pid},_])->
	?info({started,{port,Port},{desc,Desc},{initState,InitState}}),
	process_flag(trap_exit,true),
	dispatcher:subscribe(Port,self()),
	dispatcher:sync(Port),

	alarm:notify(Port,InitState),
	HistorySize=config:get(port_handler_history_size,?DEFAULT_PORTHANDLER_HISTORY),

	Queue=aqueue:new(HistorySize),
	NewQueue=aqueue:logFsm("NULL","init",InitState,Queue),
	{ok,InitState,#state{	port=Port,
			     	desc=Desc,
			     	assertLevel=AssertLevel,
				history=NewQueue}}.

handle_sync_event(Event=pstate,_From,StateName,StateData=#state{port=Port,desc=Desc,history=H})->
	PS=#portstatus{	ioport=Port,
			pid=self(),
			description=Desc,
			iostate=atom_to_list(StateName),
			log=aqueue:dump(H)},
	{reply,PS,StateName,StateData};

handle_sync_event(Event,_From,StateName,StateData=#state{port=Port})->
	{reply,ignored,StateName,StateData}.


handle_event(Event={stop,Reason},StateName,StateData=#state{port=Port})->
	?info({handle_event,{port,Port},{event,Event},{state,StateName},{stateData,StateData}}),
	{stop,Reason,StateData};

handle_event(Event=disable,StateName,StateData=#state{port=Port,history=Queue})->
	?info({state_change,{port,Port},{event,Event},{from,StateName},{to,'DISABLED'}}),
	alarm:notify(Port,'DISABLED'),
	NewQueue=aqueue:logFsm(StateName,Event,'DISABLED',Queue),
	{next_state,'DISABLED',StateData#state{history=NewQueue}};

handle_event(Event=enable,StateName='DISABLED',StateData=#state{port=Port,history=Queue})->
	?info({state_change,{port,Port},{event,Event},{from,StateName},{to,'WAIT_SYNC'}}),
	alarm:notify(Port,'WAIT_SYNC'),
	dispatcher:sync(Port),
	NewQueue=aqueue:logFsm(StateName,Event,'WAIT_SYNC',Queue),
	{next_state,'WAIT_SYNC',StateData#state{history=NewQueue}};

handle_event(Event,StateName,StateData=#state{port=Port})->
	{next_state,StateName,StateData}.

handle_info(Event={'EXIT',_Pid,stop},StateName,StateData=#state{port=Port})->
	?info({handle_info,{port,Port},{event,Event},{state,StateName},{stateData,StateData}}),
	{stop,shutdown,StateData};

handle_info(Event,StateName,StateData=#state{port=Port})->
	{next_state,StateName,StateData}.

terminate(Reason,StateName,StateData=#state{port=Port})->
	?warn({terminating,{port,Port},{reason,Reason},{state,StateName},{stateData,StateData}}),
	{stop,Reason}.

code_change(_OldVsn,StateName,StateData,_Extra)->
	{ok,StateName,StateData}.	

%% STATE CALLBACKS

'WAIT_SYNC'(Event={sync,Level},StateData=#state{port=Port,assertLevel=Level,history=Queue})->
	?info({state_change,{port,Port},{event,Event},{from,'WAIT_SYNC'},{to,'ACTIVE'}}),
	?info({calling_notify}),
	alarm:notify(Port,'ACTIVE'),
	NewQueue=aqueue:logFsm('WAIT_SYNC',Event,'ACTIVE',Queue),
	{next_state,'ACTIVE',StateData#state{history=NewQueue}};

'WAIT_SYNC'(Event={sync,Level},StateData=#state{port=Port,history=Queue})->
	?info({state_change,{port,Port},{event,Event},{from,'WAIT_SYNC'},{to,'CLEAR'}}),
	alarm:notify(Port,'CLEAR'),
	NewQueue=aqueue:logFsm('WAIT_SYNC',Event,'CLEAR',Queue),
	{next_state,'CLEAR',StateData#state{history=NewQueue}};

'WAIT_SYNC'(Event,StateData=#state{port=Port})->
	{next_state,'WAIT_SYNC',StateData}.

'ACTIVE'(Event={level,Level},StateData=#state{port=Port,assertLevel=Level,history=Queue})->
	?info({{port,Port},{event,Event},{state,'ACTIVE'}}),
	alarm:notify(Port,'ACTIVE'),
	%% TODO why do we need this??
	NewQueue=aqueue:logFsm('ACTIVE',Event,'ACTIVE',Queue),
	{next_state,'ACTIVE',StateData#state{history=NewQueue}};

'ACTIVE'(Event={level,Level},StateData=#state{port=Port,history=Queue})->
	?info({state_change,{port,Port},{event,Event},{from,'ACTIVE'},{to,'CLEAR'}}),
	alarm:notify(Port,'CLEAR'),
	NewQueue=aqueue:logFsm('ACTIVE',Event,'CLEAR',Queue),
	{next_state,'CLEAR',StateData#state{history=NewQueue}};

'ACTIVE'(Event,StateData=#state{port=Port})->
	{next_state,'ACTIVE',StateData}.

'CLEAR'(Event={level,Level},StateData=#state{port=Port,assertLevel=Level,history=Queue})->
	?info({state_change,{port,Port},{event,Event},{from,'CLEAR'},{to,'ACTIVE'}}),
	?info({calling_notify}),
	alarm:notify(Port,'ACTIVE'),
	NewQueue=aqueue:logFsm('CLEAR',Event,'ACTIVE',Queue),
	{next_state,'ACTIVE',StateData#state{history=NewQueue}};

'CLEAR'(Event,StateData=#state{port=Port})->
	{next_state,'CLEAR',StateData}.

'DISABLED'(Event,StateData=#state{port=Port})->
        {next_state,'DISABLED',StateData}.
