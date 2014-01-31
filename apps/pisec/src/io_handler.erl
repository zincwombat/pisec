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
	sync/2,
	fsync/2
]).

-export(['WAIT_SYNC'/2,
	 'ACTIVE'/2,
	 'CLEAR'/2,
	 'FORCE_ACTIVE'/2,
	 'FORCE_CLEAR'/2,
	 'DISABLED'/2]).

-define(START_OPTIONS,  []).
-define(SERVERNAME,     ?MODULE).

%% GLOBAL DEFINES =============================================================


%% STATE DATA =================================================================

-record(state,{	port,
		assertLevel,
		desc
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

fsync(Level,Port)->
	gen_fsm:sync_send_all_state_event(Port,{fsync,{level,Level}}).


init([{Port,Desc,InitState,AssertLevel,_Pid},_])->
	?tracelevel(?TRACE_LEVEL),
	State=#state{},
	?info({started,{port,Port},{desc,Desc},{initState,InitState}}),
	process_flag(trap_exit,true),
	dispatcher:subscribe(Port,self()),
	dispatcher:sync(Port),
	?info({calling_notify}),
	alarm:notify(Port,InitState),
	{ok,InitState,State#state{port=Port,desc=Desc,assertLevel=AssertLevel}}.

handle_sync_event(Event=pstate,_From,StateName,StateData=#state{port=Port,desc=Desc})->
	?dbug({handle_sync_event,{port,Port},{event,Event},{state,StateName},{stateData,StateData}}),
	PS=#portstatus{ioport=Port,pid=self(),description=Desc,iostate=atom_to_list(StateName)},
	{reply,PS,StateName,StateData};

%%'CLEAR'(Event={level,Level},StateData=#state{port=Port,assertLevel=Level})->

handle_sync_event(Event={fsync,{level,Level}},_From,StateName,StateData=#state{port=Port,assertLevel=Level})->
	%% alarm active
	NextState=
	case StateName of
	'CLEAR'->
		'ACTIVE';
	Other->
		StateName
	end,
	%%?info({calling_notify}),
	alarm:notify(Port,NextState),
	?info({handle_sync_event,{port,Port},{event,Event},{state,StateName},{stateData,StateData}}),
	{reply,NextState,NextState,StateData};

handle_sync_event(Event={fsync,{level,_Level}},_From,StateName,StateData=#state{port=Port,assertLevel=Level})->
	%% alarm CLEAR
	NextState=
	case StateName of
	'ACTIVE'->
		'CLEAR';
	Other->
		StateName
	end,
	%%?info({calling_notify}),
	alarm:notify(Port,NextState),
	?info({handle_sync_event,{port,Port},{event,Event},{state,StateName},{stateData,StateData}}),
	{reply,NextState,NextState,StateData};

handle_sync_event(Event,_From,StateName,StateData=#state{port=Port})->
	?info({handle_sync_event,{port,Port},{event,Event},{state,StateName},{stateData,StateData}}),
	{reply,ignored,StateName,StateData}.

handle_event(Event={stop,Reason},StateName,StateData=#state{port=Port})->
	?info({handle_event,{port,Port},{event,Event},{state,StateName},{stateData,StateData}}),
	{stop,Reason,StateData};

handle_event(Event=disable,StateName,StateData=#state{port=Port})->
	?info({state_change,{port,Port},{event,Event},{from,StateName},{to,'DISABLED'}}),
	?info({calling_notify}),
	alarm:notify(Port,'DISABLED'),
	{next_state,'DISABLED',StateData};

handle_event(Event=enable,StateName='DISABLED',StateData=#state{port=Port})->
	?info({state_change,{port,Port},{event,Event},{from,StateName},{to,'WAIT_SYNC'}}),
	?info({calling_notify}),
	alarm:notify(Port,'WAIT_SYNC'),
	dispatcher:sync(Port),
	{next_state,'WAIT_SYNC',StateData};

%% SET override
handle_event(Event=set,StateName,StateData=#state{port=Port}) when StateName /= 'FORCE_ACTIVE'->
	?info({state_change,{port,Port},{event,Event},{from,StateName},{to,'FORCE_ACTIVE'}}),
	?info({calling_notify}),
	alarm:notify(Port,'FORCE_ACTIVE'),
	{next_state,'FORCE_ACTIVE',StateData};

%% CLEAR override
handle_event(Event=clear,StateName,StateData=#state{port=Port}) when StateName /= 'FORCE_CLEAR'->
	?info({state_change,{port,Port},{event,Event},{from,StateName},{to,'FORCE_CLEAR'}}),
	?info({calling_notify}),
	alarm:notify(Port,'FORCE_CLEAR'),
	{next_state,'FORCE_CLEAR',StateData};

%% RESET override
handle_event(Event=reset,StateName,StateData=#state{port=Port}) when StateName == 'FORCE_CLEAR';
								     StateName == 'FORCE_ACTIVE'->
	?info({state_change,{port,Port},{event,Event},{from,StateName},{to,'WAIT_SYNC'}}),
	dispatcher:sync(Port),
	?info({calling_notify}),
	alarm:notify(Port,'WAIT_SYNC'),
	{next_state,'WAIT_SYNC',StateData};

handle_event(Event,StateName,StateData=#state{port=Port})->
	?dbug({handle_event,{port,Port},{event,Event},{state,StateName},{stateData,StateData}}),
	{next_state,StateName,StateData}.

handle_info(Event={'EXIT',_Pid,stop},StateName,StateData=#state{port=Port})->
	?info({handle_info,{port,Port},{event,Event},{state,StateName},{stateData,StateData}}),
	{stop,shutdown,StateData};

handle_info(Event,StateName,StateData=#state{port=Port})->
	?dbug({handle_info,{port,Port},{event,Event},{state,StateName},{stateData,StateData}}),
	{next_state,StateName,StateData}.

terminate(Reason,StateName,StateData=#state{port=Port})->
	?warn({terminating,{port,Port},{reason,Reason},{state,StateName},{stateData,StateData}}),
	{stop,Reason}.

code_change(_OldVsn,StateName,StateData,_Extra)->
	{ok,StateName,StateData}.	

%% STATE CALLBACKS

'WAIT_SYNC'(Event={sync,Level},StateData=#state{port=Port,assertLevel=Level})->
	?info({state_change,{port,Port},{event,Event},{from,'WAIT_SYNC'},{to,'ACTIVE'}}),
	?info({calling_notify}),
	alarm:notify(Port,'ACTIVE'),
	{next_state,'ACTIVE',StateData};

'WAIT_SYNC'(Event={sync,_Level},StateData=#state{port=Port})->
	?info({state_change,{port,Port},{event,Event},{from,'WAIT_SYNC'},{to,'CLEAR'}}),
	?info({calling_notify}),
	alarm:notify(Port,'CLEAR'),
	{next_state,'CLEAR',StateData};

'WAIT_SYNC'(Event,StateData=#state{port=Port})->
	?info({unhandled,{port,Port},{event,Event},{state,'WAIT_SYNC'}}),
	{next_state,'WAIT_SYNC',StateData}.

'DISABLED'(Event,StateData=#state{port=Port})->
	?info({unhandled,{port,Port},{event,Event},{state,'FORCE_CLEAR'}}),
	{next_state,'DISABLED',StateData}.

'ACTIVE'(Event={level,Level},StateData=#state{port=Port,assertLevel=Level})->
	?info({{port,Port},{event,Event},{state,'ACTIVE'}}),
	?info({calling_notify}),
	alarm:notify(Port,'ACTIVE'),
	{next_state,'ACTIVE',StateData};

'ACTIVE'(Event={level,_Level},StateData=#state{port=Port})->
	?info({state_change,{port,Port},{event,Event},{from,'ACTIVE'},{to,'CLEAR'}}),
	?info({"calling alarm:notify()",{port,Port},'CLEAR'}),
	alarm:notify(Port,'CLEAR'),
	{next_state,'CLEAR',StateData};

'ACTIVE'(Event,StateData=#state{port=Port})->
	?info({unhandled,{port,Port},{event,Event},{state,'ACTIVE'}}),
	{next_state,'ACTIVE',StateData}.

'FORCE_ACTIVE'(Event,StateData=#state{port=Port})->
	?info({unhandled,{port,Port},{event,Event},{state,'FORCE_ACTIVE'}}),
	{next_state,'FORCE_ACTIVE',StateData}.

'FORCE_CLEAR'(Event,StateData=#state{port=Port})->
	?info({unhandled,{port,Port},{event,Event},{state,'FORCE_CLEAR'}}),
	{next_state,'FORCE_CLEAR',StateData}.

'CLEAR'(Event={level,Level},StateData=#state{port=Port,assertLevel=Level})->
	?info({state_change,{port,Port},{event,Event},{from,'CLEAR'},{to,'ACTIVE'}}),
	?info({calling_notify}),
	alarm:notify(Port,'ACTIVE'),
	{next_state,'ACTIVE',StateData};

'CLEAR'(Event,StateData=#state{port=Port})->
	?info({unhandled,{port,Port},{event,Event},{state,'CLEAR'}}),
	{next_state,'CLEAR',StateData}.
