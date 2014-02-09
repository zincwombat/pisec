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

log(Event,#state{history=H})->
	log(Event,H);

log(Event,Queue)->
	aqueue:add({Event,iso8601:format(erlang:now())},Queue).

init([{Port,Desc,InitState,AssertLevel,_Pid},_])->
	?tracelevel(?TRACE_LEVEL),
	?info({started,{port,Port},{desc,Desc},{initState,InitState}}),
	process_flag(trap_exit,true),
	dispatcher:subscribe(Port,self()),
	dispatcher:sync(Port),
	alarm:notify(Port,InitState),
	HistorySize=config:get(port_handler_history_size,?DEFAULT_PORTHANDLER_HISTORY),
	LogMessage="(INIT) NULL->" ++ atom_to_list(InitState),
	Q=log(LogMessage,aqueue:new(HistorySize)),
	{ok,InitState,#state{port=Port,desc=Desc,assertLevel=AssertLevel,history=Q}}.

handle_sync_event(Event=pstate,_From,StateName,StateData=#state{port=Port,desc=Desc,history=H})->
	?dbug({handle_sync_event,{port,Port},{event,Event},{state,StateName},{stateData,StateData}}),
	PS=#portstatus{	ioport=Port,
			pid=self(),
			description=Desc,
			iostate=atom_to_list(StateName),
			log=aqueue:dump(H)},
	{reply,PS,StateName,StateData};

handle_sync_event(Event,_From,StateName,StateData=#state{port=Port})->
	?info({handle_sync_event,{port,Port},{event,Event},{state,StateName},{stateData,StateData}}),
	{reply,ignored,StateName,StateData}.

handle_event(Event={stop,Reason},StateName,StateData=#state{port=Port})->
	?info({handle_event,{port,Port},{event,Event},{state,StateName},{stateData,StateData}}),
	{stop,Reason,StateData};

handle_event(Event=disable,StateName,StateData=#state{port=Port,history=Q})->
	?info({state_change,{port,Port},{event,Event},{from,StateName},{to,'DISABLED'}}),
	?info({calling_notify}),
	%% TODO -- log in fifo
	alarm:notify(Port,'DISABLED'),
	LogMessage="(DISABLE) " ++ atom_to_list(StateName) ++ "->DISABLED",
	{next_state,'DISABLED',StateData#state{history=log(LogMessage,Q)}};

handle_event(Event=enable,StateName='DISABLED',StateData=#state{port=Port,history=Q})->
	?info({state_change,{port,Port},{event,Event},{from,StateName},{to,'WAIT_SYNC'}}),
	?info({calling_notify}),
	%% TODO -- log in fifo
	alarm:notify(Port,'WAIT_SYNC'),
	dispatcher:sync(Port),
	LogMessage="(ENABLE) DISABLED->WAIT_SYNC",
	{next_state,'WAIT_SYNC',StateData#state{history=log(LogMessage,Q)}};

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

'WAIT_SYNC'(Event={sync,Level},StateData=#state{port=Port,assertLevel=Level,history=Q})->
	?info({state_change,{port,Port},{event,Event},{from,'WAIT_SYNC'},{to,'ACTIVE'}}),
	?info({calling_notify}),
	%% TODO -- log in fifo
	alarm:notify(Port,'ACTIVE'),
	LogMessage="(SYNC " ++ integer_to_list(Level) ++ ") WAIT_SYNC->ACTIVE",
	{next_state,'ACTIVE',StateData#state{history=log(LogMessage,Q)}};

'WAIT_SYNC'(Event={sync,Level},StateData=#state{port=Port,history=Q})->
	?info({state_change,{port,Port},{event,Event},{from,'WAIT_SYNC'},{to,'CLEAR'}}),
	?info({calling_notify}),
	%% TODO -- log in fifo
	alarm:notify(Port,'CLEAR'),
	LogMessage="(SYNC " ++ integer_to_list(Level) ++ ") WAIT_SYNC->CLEAR",
	{next_state,'CLEAR',StateData#state{history=log(LogMessage,Q)}};

'WAIT_SYNC'(Event,StateData=#state{port=Port})->
	?info({unhandled,{port,Port},{event,Event},{state,'WAIT_SYNC'}}),
	{next_state,'WAIT_SYNC',StateData}.

'ACTIVE'(Event={level,Level},StateData=#state{port=Port,assertLevel=Level,history=Q})->
	?info({{port,Port},{event,Event},{state,'ACTIVE'}}),
	?info({calling_notify}),
	%% TODO -- log in fifo
	alarm:notify(Port,'ACTIVE'),
	LogMessage="(SYNC " ++ integer_to_list(Level) ++ ") ACTIVE->ACTIVE",
	{next_state,'ACTIVE',StateData#state{history=log(LogMessage,Q)}};

'ACTIVE'(Event={level,Level},StateData=#state{port=Port,history=Q})->
	?info({state_change,{port,Port},{event,Event},{from,'ACTIVE'},{to,'CLEAR'}}),
	?info({"calling alarm:notify()",{port,Port},'CLEAR'}),
	%% TODO -- log in fifo
	alarm:notify(Port,'CLEAR'),
	LogMessage="(SYNC " ++ integer_to_list(Level) ++ ") ACTIVE->CLEAR",
	{next_state,'CLEAR',StateData#state{history=log(LogMessage,Q)}};

'ACTIVE'(Event,StateData=#state{port=Port})->
	?info({unhandled,{port,Port},{event,Event},{state,'ACTIVE'}}),
	{next_state,'ACTIVE',StateData}.


'CLEAR'(Event={level,Level},StateData=#state{port=Port,assertLevel=Level,history=Q})->
	?info({state_change,{port,Port},{event,Event},{from,'CLEAR'},{to,'ACTIVE'}}),
	?info({calling_notify}),
	%% TODO -- log in fifo
	alarm:notify(Port,'ACTIVE'),
	LogMessage="(SYNC " ++ integer_to_list(Level) ++ ") CLEAR->ACTIVE",
	{next_state,'ACTIVE',StateData#state{history=log(LogMessage,Q)}};

'CLEAR'(Event,StateData=#state{port=Port})->
	?info({unhandled,{port,Port},{event,Event},{state,'CLEAR'}}),
	{next_state,'CLEAR',StateData}.

'DISABLED'(Event,StateData=#state{port=Port})->
        ?info({unhandled,{port,Port},{event,Event},{state,'DISABLED'}}),
        {next_state,'DISABLED',StateData}.
