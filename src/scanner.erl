-module(scanner).
-behaviour(gen_server).

-export([start/0,
	 start/1,
         stop/0]).

-include("debug.hrl").
-include("alarm.hrl").

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([setInterval/1,
	 state/0,
	 simulator/1,
	 getMasks/0,
	 getMasks/1,
	 subscribe/0,
	 readPorts/0,
	 setDebug/1,
	 setPort/1,
	 clearPort/1,
	 resetPort/1,
	 resetAllPorts/0,
	 setDefaults/0]).


-define(DEBUG,false).

-ifndef(INTERVAL).
-define(INTERVAL,?DEFAULT_SCAN_INTERVAL).		%% 50 millisec = 20 Hz
-endif.

-define(MIN_INTERVAL,20).	%% 20 millisec = 50 Hz
-define(MAX_INTERVAL,30000).	%% 30 seconds = 1 Hz

-record(state, {inputs,
		tref,
		interval,
		dispatcher,
		setmask=0,
		clearmask=0,
		simulator=false,
		scanner=fun()->piface:read_input() end,
		debug=false}).

start()->
	start([]).

start(Args)->
	gen_server:start_link({local,?MODULE},?MODULE,Args,[]).

stop() ->
	gen_server:call(?MODULE,stop).

simulator(Value)->
	gen_server:call(?MODULE,{simulator,Value}).

setPort(Port)->
	%% override -- set a "1" in the right bit position for Port in State#state.setmask
	gen_server:call(?MODULE,{setPort,Port}).

clearPort(Port)->
	%% override set a "1" in the right bit position for Port in State#state.clearmask
	gen_server:call(?MODULE,{clearPort,Port}).

resetPort(Port)->
	%% remove any overrides for Port
	gen_server:call(?MODULE,{resetPort,Port}).

resetAllPorts()->
	%% remove any overrides for all Ports
	gen_server:call(?MODULE,resetAllPorts).

setInterval(Interval)->
	gen_server:call(?MODULE,{interval,Interval}).

setDefaults()->
	gen_server:call(?MODULE,setDefaults).

setDebug(Bool)->
	gen_server:call(?MODULE,{setDebug,Bool}).

state()->
	gen_server:call(?MODULE,state).

getMasks()->
	gen_server:call(?MODULE,getMasks).

getMasks(IOPort)->
	gen_server:call(?MODULE,{getMasks,IOPort}).

readPorts()->
	gen_server:call(?MODULE,readPorts).

subscribe()->
	gen_server:call(?MODULE,subscribe).

init(AC=#alarmconf{setmask=SetMask,clearmask=ClearMask,simulator=Simulator})->
	?info({init_args,AC}),
	?tracelevel(?TRACE_LEVEL),

	State=setSimulatorState(Simulator,#state{interval=?INTERVAL,debug=?DEBUG,setmask=SetMask,clearmask=ClearMask}),
	?info({starting,{pid,self()},{setmask,SetMask},{clearmask,ClearMask},{simulator,Simulator},{scan_interval,State#state.interval}}),

	Inputs=(State#state.scanner)(),
	TRef=erlang:start_timer(State#state.interval,self(),scan),
	{ok,State#state{inputs=Inputs,tref=TRef}}.

handle_call(stop,_From,State)->
	{stop,normal,ok,State};

handle_call({simulator,Boolean},_From,State)->
	{reply,ok,setSimulatorState(Boolean,State)};

handle_call({interval,NewInterval},_From,State=#state{interval=Interval}) when 
	NewInterval=<?MAX_INTERVAL, NewInterval>=?MIN_INTERVAL->
	?info({interval_change,{from,Interval},{to,NewInterval}}),
	{reply,ok,State#state{interval=NewInterval}};

handle_call({interval,BadInterval},_From,State)->
	Error={error,{outofrange,BadInterval}},
	?warn(Error),
	{reply,Error,State};

handle_call({setPort,Port},_From,State=#state{setmask=SetMask,clearmask=ClearMask}) when is_integer(Port), Port>0, Port<9->
	NewSetMask=SetMask bor (1 bsl (Port-1)),
	NewClearMask=ClearMask band bnot (1 bsl (Port-1)),
	?info({setPort,{port,Port},{setmask,NewSetMask},{clearMask,NewClearMask}}),
	config:set({setmask,NewSetMask}),
	config:set({clearmask,NewClearMask}),
	{reply,ok,State#state{setmask=NewSetMask,clearmask=NewClearMask}};

handle_call({clearPort,Port},_From,State=#state{setmask=SetMask,clearmask=ClearMask}) when is_integer(Port), Port>0, Port<9->
	NewClearMask=ClearMask bor (1 bsl (Port-1)),
	NewSetMask=SetMask band bnot (1 bsl (Port-1)),
	config:set({setmask,NewSetMask}),
	config:set({clearmask,NewClearMask}),
	?info({clearPort,{port,Port},{setmask,NewSetMask},{clearMask,NewClearMask}}),
	{reply,ok,State#state{clearmask=NewClearMask,setmask=NewSetMask}};

handle_call({resetPort,Port},_From,State=#state{setmask=SetMask,clearmask=ClearMask}) when is_integer(Port), Port>0, Port<9->
	NewClearMask=ClearMask band  bnot (1 bsl (Port-1)),
	NewSetMask=SetMask band bnot (1 bsl (Port-1)),
	config:set({setmask,NewSetMask}),
	config:set({clearmask,NewClearMask}),
	?info({resetPort,{port,Port},{setmask,NewSetMask},{clearMask,NewClearMask}}),
	{reply,ok,State#state{clearmask=NewClearMask,setmask=NewSetMask}};

handle_call(resetAllPorts,_From,State=#state{})->
	?info({resetAllPorts}),
	NewSetMask=0,
	NewClearMask=0,
	config:set({setmask,NewSetMask}),
	config:set({clearmask,NewClearMask}),
	?info({resetAllPorts,{setmask,NewSetMask},{clearMask,NewClearMask}}),
	{reply,ok,State#state{clearmask=NewClearMask,setmask=NewSetMask}};

handle_call({setDebug,Bool},_From,State) when is_boolean(Bool)->
	{reply,ok,State#state{debug=Bool}};

handle_call({setDebug,Badarg},_From,State)->
	{reply,{error,{badarg,Badarg}},State};

handle_call(subscribe,{Pid,_},State=#state{inputs=Inputs})->
	?info({dispatcher_subscribed,{pid,Pid}}),
	link(Pid),
	process_flag(trap_exit,true),
	{reply,{ok,Inputs},State#state{dispatcher=Pid}};

handle_call(state,_From,State)->
	{reply,{ok,State},State};

handle_call(getMasks,_From,State=#state{})->
	MS=buildPortMaskSet(State),
	{reply,MS,State};

handle_call({getMasks,IOPort},_From,State=#state{})->
	MS=buildPortMaskSet(State),
	Reply=
	case lists:keysearch(IOPort,2,MS) of
	{value,PM}->
		PM;
	false->
		{error,{badarg,IOPort}}
	end,
	{reply,Reply,State};

handle_call(readPorts,_From,State=#state{inputs=Inputs})->
	{reply,{ok,Inputs},State};

handle_call(Msg,From,State)->
	Unhandled={unhandled_call,{msg,Msg},{from,From}},
	?warn(Unhandled),
	{reply,Unhandled,State}.

handle_cast(Msg,State)->
	Unhandled={unhandled_cast,{msg,Msg}},
	?warn(Unhandled),
        {noreply,State}.

handle_info({timeout,_TRef,scan},State=#state{interval=Interval,setmask=SetMask,clearmask=ClearMask,scanner=Scanner})->
	%% timer has fired, trigger a scan
	%% read the raw io values
	Inputs=Scanner(),
	Override=Inputs band (bnot SetMask) bor ClearMask,
	%% restart timer
	TRef=erlang:start_timer(Interval,self(),scan),
	{ok,NewState}=handle_inputs(Override,State#state{tref=TRef}),
	{noreply,NewState};

handle_info({'EXIT',Pid,Reason},State=#state{dispatcher=Pid})->
	?warn({dispatcher_exit,{reason,Reason}}),
        {noreply,State#state{dispatcher=undefined}};

handle_info(Msg,State)->
	Unhandled={unhandled_info,{msg,Msg}},
	?warn(Unhandled),
        {noreply,State}.

code_change(_OldVsn,Ctx,_Extra) ->
	{ok,Ctx}.	

terminate(Reason,_State)->
	?info({terminating,Reason}),
	ok.

%% utility function

buildPortMaskSet(#state{setmask=SetMask,clearmask=ClearMask})->
	I=lists:seq(1,8),
	lists:map(fun(Z)->i_mask(Z,SetMask,ClearMask) end,I).

i_mask(IOPort,SetMask,ClearMask)->
	MaskVal=case ioutils:isSet(IOPort,SetMask) of
                true->"SET";
                false->
                case ioutils:isSet(IOPort,ClearMask) of
                        true->"CLEAR";
                        false->"AUTO"
                end
        end,
	#portmask{ioport=IOPort,maskstate=MaskVal}.

%% logic

handle_inputs(Inputs,State=#state{inputs=Inputs})->
	{ok,State#state{inputs=Inputs}};

handle_inputs(Inputs,State=#state{inputs=LastInputs,dispatcher=Pid}) when is_pid(Pid)->
	?info({state_change,{new,ioutils:blist(Inputs)},{old,ioutils:blist(LastInputs)}}),
	%% TODO -- split control inputs from alarm inputs
	Pid ! {inputs,Inputs},
	{ok,State#state{inputs=Inputs}};

handle_inputs(Inputs,State)->
	?warn({unhandled,{inputs,Inputs},{state,State}}),
	{ok,State}.

setSimulatorState(true,State=#state{simulator=false,interval=_Interval})->
	?warn("**** activating SIMULATOR ****"),
	Scanner=fun()->piface_simulator:read_input() end,
	State#state{simulator=true,interval=?SIMULATOR_SCAN_INTERVAL,scanner=Scanner};

setSimulatorState(false,State=#state{simulator=true,interval=_Interval})->
	?warn("**** entering NORMAL mode ****"),
	Scanner=fun()->piface:read_input() end,
	State#state{simulator=false,interval=?INTERVAL,scanner=Scanner};

setSimulatorState(_,State)->
	State.
