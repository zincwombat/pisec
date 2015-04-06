-module(output_manager).
-behaviour(gen_server).

-include("debug.hrl").
-include("ports.hrl").
-include("alarm.hrl").

-export([
	start/0,
	stop/0
]).

-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-export([
	state/0,
	get/1,
	set/1,
	cmd/2,
	clear/1,
	clear/0,
	flash/1,
	flash/2
]).

-export ([siren/1]).


-record(state, {
	otab,
	slow_tm_int,
	normal_tm_int,
	fast_tm_int,
	slow_set=0,
	normal_set=0,
	fast_set=0
}).

start()->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).

stop()->
	gen_server:call(?MODULE,stop).

state()->
	gen_server:call(?MODULE,state).

cmd(on,OutputPort)->
	set(OutputPort);

cmd(off,OutputPort)->
	clear(OutputPort);

cmd(Other,_)->
	{error,{badarg,Other}}.


set(OutputPort)->
	gen_server:call(?MODULE,{set,OutputPort}).

get(OutputPort)->
	gen_server:call(?MODULE,{get,OutputPort}).

clear(OutputPort)->
	gen_server:call(?MODULE,{clear,OutputPort}).

flash(OutputPort)->
	flash(OutputPort,normal).

flash(OutputPort,Speed)->
	gen_server:call(?MODULE,{flash,OutputPort,Speed}).

clear()->
	gen_server:call(?MODULE,clear).


%========================================================================================
% Utility functions
%========================================================================================

siren(Control) when ?is_onoff(Control)->
	Outputs=config:get(outputs),
	case lists:keyfind(siren,2,Outputs) of
		{Port,siren,_,_}->
			cmd(Control,Port);

		false->
			{error,badarg}
	end;

siren(Control)->
	{error,{badarg,Control}}.

init([])->
	State=#state{},
	?info({starting,{pid,self()}}),
	process_flag(trap_exit,true),

	piface:write_output(0),

	%% this stores the flash state of each LED

	OTab=ets:new(otab,[set,public,named_table]),

	SlowFlash=config:get(output_flash_slow,?FLASH_SLOW),
	NormalFlash=config:get(output_flash_normal,?FLASH_NORMAL),
	FastFlash=config:get(output_flash_fast,?FLASH_FAST),

	OutputPorts=config:get(outputs),

	%% process the initial state of each output port

	lists:map(fun(Z)->i_handleOutput(Z) end,OutputPorts),

	%% start the flash timers

	T1=erlang:start_timer(SlowFlash,self(),{tm,slow}),
	T2=erlang:start_timer(NormalFlash,self(),{tm,normal}),
	T3=erlang:start_timer(FastFlash,self(),{tm,fast}),
	
	{ok,State#state{otab=OTab,
			slow_tm_int=SlowFlash,
			normal_tm_int=NormalFlash,
			fast_tm_int=FastFlash}}.

i_handleOutput(O={PortNum,_,_,on})->
	i_setPort(PortNum);

i_handleOutput(O={PortNum,_,_,off})->
	i_clearPort(PortNum).

i_clearPort(PortNum)->
	CurrentOutputs=piface:read_output(),
	NewOutputs=CurrentOutputs band bnot (1 bsl (PortNum-1)),
	Reply=piface:write_output(NewOutputs).

i_setPort(PortNum)->
	CurrentOutputs=piface:read_output(),
	NewOutputs=CurrentOutputs bor (1 bsl (PortNum-1)),
	Reply=piface:write_output(NewOutputs).

is(Speed,{_Port,Speed})->
	true;

is(_,_)->
	false.

handle_call(stop,_from,State)->
	{stop,normal,ok,State};

handle_call(state,_From,State)->
	{reply,{ok,State},State};

handle_call({set,OutputPort},_From,State) when ?is_oport(OutputPort)->
	CurrentOutputs=piface:read_output(),
	NewOutputs=CurrentOutputs bor (1 bsl (OutputPort-1)),
	Reply=piface:write_output(NewOutputs),
	{reply,Reply,State};

handle_call(clear,_From,State)->
	ets:delete_all_objects(State#state.otab),
	Reply=piface:write_output(0),
	{reply,Reply,State#state{slow_set=0,normal_set=0,fast_set=0}};

handle_call({clear,OutputPort},_From,State) when ?is_oport(OutputPort)->
	i_clearPort(OutputPort),
	ets:delete(State#state.otab,OutputPort),
	TL=ets:tab2list(State#state.otab),
	Fast=lists:filter(fun(Z)->is(fast,Z) end,TL),
	Normal=lists:filter(fun(Z)->is(normal,Z) end,TL),
	Slow=lists:filter(fun(Z)->is(slow,Z) end,TL),
	{reply,{Slow,Normal,Fast},State#state{slow_set=buildMask(Slow),
					      				  normal_set=buildMask(Normal),
					      				  fast_set=buildMask(Fast)}};

handle_call({flash,OutputPort,off},_From,State) when ?is_led(OutputPort)->
	ets:delete(State#state.otab,OutputPort),
	TL=ets:tab2list(State#state.otab),
	Fast=lists:filter(fun(Z)->is(fast,Z) end,TL),
	Normal=lists:filter(fun(Z)->is(normal,Z) end,TL),
	Slow=lists:filter(fun(Z)->is(slow,Z) end,TL),
	i_clearPort(OutputPort),
	{reply,{Slow,Normal,Fast},State#state{slow_set=buildMask(Slow),
					      				  normal_set=buildMask(Normal),
					      				  fast_set=buildMask(Fast)}};

handle_call({flash,OutputPort,Speed},_From,State) when ?is_led(OutputPort),?is_speed(Speed)->
	ets:insert(State#state.otab,{OutputPort,Speed}),
	TL=ets:tab2list(State#state.otab),
	Fast=lists:filter(fun(Z)->is(fast,Z) end,TL),
	Normal=lists:filter(fun(Z)->is(normal,Z) end,TL),
	Slow=lists:filter(fun(Z)->is(slow,Z) end,TL),
	i_setPort(OutputPort),
	{reply,{Slow,Normal,Fast},State#state{slow_set=buildMask(Slow),
					      				  normal_set=buildMask(Normal),
					      				  fast_set=buildMask(Fast)}};

handle_call(Msg,From,State)->
	?warn({unhandled_call,{msg,Msg},{from,From}}),
	{reply,ignored,State}.

handle_cast(Msg,State)->
	{noreply,State}.

handle_info({timeout,_TRef,{tm,slow}},State=#state{slow_tm_int=Slow,slow_set=S})->

	CurrentOutputs=piface:read_output(),
	NewOutputs=CurrentOutputs bxor S,
	Reply=piface:write_output(NewOutputs),

	erlang:start_timer(Slow,self(),{tm,slow}),
	{noreply,State};

handle_info({timeout,_TRef,{tm,normal}},State=#state{normal_tm_int=Normal,normal_set=S})->

	CurrentOutputs=piface:read_output(),
	NewOutputs=CurrentOutputs bxor S,
	Reply=piface:write_output(NewOutputs),

	erlang:start_timer(Normal,self(),{tm,normal}),
	{noreply,State};

handle_info({timeout,_TRef,{tm,fast}},State=#state{fast_tm_int=Fast,fast_set=S})->

	CurrentOutputs=piface:read_output(),
	NewOutputs=CurrentOutputs bxor S,
	Reply=piface:write_output(NewOutputs),

	erlang:start_timer(Fast,self(),{tm,fast}),
	{noreply,State};

handle_info({timeout,_TRef,{tm,Speed}},State=#state{})->
	{noreply,State};

handle_info(Msg,State)->
	?warn({unhandled_info,{msg,Msg}}),
	{noreply,State}.

code_change(_OldVsn,Ctx,_Extra) ->
	{ok,Ctx}.	

terminate(Reason,_State)->
	?info({terminating,Reason}),
	ok.

buildMask(S) when is_list(S)->
	lists:foldl(fun(Z,Byte)->Byte bor (1 bsl (element(1,Z)-1)) end,0,S);

buildMask(_)->
	0.

