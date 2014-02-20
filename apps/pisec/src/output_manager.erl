-module(output_manager).
-behaviour(gen_server).

-include("debug.hrl").
-include("ports.hrl").
-include("alarm.hrl").

-export([start/0,
         stop/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([
	 state/0,
	 get/1,
	 set/1,
	 clear/1,
	 clear/0,
	 flash/1
	]).

-record(state, {slow_tm_interval,
		normal_tm_interval,
		fast_tm_interval}).

start()->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).

stop()->
	gen_server:call(?MODULE,stop).

state()->
	gen_server:call(?MODULE,state).

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

init([])->
	?tracelevel(?TRACE_LEVEL),
	State=#state{},
	?info({starting,{pid,self()}}),
	process_flag(trap_exit,true),

	SlowFlash=config:get(output_flash_slow,?FLASH_SLOW),
	NormalFlash=config:get(output_flash_normal,?FLASH_NORMAL),
	FastFlash=config:get(output_flash_fast,?FLASH_FAST),

	OutputPorts=config:get(outputs),
	lists:map(fun(Z)->i_handleOutput(Z) end,OutputPorts),

	erlang:start_timer(SlowFlash,self(),{tm,slow}),
	erlang:start_timer(NormalFlash,self(),{tm,normal}),
	erlang:start_timer(FastFlash,self(),{tm,fast}),
	
	{ok,State#state{slow_tm_interval=SlowFlash,
			normal_tm_interval=NormalFlash,
			fast_tm_interval=FastFlash}}.

i_handleOutput(O={PortNum,PortDescription,OnOff})->
	case OnOff of
	on->
		?info(O);
	off->
		?info(O);
	Other->
		?error(Other)
	end.
	

handle_call(stop,_from,State)->
	{stop,normal,ok,State};

handle_call(state,_From,State)->
	{reply,{ok,State},State};

handle_call({set,OutputPort},_From,State) when ?is_portnum(OutputPort)->
	CurrentOutputs=piface:read_output(),
	NewOutputs=CurrentOutputs bor (1 bsl (OutputPort-1)),
	Reply=piface:write_output(NewOutputs),
	{reply,Reply,State};

handle_call({clear,OutputPort},_From,State) when ?is_portnum(OutputPort)->
	CurrentOutputs=piface:read_output(),
	NewOutputs=CurrentOutputs band bnot (1 bsl (OutputPort-1)),
	Reply=piface:write_output(NewOutputs),
	{reply,Reply,State};

handle_call({flash,OutputPort,Speed},_From,State) when ?is_portnum(OutputPort)->
	{reply,ok,State};

handle_call(Msg,From,State)->
	?warn({unhandled_call,{msg,Msg},{from,From}}),
	{reply,ignored,State}.

handle_cast(Msg,State)->
	?warn({unhandled_cast,{msg,Msg}}),
	{noreply,State}.

handle_info({timeout,_TRef,{tm,slow}},State=#state{slow_tm_interval=SlowFlash})->
	%% to test, just toggle port 3 
	CurrentOutputs=piface:read_output(),
	NewOutputs=CurrentOutputs bxor (1 bsl 3),
	Reply=piface:write_output(NewOutputs),

	erlang:start_timer(SlowFlash,self(),{tm,slow}),
	{noreply,State};

handle_info({timeout,_TRef,{tm,normal}},State=#state{normal_tm_interval=NormalFlash})->
	%% to test, just toggle port 4 
	CurrentOutputs=piface:read_output(),
	NewOutputs=CurrentOutputs bxor (1 bsl 4),
	Reply=piface:write_output(NewOutputs),

	erlang:start_timer(NormalFlash,self(),{tm,normal}),
	{noreply,State};

handle_info({timeout,_TRef,{tm,fast}},State=#state{fast_tm_interval=FastFlash})->
	%% to test, just toggle port 5 
	CurrentOutputs=piface:read_output(),
	NewOutputs=CurrentOutputs bxor (1 bsl 5),
	Reply=piface:write_output(NewOutputs),

	erlang:start_timer(FastFlash,self(),{tm,fast}),
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
