-module(power_handler).
-behaviour(gen_server).

-export([start/1,
         stop/1]).

-include("debug.hrl").
-include("ports.hrl").
-include("alarm.hrl").

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

% API

-export ([on/1]).
-export ([off/1]).
-export ([getState/1]).


-record (state, {port,powerStatus,label,desc,timer,timer_intv}).

%==============================================================================
% API
%==============================================================================

start({Port,Label,Desc,true,InitState,power}) ->
	{ok,Pid}=gen_server:start_link(?MODULE,[{Port,Label,Desc,true,InitState,power}],[]).

stop(Pid) ->
	gen_server:call(Pid,stop).

on(Pid)->
	gen_server:call(Pid,on).

off(Pid)->
	gen_server:call(Pid,off).

getState(Pid)->
	gen_server:call(Pid,getStatus).

%==============================================================================
% callback functions
%==============================================================================

init([X={Port,Label,Desc,true,InitState,power}])->
	?info({pid,self()}),
	process_flag(trap_exit,true),
	output_manager:register(Port,Label,power),
	i_set(Port,InitState),
	% get the initial state by reading the register
	State=#state{port=Port,powerStatus=InitState,label=Label,desc=Desc},
	{ok,State}.

handle_call(on,_From,State=#state{port=Port})->
	% turn led on
	i_on(Port),
	{reply,ok,State#state{powerStatus=on}};

handle_call(off,_From,State=#state{port=Port})->
	% turn led off
	i_off(Port),
	{reply,ok,State#state{powerStatus=off}};

handle_call(stop,_From,State)->
	{stop,normal,ok,State};

handle_call(Msg=getState,_From,State)->
	Reply={
		{port,	State#state.port},
		{state,	State#state.powerStatus},
		{label,	State#state.label},
		{desc,	State#state.desc},
		{type,	power}
	},
	{reply,Reply,State};

handle_call(Msg,From,State)->
	Unhandled={unhandled_call,{msg,Msg},{from,From}},
	?warn(Unhandled),
	{reply,Unhandled,State}.

handle_cast(Msg,State)->
	Unhandled={unhandled_cast,{msg,Msg}},
	?warn(Unhandled),
	{noreply,State}.

handle_info(Msg,State)->
	Unhandled={unhandled_info,{msg,Msg}},
	?warn(Unhandled),
	{noreply,State}.

code_change(_OldVsn,Ctx,_Extra) ->
	{ok,Ctx}.	

terminate(Reason,#state{})->
	?info({terminating,Reason}),
	ok.

%==============================================================================
% Utility
%==============================================================================

i_set(Port,on)->
	i_on(Port);

i_set(Port,off)->
	i_off(Port).

i_on(Port)->
	CurrentOutputs=piface2:read_output(),
	NewOutputs=CurrentOutputs bor (1 bsl (Port)),
	piface2:write_output(NewOutputs).

i_off(Port)->
	CurrentOutputs=piface2:read_output(),
	NewOutputs=CurrentOutputs band bnot (1 bsl (Port)),
	piface2:write_output(NewOutputs).

i_toggle(Port)->
	CurrentOutputs=piface2:read_output(),
	NewOutputs=CurrentOutputs bxor (1 bsl (Port)),
	piface2:write_output(NewOutputs).

i_state(Port)->
	CurrentOutputs=piface2:read_output(),
	case (CurrentOutputs band (1 bsl (Port))) of
		0->
			off;
		_->
			on
	end.





