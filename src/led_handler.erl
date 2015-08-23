-module(led_handler).
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
-export ([flash/2]).
-export ([getState/1]).

-record (state, {port,ledStatus,label,desc,timer,timer_intv}).

%==============================================================================
% API
%==============================================================================

start({Port,Label,Desc,true,InitState,led}) ->
	{ok,Pid}=gen_server:start_link(?MODULE,[{Port,Label,Desc,true,InitState,led}],[]).

stop(Pid) ->
	gen_server:call(Pid,stop).

state(Pid) ->
	gen_server:call(Pid,state).

on(Pid) ->
	gen_server:call(Pid,on).

off(Pid) ->
	gen_server:call(Pid,off).

flash(Pid,Speed)->
	gen_server:call(Pid,{flash,Speed}).

getState(Pid)->
	gen_server:call(Pid,getState).

%==============================================================================
% callback functions
%==============================================================================

init([X={Port,Label,Desc,true,InitState,led}])->
	?info({pid,self()}),
	process_flag(trap_exit,true),
	output_manager:register(Port,Label,led),
	i_set(Port,InitState),
	State=#state{port=Port,ledStatus=InitState,label=Label,desc=Desc},
	?info({init,State}),
	{ok,State}.

handle_call(stop,_From,State)->
	{stop,normal,ok,State};

handle_call(state,_From,State)->
	{reply,{ok,State},State};

handle_call(on,_From,State=#state{port=Port,timer=TRef})->
	% turn led on
	case is_reference(TRef) of
		true->
			erlang:cancel_timer(TRef);
		_->
			ok
	end,
	i_on(Port),
	{reply,ok,State#state{ledStatus=on}};

handle_call(off,_From,State=#state{port=Port,timer=TRef})->
	% turn led off
	case is_reference(TRef) of
		true->
			erlang:cancel_timer(TRef);
		_->
			ok
	end,
	i_off(Port),
	{reply,ok,State#state{ledStatus=off}};

handle_call({flash,off},_From,State=#state{port=Port,timer=TRef}) when is_reference(TRef)->
	erlang:cancel_timer(TRef),
	i_off(Port),
	{reply,ok,State#state{timer=null,ledStatus=off,timer_intv=null}};

handle_call(Msg={flash,off},_From,State)->
	?info({ignored,Msg}),
	{reply,ok,State};

handle_call({flash,Speed},_From,State=#state{port=Port,timer=TRef}) when is_reference(TRef)->
	erlang:cancel_timer(TRef),
	NewTRef=erlang:start_timer(Speed,self(),{fl_timeout,Speed}),
	{reply,ok,State#state{timer=NewTRef,ledStatus={flash,Speed},timer_intv=Speed}};

handle_call({flash,Speed},_From,State=#state{port=Port,timer=TRef})->
	NewTRef=erlang:start_timer(Speed,self(),{fl_timeout,Speed}),
	{reply,ok,State#state{timer=NewTRef,ledStatus={flash,Speed},timer_intv=Speed}};

handle_call(Msg=getState,_From,State)->
	Reply={
		{port,	State#state.port},
		{state,	State#state.ledStatus},
		{label,	State#state.label},
		{desc,	State#state.desc},
		{type,	led}
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

handle_info({timeout,TRef,{fl_timeout,Speed}},State=#state{port=Port,timer=TRef,timer_intv=S})->
	i_toggle(Port),
	NewTRef=erlang:start_timer(S,self(),{fl_timeout,S}),
	{noreply,State#state{timer=NewTRef}};

handle_info(Msg,State)->
	Unhandled={unhandled_info,{msg,Msg},{state,State}},
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
	NewOutputs=CurrentOutputs bxor (1 bsl (Port)),
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

