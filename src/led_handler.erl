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

-record (state, {port,ledStatus,label,desc,timer,timer_intv}).

%==============================================================================
% API
%==============================================================================

start({Port,Label,Desc,true,AssertLevel,led}) ->
	{ok,Pid}=gen_server:start_link(?MODULE,[{Port,Label,Desc,true,AssertLevel,led}],[]).

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

%==============================================================================
% callback functions
%==============================================================================

init([X={Port,Label,Desc,true,AssertLevel,led}])->
	?info({pid,self()}),
	process_flag(trap_exit,true),
	output_manager:register(Port,Label,led),
	State=#state{port=Port,label=Label,desc=Desc},
	{ok,State}.

handle_call(stop,_From,State)->
	{stop,normal,ok,State};

handle_call(state,_From,State)->
	{reply,{ok,State},State};

handle_call(on,_From,State=#state{port=Port})->
	% turn led on
	i_on(Port),
	{reply,ok,State#state{ledStatus=on}};

handle_call(off,_From,State=#state{port=Port})->
	% turn led off
	i_off(Port),
	{reply,ok,State#state{ledStatus=off}};

handle_call({flash,off},_From,State=#state{port=Port,timer=TRef}) when is_reference(TRef)->
	erlang:cancel_timer(TRef),
	i_off(Port),
	{reply,ok,State#state{timer=null,ledStatus=off,timer_intv=null}};

handle_call({flash,Speed},_From,State=#state{port=Port,timer=TRef}) when is_reference(TRef)->
	erlang:cancel_timer(TRef),
	NewTRef=erlang:start_timer(Speed,self(),{fl_timeout,Speed}),
	{reply,ok,State#state{timer=NewTRef,ledStatus={flash,Speed},timer_intv=Speed}};

handle_call({flash,Speed},_From,State=#state{port=Port,timer=TRef})->
	NewTRef=erlang:start_timer(Speed,self(),{fl_timeout,Speed}),
	{reply,ok,State#state{timer=NewTRef,ledStatus={flash,Speed},timer_intv=Speed}};

handle_call(Msg,From,State)->
	Unhandled={unhandled_call,{msg,Msg},{from,From}},
	?warn(Unhandled),
	{reply,Unhandled,State}.

handle_cast(Msg,State)->
	Unhandled={unhandled_cast,{msg,Msg}},
	?warn(Unhandled),
	{noreply,State}.

handle_info({timeout,TRef,{fl_timeout,Speed}},State=#state{timer=TRef,timer_intv=S})->

	CurrentOutputs=piface2:read_output(),
	NewOutputs=CurrentOutputs bxor S,
	Reply=piface2:write_output(NewOutputs),

	erlang:start_timer(S,self(),{fl_timeout,S}),
	{noreply,State};

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

