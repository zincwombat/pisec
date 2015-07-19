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

-record (state, {port,ledStatus,label,desc}).

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

%==============================================================================
% callback functions
%==============================================================================

init([X={Port,Label,Desc,true,AssertLevel,led}])->
	?info({pid,self()}),
	process_flag(trap_exit,true),
	State=#state{port=Port,label=Label,desc=Desc},
	{ok,State}.

handle_call(stop,_From,State)->
	{stop,normal,ok,State};

handle_call(state,_From,State)->
	{reply,{ok,State},State};

handle_call(on,_From,State=#state{port=Port})->
	% turn led on
	CurrentOutputs=piface2:read_output(),
	NewOutputs=CurrentOutputs bor (1 bsl (Port)),
	Reply=piface2:write_output(NewOutputs),
	{reply,ok,State#state{ledStatus=on}};

handle_call(off,_From,State)->
	% turn led off
	CurrentOutputs=piface2:read_output(),
	NewOutputs=CurrentOutputs band bnot (1 bsl (PortNum)),
	Reply=piface2:write_output(NewOutputs),
	{reply,ok,State#state{ledStatus=off}};

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




