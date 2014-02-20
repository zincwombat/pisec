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

-record(state, {}).

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
	gen_server:call(?MODULE,{flash,OutputPort}).

clear()->
	gen_server:call(?MODULE,clear).

init([])->
	?tracelevel(?TRACE_LEVEL),
	State=#state{},
	?info({starting,{pid,self()}}),
	process_flag(trap_exit,true),
	OutputPorts=config:get(outputs),
	lists:map(fun(Z)->i_handleOutput(Z) end,OutputPorts),
	{ok,State#state{}}.

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

handle_call(Msg,From,State)->
	?warn({unhandled_call,{msg,Msg},{from,From}}),
	{reply,ignored,State}.

handle_cast(Msg,State)->
	?warn({unhandled_cast,{msg,Msg}}),
        {noreply,State}.

handle_info(Msg,State)->
	?warn({unhandled_info,{msg,Msg}}),
        {noreply,State}.

code_change(_OldVsn,Ctx,_Extra) ->
	{ok,Ctx}.	

terminate(Reason,_State)->
	?info({terminating,Reason}),
	ok.
