-module(piface_simulator).
-behaviour(gen_server).

-include_lib("pisec/include/debug.hrl").
-include_lib("pisec/include/alarm.hrl").

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
	 read_input/0,
	 setScanInterval/1
	]).

-record(state, {scan_interval,
		ports=114, % initial value
		tref}).

-define(DEFAULT_SIM_SCAN_INTERVAL,	30000).	%% milliseconds

start()->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).

stop()->
	gen_server:call(?MODULE,stop).

state()->
	gen_server:call(?MODULE,state).

setScanInterval(Interval)->
	gen_server:call(?MODULE,{setScanInterval,Interval}).

read_input()->
	gen_server:call(?MODULE,read_input).

init([])->
	?tracelevel(?TRACE_LEVEL),
	?info({starting,{pid,self()}}),
	TRef=erlang:start_timer(?DEFAULT_SIM_SCAN_INTERVAL,self(),scan),
	{ok,#state{scan_interval=?DEFAULT_SIM_SCAN_INTERVAL,tref=TRef}}.

handle_call(stop,_from,State)->
	{stop,normal,ok,State};

handle_call(state,_From,State)->
	{reply,{ok,State},State};

handle_call(read_input,_From,State=#state{ports=P})->
	{reply,P,State};

handle_call({setInterval,Interval},_From,State) when is_integer(Interval)->
	{reply,ok,State#state{scan_interval=Interval}};

handle_call(Msg,From,State)->
        ?warn({unhandled_call,{msg,Msg},{from,From}}),
        {reply,ignored,State}.

handle_cast(Msg,State)->
	?warn({unhandled_cast,{msg,Msg}}),
        {noreply,State}.

handle_info(T={timeout,_TRef,scan},State=#state{scan_interval=Interval})->
	?info(T),
	TRef=erlang:start_timer(Interval,self(),scan),
        {noreply,State#state{tref=TRef}};

handle_info(Msg,State)->
	?warn({unhandled_info,{msg,Msg}}),
        {noreply,State}.

code_change(_OldVsn,Ctx,_Extra) ->
	{ok,Ctx}.	

terminate(Reason,_State)->
	?info({terminating,Reason}),
	ok.
