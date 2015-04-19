-module(scanner2).
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
	 	 state/0]).

-ifndef(INTERVAL).
-define(INTERVAL,?DEFAULT_SCAN_INTERVAL).		%% 50 millisec = 20 Hz
-endif.

-define(MIN_INTERVAL,20).		%% 20 millisec = 50 Hz
-define(MAX_INTERVAL,30000).	%% 30 seconds = 1 Hz

-record(state, {inputs,
		tref,
		interval=?INTERVAL,
		dispatcher,
		setmask=0,
		clearmask=0,
		simulator=false,
		scanner=fun()->piface2:read_input() end,
		debug=false}).

start()->
	start([]).

start(Args)->
	gen_server:start_link({local,?MODULE},?MODULE,Args,[]).

stop() ->
	gen_server:call(?MODULE,stop).

setInterval(Interval)->
	gen_server:call(?MODULE,{interval,Interval}).

state()->
	gen_server:call(?MODULE,state).

init(_)->
	State=#state{},
	TRef=erlang:start_timer(State#state.interval,self(),scan),
	{ok,State#state{tref=TRef}}.

handle_call(stop,_From,State)->
	{stop,normal,ok,State};

handle_call({interval,NewInterval},_From,State=#state{interval=Interval}) when 
	NewInterval=<?MAX_INTERVAL, NewInterval>=?MIN_INTERVAL->
	?info({interval_change,{from,Interval},{to,NewInterval}}),
	{reply,ok,State#state{interval=NewInterval}};

handle_call(state,_From,State)->
	{reply,{ok,State},State};

handle_call(Msg,From,State)->
	Unhandled={unhandled_call,{msg,Msg},{from,From}},
	?warn(Unhandled),
	{reply,Unhandled,State}.

handle_cast(Msg,State)->
	Unhandled={unhandled_cast,{msg,Msg}},
	?warn(Unhandled),
    {noreply,State}.

handle_info({timeout,_TRef,scan},State=#state{interval=Interval,scanner=Scanner})->
	%% timer has fired, trigger a scan
	%% read the raw io values
	Inputs=Scanner(),
	% Override=Inputs band (bnot SetMask) bor ClearMask,
	%% restart timer
	TRef=erlang:start_timer(Interval,self(),scan),
	% {ok,NewState}=handle_inputs(Override,State#state{tref=TRef}),
	{noreply,State#state{tref=TRef}};

handle_info(Msg,State)->
	Unhandled={unhandled_info,{msg,Msg}},
	?warn(Unhandled),
    {noreply,State}.

code_change(_OldVsn,Ctx,_Extra) ->
	{ok,Ctx}.	

terminate(Reason,_State)->
	?info({terminating,Reason}),
	ok.
