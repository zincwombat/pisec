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
	Scanner=State#state.scanner,
	Inputs=Scanner(),
	TRef=erlang:start_timer(State#state.interval,self(),scan),
	{ok,State#state{tref=TRef,inputs=Inputs}}.

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

handle_info({timeout,_TRef,scan},State=#state{interval=Interval,scanner=Scanner,inputs=Inputs})->
	%% timer has fired, trigger a scan
	%% read the raw io values
	NewInputs=Scanner(),
	% TODO -- apply set and clear masks 


	handle_changes(NewInputs,Inputs),
	
	TRef=erlang:start_timer(Interval,self(),scan),
	{noreply,State#state{tref=TRef,inputs=NewInputs}};

handle_info(Msg,State)->
	Unhandled={unhandled_info,{msg,Msg}},
	?warn(Unhandled),
    {noreply,State}.

code_change(_OldVsn,Ctx,_Extra) ->
	{ok,Ctx}.	

terminate(Reason,_State)->
	?info({terminating,Reason}),
	ok.

%==============================================================================
% Miscellaneous
%==============================================================================

handle_changes(Inputs,Inputs)->
	% no change -- nothing to do
	ok;

handle_changes(NewInputValues= << N7:1,N6:1,N5:1,N4:1,N3:1,N2:1,N1:1,N0:1 >>,
			   OldInputValues= << O7:1,O6:1,O5:1,O4:1,O3:1,O2:1,O1:1,O0:1 >>)->

	% values have changed, process the changes
	% pseudo code
	% for i=0..7 if old(i) != new(i) update io_handler responsible for i

	% TODO -- do this in a more efficient manner taking into account set and 
	% clear masks etc

	notify_change(7,N7,O7),
	notify_change(6,N6,O6),
	notify_change(5,N5,O5),
	notify_change(4,N4,O4),
	notify_change(3,N3,O3),
	notify_change(2,N2,O2),
	notify_change(1,N1,O1),
	notify_change(0,N0,O0),

	?info({new,NewInputValues,old,OldInputValues}),
	ok.

notify_change(_PortNumber,Value,Value)->
	% no change, ignore
	ok;

notify_change(PortNumber,NewValue,OldValue)->
	io_manager:notify(PortNumber,NewValue,OldValue).
