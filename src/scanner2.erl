-module(scanner2).
-behaviour(gen_server).

-export([start/0,
	 	start/1,
        stop/0]).

-include("debug.hrl").
-include("alarm.hrl").
-include("ports.hrl").

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([setInterval/1,
	 	 state/0]).

-export([getChangeSet/2]).
-export([handle_changes/2]).
-export([isSet/2]).

-export([setAssertMask/1]).
-export([setDeAssertMask/1]).
-export([processAssertMask/2]).
-export([processDeAssertMask/2]).

-ifndef(INTERVAL).
-define(INTERVAL,?DEFAULT_SCAN_INTERVAL).		%% 50 millisec = 20 Hz
-endif.

-define(MIN_INTERVAL,20).		%% 20 millisec = 50 Hz
-define(MAX_INTERVAL,30000).	%% 30 seconds = 1 Hz

-record(state, {inputs,
		tref,
		interval=?INTERVAL,
		dispatcher,
		config,
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

setAssertMask(Mask)->
	gen_server:call(?MODULE,{setAssertMask,Mask}).

setDeAssertMask(Mask)->
	gen_server:call(?MODULE,{setDeAssertMask,Mask}).

init(_)->
	State=#state{},
	Scanner=State#state.scanner,
	Inputs=Scanner(),
	Config=config:get(inputs),
	TRef=erlang:start_timer(State#state.interval,self(),scan),
	{ok,State#state{tref=TRef,inputs=Inputs,config=Config}}.

handle_call(stop,_From,State)->
	{stop,normal,ok,State};

handle_call({interval,NewInterval},_From,State=#state{interval=Interval}) when 
	NewInterval=<?MAX_INTERVAL, NewInterval>=?MIN_INTERVAL->
	?info({interval_change,{from,Interval},{to,NewInterval}}),
	{reply,ok,State#state{interval=NewInterval}};

handle_call(state,_From,State)->
	{reply,{ok,State},State};

handle_call(Msg={setAssertMask,Mask},_From,State) when ?is_uint8(Mask)->
	?info(Msg),
	{reply,ok,State#state{setmask=Mask}};

handle_call(Msg={setDeAssertMask,Mask},_From,State) when ?is_uint8(Mask)->
	?info(Msg),
	{reply,ok,State#state{clearmask=Mask}};

handle_call(Msg,From,State)->
	Unhandled={unhandled_call,{msg,Msg},{from,From}},
	?warn(Unhandled),
	{reply,Unhandled,State}.

handle_cast(Msg,State)->
	Unhandled={unhandled_cast,{msg,Msg}},
	?warn(Unhandled),
    {noreply,State}.

handle_info({timeout,_TRef,scan},State=#state{interval=Interval,
											  scanner=Scanner,
											  inputs=Inputs,
											  setmask=SetMask,
											  clearmask=ClearMask})->
	%% timer has fired, trigger a scan
	%% read the raw io values
	NewInputs=Scanner(),

	% TODO -- apply set and clear masks 
	% foreach bit that is set in SetMask, apply the correct level
	% foreach bit that is set in ClearMask, apply the correct level

	handle_changes(<<NewInputs>>,<<Inputs>>),
	
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

getChangeSet(Same,Same)->
	% no changes to process
	[];

getChangeSet(Current,Last)->
	Changes=Current bxor Last,
	lists:filter(fun(Z)->isSet(Z,Changes) end,?PORTS).

isSet(Pos,Byte)->
	(Byte band (1 bsl (Pos)) > 0).

handle_changes(NewInput,OldInput)->
	ChangeSet=getChangeSet(NewInput,OldInput),
	lists:foreach(fun(Z)->notify_change(Z,isSet(Z,NewInput),isSet(Z,OldInput)) end, ChangeSet).

notify_change(_PortNumber,Value,Value)->
	% no change, ignore
	ok;

notify_change(PortNumber,NewValue,OldValue)->
	io_manager:notify(PortNumber,NewValue,OldValue).


override(Type,PortNum,PortValues)->
	override(Type,PortNum,PortValues,getAssertionLevel(PortNum));

override(assert,PortNum,PortValues,1)->
	PortValues bor (1 bsl (PortNum));

override(assert,PortNum,PortValues,0)->
	PortValues band (bnot(1 bsl (PortNum)));

override(deAssert,PortNum,PortValues,0)->
	PortValues bor (1 bsl (PortNum));

override(deAssert,PortNum,PortValues,1)->
	PortValues band (bnot(1 bsl (PortNum))).
	
getAssertionLevel(PortNum) when ?is_portnum(PortNum)->
	{_,_,_,_,AssertLevel,_}=lists:keyfind(PortNum,1,config:get(inputs)),
	AssertLevel.

processAssertMask(SetMask,PortValues)->
	% get the set of ports to be asserted
	AssertPortSet=lists:filter(fun(Z)->isSet(Z,SetMask) end,?PORTS),
	?info({assertPortSet,AssertPortSet}),
	lists:foldl(fun(Z,Acc)->override(assert,Z,Acc) end,PortValues,AssertPortSet).

processDeAssertMask(ClearMask,PortValues)->
	% get the set of ports to be asserted
	DeAssertPortSet=lists:filter(fun(Z)->isSet(Z,ClearMask) end,?PORTS),
	?info({deAssertPortSet,DeAssertPortSet}),
	lists:foldl(fun(Z,Acc)->override(deAssert,Z,Acc) end,PortValues,DeAssertPortSet).



