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

-export([setPort/1]).
-export([clearPort/1]).
-export([readInput/0]).

-export([setOverride/2]).
-export([clrOverride/1]).

% -export([processMask/3]).

-ifndef(INTERVAL).
-define(INTERVAL,?DEFAULT_SCAN_INTERVAL).		%% 50 millisec = 20 Hz
-endif.

-define(MIN_INTERVAL,20).		%% 20 millisec = 50 Hz
-define(MAX_INTERVAL,30000).	%% 30 seconds = 1 Hz

-record(state, {
		raw,
		inputs,
		tref,
		interval=?INTERVAL,
		dispatcher,
		config,
		ovr_mask=0,
		ovr_val=0,	% 1 = asserted, 0 = deasserted
		assertionLevels,
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

setPort(PortNum)->
	gen_server:call(?MODULE,{setPort,PortNum}).

clearPort(PortNum)->
	gen_server:call(?MODULE,{clearPort,PortNum}).

readInput()->
	gen_server:call(?MODULE,readInput).

setOverride(PortNum,Level)->
	gen_server:call(?MODULE,{setOverride,PortNum,Level}).

clrOverride(PortNum)->
	gen_server:call(?MODULE,{clrOverride,PortNum}).


init(_)->
	% TODO -- apply masks on input
	State=#state{},
	Scanner=State#state.scanner,
	Inputs=Scanner(),
	Config=config:get(inputs),
	AssertionLevels=getAssertionLevels(Config),
	TRef=erlang:start_timer(State#state.interval,self(),scan),
	{ok,State#state{tref=TRef,
					raw=Inputs,
					inputs=Inputs,
					assertionLevels=AssertionLevels,
					config=Config}}.

handle_call(stop,_From,State)->
	{stop,normal,ok,State};

handle_call({interval,NewInterval},_From,State=#state{interval=Interval}) when 
	NewInterval=<?MAX_INTERVAL, NewInterval>=?MIN_INTERVAL->
	?info({interval_change,{from,Interval},{to,NewInterval}}),
	{reply,ok,State#state{interval=NewInterval}};

handle_call(state,_From,State)->
	{reply,{ok,State},State};

handle_call(readInput,_From,State)->
	Raw=ioutils:blist(State#state.raw),
	Asserted=ioutils:blist(State#state.inputs),
	Ovr=ioutils:blist(State#state.ovr_mask),
	Val=ioutils:blist(State#state.ovr_val),
	AssertionLevels=ioutils:blist(State#state.assertionLevels),
	Reply={	{key_________,[7,6,5,4,3,2,1,0]},
			{raw_________,Raw},
			{assertLevels,AssertionLevels},
			{asserted____,Asserted},
			{ovr_mask____,Ovr},
			{ovr_val_____,Val}},
	{reply,Reply,State};

handle_call(Msg={setOverride,PortNum,Level},_From,State=#state{ovr_mask=OvrMask,ovr_val=OvrVal})->
	?info(Msg),
	NewOvrMask=setBit(PortNum,OvrMask,1),
	NewOvrVal=setBit(PortNum,OvrVal,Level),
	{reply,ok,State#state{ovr_mask=NewOvrMask,ovr_val=NewOvrVal}};

handle_call(Msg={clrOverride,PortNum},_From,State=#state{ovr_mask=OvrMask})->
	?info(Msg),
	NewOvrMask=setBit(PortNum,OvrMask,0),
	{reply,ok,State#state{ovr_mask=NewOvrMask}};

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
											  assertionLevels=AssertionLevels,
											  ovr_mask=OvrMask,
											  ovr_val=OvrVal})->
	%% timer has fired, trigger a scan
	%% read the raw io values
	RawInputs=Scanner(),

	%% apply the logic to see if the port is asserted  
	Asserted = (OvrMask band OvrVal) bor ((bnot OvrMask) band (bnot(RawInputs bxor AssertionLevels))),

	handle_changes(Asserted,Inputs),
	
	TRef=erlang:start_timer(Interval,self(),scan),
	{noreply,State#state{tref=TRef,raw=RawInputs,inputs=Asserted}};

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
	?info({current,blist(Current),last,blist(Last)}),
	Changes=Current bxor Last,
	?info({changes,Changes}),
	ChangeSet=lists:filter(fun(Z)->isSet(Z,Changes) end,?PORTS),
	?info({changeSet,blist(ChangeSet)}),
	ChangeSet.

blist(Port)->
	<<I7:1,I6:1,I5:1,I4:1,I3:1,I2:1,I1:1,I0:1>> = <<Port>>,
	[I7,I6,I5,I4,I3,I2,I1,I0].

isSet(Pos,Byte)->
	(Byte band (1 bsl (Pos)) > 0).

handle_changes(NewInput,OldInput)->
	ChangeSet=getChangeSet(NewInput,OldInput),
	lists:foreach(fun(Z)->notify_change(Z,isSet(Z,NewInput),isSet(Z,OldInput)) end, ChangeSet).

notify_change(_PortNumber,Value,Value)->
	% no change, ignore
	ok;

notify_change(PortNumber,NewValue,OldValue)->
	?info({notify,{port,PortNumber},{new,NewValue},{old,OldValue}}),
	io_manager:notify(PortNumber,NewValue,OldValue).

getAssertionLevels(Config)->
	[AL0,AL1,AL2,AL3,AL4,AL5,AL6,AL7]=lists:map(fun(Z)->element(5,Z) end,Config),
	<< Byte >> = << AL7:1,AL6:1,AL5:1,AL4:1,AL3:1,AL2:1,AL1:1,AL0:1 >>,
	?info({assertionLevels,Byte}),
	Byte.
	
setBit(Bit,Byte,0)->
	Byte band bnot(1 bsl Bit);

setBit(Bit,Byte,1)->
	Byte bor (1 bsl Bit).


