-module(input_handler).
-behaviour(gen_server).

-export([start/1,
         stop/1]).

-include("debug.hrl").
-include("alarm.hrl").

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([getState/1]).

-record (state, {port,sensorStatus,label,desc,assertLevel,type}).

%==============================================================================
% API
%==============================================================================

start({Port,Label,Desc,true,AssertLevel,Type}) ->
	{ok,Pid}=gen_server:start_link(?MODULE,[{Port,Label,Desc,true,AssertLevel,Type}],[]).

stop(Pid) ->
	gen_server:call(Pid,stop).

state(Pid) ->
	gen_server:call(Pid,state).

getState(Pid)->
	gen_server:call(Pid,getState).

%==============================================================================
% callback functions
%==============================================================================

init([X={Port,Label,Desc,true,AssertLevel,Type}])->
	?info({pid,self(),{args,X}}),
	io_manager:register(Port,Label),
	State=#state{port=Port,label=Label,desc=Desc,assertLevel=AssertLevel,type=Type},
	{ok,State}.

handle_call(stop,_From,State)->
	{stop,normal,ok,State};

handle_call(state,_From,State)->
	{reply,{ok,State},State};

handle_call(getState,_From,State=#state{port=Port})->

	Asserted=scanner2:getState(Port),
	SensorStatus=
	case Asserted of
		true->
			asserted;
		_->
			deAsserted
	end,
	NewState=State#state{sensorStatus=SensorStatus},

	{reply,stateToEvent(NewState),NewState};

handle_call(Msg,From,State)->
	Unhandled={unhandled_call,{msg,Msg},{from,From}},
	?warn(Unhandled),
	{reply,Unhandled,State}.

handle_cast(Msg,State)->
	Unhandled={unhandled_cast,{msg,Msg}},
	?warn(Unhandled),
	{noreply,State}.

handle_info(Msg={stateChange,NewValue,OldValue},State)->
	NewState=handle_state_change(NewValue,OldValue,State),
	{noreply,NewState};

handle_info(Msg,State)->
	Unhandled={unhandled_info,{msg,Msg}},
	?warn(Unhandled),
	{noreply,State}.

code_change(_OldVsn,Ctx,_Extra) ->
	{ok,Ctx}.	

terminate(Reason,#state{})->
	?info({terminating,Reason}),
	ok.

% =============================================================================
% Miscellaneous
% =============================================================================

handle_state_change(false,true,State)->
	handle_deassert(State);

handle_state_change(true,false,State)->
	handle_assert(State).

% =============================================================================
%  Handling, send changes to the alarm manager FSM
% =============================================================================

handle_assert(State)->
	?info({sensor_asserted,State#state.label,State#state.desc}),
	NewState=State#state{sensorStatus=asserted},
	alarm2:notify(stateToEvent(NewState)),
	NewState.

handle_deassert(State)->
	?info({sensor_deasserted,State#state.label,State#state.desc}),
	NewState=State#state{sensorStatus=deAsserted},
	alarm2:notify(stateToEvent(NewState)),
	NewState.

% =============================================================================
%  Convert to a record type that alarm FSM expects
% =============================================================================

stateToEvent(State=#state{	port=Port,
							assertLevel=AssertLevel,
							sensorStatus=SensorStatus,
							desc=Desc,
							label=Label,
							type=Type })->
	#event{	port=Port,
			sensorStatus=SensorStatus,
			label=Label,
			desc=Desc,
			assertLevel=AssertLevel,
			type=Type}.



	

