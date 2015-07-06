-module(input_handler).
-behaviour(gen_server).

-export([start/1,
         stop/1]).

-include("debug.hrl").

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
	io_manager:register(Port),
	State=#state{port=Port,label=Label,desc=Desc,assertLevel=AssertLevel,type=Type},
	{ok,State}.

handle_call(stop,_From,State)->
	{stop,normal,ok,State};

handle_call(state,_From,State)->
	{reply,{ok,State},State};

handle_call(getState,_From,State=#state{port=Port,
										desc=Desc})->

	Asserted=scanner2:getState(Port),
	SensorStatus=
	case Asserted of
		true->
			asserted;
		_->
			deAsserted
	end,
	{reply,{sensorStatus,{port,Port},{desc,Desc},{status,SensorStatus}},State#state{sensorStatus=SensorStatus}};

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

handle_state_change(false,true,State=#state{port=Port,assertLevel=1})->
	% from 1 to 0, where 1 is asserted
	handle_deassert(State);

handle_state_change(true,false,State=#state{port=Port,assertLevel=0})->
	% from 0 to 1 where 0 is asserted
	handle_deassert(State);

handle_state_change(false,true,State=#state{port=Port,assertLevel=0})->
	% from 1 to 0 where 0 is asserted
	handle_assert(State);

handle_state_change(true,false,State=#state{port=Port,assertLevel=1})->
	% from 0 to 1 where 1 is asserted
	handle_assert(State).

% =============================================================================
%  Handling
% =============================================================================

handle_assert(State=#state{type=sensor})->
	?info({sensor_asserted,State#state.label,State#state.desc}),
	State#state{sensorStatus=asserted};

handle_assert(State=#state{type=control})->
	?info({control_asserted,State#state.label,State#state.desc}),
	State#state{sensorStatus=asserted}.

handle_deassert(State=#state{type=sensor})->
	?info({sensor_deasserted,State#state.label,State#state.desc}),
	State#state{sensorStatus=deAsserted};

handle_deassert(State=#state{type=control})->
	?info({control_deasserted,State#state.label,State#state.desc}),
	State#state{sensorStatus=deAsserted}.


	

