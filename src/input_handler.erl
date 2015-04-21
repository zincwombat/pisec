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

-record (state, {port,label,desc,assertLevel,type}).

%==============================================================================
% API
%==============================================================================

start({Port,Label,Desc,true,AssertLevel}) ->
	{ok,Pid}=gen_server:start_link(?MODULE,[{Port,Label,Desc,true,AssertLevel}],[]).

stop(Pid) ->
	gen_server:call(Pid,stop).

state(Pid) ->
	gen_server:call(Pid,state).

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

handle_call(Msg,From,State)->
	Unhandled={unhandled_call,{msg,Msg},{from,From}},
	?warn(Unhandled),
	{reply,Unhandled,State}.

handle_cast(Msg,State)->
	Unhandled={unhandled_cast,{msg,Msg}},
	?warn(Unhandled),
	{noreply,State}.

handle_info(Msg={stateChange,NewValue,OldValue},State)->
	?info(Msg),
	handle_state_change(NewValue,OldValue,State),
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

% =============================================================================
% Miscellaneous
% =============================================================================

handle_state_change(false,true,State=#state{port=Port,assertLevel=1})->
	% asserted
	handle_assert(State);

handle_state_change(true,false,State=#state{port=Port,assertLevel=0})->
	% asserted
	handle_assert(State);

handle_state_change(false,true,State=#state{port=Port,assertLevel=0})->
	% asserted
	handle_deassert(State);

handle_state_change(true,false,State=#state{port=Port,assertLevel=1})->
	% asserted
	handle_deassert(State);

handle_state_change(New,Old,State)->
	% should be unreachable
	?error({unreachable,New,Old,State}),
	error.

handle_assert(State)->
	?info({asserted,State}),
	ok.

handle_deassert(State)->
	?info({deasserted,State}),
	ok.





	

