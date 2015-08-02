-module(input_manager).
-behaviour(gen_server).

-export([start_link/0,
         stop/0]).

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

-export ([register/2]).
-export ([unregister/0]).
-export ([show/0]).
-export ([notify/3]).
-export ([getState/0]).
-export ([getState/1]).
-export ([getAssertedAlarms/0]).
-export ([getAssertedControls/0]).


-record (state, {itab}).

%==============================================================================
% API
%==============================================================================

start_link() ->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).

stop() ->
	gen_server:call(?MODULE,stop).

state(Pid) ->
	gen_server:call(?MODULE,state).

register(Port,Label)->
	gen_server:call(?MODULE,{register,Port,self(),Label}).

unregister()->
	gen_server:call(?MODULE,{unregister,self()}).

show()->
	gen_server:call(?MODULE,show).

getState()->
	gen_server:call(?MODULE,getState).

getState(Port)->
	gen_server:call(?MODULE,{getState,Port}).

isAsserted(Port)->
	gen_server:call(?MODULE,{isAsserted,Port}).

getAssertedAlarms()->
	gen_server:call(?MODULE,getAssertedAlarms).

getAssertedControls()->
	gen_server:call(?MODULE,getAssertedControls).

notify(PortNumber,NewValue,OldValue)->
	gen_server:cast(?MODULE,{notify,PortNumber,NewValue,OldValue}).

%==============================================================================
% callback functions
%==============================================================================

init([])->
	?info({pid,self()}),
	process_flag(trap_exit,true),
	ITab=ets:new(itab,[set,public]),
	State=#state{itab=ITab},
	{ok,State}.

handle_call(stop,_From,State)->
	{stop,normal,ok,State};

handle_call(state,_From,State)->
	{reply,{ok,State},State};

handle_call({register,Port,Pid,Label},_From,State=#state{itab=ITab}) when is_pid(Pid)->
    ets:insert(ITab,{Port,Pid,Label}),
    ?info({added_handler,{port,Port}}),
    link(Pid),	
   	{reply,ok,State};

handle_call({unregister,Pid},_From,State=#state{itab=ITab}) when is_pid(Pid)->
    ets:match_delete(ITab,{'_',Pid,'_'}),
    unlink(Pid),	
   	{reply,ok,State};

handle_call(show,_From,State=#state{itab=ITab})->
    Reply=ets:tab2list(ITab),
   	{reply,Reply,State};

handle_call(getState,_From,State=#state{itab=ITab})->
    Handlers=ets:tab2list(ITab),
    Reply=lists:map(fun(Z)->input_handler:getState(element(2,Z)) end,Handlers),
   	{reply,Reply,State};

handle_call(getAssertedAlarms,_From,State=#state{itab=ITab})->
    Handlers=ets:tab2list(ITab),
    Status=lists:map(fun(Z)->input_handler:getState(element(2,Z)) end,Handlers),
    Alarms=lists:filter(fun(Z)->isAssertedAlarm(Z) end,Status),
   	{reply,{alarms,Alarms},State};

handle_call(getAssertedControls,_From,State=#state{itab=ITab})->
    Handlers=ets:tab2list(ITab),
    Status=lists:map(fun(Z)->input_handler:getState(element(2,Z)) end,Handlers),
    Controls=lists:filter(fun(Z)->isAssertedControl(Z) end,Status),
   	{reply,{controls,Controls},State};

% 	allow both port labels (atoms) and integers to be used 

handle_call({getState,Port},_From,State=#state{itab=ITab}) when ?is_portnum(Port)->
    Reply=
    case ets:lookup(ITab,Port) of
    	[{Port,Pid,_}]->
    		input_handler:getState(Pid);
    	_->
    		?error({badarg,{port,Port}}),
    		[]
    end,
   	{reply,Reply,State};

handle_call({getState,Label},_From,State=#state{itab=ITab}) when is_atom(Label)->
	Handlers=ets:tab2list(ITab),
	Reply=
	case lists:keyfind(Label,3,Handlers) of
		{Port,Pid,Label}->
			input_handler:getState(Pid);
		_->
			?error({badarg,{label,Label}}),
			[]
	end,
	{reply,Reply,State};


handle_call(Msg,From,State)->
	Unhandled={unhandled_call,{msg,Msg},{from,From}},
	?warn(Unhandled),
	{reply,Unhandled,State}.

handle_cast({notify,PortNumber,NewValue,OldValue},State=#state{itab=ITab})->
	% ?info({port,PortNumber,new,NewValue,old,OldValue}),
	% dispatch the message to the handler
	case ets:lookup(ITab, PortNumber) of
		[{PortNumber, Pid, _}]->
			Pid ! {stateChange,NewValue,OldValue};
		_->
			% no registered handler
			?warn({no_handler,{port,PortNumber}})
	end,
	{noreply,State};

handle_cast(Msg,State)->
	Unhandled={unhandled_cast,{msg,Msg}},
	?warn(Unhandled),
	{noreply,State}.

handle_info(Msg={'EXIT',Pid,Reason},State=#state{itab=ITab})->
	?warn(Msg),
	% if this is an input port handler, delete it from itab
	ets:match_delete(ITab,{'_',Pid,'_'}),
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

%==============================================================================
% Utility
%==============================================================================

isAssertedAlarm(#sensor{type=sensor,state=asserted})->
	true;

isAssertedAlarm(_)->
	false.

isAssertedControl(#sensor{type=control,state=asserted})->
	true;

isAssertedControl(_)->
	false.



