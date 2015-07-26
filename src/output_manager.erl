-module(output_manager).
-behaviour(gen_server).

-export([start/0,
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

-export ([register/3]).
-export ([unregister/0]).
-export ([notify/3]).
-export ([getState/0]).
-export ([getState/1]).

-export ([set/1]).
-export ([clear/1]).
-export ([flash/2]).

-export ([getLedStatus/0]).
-export ([getPowerStatus/0]).

-record (state, {itab}).

%==============================================================================
% API
%==============================================================================

start() ->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).

stop() ->
	gen_server:call(?MODULE,stop).

state(Pid) ->
	gen_server:call(?MODULE,state).

register(Port,Label,Type)->
	gen_server:call(?MODULE,{register,Port,self(),Label,Type}).

unregister()->
	gen_server:call(?MODULE,{unregister,self()}).

getState()->
	gen_server:call(?MODULE,getState).

getState(Port)->
	gen_server:call(?MODULE,{getState,Port}).

notify(PortNumber,NewValue,OldValue)->
	gen_server:cast(?MODULE,{notify,PortNumber,NewValue,OldValue}).

set(Port)->
	gen_server:call(?MODULE,{set,Port}).

clear(Port)->
	gen_server:call(?MODULE,{clear,Port}).

flash(Port,Speed)->
	gen_server:call(?MODULE,{flash,Port,Speed}).

getLedStatus()->
	gen_server:call(?MODULE,{getStatus,led}).

getPowerStatus()->
	gen_server:call(?MODULE,{getStatus,power).


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

handle_call({register,Port,Pid,Label,Type},_From,State=#state{itab=ITab}) when is_pid(Pid)->
    ets:insert(ITab,X={Port,Pid,Label,Type}),
    ?info({added_handler,X}),
    link(Pid),	
   	{reply,ok,State};

handle_call({unregister,Pid},_From,State=#state{itab=ITab}) when is_pid(Pid)->
    ets:match_delete(ITab,{'_',Pid,'_','_'}),
    unlink(Pid),	
   	{reply,ok,State};


handle_call(getState,_From,State=#state{itab=ITab})->
    Handlers=ets:tab2list(ITab),
    Reply=lists:map(fun(Z)->input_handler:getState(element(2,Z)) end,Handlers),
   	{reply,Reply,State};


handle_call({set,Port},_From,State=#state{itab=ITab}) when ?is_portnum(Port)->
	Reply=
    case ets:lookup(ITab,Port) of
    	[{Port,Pid,_,led}]->
    		led_handler:on(Pid);
    	[{Port,Pid,_,power}]->
    		power_handler:on(Pid);
    	_->
    		?error({badarg,{port,Port}}),
    		[]
    end,
   	{reply,Reply,State};

handle_call({clear,Port},_From,State=#state{itab=ITab}) when ?is_portnum(Port)->
	Reply=
    case ets:lookup(ITab,Port) of
    	[{Port,Pid,_,led}]->
    		led_handler:off(Pid);
    	[{Port,Pid,_,power}]->
    		power_handler:off(Pid);
    	_->
    		?error({badarg,{port,Port}}),
    		[]
    end,
   	{reply,Reply,State};

handle_call({flash,Port,Speed},_From,State=#state{itab=ITab}) when ?is_portnum(Port)->
	Reply=
    case ets:lookup(ITab,Port) of
    	[{Port,Pid,_,led}]->
    		led_handler:flash(Pid,Speed);
    	[{Port,Pid,_,power}]->
    		?error({flash,{ignored,{port,Port}}}),
    		ignored;
    	_->
    		?error({badarg,{port,Port}}),
    		[]
    end,
   	{reply,Reply,State};

% 	allow both port labels (atoms) and integers to be used 

handle_call({getState,Port},_From,State=#state{itab=ITab}) when ?is_portnum(Port)->
   	{reply,not_implemented,State};

handle_call({getState,Label},_From,State=#state{itab=ITab}) when is_atom(Label)->
	{reply,not_implemented,State};

handle_call({getStatus,Type},_From,State=#state{itab=ITab})->
	Handlers=getHandlers(Type,ITab),
	{reply,{Type,Handlers},State};

handle_call(Msg,From,State)->
	Unhandled={unhandled_call,{msg,Msg},{from,From}},
	?warn(Unhandled),
	{reply,Unhandled,State}.

handle_cast(Msg,State)->
	Unhandled={unhandled_cast,{msg,Msg}},
	?warn(Unhandled),
	{noreply,State}.

handle_info(Msg={'EXIT',Pid,Reason},State=#state{itab=ITab})->
	?warn(Msg),
	% if this is an input port handler, delete it from itab
	ets:match_delete(ITab,{'_',Pid,'_','_'}),
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
% Utility Functions
% =============================================================================

getHandlers(Type,ITab)->
	lists:filter(fun(Z)->is(Type,Z) end,ets:tab2list(ITab)).

is(Type,{_,_,_,Type})->
	true;

is(_)->
	false.
