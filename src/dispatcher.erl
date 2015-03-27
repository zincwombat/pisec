-module(dispatcher).
-behaviour(gen_server).

-include("debug.hrl").
-include("ports.hrl").
-include("alarm.hrl").

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
	 restartHandler/1,
	 sync/1,
	 enable/0,
	 disable/0,
	 pstate/0,
	 pstate/1,
	 enable/1,
	 disable/1,
	 set/1,
	 clear/1,
	 reset/1,
	 getActive/0,
	 subscribe/2,
	 io_handlers/0
	]).

-record(state, {inputs,ptab,portrec=#portRec{}}).

start()->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).

stop()->
	gen_server:call(?MODULE,stop).

state()->
	gen_server:call(?MODULE,state).

restartHandler(Port)->
	gen_server:call(?MODULE,{restartHandler,Port}).

pstate()->
	gen_server:call(?MODULE,pstate).

sync(Port)->
	gen_server:cast(?MODULE,{sync,Port}).

enable()->
	gen_server:call(?MODULE,enable).

getActive()->
	gen_server:call(?MODULE,getActive).

disable()->
	gen_server:call(?MODULE,disable).

pstate(Port)->
	gen_server:call(?MODULE,{pstate,{port,Port}}).

enable(Port)->
	gen_server:call(?MODULE,{enable,{port,Port}}).

disable(Port)->
	gen_server:call(?MODULE,{disable,{port,Port}}).

set(Port)->
	gen_server:call(?MODULE,{set,{port,Port}}).

clear(Port)->
	gen_server:call(?MODULE,{clear,{port,Port}}).

reset(Port)->
	gen_server:call(?MODULE,{reset,{port,Port}}).

subscribe(Port,Pid)->
	gen_server:call(?MODULE,{subscribe,Port,Pid}).

io_handlers()->
	gen_server:call(?MODULE,io_handlers).

init([])->
	State=#state{},
	?info({starting,{pid,self()}}),
	%% subscribe to the scanner

	%% TODO -- we need to be able to detect the scanner exiting, or else
	%% handle this in the supervision heirarchy

	{ok,Inputs}=scanner:subscribe(),

	PTab=ets:new(ptab,[set,public,named_table]),
	process_flag(trap_exit,true),

	{ok,State#state{inputs=Inputs,ptab=PTab}}.

handle_call(stop,_from,State)->
	{stop,normal,ok,State};

handle_call(enable,_from,State)->
	Reply=broadcast(enable,State),
	{reply,Reply,State};

handle_call(disable,_from,State)->
	Reply=broadcast(disable,State),
	{reply,Reply,State};

handle_call({restartHandler,Port},_from,State=#state{ptab=PTab}) when is_integer(Port)->
	Reply=
	case ets:lookup(PTab,Port) of
	[]->
		?warn({port_not_registered,Port}),
		ignored;
	[{Port,Pid}]->
		?info({restartHandler,{port,Port}}),
		io_handler:stop(Pid)
	end,
	{reply,Reply,State};

handle_call(pstate,_from,State)->
	%% send the command "pstate" to each of the registered
	%% io_handlers

	Reply=broadcast(pstate,State),
	{reply,Reply,State};

handle_call(getActive,_from,State)->
	PState=broadcast(pstate,State),
	%% get all of the ports in the 'ACTIVE' state
	Reply=lists:filter(fun(Z)->i_isActive(Z) end,PState),
	{reply,Reply,State};

handle_call({subscribe,Port,Pid},_From,State=#state{ptab=PTab})->
	%% a handler has subscribed, it needs to provide both a portname and a pid
	case ets:lookup(PTab,Port) of
	[]->
		?info({io_handler_registered,{port,Port},{pid,Pid}}),
		ets:insert(PTab,{Port,Pid}),
		link(Pid);

	[{Port,Pid}]->
		?warn({io_handler_already_registered,{port,Port},{pid,Pid}}),
		ok;

	[{Port,OPid}]->
		?info({io_handler_reregister,{port,Port},{pid,OPid}}),
		unlink(OPid),
		link(Pid),
		ets:insert(PTab,{Port,Pid})
	end,
	{reply,ok,State};

handle_call(state,_From,State)->
	{reply,{ok,State},State};

handle_call(io_handlers,_From,State=#state{ptab=PTab})->
	{reply,ets:tab2list(PTab),State};

handle_call({Cmd,{port,Port}},_From,State=#state{ptab=PTab}) when is_integer(Port)->
	Reply=
	case ets:lookup(PTab,Port) of
	[]->
		E={no_port_handler,{port,Port}},
		?warn(E),
		{error,E};

	[{Port,Pid}] when is_pid(Pid)->
		io_handler:cmd(Pid,Cmd)
	end,
	{reply,Reply,State};

handle_call(Msg,From,State)->
	?warn({unhandled_call,{msg,Msg},{from,From}}),
	{reply,ignored,State}.

handle_cast(Msg={sync,Port},State=#state{ptab=PTab})->
	case ets:lookup(PTab,Port) of
	[]->
		ok;
	[{Port,Pid}]->
		?dbug(Msg),
		{ok,Inputs}=scanner:readPorts(),
		handle_sync(Inputs,Port,Pid)
	end,
	{noreply,State};

handle_cast(Msg,State)->
	?warn({unhandled_cast,{msg,Msg}}),
        {noreply,State}.


handle_info({inputs,Inputs},State)->
	?info({notification,{new,ioutils:blist(Inputs)}}),
	NewState=handle_notify(Inputs,State),
        {noreply,NewState};

handle_info(Msg={'EXIT',Pid,_Reason},State=#state{ptab=PTab})->
	?warn({msg,Msg}),
	IOs=ets:tab2list(PTab),
	case lists:keyfind(Pid,2,IOs) of
	false->
		?warn({unhandled_exit,{pid,Pid}});
	{Port,Pid}->
		?warn({io_handler_exiting,{port,Port},{pid,Pid}}),
		unlink(Pid),
		ets:delete(PTab,Port)
	end,
        {noreply,State};

handle_info(Msg,State)->
	?warn({unhandled_info,{msg,Msg}}),
        {noreply,State}.

code_change(_OldVsn,Ctx,_Extra) ->
	{ok,Ctx}.	

terminate(Reason,_State)->
	?info({terminating,Reason}),
	ok.

%% broadcast the command "Cmd" to all of the registered io_handlers.
%% the io_handlers are stored in the ets table as {port,pid} so each
%% io_handler is addressed by element 2 
%% io_handler implements cmd/2= cmd(Pid,Cmd)

broadcast(Cmd,#state{ptab=PTab})->
	broadcast(Cmd,ets:tab2list(PTab));

broadcast(Cmd,IOList) when is_list(IOList)->
	lists:map(fun(Z)->io_handler:cmd(element(2,Z),Cmd) end,IOList);

broadcast(_Cmd,Badarg)->
	{error,{badarg,Badarg}}.

handle_sync(Inputs,Port,Pid)->	
	Level=ioutils:portVal(Port,Inputs),
	io_handler:sync(Level,Pid),
	ok.

handle_notify(NewPorts,State=#state{ptab=PTab,portrec=#portRec{port=LastPorts}})->
	ChangeSet=ioutils:notifySet(LastPorts,NewPorts),
	?info({handle_notify,{new,ioutils:blist(NewPorts)},{old,ioutils:blist(LastPorts)},{changeSet,ChangeSet}}),
	lists:map(fun(Z)->handle_notify(Z,PTab) end,ChangeSet),
	State#state{portrec=ioutils:mkRec(NewPorts)};

handle_notify({PortNum,NewVal},PTab)->
	i_handle_notify(PortNum,NewVal,ets:lookup(PTab,PortNum)).

i_handle_notify(_PortNum,NewVal,[{_,Pid}])->
	io_handler:notify(NewVal,Pid);

i_handle_notify(_,_,_)->
	?error(badarg).

i_isActive(#portstatus{iostate="ACTIVE"})->
	true;

i_isActive(#portstatus{})->
	false.

	
