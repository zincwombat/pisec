-module(io_manager).
-behaviour(gen_server).

-export([start_link/0,
         stop/0]).

-include("debug.hrl").

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export ([register/2]).
-export ([unregister/0]).

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

register(Port)->
	gen_server:call(?MODULE,{register,Port,self()}).

unregister()->
	gen_server:call(?MODULE,{unregister,self()}).

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

handle_call({register,Port,Pid},_From,State=#state{itab=ITab}) when is_pid(Pid)->
    ets:insert(ITab,{Port,Pid}),
    ?info({added_handler,{port,Port}}),
    link(Pid),	
   	{reply,ok,State};

handle_call({unregister,Pid},_From,State=#state{itab=ITab}) when is_pid(Pid)->
    ets:match_delete(ITab,{'_',Pid}),
    unlink(Pid),	
   	{reply,ok,State};

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
	ets:match_delete(ITab,{'_',Pid}),
	{noreply,State}.

handle_info(Msg,State)->
	Unhandled={unhandled_info,{msg,Msg}},
	?warn(Unhandled),
	{noreply,State}.

code_change(_OldVsn,Ctx,_Extra) ->
	{ok,Ctx}.	

terminate(Reason,#state{})->
	?info({terminating,Reason}),
	ok.
