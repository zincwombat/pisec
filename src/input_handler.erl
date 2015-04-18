-module(input_handler).
-behaviour(gen_server).

-export([start/0,
         stop/0]).

-include("debug.hrl").

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record (state, {port,desc,assertLevel}).

start({Port,Desc,true,AssertLevel}) ->
	{ok,Pid}=gen_server:start_link(?MODULE,[{Port,Desc,true,AssertLevel}],[]).

stop(Pid) ->
	gen_server:call(Pid,stop).

state() ->
	gen_server:call(Pid,state).

init([X={Port,Desc,true,AssertLevel}])->
	?info({pid,self(),{args,X}}),
	State=#state{port=Port,desc=Desc,assertLevel=AssertLevel},
	{ok,State};

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

handle_info(Msg,State)->
	Unhandled={unhandled_info,{msg,Msg}},
	?warn(Unhandled),
	{noreply,State}.

code_change(_OldVsn,Ctx,_Extra) ->
	{ok,Ctx}.	

terminate(Reason,#state{})->
	?info({terminating,Reason}),
	ok.
