-module (history_manager).
-behaviour(gen_server).

-export([start/0,
         stop/0]).

-include("debug.hrl").
-include("alarm.hrl").

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export ([put/1]).
-export ([getAll/0]).
-export ([getLast/0]).
-export ([getLastN/1]).
-export ([flush/0]).
-export ([size/0]).

-record(state, {htab}).

start() ->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).

stop() ->
	gen_server:call(?MODULE,stop).

state() ->
	gen_server:call(?MODULE,state).

init([])->
	State=#state{},
	{ok,DETSFile}=application:get_env(?APPNAME,history),
	DArgs=[{file,DETSFile}],
	{ok,history}=dets:open_file(history,DArgs),
	{ok,State#state{htab=config}}.
	
handle_call(stop,_From,State)->
	{stop,normal,ok,State};

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

terminate(Reason,#state{htab=HTab})->
	?info({terminating,Reason}),
	R=dets:close(HTab),
	?info({dets_close_status,R}),
	ok.
