-module(twilio_manager).
-behaviour(gen_server).

-export([start/0,
         stop/0]).

-include("debug.hrl").
-include("alarm.hrl").
-include("twilio.hrl").

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export ([state/0]).
-export ([notify/1]).
-export ([notify/2]).

-record(state, {}).

start() ->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).

stop() ->
    gen_server:call(?MODULE,stop).

state() ->
    gen_server:call(?MODULE,state).

init([])->
    State=#state{},
    {ok,State}.

notify(Message)->
    notify(Message,?NOTIFY_MSISDNS).

notify(Message,MSISDN_list)->
    gen_server:cast(?MODULE,{notify,Message,MSISDN_list}).

handle_call(stop,_From,State)->
    {stop,normal,ok,State};

handle_call(state,_From,State)->
    {reply,{ok,State},State};

handle_call(Msg,From,State)->
    Unhandled={unhandled_call,{msg,Msg},{from,From}},
    ?warn(Unhandled),
    {reply,Unhandled,State}.

handle_cast({notify,Message,MSISDNS},State)->
    lists:foreach(fun(Z)->i_notify(Message,Z,State) end,MSISDNS),
    {noreply,State}.

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

terminate(Reason,State)->
    ?info({terminating,Reason}),
    ok.

%% ============================================================================
%% INTERNAL FUNCTIONS
%% ============================================================================

i_notify(Message,MSISDN,State)->
    ?info({notify,{to,MSISDN},{msg,Message}}),
    % insert API client code here
    ok.
