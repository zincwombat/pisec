%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Name       : esn_report_h
%% Abstract   : An Event Handler sending all events in the system to  
%%              esn_event_mgr. 
%% API        :
%%              /       -- 
%% Modified By: 1.1 Pär-Anders Aronsson, Original version
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(esn_report_h).
-vsn('$Revision: 1.1 $').
-author('$Author: thobbins $').
-date('$Date: 2005/01/25 02:04:43 $').
-rcsid('@(#) $Id: esn_report_h.erl,v 1.1 2005/01/25 02:04:43 thobbins Exp $\n').

-behaviour(gen_event).

%%% SASL API
-export([init/1, 
	 handle_event/2, 
	 handle_call/2, 
	 handle_info/2, 
	 code_change/3,
	 terminate/2]).

init([]) ->
	{ok, []}.

handle_event(Event, State) ->
	Type=eventType(Event),
	Time=calendar:local_time(),
	catch esn_logger:logevent({Time,node(),Event},Type),
    	{ok, State}.

handle_info({emulator,GL,Chars},State)->
	handle_event({emulator,GL,Chars},State);

handle_info(_Info, State) ->
	{ok, State}.

handle_call(_Query, _State) -> 
	{error, bad_query}.

code_change(_OldVsn,State,_Extra)->
	{ok,State}.

terminate(_Reason, _State) ->
	ok.

eventType(Event)->
        case Event of
        {_T,_Pid,{_OPid,esn_report,{Type,_Detail}}}->
                Type;

        {_T,_Pid,{_OPid,progress,_Detail}}->
                sasl_progress;

        {_T,_Pid,{_OPid,supervisor_report,_Detail}}->
                sasl_supervisor;

        {_T,_Pid,{_OPid,crash_report,_Detail}}->
                sasl_crash;

        {error,_Pid,{_OPid,_ErrorFormat,_ErrorInfo}}->
                erlang_error;

        {info_report,_GL,{_OPid,_Type,_Info}}->
                erlang_info;

        {_T,_Pid,{_OPid,Type,_Detail}}->
                Type;

        {emulator,_GL,_Detail}->
                erlang_emulator_error;

        _Other->
                unknown
        end.

