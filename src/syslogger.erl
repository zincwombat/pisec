-module(syslogger).

%% TBD - add support for override, i.e. syslogger:override(debug)
%% will log ALL debug messages, independent of module
%% need to add override/1

-behaviour(gen_server).

-export([	init/1,
                handle_call/3,
                handle_cast/2,
                handle_info/2,
                terminate/2,
		code_change/3
                ]).

-export([	start/0,
                start/1,
		status/0,
                stop/0]).

-export([	set/2,
		add/2,
		delete/1,
		info/2,
		debug/2,
		warn/2,
		critical/2,
		trace/2,
		list/0,		%% list all Module/Trace Levels
		only/1,		%% log messages only from Module
		override/1,	%% log all messages of given severity
		all/0		%% log all messages
		]).


-define(START_OPTIONS,          []).
-define(SERVERNAME,             ?MODULE).
-define(SERVERACCESS,           {local,?SERVERNAME}).
-define(SERVERCALL,             ?SERVERNAME).

-record(state,{	ttab,
		override=[],
		only=[]}).

-include("debug.hrl").

start()->
	start([]).

start(Arg)->
        gen_server:start_link(  ?SERVERACCESS,
                                ?SERVERNAME,
                                [Arg],
                                ?START_OPTIONS).

stop()->
        gen_server:call(?SERVERCALL,die).

%% External API

only(Module)->
	gen_server:call(?SERVERCALL,{only,Module}).

all()->
	gen_server:call(?SERVERCALL,all).

override(clear)->
	gen_server:call(?SERVERCALL,{override,clear});

override(Level) when is_atom(Level)->
	L=level(Level),
	gen_server:call(?SERVERCALL,{override,L}).

level(debug)->
	?TRACE_DEBUG;

level(info)->
	?TRACE_INFO;

level(warn)->
	?TRACE_WARN;

level(critical)->
	?TRACE_CRITICAL;

level(_Other)->
	level(debug).

list()->
	gen_server:call(?SERVERCALL,list).

set(Module,Level) when is_atom(Level)->
	add(Module,level(Level)).

add(Module,TraceLevel)->
	gen_server:call(?SERVERCALL,{add,Module,TraceLevel}).

delete(Module)->
	gen_server:call(?SERVERCALL,{delete,Module}).

debug(Module,Message)->
	gen_server:cast(?SERVERCALL,{log,?TRACE_DEBUG,Module,Message}).

trace(Module,Message)->
	gen_server:cast(?SERVERCALL,{log,?TRACE_DEBUG,Module,Message}).

info(Module,Message)->
	gen_server:cast(?SERVERCALL,{log,?TRACE_INFO,Module,Message}).

warn(Module,Message)->
	gen_server:cast(?SERVERCALL,{log,?TRACE_WARN,Module,Message}).

critical(Module,Message)->
	gen_server:cast(?SERVERCALL,{log,?TRACE_CRITICAL,Module,Message}).

init(_Args)->
	process_flag(trap_exit,true),
	Ttab=ets:new(ttab,[set,private]),
	{ok,#state{ttab=Ttab}}.

status()->
	gen_server:call(?SERVERCALL,status).

handle_call(die,_,State) ->
        {stop,normal,State};

handle_call(status,_,State) ->
        {reply,{ok,State},State};

handle_call({add,Module,TraceLevel},_From,State=#state{ttab=TTab}) ->
	Reply=ets:insert(TTab,{Module,TraceLevel}),
        {reply,Reply,State};

handle_call({delete,Module},_From,State=#state{ttab=TTab}) ->
	Reply=ets:delete(TTab,Module),
        {reply,Reply,State};

handle_call(list,_From,State=#state{ttab=TTab}) ->
	Reply=ets:tab2list(TTab),
	Reply2=lists:map(fun(Z)->{element(1,Z),to_string(element(2,Z))} end,Reply),
        {reply,Reply2,State};

handle_call({override,clear},_From,State) ->
        {reply,ok,State#state{override=[]}};

handle_call({override,Level},_From,State) ->
        {reply,ok,State#state{override=Level}};

handle_call({only,Module},_From,State) ->
        {reply,ok,State#state{only=Module}};

handle_call(all,_From,State) ->
        {reply,ok,State#state{only=[]}};

handle_call(_Msg,_,State) ->
        {reply,ok,State}.

%% this is where the actual logging is handled. Clients call the public API
%% trace/2, dbug/2, info/2, warn/2, critical/2 which is a gen_server:cast
%% operation

handle_cast({log,Level,Module,Message},State=#state{only=Module})->
	%% we are only interested in messages from module Module
	handle_log(Level,Module,Message,State),
        {noreply,State};

handle_cast({log,Level,Module,Message},State=#state{only=[]})->
	%% all messages are being handled
	handle_log(Level,Module,Message,State),
        {noreply,State};

handle_cast({log,_Level,_Module,_Message},State)->
        {noreply,State};

handle_cast(_Msg,State)->
        {noreply,State}.

handle_info({'EXIT',_Pid,_Reason},State)->
        {noreply,State};

handle_info(_Msg,State)->
        {noreply,State}.

code_change(_OldVsn,State,_Extra)->
	{ok,State}.

terminate(_Reason,_State) ->
        ok.

%% output function

handle_log(Level,Module,Message,#state{override=OLevel}) when is_integer(OLevel)->
	%% if override is set to a particular level, then all
	%% messages of that level are to be logged
	i_handle_log(Level,[{Module,OLevel}],Module,Message);

handle_log(Level,Module,Message,#state{ttab=TTab})->
	LogLevel=ets:lookup(TTab,Module),
	i_handle_log(Level,LogLevel,Module,Message).

i_handle_log(Level,[],Module,Message)->
	%% no module tracel level has been set
	i_trace(Level,Module,Message);

i_handle_log(Level,[{_,LogLevel}],Module,Message) when Level>=LogLevel->
	i_trace(Level,Module,Message);

i_handle_log(_Level,_LogLevel,_Module,_Message)->
	ignore.

i_trace(?TRACE_CRITICAL,Module,Detail)->
        esn_report:critical(Module,Detail);

i_trace(?TRACE_WARN,Module,Detail)->
        esn_report:warn(Module,Detail);

i_trace(?TRACE_INFO,Module,Detail)->
        esn_report:info(Module,Detail);

i_trace(?TRACE_DEBUG,Module,Detail)->
        esn_report:debug(Module,Detail);

i_trace(_,Module,Detail)->
        esn_report:trace(Module,Detail).

to_string(?TRACE_DEBUG)->
	debug;

to_string(?TRACE_INFO)->
	info;

to_string(?TRACE_WARN)->
	warn;

to_string(?TRACE_CRITICAL)->
	critical;

to_string(_)->
	unknown.
