-module(esn_kernel_sup).

-behaviour(supervisor).

%% API
-export([start/0,
	 stop/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor

-define(CHILD(I,Type),{I,{I,start,[]},permanent,5000,Type,[I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start()-> 
	supervisor:start_link({local,?MODULE},?MODULE,[]).

stop()->
	exit(whereis(?MODULE),shutdown).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
        Logger=?CHILD(esn_logger,worker),
        Syslogger=?CHILD(syslogger,worker),
        {ok,{{one_for_one,5,10},[Logger,Syslogger]}}.

