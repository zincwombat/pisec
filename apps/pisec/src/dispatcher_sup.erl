-module(dispatcher_sup).

-behaviour(supervisor).
-include("debug.hrl").

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
	Dispatcher=?CHILD(dispatcher,worker),
	Handlers=?CHILD(io_handler_sup,supervisor),
	Cs=[Dispatcher,Handlers],
	?info({starting,Cs}),
	{ok,{{one_for_all,5,10},Cs}}.

