-module(pisec_sup).

-behaviour(supervisor).
-include("debug.hrl").

%% API
-export([start/0,
	 start/1,
	 restart/0,
	 stop/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor

-define(CHILD(I,Type),{I,{I,start,[]},permanent,5000,Type,[I]}).
-define(CHILD(I,Type,Args),{I,{I,start,[Args]},permanent,5000,Type,[I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start()->
	supervisor:start_link({local,?MODULE},?MODULE,[]).

start(Args)->
	supervisor:start_link({local,?MODULE},?MODULE,Args).

stop()->
	exit(whereis(?MODULE),shutdown).
	

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init(Args) ->
	Scanner=?CHILD(scanner_sup,supervisor,Args),
	Yaws=?CHILD(pisec_yaws_sup,supervisor),
	Config=?CHILD(config,worker),
	IO=?CHILD(io_handler_sup,supervisor),
	% Cs=[Config,Yaws,Scanner],
	Cs=[Config,IO],
	?info({starting,Cs}),
	{ok,{{one_for_one,5,10},Cs}}.

restart()->
	Children=supervisor:which_children(?MODULE),
	case lists:keysearch(scanner_sup,1,Children) of
	false->
		?warn({child_not_defined,scanner_sup}),
		{error,not_running};
	{value,{scanner_sup,undefined,_,_}}->
		?info({starting_child,scanner_sup}),
		supervisor:restart_child(?MODULE,scanner_sup);
	{value,{scanner_sup,Pid,_,_}} when is_pid(Pid)->
		?info({stopping_child,scannr_sup}),
		supervisor:terminate_child(?MODULE,scanner_sup),
		?info({starting_child,scannr_sup}),
		supervisor:restart_child(?MODULE,scanner_sup)
	end.
