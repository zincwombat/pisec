-module(output_handler_sup).
-behaviour(supervisor).
-include("debug.hrl").
-include("alarm.hrl").


-export([start/0,
	 	 stop/0]).

-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start()->
	supervisor:start_link({local,?MODULE},?MODULE,[]).

stop()->
	exit(whereis(?MODULE),shutdown).

children()->
	children(config:get(outputs)).

children(Ports) when is_list(Ports)->
	lists:map(fun(Z)->child(Z) end, Ports);

children(_)->
	[].

child(X={Port,Label,Desc,true,InitState,led})->
	{"led_" ++ integer_to_list(Port),{led_handler,start,[X]},permanent,5000,worker,[Port]};

child(X={Port,Label,Desc,true,InitState,power})->
	{"power_" ++ integer_to_list(Port),{power_handler,start,[X]},permanent,5000,worker,[Port]};

child(_)->
	[].


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([])->
	Children=children(),
	SSpec={ok,{{one_for_one,5,10},Children}},
	SSpec.
