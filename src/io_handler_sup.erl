-module(io_handler_sup).
-behaviour(supervisor).
-include("debug.hrl").
-include("alarm.hrl").


-export([start/0,
	 	 stop/0]).

-export([init/1]).
-export([getConf/0]).
-export([ports/0]).
-export([restart/1]).

-define(PORTNAME(I), list_to_atom("port_" ++ integer_to_list(I))).
-define(ICHILD(I,Args,Type),{?PORTNAME(element(1,Args)),{I,start,[Args]},permanent,5000,Type,[I]}).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


%% ===================================================================
%% API functions
%% ===================================================================

start()->
	supervisor:start_link({local,?MODULE},?MODULE,[]).

stop()->
	exit(whereis(?MODULE),shutdown).


children(Ports) when is_list(Ports)->
	lists:map(fun(Z)->?ICHILD(input_handler,Z,worker) end,Ports).

getConf()->
	case config:get(ports) of
	undefined->
		[];
	Ports when is_list(Ports)->
		EnabledPorts=lists:filter(fun(Z)->isEnabled(Z) end,Ports),
		children(EnabledPorts)
	end.

isEnabled({_,_,_,true,_})->
	true;

isEnabled(_)->
	false.
	
ports()->
	ports(getConf()).

ports([])->
	[];

ports(P) when is_list(P)->
	lists:map(fun(Z)->element(1,Z) end,P).


restart(Port) when is_atom(Port)->
	?info({restart_request,{port,Port}}),
	%% stop and restart child
	case supervisor:which_children(?MODULE) of
	[]->
		?warn(no_children),
		{error,{not_a_child,Port}};
	Children->
		case lists:keyfind(Port,1,Children) of
		{Port,Pid,_,_}->
			?info({stopping_pid,{pid,Pid}}),
			exit(Pid,stop);
		false->
			{error,{child_not_found,Port}}
		end
	end.


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([])->
	%% {ok,{{one_for_one,5,10},children()}}.
	IOManager=?CHILD(io_manager,worker),
	Children=getConf(),
	SSpec={ok,{{one_for_one,5,10},lists:append([[IOManager],Children])}},
	SSpec.
