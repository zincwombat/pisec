-module(input_handler_sup).
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


% {start_spec,
% 	{ok,{{one_for_one,5,10},[{input_manager,{input_manager,start_link,[]},permanent,5000,worker,[input_manager]},
% 							 {port_0,{input_handler,start,[{0,enable,"Alarm Enable",true,0,control}]},permanent,5000,worker,[input_handler]},
% 							 {port_1,{input_handler,start,[{1,test,"Test",true,0,control}]},permanent,5000,worker,[input_handler]},
% 							 {port_2,{input_handler,start,[{2,front_door,"Front Door Sensor",true,1,sensor}]},permanent,5000,worker,[input_handler]},
% 							 {port_3,{input_handler,start,[{3,rear_door,"Rear Door Sensor",true,1,sensor}]},permanent,5000,worker,[input_handler]}]}}};


% {start_spec,
% 	{ok,{{one_for_one,5,10},[{"power_0",{power_handler,start,[{0,siren,"Siren (Relay 1)",true,off,power}]},permanent,5000,worker,[0]},
% 							 {"power_1",{power_handler,start,[{1,relay1,"Relay 2",true,off,power}]},permanent,5000,worker,[1]},
% 							 {"led_2",{led_handler,start,[{2,power_led,"Power On LED",true,off,led}]},permanent,5000,worker,[2]},
% 							 {"led_3",{led_handler,start,[{3,alarm_status_led,"Alarm Status LED",true,off,led}]},permanent,5000,worker,[3]},
% 							 {"power_4",{power_handler,start,[{4,output4,"Output 4",true,off,power}]},permanent,5000,worker,[4]},
% 							 {"power_5",{power_handler,start,[{5,output5,"Output 5",true,off,power}]},permanent,5000,worker,[5]},
% 							 {"power_6",{power_handler,start,[{6,output6,"Output 6",true,off,power}]},permanent,5000,worker,[6]},
% 							 {"power_7",{power_handler,start,[{7,output7,"Output 7",true,off,power}]},permanent,5000,worker,[7]}]}}}

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
	case config:get(inputs) of
	undefined->
		[];
	Ports when is_list(Ports)->
		EnabledPorts=lists:filter(fun(Z)->isEnabled(Z) end,Ports),
		children(EnabledPorts)
	end.

isEnabled({_,_,_,true,_,_})->
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
	IOManager=?CHILD(input_manager,worker),
	Children=getConf(),
	SSpec={ok,{{one_for_one,5,10},lists:append([[IOManager],Children])}},
	?info({start_spec,SSpec}),
	SSpec.
