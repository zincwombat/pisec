-module (api).

% remote arm and disarm, overriding actual switches

-export ([arm/0]).	
-export ([disarm/0]).
-export ([reset/0]).
-export ([alarms/0]).
-export ([state/0]).
-export ([sensors/0]).
-export ([controls/0]).
-export ([led_outputs/0]).
-export ([power_outputs/0]).
-export ([ack/0]).
-export ([unack/0]).
-export ([deAssertPort/1]).
-export ([assertPort/1]).
-export ([getInputPort/1]).
-export ([getOutputPort/1]).
-export ([getLog/0]).
-export ([flushLog/0]).

-include ("ports.hrl").


arm()->
	% returns ok
	assertPort(enable).

disarm()->
	% returns ok
	scanner2:deAssertPort(getInputPort(enable)).

ack()->
	% returns {ok,NextState} or {error,{unhandled,Event}}
	alarm2:ack().

unack()->
	% returns {ok,NextState} or {error,{unhandled,Event}}
	alarm2:unack().

assertPort(Port) when is_atom(Port)->
	% returns ok
	assertPort(getInputPort(Port));

assertPort(Port) when ?is_portnum(Port)->
	% returns ok
	scanner2:assertPort(Port);

assertPort(_)->
	{error,badarg}.

deAssertPort(Port) when is_atom(Port)->
	% returns ok
	deAssertPort(getInputPort(Port));

deAssertPort(Port) when ?is_portnum(Port)->
	% returns ok
	scanner2:deAssertPort(Port);

deAssertPort(_)->
	{error,badarg}.

reset()->
	% returns ok
	scanner2:reset().

alarms()->
	% returns {alarms,ListofAlarms}
	input_manager:getAssertedAlarms().

state()->
	% {state,State}
	alarm2:state().

sensors()->
	scanner2:readInput().

controls()->
	% returns {controls,ListofControls}
	input_manager:getAssertedControls().

led_outputs()->
	output_manager:getLedStatus().

power_outputs()->
	output_manager:getPowerStatus().

getLog()->
	history_manager:getAll().

flushLog()->
	history_manager:flush().

getPort(Type,Label)->
	Config=config:get(Type),
	case lists:keyfind(enable,2,Config) of
		X when is_tuple(X)->
			element(1,X);
		_->
			throw({error,{badarg,Label}})
	end.

getInputPort(Label) when is_atom(Label)->
	getPort(inputs,Label).

getOutputPort(Label) when is_atom(Label)->
	getPort(outputs,Label).
