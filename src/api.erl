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

-include ("ports.hrl").


arm()->
	assertPort(enable).

disarm()->
	scanner2:deAssertPort(getPort(enable)).

ack()->
	alarm2:ack().

unack()->
	alarm2:unack().

assertPort(Port) when is_atom(Port)->
	assertPort(getPort(Port));

assertPort(Port) when ?is_portnum(Port)->
	scanner2:assertPort(Port).

deAssertPort(Port) when is_atom(Port)->
	deAssertPort(getPort(Port));

deAssertPort(Port) when ?is_portnum(Port)->
	scanner2:deAssertPort(Port).

reset()->
	scanner2:reset().

alarms()->
	input_manager:getAssertedAlarms().

state()->
	alarm2:state().

sensors()->
	scanner2:readInput().

controls()->
	input_manager:getAssertedControls().

led_outputs()->
	output_manager:getLedStatus().

power_outputs()->
	output_manager:getPowerStatus().

% ==============================================================================
% utility functions
% ==============================================================================

getPort(Label) when is_atom(Label)->
	Config=config:get(inputs),
	case lists:keyfind(enable,2,Config) of
		X when is_tuple(X)->
			element(1,X);
		_->
			throw({error,{badarg,Label}})
	end.
