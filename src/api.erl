-module (api).

% remote arm and disarm, overriding actual switches

-export ([arm/0]).	
-export ([disarm/0]).
-export ([reset/0]).
-export ([alarms/0]).
-export ([state/0]).
-export ([sensors/0]).
-export ([controls/0]).
-export ([leds/0]).
-export ([relays/0]).

arm()->
	scanner2:assertPort(getPort(enable)).

disarm()->
	scanner2:deAssertPort(getPort(enable)).

reset()->
	scanner2:reset().

alarms()->
	io_manager:getAssertedAlarms().

state()->
	alarm2:state().

sensors()->
	scanner2:readInput().

controls()->
	io_manager:getAssertedControls().

leds()->
	not_yet_implemented.

relays()->
	not_yet_implemented.

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
