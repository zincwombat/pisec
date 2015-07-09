-module (api).

% remote arm and disarm, overriding actual switches

-export ([arm/0]).	
-export ([disarm/0]).
-export ([reset/0]).
-export ([active/0]).
-export ([state/0]).
-export ([sensors/0]).

arm()->
	scanner2:assertPort(getPort(enable)).

disarm()->
	scanner2:deAssertPort(getPort(enable)).

reset()->
	scanner2:reset().

active()->
	io_manager:getAssertedAlarms().

state()->
	{ok,State}=alarm2:state(),
	{state,State}.

sensors()->
	scanner2:readInput().

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
