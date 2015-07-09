-module (api).

% remote arm and disarm, overriding actual switches

-export ([arm/0]).	
-export ([disarm/0]).
-export ([reset/0]).

arm()->
	scanner2:assertPort(getPort(enable)).

disarm()->
	scanner2:deAssertPort(getPort(enable)).

reset()->
	scanner2:reset().

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
