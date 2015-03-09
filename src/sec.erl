-module(sec).

-export([start/0,
	 restart/0,
	 disable/0,
	 enable/0,
         stop/0]).

-define(APPS,[gpio,spi,piface,security]).
-define(SIMAPPS,[security]).

start()->
	Apps=
	case init:get_plain_arguments() of
	["+simulator"]->
		?SIMAPPS;
	_->
		?APPS
	end,
	lists:map(fun(Z)->application:start(Z) end,Apps).

stop()->
	Apps=
	case init:get_plain_arguments() of
	["+simulator"]->
		?SIMAPPS;
	_->
		?APPS
	end,
	lists:map(fun(Z)->application:stop(Z) end,lists:reverse(Apps)).

restart()->
	security_sup:restart().

disable()->
	%% 
	ok.

enable()->
	%%
	ok.
