-module(pisec_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	StartArg=
	case application:get_env(simulator) of
        {ok,true}->
                [{simulator,true}];
        _->
		[]
        end,
    	pisec_sup:start(StartArg).

stop(_State) ->
    	ok.
