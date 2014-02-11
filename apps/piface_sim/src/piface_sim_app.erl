-module(piface_sim_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
        piface_sim_sup:start().

stop(_State) ->
        ok.
