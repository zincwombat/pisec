-module(piface_sim).

-include("debug.hrl").
-export([read_input/0]).

read_input()->
	random:uniform(255).
