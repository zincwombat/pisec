-module(piface_sim_sup).
-behaviour(supervisor).
-export([
	start/0,
	init/1
]).
-define(CHILD(I,Type),{I,{I,start,[]},permanent,5000,Type,[I]}).

start() ->
	supervisor:start_link({local,?MODULE},?MODULE,[]).

stop()->
        exit(whereis(?MODULE),shutdown).

init([])->
	Simulator=?CHILD(piface_simulator,worker),
	Cs=[Simulator],
        {ok,{{one_for_all,5,10},Cs}}.
