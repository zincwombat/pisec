-module(alarm_api).
-include("debug.hrl").
-include("alarm.hrl").

-export([
	arm/0,
	disarm/0,
	ack/0,
	unack/0,
	resetall/0,
%%	active/0,
	reset/1,
	set/1,
	clear/1
]).


disarm()->
	alarm:disarm().

arm()->
	alarm:arm().

ack()->
	alarm:ack().

unack()->
	alarm:unack().

resetall()->
	scanner:resetAllPorts().

reset(Port)->
	scanner:resetPort(Port).

set(Port)->
	scanner:setPort(Port).

clear(Port)->
	scanner:clearPort(Port).
