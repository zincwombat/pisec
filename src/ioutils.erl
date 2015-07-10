-module(ioutils).
-include ("ports.hrl").
-include ("debug.hrl").
-include ("alarm.hrl").

-export([
	 blist/1,
	 % eventToAlarm/1,
	 % eventToControl/1,
	 eventToSensor/1
]).


blist(Port)->
	<<I7:1,I6:1,I5:1,I4:1,I3:1,I2:1,I1:1,I0:1>> = <<Port>>,
	[I7,I6,I5,I4,I3,I2,I1,I0].

eventToSensor(#event{port=Port,sensorStatus=SensorStatus,label=Label,desc=Desc,type=Type})->
	#sensor{type=Type,label=Label,desc=Desc,state=SensorStatus}.