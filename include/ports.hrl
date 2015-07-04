-record(portRec,		{port}).
-define(is_uint8(T), 	(((T) band (bnot 16#ff)) =:=  0)).
-define(is_uint16(T), 	(((T) band (bnot 16#ffff)) =:=  0)).
-define(is_uint32(T), 	(((T) band (bnot 16#ffffffff)) =:=  0)).

%% INPUT PORTS
-define(PORTS,			[7,6,5,4,3,2,1,0]).
-define(is_portnum(T),	(T >= 0) andalso (T =< 7)). 
-define(is_control(T),	(T >= 0) andalso (T =< 2)). 
-define(is_sensor(T),	(T >= 3) andalso (T =< 7)).

%% OUTPUT PORTS
-define(is_oport(T),	(T >= 2) andalso (T =< 7)).
-define(is_relay(T),	(T >= 0) andalso (T =< 1)). 
-define(is_led(T),		(T >= 2) andalso (T =< 7)). 

-define(is_onoff(T),	(T == on) orelse (T == off)).

-define(is_speed(T),	(T == fast) orelse (T == normal) orelse (T == slow) orelse (T == off)).

