-record(portRec,		{port}).
-define(is_uint8(T), 	(((T) band (bnot 16#ff)) =:=  0)).
-define(is_uint16(T), 	(((T) band (bnot 16#ffff)) =:=  0)).
-define(is_uint32(T), 	(((T) band (bnot 16#ffffffff)) =:=  0)).


-define(is_oport(T),		(T >= 1) andalso (T =< 8))

%% INPUT PORTS
-define(is_portnum(T),	(T >= 1) andalso (T =< 6)). 
-define(is_control(T),	(T >= 7) andalso (T =< 8)). 

%% OUTPUT PORTS
-define(is_oport(T),	(T >= 1) andalso (T =< 8))
-define(is_relay(T),	(T >= 1) andalso (T =< 2)). 
-define(is_led(T),		(T >= 3) andalso (T =< 8)). 

-define(is_speed(T),	(T == fast) orelse (T == normal) orelse (T == slow) orelse (T == off)).

