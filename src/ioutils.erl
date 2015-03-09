-module(ioutils).
-include("ports.hrl").
-include("debug.hrl").

-export([
	 mkRec/1,
	 isSet/2,
	 portVal/2,
	 setPort/3,
	 notify/1,
	 notifySet/2,
	 blist/1
	 ]).

-define(BIT8,	{2#10000000,8}).
-define(BIT7,	{2#01000000,7}).
-define(BIT6,	{2#00100000,6}).
-define(BIT5,	{2#00010000,5}).
-define(BIT4,	{2#00001000,4}).
-define(BIT3,	{2#00000100,3}).
-define(BIT2,	{2#00000010,2}).
-define(BIT1,	{2#00000001,1}).

-define(MASKS,	[?BIT8,?BIT7,?BIT6,?BIT5,?BIT4,?BIT3,?BIT2,?BIT1]).

mkRec(Port) when is_integer(Port),
		 Port =< 255,
		 Port >= 0->
	#portRec{port=Port};

mkRec(_)->
	{error,badarg}.

isSet(PortNum,#portRec{port=P})->
	isSet(PortNum,P);

isSet(PortNum,P)->
	(P band (1 bsl (PortNum-1)) > 0).

portVal(PortNum,#portRec{port=P})->
        portVal(PortNum,P);

portVal(PortNum,P)->
	case isSet(PortNum,P) of
	false->
		0;
	true->
		1
	end.

setPort(PortNum,1,PortRec=#portRec{port=Port})->
	P2=Port bor (1 bsl (PortNum-1)),
	PortRec#portRec{port=P2};

setPort(PortNum,0,PortRec=#portRec{port=Port})->
	P2=Port band bnot((1 bsl (PortNum-1))),
	PortRec#portRec{port=P2}.

blist(Port) when is_integer(Port),
                 Port =< 255,
                 Port >= 0->
	<<I8:1,I7:1,I6:1,I5:1,I4:1,I3:1,I2:1,I1:1>> = <<Port>>,
	[I8,I7,I6,I5,I4,I3,I2,I1];

blist(_)->
	{error,badarg}.

notifySet(undefined,CurrentPort)->
	ChangeMask=255,
	i_notifySet(blist(ChangeMask),blist(CurrentPort));

notifySet(PreviousPort,CurrentPort)->
	ChangeMask=PreviousPort bxor CurrentPort,
	i_notifySet(blist(ChangeMask),blist(CurrentPort)).

i_notifySet(ChangeMask,CurrentVals)->
	i_notifySet(ChangeMask,CurrentVals,8,[]).

i_notifySet([0|CMTail],[_|CVTail],BitPos,Changes)->
	i_notifySet(CMTail,CVTail,BitPos-1,Changes);

i_notifySet([1|CMTail],[BitVal|CVTail],BitPos,Changes)->
	i_notifySet(CMTail,CVTail,BitPos-1,lists:append([Changes,[{BitPos,BitVal}]]));

i_notifySet([],_,_,Changes)->
	Changes.


notify(#portRec{port=Port})->
	notify(Port,?MASKS).

notify(Port,Masks) when is_list(Masks)->
	lists:map(fun(Z)->notify(Port,Z) end,Masks); 

notify(Port,{_Mask,Num})->
	?info({notify,{Num,isSet(Num,Port)}}).

