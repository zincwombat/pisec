-module(jsonapi).

-include("debug.hrl").
-include("alarm.hrl").
-compile(export_all).

-define(L2B(X),			erlang:list_to_binary(X)).

parseMeta(MDList) when is_list(MDList)->
	lists:map(fun(Z)->parseMeta(Z) end,MDList);

parseMeta({Key,Value})->
	{Key,Value}.

encode(Meta,Data)->
	encode(Meta,Data,[]).

encode(Meta,Data,Links)->
	Object=toJson(Meta,Data,Links),
	rfc4627:encode(Object).

toJson(Meta,Data)->
	toJson(Meta,Data,[]).

toJson(Meta,ok,_Links)->
	MetaData = {obj,parseMeta(Meta)},

    	{obj, [{meta,	MetaData},
	       {data,	ok}]};

toJson(Meta,{error,{L={location,{module,_Module},{line,_Line}},{detail,E}}},_Links)->
	toJson(Meta,errorutils:mkError(500,500,E,L));

toJson(Meta,{error,Reason},_Links)->
	toJson(Meta,errorutils:mkError(500,500,Reason,"error tuple returned by backend"));

toJson(Meta,#portstatus{ioport=IOPort,
			description=Desc,
			iostate=IOState,
			maskstate=MaskState,
			log=Queue},_Links)->
	Port={obj,
		[
			{port,IOPort},
		 	{description,?L2B(Desc)},
		 	{state,?L2B(IOState)},
		 	{mask,?L2B(MaskState)}
		 ]},

	MetaData = {obj,parseMeta(Meta)},
	PortData = {obj,[{portstatus,Port}]},
	
    	{obj, [{meta,	MetaData},
	       {data,	PortData}]};


toJson(Meta,A=#alarmstatus{
			alarmstate=AS,
			portfilter=PortFilter,
			portstate=PortStates,
			log=Queue},_Links)->

	PortData=[{obj, 
		[
			{port, 		PS#portstatus.ioport},
	      		{description,  	?L2B(PS#portstatus.description)},
	      		{state, 	?L2B(PS#portstatus.iostate)},
	      		{mask,    	?L2B(PS#portstatus.maskstate)},
			{history,	genQueue(PS#portstatus.log)},
			{links,		genLinks(PS)}]}
	    || PS <- PortStates],


	MetaData = {obj,parseMeta(Meta)},

	Alarm =     {obj,[{alarmstate,	?L2B(AS)},
                          {portfilter,	?L2B(PortFilter)},
                          {portstatus, 	PortData},
			  {history,	genQueue(Queue)},
			  {links,	genLinks(A)}]},

	AlarmData = {obj, [{alarmdata,Alarm}]},

    	{obj, [{meta,	MetaData},
	       {data,	AlarmData}]};


toJson(Meta,E=#error{},_Links)->
	Error={obj,
		[
			{code,		E#error.code},
			{status,	E#error.status},
			{description,	E#error.description},
			{extended,	E#error.extended}
		]},

	MetaData = {obj,parseMeta(Meta)},
	ErrorData= {obj,[{error,Error}]},

	{obj,[{meta,MetaData},
	      {data,ErrorData}]}.

genQueue(Queue) when is_list(Queue)->
	lists:map(fun(Z)->genQueue(Z) end, Queue);

genQueue({TimeStamp,Message})->
	{obj,	[{timestamp,TimeStamp},
		 {message,Message}]}.
	


%%========================
%% BEGIN BUSINESS LOGIC
%%========================

genLinks(#alarmstatus{alarmstate=AS})->
	case AS of
		"DISARMED"->
			[arm()];
		"ACTIVE"->
			[disarm(),ack(),resetall()];
		"CLEAR"->
			[disarm(),resetall()];
		"ACK"->
			[disarm(),unack(),resetall()];
		_Other->
			[]
	end;


genLinks(#portstatus{ioport=Port,iostate=Status,maskstate="AUTO"})->
	LinkSet=
	case Status of
		"ACTIVE"->
			[clear(Port),set(Port),reset(Port)];
		"CLEAR"->
			[clear(Port),set(Port),reset(Port)];
		"DISABLED"->
			[enable(Port)];
		_Other->
			[]
	end,
	LinkSet;

genLinks(#portstatus{ioport=Port,iostate=_Status,maskstate="SET"})->
	LinkSet=[reset(Port),clear(Port)],
	LinkSet;

genLinks(#portstatus{ioport=Port,iostate=_Status,maskstate="CLEAR"})->
	LinkSet=[reset(Port),set(Port)],
	LinkSet;

genLinks(Other)->
	?info({unhandledlinks,Other}),
	[].
%%========================
%% END BUSINESS LOGIC
%%========================

%%=================
%% port state links
%%=================

clear(Port)->
	mkObj(mkLink("/alarms/clear/" ++ integer_to_list(Port) ,"CLEAR")).

set(Port)->
	mkObj(mkLink("/alarms/set/" ++ integer_to_list(Port) ,"SET")).

reset(Port)->
	mkObj(mkLink("/alarms/reset/" ++ integer_to_list(Port) ,"RESET")).

enable(Port)->
	mkObj(mkLink("/alarms/enable/" ++ integer_to_list(Port) ,"ENABLE")).

disble(Port)->
	mkObj(mkLink("/alarms/disable/" ++ integer_to_list(Port) ,"DISABLE")).

%%==================
%% alarm state links
%%==================

resetall()->
	mkObj(mkLink("/alarms/resetall","RESETALL")).

arm()->
	mkObj(mkLink("/alarms/arm","ARM")).

disarm()->
	mkObj(mkLink("/alarms/disarm","DISARM")).

ack()->
	mkObj(mkLink("/alarms/ack","ACK")).

unack()->
	mkObj(mkLink("/alarms/unack","UNACK")).

%%==================
%% utility routines
%%==================

mkObj(#link{href=HRef,rel=Rel})->
	mkObj([{href,HRef},{rel,Rel}]);	

mkObj(KVList) when is_list(KVList)->
	{obj,lists:map(fun(Z)->mkObj(Z) end,KVList)};

mkObj({Key,Value}) when is_list(Value)->
	mkObj({Key,?L2B(Value)});

mkObj({Key,Value}) when is_binary(Value)->
	{Key,Value};

mkObj(_Other)->
	[].
	
mkLink(HRef,Rel)->
	#link{href=HRef,rel=?L2B(Rel)}.	














