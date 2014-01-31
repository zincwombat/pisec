-module(pisec).

-include_lib("yaws/include/yaws_api.hrl").
-include("debug.hrl").
-include("alarm.hrl").
-compile(export_all).

-define(XJSON(STATUS,META,DATA),[{status,STATUS},
				 {content,"application/json",jsonapi:encode(addHttpCode(STATUS,META),DATA)}]).


meta(Arg=#arg{})->
	TimeStamp=iso8601:format(now()),
	%%Headers=Arg#arg.headers,
	%%UA=Headers#headers.user_agent,
	%%QueryData=Arg#arg.querydata,
	[{timestamp,TimeStamp}
%%	 {user_agent,list_to_binary(UA)}
	 ].

mkHttpPhrase(Status) when is_integer(Status)->
	case (catch yaws_api:code_to_phrase(Status)) of
	Phrase when is_list(Phrase)->
		Phrase;
	_->
           	"Unknown HTTP Status Code"
	end.

addMeta({_,undefined},M)->
	M;

addMeta({Key,Value},M) when is_integer(Value)->
	lists:append(M,[{Key,integer_to_binary(Value)}]);

addMeta({Key,Value},M) when is_list(Value)->
	lists:append(M,[{Key,list_to_binary(Value)}]).

addHttpCode(Code,Meta)->
	M1=addMeta({http_code,Code},Meta),
	addMeta({http_phrase,mkHttpPhrase(Code)},M1).

out(Arg=#arg{}) ->
	Method=(Arg#arg.req)#http_request.method,
	Path=Arg#arg.pathinfo,
	?info({handling,Method,Path}),
	Meta=addMeta({path,Path},meta(Arg)),

	Headers=Arg#arg.headers,
	case Method of
	'GET'->
		QueryParams=yaws_api:parse_query(Arg),
		case lists:keysearch("raise",1,QueryParams) of
		{value,{"raise",Code}}->
			%% we allow a client to raise a specific HTTP Error
			case (catch list_to_integer(Code)) of
			ICode when is_integer(ICode)->
				Payload=errorutils:mkHttpError(ICode,"User raised"),
				?XJSON(ICode,Meta,Payload);
			_Error->
				Payload=errorutils:mkError(400,400,"Bad raise value"),
				?XJSON(400,Meta,Payload)
			end;
		false->
			case (catch handle_get(Meta,Arg)) of
			{'EXIT',Reason}->
				Payload=errorutils:mkHttpError(500,Reason),
				?XJSON(500,Meta,Payload);
			Response->
				Response
			end
		end;
	'POST'->
		%% verify that the content-type is correct first
		ContentType=Headers#headers.content_type,	
		case ContentType of
		"application/json" ++ _Rest ->
			case (catch handle_post(Meta,Arg)) of
                        {'EXIT',Reason}->
                                Payload=errorutils:mkHttpError(500,Reason),
                                ?XJSON(500,Meta,Payload);
                        Response->
                                Response
                        end;
		Other->
			Payload=errorutils:mkHttpError(406,Other),
			?XJSON(406,Meta,Payload)
		end;
	Other->
		?XJSON(405,Meta,errorutils:mkHttpError(405,Other))
	end.

handle_get(Meta,Arg)->
        Path=string:tokens(Arg#arg.appmoddata,"/"),
	QueryParams=yaws_api:parse_query(Arg),
	AlarmStatusFilter=lists:keysearch("status",1,QueryParams),
	case Path of
	["abort"]->
		%% throw an error on function_clause
		exit('abort requested');

	["alarms"]->
		%% check is any query params are provided to filter the 
		%% alarms on status
		AS=
		case AlarmStatusFilter of
		false->
			alarm:state();
		{value,{_,AlarmStatus}}->
			alarm:state(AlarmStatus)
		end,
		?XJSON(200,Meta,AS);

	["alarms",AlarmID]->
		%% TODO - need to catch this is case of badarg
		case (catch list_to_integer(AlarmID)) of
		IAlarmID when ?IN_RANGE(IAlarmID)->
			R=alarm:portstate(IAlarmID),
			?XJSON(200,Meta,R);
		_->
			%% either not an integer or out of range
			E=errorutils:mkHttpError(400,"Invalid alarm port"),
			?XJSON(400,Meta,E)
		end;

	Unhandled->
		E=errorutils:mkHttpError(404,Unhandled),
		?XJSON(404,Meta,E)
	end.

handle_post(Meta,Arg)->
        Path=string:tokens(Arg#arg.appmoddata,"/"),
	case Path of
	["alarms","disarm"]->
		alarm_api:disarm(),
		AS=alarm:state(),
                ?XJSON(200,Meta,AS);

	["alarms","arm"]->
		alarm_api:arm(),
		AS=alarm:state(),
                ?XJSON(200,Meta,AS);

	["alarms","ack"]->
		alarm_api:ack(),
		AS=alarm:state(),
                ?XJSON(200,Meta,AS);

	["alarms","unack"]->
		alarm_api:unack(),
		AS=alarm:state(),
                ?XJSON(200,Meta,AS);

	["alarms","scan"]->
		alarm:scan(),
		AS=alarm:state(),
                ?XJSON(200,Meta,AS);

	["alarms","resetall"]->
		R=alarm_api:resetall(),
                ?XJSON(202,Meta,R);


	["alarms","reset",AlarmID]->
		case (catch list_to_integer(AlarmID)) of
		IAlarmID when ?IN_RANGE(IAlarmID)->
			R=alarm_api:reset(IAlarmID),
                	?XJSON(202,Meta,R);
		_->
			%% either not an integer or out of range
			E=errorutils:mkHttpError(400,"Invalid alarm port"),
			?XJSON(400,Meta,E)
		end;

	["alarms","set",AlarmID]->
		case (catch list_to_integer(AlarmID)) of
		IAlarmID when ?IN_RANGE(IAlarmID)->
			R=alarm_api:set(IAlarmID),
                	?XJSON(202,Meta,R);
		_->
			%% either not an integer or out of range
			E=errorutils:mkHttpError(400,"Invalid alarm port"),
			?XJSON(400,Meta,E)
		end;

	["alarms","clear",AlarmID]->
		case (catch list_to_integer(AlarmID)) of
		IAlarmID when ?IN_RANGE(IAlarmID)->
			R=alarm_api:clear(IAlarmID),
                	?XJSON(202,Meta,R);
		_->
			%% either not an integer or out of range
			E=errorutils:mkHttpError(400,"Invalid alarm port"),
			?XJSON(400,Meta,E)
		end;

	Unhandled->
		E=errorutils:mkHttpError(404,Unhandled),
		?XJSON(404,Meta,E)

	end.
