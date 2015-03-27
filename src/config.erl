-module(config).
-behaviour(gen_server).

-export([start/0,
         stop/0]).

-include("debug.hrl").
-include("alarm.hrl").

-define(DFILE,	"config/config.dets").

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([
		state/0,
		get/0,
		get/1,
		get/2,
		delete/1,
		set/1,
		set/2
	 ]).


-record(state, {ctab}).

start() ->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).

stop() ->
	gen_server:call(?MODULE,stop).

state() ->
	gen_server:call(?MODULE,state).

init([])->
	State=#state{},
	DArgs=[{file,?DFILE}],
	{ok,config}=dets:open_file(config,DArgs),
	merge_config(config,State).

merge_config(Config,State)->
	%% merge the values in the configuration file

	ConfigFile=code:priv_dir(?APPNAME) ++ "/" ++ ?ALARMCONFIG,

        case file:consult(ConfigFile) of
        {ok,KVL}->
		lists:map(fun(Z)->i_merge(Z,Config) end,KVL),
		{ok,State#state{ctab=config}};

        E={error,_Error}->
                ?critical({failed_to_open_file,E}),
                {stop,{config_file,{file,ConfigFile},E}}
        end.

i_merge(KV={Key,Value},Config)->
	case dets:lookup(Config,Key) of
	[{Key,Value}]->
		ok;
	[{Key,_DifferentValue}]->
		ok;
	[]->
		dets:insert(Config,KV);
	E={error,_Reason}->
		E;
	_->
		ignored
	end.
	

get()->
	gen_server:call(?MODULE,get).

get(Key)->
	get(Key,undefined).

get(Key,Default)->
	gen_server:call(?MODULE,{get,Key,Default}).

delete(Key)->
	gen_server:call(?MODULE,{delete,Key}).
	

set(Key,Value)->
	set({Key,Value}).

set(KV={_Key,_Value})->
	gen_server:call(?MODULE,{set,KV}).

handle_call(stop,_From,State)->
	{stop,normal,ok,State};

handle_call(state,_From,State)->
	{reply,{ok,State},State};

handle_call(get,_From,State=#state{ctab=CTab})->
	dets:traverse(CTab,fun(X) -> io:format("~p~n", [X]), continue end),
	{reply,ok,State};

handle_call({get,Key,Default},_From,State=#state{ctab=CTab})->
	Reply=
	case dets:lookup(CTab,Key) of
	[]->
		Default;
	[{Key,Value}]->
		Value
	end,	
	{reply,Reply,State};

handle_call({delete,Key},_From,State=#state{ctab=CTab})->
	Reply=dets:delete(CTab,Key),
	{reply,Reply,State};

handle_call({set,KV={_Key,_Value}},_From,State=#state{ctab=CTab})->
	Reply=dets:insert(CTab,KV),
	{reply,Reply,State};

handle_call(Msg,From,State)->
	Unhandled={unhandled_call,{msg,Msg},{from,From}},
	?warn(Unhandled),
	{reply,Unhandled,State}.

handle_cast(Msg,State)->
	Unhandled={unhandled_cast,{msg,Msg}},
	?warn(Unhandled),
        {noreply,State}.

handle_info(Msg,State)->
	Unhandled={unhandled_info,{msg,Msg}},
	?warn(Unhandled),
        {noreply,State}.

code_change(_OldVsn,Ctx,_Extra) ->
	{ok,Ctx}.	

terminate(Reason,#state{ctab=CTab})->
	?info({terminating,Reason}),
	R=dets:close(CTab),
	?info({dets_close_status,R}),
	ok.
