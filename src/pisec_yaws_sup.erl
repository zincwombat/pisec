-module(pisec_yaws_sup).

-behaviour(supervisor).
-include("debug.hrl").
-include("piyaws.hrl").
-include_lib("yaws/include/yaws.hrl").
%%-include("../yaws/include/yaws.hrl").

%% API
-export([start/0,
	 stop/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor

-define(CHILD(I,Type),{I,{I,start,[]},permanent,5000,Type,[I]}).

%% ===================================================================
%% API functions
%% ===================================================================


start()->
	SConf=[ {port,?YAWS_PORT},
		{listen,?YAWS_LISTEN},
		{servername,?YAWS_SERVERNAME},
		{appmods,?YAWS_APPMODS}],

	GConf=[ {logdir,?YAWS_LOGDIR},
		%% {ebin_dir,?YAWS_EBINDIRS},
	 	{id,?YAWS_ID}],

        {ok,SCList,GC,ChildSpecs}=yaws_api:embedded_start_conf(?YAWS_DOCROOT,SConf,GConf,?YAWS_ID),
	Reply=supervisor:start_link({local,?MODULE},?MODULE,[ChildSpecs]),
	yaws_api:setconf(GC,SCList),
	Reply.

stop()->
	stop(whereis(?MODULE)).

stop(Pid) when is_pid(Pid)->
	exit(Pid,shutdown);

stop(Reason)->
	{error,Reason}.

	

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([ChildSpecs]) ->
	{ok,{{one_for_one,5,10},ChildSpecs}}.
