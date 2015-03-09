-module(aqueue).
-compile(export_all).


new(Max) when is_integer(Max), Max > 0 ->
	{0,Max,[],[]}.                                      %Length, Max, Rear and Front

empty({_L,Max,_R,_F})->
	{0,Max,[],[]}.                                      

dump({_L,_M,[],F})->
	%% F;
	lists:reverse(F);

dump({_L,_M,R,[]})->
	%% lists:reverse(R);
	R;

dump({_L,_M,R,F})->
	%%lists:append(F,lists:reverse(R)).
	lists:append(lists:reverse(F),R).

take({L,M,R,[H|T]})->
	{H,{L-1,M,R,T}};

take({L,M,R,[]}) when L > 0->
	take({L,M,[],lists:reverse(R)}).                %Move the rear to the front

add(E, {L,M,R,F}) when L < M->
	{L+1,M,lists:append([E],R),F}; 				%Add element to rear

add(E, {M,M,R,[H|T]})->
	{M,M,lists:append([E],R),T};                                  %Add element to rear

add(E, {M,M,R,[]})->
	add(E, {M,M,[],lists:reverse(R)}).              %Move the rear to the front

%% message handling and formatting functions

fsmFmtLogMessage(CurrentState,Event,NextState) when is_list(Event)->
        S=io_lib:format("[~s] :: ~s -> [~s]",[CurrentState,Event,NextState]),
        list_to_binary(S);

fsmFmtLogMessage(CurrentState,Event,NextState)->
        S=io_lib:format("[~s] :: ~p -> [~s]",[CurrentState,Event,NextState]),
        list_to_binary(S).

log(Event,Queue={_L,_Max,_R,_F})->
	add({Event,iso8601:format(calendar:local_time())},Queue).

logFsm(CurrentState,Event,NextState,Queue)->
	MEvent=fsmFmtLogMessage(CurrentState,Event,NextState),
	log(MEvent,Queue).
	
