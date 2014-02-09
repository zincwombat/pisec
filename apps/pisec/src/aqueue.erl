-module(aqueue).
-compile(export_all).


new(Max) when is_integer(Max), Max > 0 ->
	{0,Max,[],[]}.                                      %Length, Max, Rear and Front

empty({_L,Max,_R,_F})->
	{0,Max,[],[]}.                                      

dump({_L,_M,[],F})->
	F;

dump({_L,_M,R,[]})->
	lists:reverse(R);

dump({_L,_M,R,F})->
	lists:append(F,lists:reverse(R)).

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
