-module(errorutils).

-include("debug.hrl").
-include("alarm.hrl").
-compile(export_all).


mkHttpPhrase(Status) when is_integer(Status)->
	case (catch yaws_api:code_to_phrase(Status)) of
	Phrase when is_list(Phrase)->
		Phrase;
	_->
           	"Unknown HTTP Status Code"
	end.

mkHttpError(Status) when is_integer(Status)->
	mkError(Status,Status,mkHttpPhrase(Status)).

mkHttpError(Status,Extended) when is_integer(Status)->
	{_Code,Status,Description}=
	case (catch yaws_api:code_to_phrase(Status)) of
	Phrase when is_list(Phrase)->
		{Status,Status,Phrase};
	_->
		{Status,Status,"Unknown HTTP Status Code"}
	end,
	mkError(Status,Status,Description,Extended).


mkError()->
	#error{	status= <<"999">>,
		code= <<"123">>,
		description= <<"dummy error description">>,
		extended= <<"extended description">>}.

mkError(Status)->
	#error{	status= mkString(Status)}.

mkError(Status,Code)->
	#error{	status= mkString(Status),
		code = mkString(Code)}.

mkError(Status,Code,Description)->
	#error{	status= mkString(Status),
		code = mkString(Code),
		description = mkString(Description)}.

mkError(Status,Code,Description,Extended)->
	#error{	status= mkString(Status),
		code = mkString(Code),
		description = mkString(Description),
		extended = mkString(Extended)}.


mkString(Integer) when is_integer(Integer)->
	list_to_binary(integer_to_list(Integer));

mkString(Atom) when is_atom(Atom)->
	list_to_binary(atom_to_list(Atom));

mkString(List) when is_list(List)->
	list_to_binary(List);

mkString(Other)->
	list_to_binary(io_lib:format("~p",[Other])).
