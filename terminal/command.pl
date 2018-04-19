
:- module(command, [
	      command/2,        % ?Command, ?Code
	      command//1        % ?Command
	  ]).

%!	command(+Command, ?Header) is det.
%!	command(?Command, ?Header) is nondet.
%
%	The message header of different commands
command(update_program, 0x00).
command(run_query, 0x10).
command(get_next_answer, 0x11).
command(fetch_value, 0x20).

%!	command(+Command)// is det.
%!	command(?Command)// is nondet.
%
%	The DCG version of command/2.
command(Command, [Code|Tail], Tail) :-
	command(Command, Code).


