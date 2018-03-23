
:- module(command, [
	      command/2,        % ?Command, ?Code
	      command//1        % ?Command
	  ]).


command(update_program, 0x00).
command(run_query, 0x10).
command(get_next_answer, 0x11).
command(fetch_value, 0x20).


command(Command, [Code|Tail], Tail) :-
	command(Command, Code).


