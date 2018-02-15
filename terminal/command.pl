
:- module(command, [
	      ping_request//1,  % ?Body
	      ping_response//1, % ?Response
	      command/2,        % ?Command, ?Code
	      command//1        % ?Command
	  ]).


ping_request(Body) -->
	command(ping),
	[Body].


ping_response(Response) -->
	[Response].



command(ping, 0x00).
command(check_hash, 0x10).
command(update_hash, 0x20).
command(update_program, 0x21).
command(update_label_table, 0x22).
command(reset_machine, 0x30).
command(run_query, 0x31).
command(read_register, 0x40).
command(read_memory, 0x41).
command(read_functor, 0x42).


command(Command, [Code|Tail], Tail) :-
	command(Command, Code).


