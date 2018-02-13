
:- module(terminal, [
	      open_connection/0,
	      close_connection/0,
	      ping/1,                   % -Result
	      compile_program/2,        % +Terms, -Result
	      run_query/1               % +Query
	  ]).


:- use_module(library(socket)).
:- use_module(library(random)).

:- use_module('..'/compiler/compiler).
:- use_module('..'/utility/bytes).

:- dynamic(current_connection/1).
:- dynamic(program_compile_state/1).


open_connection :-
	close_connection,
	tcp_connect(ip(192,168,167,1):1, Stream, []),
	assertz(current_connection(Stream)).


close_connection :-
	retract(current_connection(Stream)),
	!,
	close(Stream),
	close_connection.

close_connection.


ping(Result) :-
	command(ping, Header),
	random_between(0, 255, Body),
	current_connection(Stream),
	put_bytes([Header, Body], Stream),
	get_byte(Stream, Response),
	(   Body = Response
	->  Result = success
	;   Result = failure(Body, Response)).


compile_program(Program, Result) :-
	retractall(program_compile_state(_)),
	compile_program(Program, State, Program_Bytes, Label_Table_Bytes),
	assertz(program_compile_state(State)),
	append(Program_Bytes, Label_Table_Bytes, All_Bytes),
	sha_hash(All_Bytes, Hash, [algorithm(sha256)]),
	current_connection(Stream),
	(   check_hash(Stream, Hash)
	->  Result = nothing_to_do
	;   update_hash(Stream, Hash),
	    update_program(Stream, Program_Bytes),
	    update_label_table(Stream, Label_Table_Bytes),
	    Result = updated_program
	).


run_query(Query) :-
	program_compile_state(State),
	compile_query(Query, State, Bytes),
	current_connection(Stream),
	reset_machine(Stream),
	run_query_section(Stream, Bytes).


check_hash(Stream, Hash) :-
	length(Hash, Length),
	uint8(Length, Bytes, Hash),
	put_byte_block(Stream, check_hash, Bytes).


update_hash(Stream, Hash) :-
	length(Hash, Length),
	uint8(Length, Bytes, Hash),
	put_byte_block(Stream, update_hash, Bytes).


update_program(Stream, Program_Bytes) :-
	length(Program_Bytes, Length),
	uint32(Length, Bytes, Program_Bytes),
	put_byte_block(Stream, update_program, Bytes).


update_label_table(Stream, Label_Table_Bytes) :-
	length(Label_Table_Bytes, Length),
	uint32(Length, Bytes, Label_Table_Bytes),
	put_byte_block(Stream, update_label_table, Bytes).


reset_machine(Stream) :-
	command(reset_machine, Header),
	put_bytes([Header], Stream),
	get_byte([1], Stream).


run_query_section(Stream, Query_Bytes) :-
	length(Query_Bytes, Length),
	uint32(Length, Bytes, Query_Bytes),
	put_byte_block(Stream, run_query, Bytes).


put_byte_block(Stream, Command, Block) :-
	command(Command, Header),
	put_bytes([Header|Block], Stream),
	get_byte(Stream, 1).


command(ping, 0x00).
command(check_hash, 0x10).
command(update_hash, 0x20).
command(update_program, 0x21).
command(update_label_table, 0x22).
command(reset_machine, 0x30).
command(run_query, 0x31).
command(read_register, 0x40).
command(read_memory, 0x41).


put_bytes([], Stream) :-
	flush_output(Stream).

put_bytes([Code|Codes], Stream) :-
	put_byte(Stream, Code),
	put_bytes(Codes, Stream).
