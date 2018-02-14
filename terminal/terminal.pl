
:- module(terminal, [
	      open_connection/0,
	      close_connection/0,
	      ping/1,                   % -Result
	      compile_program/2,        % +Terms, -Result
	      run_query/1,              % +Query
	      read_register/2,          % +Xn, -Value
	      read_memory/2             % +H, -Value
	  ]).


:- use_module(library(socket)).
:- use_module(library(random)).

:- use_module('..'/compiler/compiler).
:- use_module('..'/utility/bytes).
:- use_module(command).
:- use_module(value).

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


read_register(Xn, Value) :-
	current_connection(Stream),
	read_register(Stream, Xn, Value).


read_memory(H, Value) :-
	current_connection(Stream),
	read_memory(Stream, H, Value).


check_hash(Stream, Hash) :-
	length(Hash, Length),
	uint8(Length, Bytes, Hash),
	put_command_with_block(Stream, check_hash, Bytes),
	get_boolean(Stream).


update_hash(Stream, Hash) :-
	length(Hash, Length),
	uint8(Length, Bytes, Hash),
	put_command_with_block(Stream, update_hash, Bytes),
	get_boolean(Stream).


update_program(Stream, Program_Bytes) :-
	length(Program_Bytes, Length),
	uint32(Length, Bytes, Program_Bytes),
	put_command_with_block(Stream, update_program, Bytes),
	get_boolean(Stream).


update_label_table(Stream, Label_Table_Bytes) :-
	length(Label_Table_Bytes, Length),
	uint32(Length, Bytes, Label_Table_Bytes),
	put_command_with_block(Stream, update_label_table, Bytes),
	get_boolean(Stream).


reset_machine(Stream) :-
	put_command_with_block(Stream, reset_machine, []),
	get_boolean(Stream).


run_query_section(Stream, Bytes) :-
	put_command_with_block(Stream, run_query, Bytes).


read_register(Stream, Xn, Value) :-
	uint8(Xn, TxBytes, []),
	put_command_with_block(Stream, read_register, TxBytes),
	get_byte_block(Stream, RxBytes),
	value(Value, RxBytes, []).


read_memory(Stream, H, Value) :-
	uint16(H, TxBytes, []),
	put_command_with_block(Stream, read_memory, TxBytes),
	get_byte_block(Stream, RxBytes),
	value(Value, RxBytes, []).



put_command_with_block(Stream, Command, Block) :-
	command(Command, Header),
	put_bytes([Header|Block], Stream).


put_bytes([], Stream) :-
	flush_output(Stream).

put_bytes([Code|Codes], Stream) :-
	put_byte(Stream, Code),
	put_bytes(Codes, Stream).


get_byte_block(Stream, Block) :-
	get_byte(Stream, Length),
	length(Block, Length),
	get_bytes(Block, Stream).


get_bytes([], _Stream).

get_bytes([Code|Codes], Stream) :-
	get_byte(Stream, Code),
	get_bytes(Codes, Stream).


get_boolean(Stream) :-
	get_byte(Stream, Code),
	(   Code = 1
	->  true
	;   Code = 0
	->  fail
	;   throw(invalid_boolean(Code))
	).
