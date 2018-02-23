
:- module(microcontroller_io, [
	      check_hash/2,
	      update_hash/2,
	      update_program/2,
	      update_label_table/2,
	      reset_machine/1,
	      run_query/3,
	      get_next_answer/2,
	      fetch_structure/4,
	      fetch_list/3,
	      get_byte_block/2,
	      put_bytes/2
	  ]).


:- use_module('..'/utility/bytes).
:- use_module(command).
:- use_module(value).


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


run_query(Stream, Bytes, Results) :-
	put_command_with_block(Stream, run_query, Bytes),
	get_results(Stream, Results).


get_next_answer(Stream, Results) :-
	put_command_with_block(Stream, get_next_answer, []),
	get_results(Stream, Results).


fetch_structure(Stream, H, Name, Arguments) :-
	heap_index(H, Bytes, []),
	put_command_with_block(Stream, fetch_structure, Bytes),
	get_byte_block(Stream, Header),
	functor_arity(Name, Arity, Header, []),
	length(Arguments, Arity),
	wrapped_values(Arguments, Stream).


fetch_list(Stream, H, [Head, Tail]) :-
	heap_index(H, Bytes, []),
	put_command_with_block(Stream, fetch_list, Bytes),
	wrapped_values([Head, Tail], Stream).


wrapped_values([], _Stream).

wrapped_values([Value|Values], Stream) :-
	wrapped_value(Stream, Value),
	wrapped_values(Values, Stream).


wrapped_value(Stream, Value) :-
	get_byte_block(Stream, Bytes),
	value(Value, Bytes, []).


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
	boolean(Code).


boolean(1) :-
	!.

boolean(0) :-
	!,
	fail.

boolean(Code) :-
	throw(invalid_boolean(Code)).


get_results(Stream, Results) :-
	get_byte(Stream, Code),
	results(Code, Results).


results(0, failure).

results(1, success).

results(2, choice_points).

results(3, exception).