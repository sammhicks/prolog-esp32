
:- module(microcontroller_io, [
	      put_command/2,
	      check_hash/3,
	      update_hash/2,
	      update_program/2,
	      run_query/3,
	      get_next_answer/2,
	      fetch_value/3,
	      put_bytes/2
	  ]).


:- use_module('..'/utility/datatypes).
:- use_module(command).
:- use_module(value).


check_hash(Stream, Hash, Hash_Matches) :-
	length(Hash, Length),
	hash_length(Length, Bytes, Hash),
	put_bytes(Bytes, Stream),
	get_byte(Stream, Code),
	hash_matches(Code, Hash_Matches).


hash_matches(0, hash_differs).
hash_matches(1, hash_matches).


update_hash(Stream, Hash) :-
	length(Hash, Length),
	hash_length(Length, Bytes, Hash),
	put_bytes(Bytes, Stream).


update_program(Stream, Program_Bytes) :-
	length(Program_Bytes, Length),
	program_length(Length, Bytes, Program_Bytes),
	put_bytes(Bytes, Stream).

run_query(Stream, Bytes, Results) :-
	put_command_with_block(Stream, run_query, Bytes),
	get_results(Stream, Results).


get_next_answer(Stream, Results) :-
	put_command_with_block(Stream, get_next_answer, []),
	get_results(Stream, Results).


fetch_value(Stream, Address, Value) :-
	registry_entry(Address, Bytes, []),
	put_command_with_block(Stream, fetch_value, Bytes),
	value(Value, Stream, Stream).


put_command(Stream, Command) :-
	command(Command, Byte),
	put_byte(Stream, Byte).


put_command_with_block(Stream, Command, Block) :-
	command(Command, Header),
	put_bytes([Header|Block], Stream).


put_bytes([], Stream) :-
	flush_output(Stream).

put_bytes([Code|Codes], Stream) :-
	put_byte(Stream, Code),
	put_bytes(Codes, Stream).


get_results(Stream, Results) :-
	get_byte(Stream, Code),
	results(Code, Results).


results(0, failure).

results(1, success).

results(2, choice_points).

results(3, exception).
