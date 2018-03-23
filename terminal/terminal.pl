
:- module(terminal, [
	      open_connection/0,
	      close_connection/0,
	      compile_program/2,        % +File, -Result
	      run_query/1
	  ]).

:- use_module(library(socket)).

:- use_module('..'/compiler/compiler).
:- use_module(microcontroller_io).
:- use_module(read_solution).
:- use_module(read_terms).

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


is_connected(Stream) :-
	current_connection(Stream),
	!.

is_connected(_) :-
	throw("Not connected to microcontroller").


compile_program(File, Result) :-
	read_terms_from_file(File, Program),
	retractall(program_compile_state(_)),
	compile_program(Program, State, Program_Bytes),
	assertz(program_compile_state(State)),
	sha_hash(Program_Bytes, Hash, [algorithm(sha256)]),
	is_connected(Stream),
	put_command(Stream, update_program),
	check_hash(Stream, Hash, Hash_Matches),
	update_program(Hash_Matches, Result, Stream, Program_Bytes, Hash).


update_program(hash_matches, nothing_to_do, _Stream, _Program_Bytes, _Hash).

update_program(hash_differs, updated_program, Stream, Program_Bytes, Hash) :-
	update_hash(Stream, Hash),
	update_program(Stream, Program_Bytes).


has_program_compile_state(State) :-
	program_compile_state(State),
	!.

has_program_compile_state(_) :-
	throw("No program compiled").


run_query(Query) :-
	has_program_compile_state(State),
	compile_query(Query, State, Bytes, Functors),
	is_connected(Stream),
	run_query(Stream, Bytes, Results),
	process_results(Results, Stream, Query, Functors).


process_results(exception, _, _, _) :-
	throw("Microcontroller threw exception").

process_results(success, Stream, Query, Functors) :-
	read_solution(Stream, Query, Functors).

process_results(choice_points, Stream, Query, Functors) :-
	read_solution(Stream, Query, Functors).

process_results(choice_points, Stream, Query, Functors) :-
	get_next_answer(Stream, Results),
	process_results(Results, Stream, Query, Functors).
