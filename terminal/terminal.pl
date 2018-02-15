
:- module(terminal, [
	      open_connection/0,
	      close_connection/0,
	      ping/1,                   % -Result
	      compile_program/2,        % +Terms, -Result
	      run_query/1,              % +Query
	      read_register/2,          % +Xn, -Value
	      read_memory/2,            % +H, -Value
	      read_functor/2	        % +H, -Value
	  ]).


:- use_module(library(socket)).
:- use_module(library(random)).

:- use_module('..'/compiler/compiler).
:- use_module(command).
:- use_module(microcontroller_io).
:- use_module(read_solution).

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
	compile_query(Query, State, Bytes, Constants, Structures),
	current_connection(Stream),
	reset_machine(Stream),
	run_query(Stream, Bytes, Results),
	\+ Results = failure,
	read_solution(Stream, Query, Constants, Structures).


read_register(Xn, Value) :-
	current_connection(Stream),
	read_register(Stream, Xn, Value).


read_memory(H, Value) :-
	current_connection(Stream),
	read_memory(Stream, H, Value).


read_functor(H, Functor/Arity) :-
	current_connection(Stream),
	read_functor(Stream, H, Functor, Arity).
