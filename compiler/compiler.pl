
:- module(compiler, [
	      compile_query/4,    % +Term, +State, -Query_Bytes, -Functors
	      compile_program/3   % +Query, -State, -Program_Bytes
	  ]).

:- use_module(parser).
:- use_module(ast_compiler).
:- use_module(assembler).

compile_query(Term, State, Query_Bytes, Functors) :-
	query(Term, Query),
	compile_query_ast(Query, Codes),
	assemble_query(Codes, State, Query_Bytes, Functors).


compile_program(Terms, State, Program_Bytes) :-
	program(Terms, Program),
	compile_program_ast(Program, Codes),
	assemble_program(Codes, State, Program_Bytes).
