
:- module(compiler, [
	      compile_query/3,    % +Query, +State, -Bytes
	      compile_program/3   % +Query, -State, -Bytes
	  ]).

:- use_module(parser).
:- use_module(ast_compiler).
:- use_module(assembler).

compile_query(Query, State, Bytes) :-
	query(Query_Term, Query, []),
	compile_query_ast(Query_Term, Codes, []),
	assemble_query(Codes, State, Bytes).

compile_program(Program, State, Bytes) :-
	definitions(Program_Term, Program, []),
	!,
	compile_program_ast(Program_Term, Codes, []),
	assemble_program(Codes, State, Bytes).
