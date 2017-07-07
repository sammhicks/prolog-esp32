
:- module(compiler, [
	      compile/3    % +Query, +Program, -Codes
	  ]).

:- use_module(parser).
:- use_module(ast_compiler).

compile(Query, Program, Codes) :-
	compile(Query, Program, Codes, []).


compile(Query, Program) -->
	compile_query(Query),
	compile_program(Program).


compile_query(Query, Query_Codes, Query_Codes_Tail) :-
	term(Query_Term, Query, []),
	compile_query_ast(Query_Term, Query_Codes, Query_Codes_Tail).

compile_program(Program, Program_Codes, Program_Codes_Tail) :-
	term(Program_Term, Program, []),
	compile_program_ast(Program_Term, Program_Codes, Program_Codes_Tail).
