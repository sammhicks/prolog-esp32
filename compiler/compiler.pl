
:- module(compiler, [
	      compile_query/3,    % +Query, +State, -Query_Bytes
	      compile_program/4   % +Query, -State, -Program_Bytes, -Label_Table_Bytes
	  ]).

:- use_module(parser).
:- use_module(ast_compiler).
:- use_module(assembler).

compile_query(Stream, State, Query_Bytes) :-
	read_options(Options),
	read_term(Stream, Term, Options),
	query(Term, Options, Query),
	compile_query_ast(Query, Codes, []),
	assemble_query(Codes, State, Query_Bytes).

compile_program(Stream, State, Program_Bytes, Label_Table_Bytes) :-
	program(Stream, Program),
	compile_program_ast(Program, Codes, []),
	assemble_program(Codes, State, Program_Bytes),
	assemble_label_table(State, Label_Table_Bytes).
