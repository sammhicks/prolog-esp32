
:- module(compiler, [
	      compile_query/5,    % +Term, +State, -Query_Bytes, -Constants, -Structures
	      compile_program/4   % +Query, -State, -Program_Bytes, -Label_Table_Bytes
	  ]).

:- use_module(parser).
:- use_module(ast_compiler).
:- use_module(assembler).

compile_query(Term, State, Query_Bytes, Constants, Structures) :-
	query(Term, Query),
	compile_query_ast(Query, Codes, []),
	assemble_query(Codes, State, Query_Bytes, Constants, Structures).


compile_program(Terms, State, Program_Bytes, Label_Table_Bytes) :-
	program(Terms, Program),
	compile_program_ast(Program, Codes, []),
	assemble_program(Codes, State, Program_Bytes),
	assemble_label_table(State, Label_Table_Bytes).
