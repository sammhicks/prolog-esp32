
:- module(compiler, [
	      compile_query/3,    % +Query, +State, -Query_Bytes
	      compile_program/4   % +Query, -State, -Program_Bytes, -Label_Table_Bytes
	  ]).

:- use_module(parser).
:- use_module(ast_compiler).
:- use_module(assembler).

compile_query(Query, State, Query_Bytes) :-
	query(Query_Term, Query, []),
	compile_query_ast(Query_Term, Codes, []),
	assemble_query(Codes, State, Query_Bytes).

compile_program(Program, State, Program_Bytes, Label_Table_Bytes) :-
	definitions(Program_Term, Program, []),
	!,
	compile_program_ast(Program_Term, Codes, []),
	assemble_program(Codes, State, Program_Bytes),
	assemble_label_table(State, Label_Table_Bytes).
