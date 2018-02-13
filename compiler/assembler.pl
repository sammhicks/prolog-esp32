
:- module(assembler, [
	      assemble_query/3,         % +Codes, +State, -Bytes
	      assemble_program/3,       % +Codes, -State, -Bytes
	      assemble_label_table/2    % +State, -Label_Table_Bytes
	  ]).

:- use_module(library(lists)).

:- use_module(assembly_sections/allocate_labels).
:- use_module(assembly_sections/remove_labels).
:- use_module(assembly_sections/allocate_structures).
:- use_module(assembly_sections/allocate_constants).
:- use_module('..'/utility/bytes).

assemble_query(Codes0, State, Bytes) :-
	assembly_state(State, Structures, Constants, Labels, _Label_Table),
	allocate_structures(Codes0, Codes1, Structures, _Structures),
	allocate_constants(Codes1, Codes2, Constants, _Constants),
	apply_labels(Codes2, Labels, Codes3),
	assemble_codes(Codes3, Bytes, []).

assemble_program(Codes0, State, Bytes) :-
	init_structures_state(Structures0),
	allocate_structures(Codes0, Codes1, Structures0, Structures),
	init_constants_state(Constants0),
	allocate_constants(Codes1, Codes2, Constants0, Constants),
	allocate_labels(Codes2, Labels),
	apply_labels(Codes2, Labels, Codes3),
	assemble_codes(Codes3, Bytes_With_Labels, []),
	same_length(Labels, Label_Table),
	remove_labels(Bytes_With_Labels, Bytes, Labels, Label_Table),
	assembly_state(State, Structures, Constants, Labels, Label_Table).


assemble_label_table(State, Label_Table_Bytes) :-
	assembly_state(State, _Structures, _Constants, _Labels, Label_Table),
	assemble_label_table_entries(Label_Table, Label_Table_Bytes, []).


assemble_codes([]) -->
	[].

assemble_codes([Code|Codes]) -->
	assemble_code(Code),
	assemble_codes(Codes).


assemble_code(put_variable(Vn, Ai)) -->
	put_variable(Vn, Ai).

assemble_code(put_value(Vn, Ai)) -->
	put_value(Vn, Ai).

assemble_code(put_structure(S, Ai)) -->
	[0x04],
	structure(S),
	ai(Ai).

assemble_code(put_list(Ai)) -->
	[0x05],
	ai(Ai).

assemble_code(put_constant(C, Ai)) -->
	[0x06],
	constant(C),
	ai(Ai).

assemble_code(put_integer(I, Ai)) -->
	[0x07],
	integer(I),
	ai(Ai).

assemble_code(get_variable(Vn, Ai)) -->
	get_variable(Vn, Ai).

assemble_code(get_value(Vn, Ai)) -->
	get_value(Vn, Ai).

assemble_code(get_structure(S, Ai)) -->
	[0x14],
	structure(S),
	ai(Ai).

assemble_code(get_list(Ai)) -->
	[0x15],
	ai(Ai).

assemble_code(get_constant(C, Ai)) -->
	[0x16],
	constant(C),
	ai(Ai).

assemble_code(get_integer(I, Ai)) -->
	[0x17],
	integer(I),
	ai(Ai).

assemble_code(set_variable(Vn)) -->
	set_variable(Vn).

assemble_code(set_value(Vn)) -->
	set_value(Vn).

assemble_code(set_constant(C)) -->
	[0x26],
	constant(C).

assemble_code(set_integer(I)) -->
	[0x27],
	constant(I).

assemble_code(unify_variable(Vn)) -->
	unify_variable(Vn).

assemble_code(unify_value(Vn)) -->
	unify_value(Vn).

assemble_code(unify_constant(C)) -->
	[0x36],
	constant(C).

assemble_code(unify_integer(I)) -->
	[0x37],
	integer(I).

assemble_code(allocate(N)) -->
	[0x40],
	uint8(N).

assemble_code(deallocate) -->
	[0x41].

assemble_code(label(L)) -->
	[label(L)].

assemble_code(call(ID)) -->
	[0x42],
	term_id(ID).

assemble_code(proceed) -->
	[0x44].

assemble_code(try_me_else(J)) -->
	[0x48],
	jump(J).

assemble_code(retry_me_else(J)) -->
	[0x49],
	jump(J).

assemble_code(trust_me) -->
	[0x49].


put_variable(x(N), Ai) -->
	[0x00],
	vnai(x(N), Ai).

put_variable(y(N), Ai) -->
	[0x01],
	vnai(y(N), Ai).


put_value(x(N), Ai) -->
	[0x02],
	vnai(x(N), Ai).

put_value(y(N), Ai) -->
	[0x03],
	vnai(y(N), Ai).


get_variable(x(N), Ai) -->
	[0x10],
	vnai(x(N), Ai).

get_variable(y(N), Ai) -->
	[0x11],
	vnai(y(N), Ai).


get_value(x(N), Ai) -->
	[0x12],
	vnai(x(N), Ai).

get_value(y(N), Ai) -->
	[0x13],
	vnai(y(N), Ai).


set_variable(x(N)) -->
	[0x20],
	vn(x(N)).

set_variable(y(N)) -->
	[0x21],
	vn(y(N)).


set_value(x(N)) -->
	[0x22],
	vn(x(N)).

set_value(y(N)) -->
	[0x23],
	vn(y(N)).


unify_variable(x(N)) -->
	[0x30],
	vn(x(N)).

unify_variable(y(N)) -->
	[0x31],
	vn(y(N)).


unify_value(x(N)) -->
	[0x32],
	vn(x(N)).

unify_value(y(N)) -->
	[0x33],
	vn(y(N)).


assemble_label_table_entries([]) -->
	[].

assemble_label_table_entries([Entry|Entries]) -->
	structure(Entry),
	assemble_label_table_entries(Entries).


register_index(N) -->
	uint8(N).


vn(x(N)) -->
	register_index(N).

vn(y(N)) -->
	register_index(N).


ai(a(I)) -->
	register_index(I).

ai(x(N)) -->
	register_index(N).


vnai(N, I) -->
	vn(N),
	ai(I).


structure(ID/Arity) -->
	uint16(ID),
	uint8(Arity).


constant(C) -->
	uint16(C).


integer(I) -->
	uint16(I).


term_id(ID) -->
	uint16(ID).


jump(J) -->
	uint16(J).


assembly_state(state(Structures, Constants, Labels, Label_Table), Structures, Constants, Labels, Label_Table).
