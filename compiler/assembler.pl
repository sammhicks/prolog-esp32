
:- module(assembler, [
	      assemble_query/3,         % +Codes, +State, -Bytes
	      assemble_program/3        % +Codes, -State, -Bytes
	  ]).

:- use_module(library(lists)).

:- use_module(assembly_sections/allocate_labels).
:- use_module(assembly_sections/remove_labels).
:- use_module(assembly_sections/allocate_structures).
:- use_module(utility/bytes).

assemble_query(Codes0, State, Bytes) :-
	assembly_state(State, Structures, Labels, _Label_Table),
	allocate_structures(Codes0, Codes1, Structures, _Structures),
	apply_labels(Codes1, Labels, Codes2),
	assemble_codes(Codes2, Bytes, []).

assemble_program(Codes0, State, Bytes) :-
	init_structures_state(Structures0),
	allocate_structures(Codes0, Codes1, Structures0, Structures),
	allocate_labels(Codes1, Labels),
	apply_labels(Codes1, Labels, Codes2),
	assemble_codes(Codes2, Bytes_With_Labels, []),
	same_length(Labels, Label_Table),
	remove_labels(Bytes_With_Labels, Bytes, Labels, Label_Table),
	assembly_state(State, Structures, Labels, Label_Table).

assemble_codes([]) -->
	[].

assemble_codes([Code|Codes]) -->
	assemble_code(Code),
	assemble_codes(Codes).


assemble_code(put_variable(Vn, Ai)) -->
	put_variable(Vn, Ai).

assemble_code(put_value(Vn, Ai)) -->
	put_value(Vn, Ai).

assemble_code(put_structure(S, a(I))) -->
	[0x06],
	structure(S),
	ai(I).

assemble_code(get_variable(Vn, Ai)) -->
	get_variable(Vn, Ai).

assemble_code(get_value(Vn, Ai)) -->
	get_value(Vn, Ai).

assemble_code(get_structure(S, a(I))) -->
	[0x16],
	structure(S),
	ai(I).

assemble_code(set_variable(Vn)) -->
	set_variable(Vn).

assemble_code(set_value(Vn)) -->
	set_value(Vn).

assemble_code(unify_variable(Vn)) -->
	unify_variable(Vn).

assemble_code(unify_value(Vn)) -->
	unify_value(Vn).

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


put_variable(x(N), a(I)) -->
	[0x00],
	vnai(N, I).

put_variable(y(N), a(I)) -->
	[0x01],
	vnai(N, I).


put_value(x(N), a(I)) -->
	[0x02],
	vnai(N, I).

put_value(y(N), a(I)) -->
	[0x03],
	vnai(N, I).


get_variable(x(N), a(I)) -->
	[0x10],
	vnai(N, I).

get_variable(y(N), a(I)) -->
	[0x11],
	vnai(N, I).


get_value(x(N), a(I)) -->
	[0x12],
	vnai(N, I).

get_value(y(N), a(I)) -->
	[0x13],
	vnai(N, I).


set_variable(x(N)) -->
	[0x20],
	vn(N).

set_variable(y(N)) -->
	[0x21],
	vn(N).


set_value(x(N)) -->
	[0x22],
	vn(N).

set_value(y(N)) -->
	[0x23],
	vn(N).


unify_variable(x(N)) -->
	[0x30],
	vn(N).

unify_variable(y(N)) -->
	[0x31],
	vn(N).


unify_value(x(N)) -->
	[0x32],
	vn(N).

unify_value(y(N)) -->
	[0x33],
	vn(N).


vn(N) -->
	uint8(N).


ai(I) -->
	uint8(I).


vnai(N, I) -->
	vn(N),
	ai(I).


structure(ID/Arity) -->
	uint16(ID),
	uint8(Arity).


term_id(ID) -->
	uint16(ID).


jump(J) -->
	uint16(J).


assembly_state(state(Structures, Labels, Label_Table), Structures, Labels, Label_Table).
