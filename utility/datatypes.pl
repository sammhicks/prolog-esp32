
:- module(datatypes, [
	      register_index//1,
	      vn//1,
	      ai//1,
	      vnai//2,
	      structure//1,
	      functor_id//1,
	      arity//1,
	      constant//1,
	      integer//1,
	      void_count//1,
	      term_id//1,
	      environment_size//1,
	      program_location//1,
	      program_length//1,
	      value_header//1,
	      registry_entry//1,
	      hash_length//1
	  ]).

:- use_module(bytes).
:- use_module("../wam/build/terminal-config/config").


register_index(N) -->
	uint8(N),
	{
	    config(register_count, Register_Count),
	    (	N < Register_Count
	    ->	true
	    ;	throw(invalid_register_index(N))
	    )
	}.


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
	functor_id(ID),
	arity(Arity).


functor_id(ID) -->
	config_bytes(functor_size, ID, unsigned).


arity(Arity) -->
	config_bytes(arity_size, Arity, unsigned).


constant(C) -->
	config_bytes(constant_size, C, unsigned).


integer(I) -->
	config_bytes(integer_size, I, signed).


void_count(N) -->
	uint8(N).


term_id(ID) -->
	uint16(ID).


environment_size(N) -->
	uint8(N).


program_location(N) -->
	uint32(N).


program_length(N) -->
	program_location(N).


value_header(N) -->
	uint8(N).


registry_entry(Entry) -->
	uint32(Entry).


hash_length(N) -->
	uint8(N).


config_bytes(Name, N, Sign, Codes, Tail) :-
	config(Name, Bits),
	Bytes is Bits // 8,
	bytes(Bytes, N, Sign, Codes, Tail).
