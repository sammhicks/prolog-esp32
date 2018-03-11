
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
	      value_header//1,
	      registry_entry//1,
	      hash_length//1
	  ]).

:- use_module(bytes).

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
	functor_id(ID),
	arity(Arity).


functor_id(ID) -->
	uint16(ID).


arity(Arity) -->
	uint8(Arity).


constant(C) -->
	uint16(C).


integer(I) -->
	int32(I).


void_count(N) -->
	uint8(N).


term_id(ID) -->
	uint16(ID).


environment_size(N) -->
	uint8(N).


program_location(N) -->
	uint32(N).


value_header(N) -->
	uint8(N).


registry_entry(Entry) -->
	uint32(Entry).


hash_length(N) -->
	uint8(N).
