
:- module(value, [
	      value//1,         % ?Value
	      functor_arity//2, % ?Functor, ?Arity
	      heap_index//1      % ?heapIndex
	  ]).


:- use_module('..'/utility/bytes).


value(Reference) -->
	reference(Reference),
	!.

value(Structure) -->
	structure(Structure),
	!.

value(List) -->
	list(List),
	!.

value(Constant) -->
	constant(Constant),
	!.

value(Integer) -->
	integer(Integer),
	!.


functor_arity(Functor, Arity) -->
	uint16(Functor),
	uint8(Arity).


reference(reference(H)) -->
	header(reference),
	heap_index(H).


structure(structure(H)) -->
	header(structure),
	heap_index(H).


list(list(H)) -->
	header(list),
	heap_index(H).


constant(constant(C)) -->
	header(constant),
	uint16(C).


integer(integer(I)) -->
	header(integer),
	int16(I).


heap_index(H) -->
	uint16(H).


header(Type, Codes, Tail) :-
	header(Type, Code),
	uint8(Code, Codes, Tail).


header(reference, 0).
header(structure, 1).
header(list, 2).
header(constant, 3).
header(integer, 4).
