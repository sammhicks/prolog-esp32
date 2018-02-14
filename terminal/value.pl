
:- module(value, [
	      value//1          % ?Value
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


reference(reference(H)) -->
	header(reference),
	heapIndex(H).


structure(structure(H)) -->
	header(structure),
	heapIndex(H).


list(list(H)) -->
	header(list),
	heapIndex(H).


constant(constant(C)) -->
	header(constant),
	uint16(C).


integer(integer(I)) -->
	header(integer),
	int16(I).


heapIndex(H) -->
	uint16(H).


header(Type, Codes, Tail) :-
	header(Type, Code),
	uint8(Code, Codes, Tail).


header(reference, 0).
header(structure, 1).
header(list, 2).
header(constant, 3).
header(integer, 4).
