
:- module(value, [
	      value//1,         % ?Value
	      registry_entry//1 % ?Registry_Entry
	  ]).


:- use_module('..'/utility/bytes).

value(Body) -->
	header(Type),
	value_body(Type, Body).


value_body(reference, Reference) -->
	reference(Reference),
	!.

value_body(structure, Structure) -->
	structure(Structure),
	!.

value_body(list, List) -->
	list(List),
	!.

value_body(constant, Constant) -->
	constant(Constant),
	!.

value_body(integer, Integer) -->
	integer(Integer),
	!.

value_body(environment, Environment) -->
	environment(Environment),
	!.


reference(reference(H)) -->
	registry_entry(H).


structure(structure(Functor, Subterms)) -->
	uint16(Functor),
	{
	    length(Subterms, Arity)
	},
	uint8(Arity),
	registry_entries(Subterms).


list(list(Head, Tail)) -->
	registry_entry(Head),
	registry_entry(Tail).


constant(constant(C)) -->
	uint16(C).


integer(integer(I)) -->
	int16(I).


environment(environment(Permanent_Variables)) -->
	uint8(Arity),
	{
	    length(Permanent_Variables, Arity)
	},
	registry_entries(Permanent_Variables).


registry_entries([]) -->
	[].

registry_entries([Entry|Entries]) -->
	registry_entry(Entry),
	registry_entries(Entries).


registry_entry(Entry) -->
	uint32(Entry).


header(Type, Codes, Tail) :-
	ground(Type),
	!,
	header(Type, Code),
	uint8(Code, Codes, Tail).

header(Type, Codes, Tail) :-
	uint8(Code, Codes, Tail),
	header(Type, Code).


header(reference, 0).
header(structure, 1).
header(list, 2).
header(constant, 3).
header(integer, 4).
header(environment, 5).
header(choice_point, 6).
header(trail_item, 7).
