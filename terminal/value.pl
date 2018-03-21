
:- module(value, [
	      value//1          % ?Value
	  ]).


:- use_module('..'/utility/datatypes).

value(Body) -->
	header(Type),
	!,
	value_body(Type, Body).


value_body(reference, Reference) -->
	reference_body(Reference),
	!.

value_body(structure, Structure) -->
	structure_body(Structure),
	!.

value_body(list, List) -->
	list_body(List),
	!.

value_body(constant, Constant) -->
	constant_body(Constant),
	!.

value_body(integer, Integer) -->
	integer_body(Integer),
	!.

value_body(environment, Environment) -->
	environment_body(Environment),
	!.


reference_body(reference(H)) -->
	registry_entry(H).


structure_body(structure(Functor_ID, Subterms)) -->
	structure(Functor_ID/Arity),
	{
	    length(Subterms, Arity)
	},
	registry_entries(Subterms).


list_body(list(Head, Tail)) -->
	registry_entry(Head),
	registry_entry(Tail).


constant_body(constant(C)) -->
	constant(C).


integer_body(integer(I)) -->
	integer(I).


environment_body(environment(Permanent_Variables)) -->
	environment_size(Arity),
	{
	    length(Permanent_Variables, Arity)
	},
	registry_entries(Permanent_Variables).


registry_entries([]) -->
	[].

registry_entries([Entry|Entries]) -->
	registry_entry(Entry),
	registry_entries(Entries).


header(Type, Codes, Tail) :-
	ground(Type),
	!,
	header(Type, Code),
	value_header(Code, Codes, Tail).

header(Type, Codes, Tail) :-
	value_header(Code, Codes, Tail),
	header(Type, Code).


header(reference, 0).
header(structure, 1).
header(list, 2).
header(constant, 3).
header(integer, 4).
header(environment, 5).
header(choice_point, 6).
header(trail_item, 7).
