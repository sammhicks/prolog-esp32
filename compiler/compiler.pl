
:- module(compiler,
	  [
	      allocate_term_registers/2
	  ]).

allocate_term_registers(Term, Allocations) :-
	allocate_term_registers(Term, [], Allocations).

allocate_terms_registers([], Allocations, Allocations).

allocate_terms_registers([Term|Terms], Allocations0, Allocations) :-
	allocate_term_registers(Term, Allocations0, Allocations1),
	allocate_terms_registers(Terms, Allocations1, Allocations).


allocate_term_registers(s(Functor, Terms), Allocations0, Allocations) :-
	reserve_register(s(Functor, Terms), Allocations0, Allocations1),
	reserve_registers(Terms, Allocations1, Allocations2),
	allocate_terms_registers(Terms, Allocations2, Allocations).

allocate_term_registers(v(Variable), Current_Allocations, New_Allocations) :-
	reserve_register(v(Variable), Current_Allocations, New_Allocations).


reserve_registers([], Allocations, Allocations).

reserve_registers([Term|Terms], Allocations, New_Allocations) :-
	reserve_register(Term, Allocations, Intermediate_Allocations),
	reserve_registers(Terms, Intermediate_Allocations, New_Allocations).


reserve_register(Term, Allocations, Allocations) :-
	member(_=Term, Allocations),
	!.

reserve_register(Term, Allocations, New_Allocations) :-
	length(Allocations, Allocations_Count),
	New_Register is Allocations_Count + 1,
	append(Allocations, [x(New_Register)=Term], New_Allocations).
