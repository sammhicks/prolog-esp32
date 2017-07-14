
:- module(register_allocation, [
	      allocate_atom_registers/2     % -Abstarct_Syntax_Tree, Allocations
	  ]).

allocate_atom_registers(a(Functor, Terms), a(Functor, Bs)) :-
	init_register_allocation(State0),
	reserve_atom_arguments_registers(Terms, As, State0, State1),
	allocate_atom_terms_registers(As, Bs, State1, _).


allocate_atom_terms_registers([], [], State, State).

allocate_atom_terms_registers([(X=A)|As], [(X=B)|Bs], State0, State) :-
	allocate_atom_term_registers(A, B, State0, State1),
	allocate_atom_terms_registers(As, Bs, State1, State).


allocate_atom_term_registers(s(F, Ts), s(F, Bs), State0, State) :-
	reserve_registers(Ts, As, State0, State1),
	allocate_structure_terms_registers(As, Bs, State1, State).

allocate_atom_term_registers(v(V), A, State0, State) :-
	reserve_register(v(V), A, State0, State).



allocate_structure_terms_registers([], [], State, State).

allocate_structure_terms_registers([(X=A)|As], [(X=B)|Bs], State0, State) :-
	allocate_structure_term_registers(A, B, State0, State1),
	allocate_structure_terms_registers(As, Bs, State1, State).


allocate_structure_term_registers(s(F, Ts), s(F, Bs), State0, State) :-
	reserve_registers(Ts, As, State0, State1),
	allocate_structure_terms_registers(As, Bs, State1, State).

allocate_structure_term_registers(v(V), v(V), State, State) :-
	true.


init_register_allocation(State) :-
	Variables = [],
	Next_Register = 1,
	register_allocation_state(State, Variables, Next_Register).


register_allocation_state(state(Variables, Next_Register), Variables, Next_Register).


reserve_atom_arguments_registers([], [], State, State).

reserve_atom_arguments_registers([Arg|Args], [Allocation|Allocations], State0, State) :-
	reserve_atom_argument_register(Arg, Allocation, State0, State1),
	reserve_atom_arguments_registers(Args, Allocations, State1, State).


reserve_atom_argument_register(Argument, a(Register)=Argument, State0, State) :-
	register_allocation_state(State0, Variables, Register),
	Next_Register is Register + 1,
	register_allocation_state(State, Variables, Next_Register).


reserve_registers([], [], State, State).

reserve_registers([Term|Terms], [Allocation|Allocations], State0, State) :-
	reserve_register(Term, Allocation, State0, State1),
	reserve_registers(Terms, Allocations, State1, State).


reserve_register(s(F, T), x(Register)=s(F, T), State0, State) :-
	register_allocation_state(State0, Variables, Register),
	Next_Register is Register + 1,
	register_allocation_state(State, Variables, Next_Register).


reserve_register(v(V), x(Register)=v(V), State, State) :-
	register_allocation_state(State, Variables, _Next_Register),
	member(Register=v(V), Variables),
	!.

reserve_register(v(V), x(Register)=v(V), State0, State) :-
	register_allocation_state(State0, Variables, Register),
	Next_Register is Register + 1,
	New_Variables = [Register=v(V)|Variables],
	register_allocation_state(State, New_Variables, Next_Register).
