
:- module(register_allocation, [
	      allocate_goals_registers/3,   % -Goals, -Permanent_Variables, +Allocation
	      allocate_atom_registers/3     % -Atom_Terms, -Permanent_Variables, +Allocation
	  ]).

allocate_goals_registers([], _, []).

allocate_goals_registers([Goal|Goals], Permanent_Variables, [Goal_Allocation|Goals_Allocation]) :-
	allocate_goal_registers(Goal, Permanent_Variables, Goal_Allocation),
	allocate_goals_registers(Goals, Permanent_Variables, Goals_Allocation).


allocate_goal_registers(goal(Functor, Terms), Permanent_Variables, goal(Functor, Allocation)) :-
	allocate_atom_registers(Terms, Permanent_Variables, Allocation).


allocate_atom_registers(Atom_Terms, Permanent_Variables, Allocation) :-
	init_register_allocation(Permanent_Variables, State0),
	reserve_atom_arguments_registers(Atom_Terms, Arguments_Allocation, State0, State1),
	allocate_atom_terms_registers(Arguments_Allocation, Allocation, State1, _).


allocate_atom_terms_registers([], [], State, State).

allocate_atom_terms_registers([(a(Addr)=A)|As], [(a(Addr)=B)|Bs], State0, State) :-
	allocate_atom_term_registers(A, B, State0, State1),
	allocate_atom_terms_registers(As, Bs, State1, State).


allocate_atom_term_registers(c(C), c(C), State, State).

allocate_atom_term_registers(s(F, Ts), s(F, Bs), State0, State) :-
	reserve_registers(Ts, As, State0, State1),
	allocate_structure_terms_registers(As, Bs, State1, State).

allocate_atom_term_registers(v(V), A, State0, State) :-
	reserve_register(v(V), A, State0, State).



allocate_structure_terms_registers([], [], State, State).

allocate_structure_terms_registers([Term|Terms], [Term_Allocation|Terms_Allocation], State0, State) :-
	allocate_structure_term_registers(Term, Term_Allocation, State0, State1),
	allocate_structure_terms_registers(Terms, Terms_Allocation, State1, State).


allocate_structure_term_registers(c(C), c(C), State, State).

allocate_structure_term_registers(X=A, X=B, State0, State) :-
	allocate_structure_term_assignment_registers(A, B, State0, State).


allocate_structure_term_assignment_registers(s(F, Terms), s(F, Terms_Allocation), State0, State) :-
	reserve_registers(Terms, Intermediate_Terms_Allocation, State0, State1),
	allocate_structure_terms_registers(Intermediate_Terms_Allocation, Terms_Allocation, State1, State).

allocate_structure_term_assignment_registers(v(V), v(V), State, State).


init_register_allocation(Permanent_Variables, State) :-
	allocate_permanent_variables(Permanent_Variables, 1, Variables),
	Next_Register = 1,
	register_allocation_state(State, Variables, Next_Register).


allocate_permanent_variables([X|Xs], Address, [y(Address)=X|Allocation]) :-
	Next_Address is Address + 1,
	allocate_permanent_variables(Xs, Next_Address, Allocation).

allocate_permanent_variables([], _, []).


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


reserve_register(c(C), c(C), State, State).

reserve_register(s(F, T), x(Register)=s(F, T), State0, State) :-
	register_allocation_state(State0, Variables, Register),
	Next_Register is Register + 1,
	register_allocation_state(State, Variables, Next_Register).


reserve_register(v(V), Register=v(V), State, State) :-
	register_allocation_state(State, Variables, _Next_Register),
	member(Register=v(V), Variables),
	!.

reserve_register(v(V), x(Register)=v(V), State0, State) :-
	register_allocation_state(State0, Variables, Register),
	Next_Register is Register + 1,
	New_Variables = [x(Register)=v(V)|Variables],
	register_allocation_state(State, New_Variables, Next_Register).
