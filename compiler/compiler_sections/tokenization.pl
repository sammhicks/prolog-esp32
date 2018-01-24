
:- module(tokenization, [
	      tokenize_query_allocation//2,     % -Functor, -Allocation
	      tokenize_fact_allocation//1,      % -Allocation
	      tokenize_rule_allocation//3       % -Allocation, -Goals, -Permanent_Variables
	  ]).

tokenize_query_allocation(Functor, Allocation) -->
	tokenize_atom_argument_allocation_list(Allocation),
	tokenize_atom_argument_subterm_list(Allocation, query),
	[call(Functor)].


tokenize_fact_allocation(Allocation) -->
	tokenize_atom_argument_allocation_list(Allocation),
	tokenize_atom_argument_subterm_list(Allocation, program),
	[proceed].


tokenize_rule_allocation(Allocation, Goals, Permanent_Variables) -->
	[allocate(Frame_Size)],
	{
	    length(Permanent_Variables, Frame_Size)
	},
	tokenize_atom_argument_allocation_list(Allocation),
	tokenize_atom_argument_subterm_list(Allocation, program),
	tokenize_goals_allocation(Goals),
	[deallocate].


tokenize_goals_allocation([]) -->
	[].

tokenize_goals_allocation([Goal|Goals]) -->
	tokenize_goal_allocation(Goal),
	tokenize_goals_allocation(Goals).


tokenize_goal_allocation(goal(Functor, Allocation), [goal(Query_Tokens)|Tokens], Tokens) :-
	tokenize_query_allocation(Functor, Allocation, Query_Tokens, []).


tokenize_atom_argument_allocation_list([]) -->
	[].

tokenize_atom_argument_allocation_list([(A=T)|As]) -->
	tokenize_atom_argument_allocation(T, A),
	tokenize_atom_argument_allocation_list(As).


tokenize_atom_argument_allocation(X=v(_), A) -->
	[xa(X, A)].

tokenize_atom_argument_allocation(c(C), A) -->
	!,
	[A=c(C)].

tokenize_atom_argument_allocation(s(F, Ts), A) -->
	[A=s(F)],
	tokenize_structure_term_registers(Ts).


tokenize_atom_argument_subterm_list([], _Mode) -->
	[].

tokenize_atom_argument_subterm_list([_=T|As], Mode) -->
	tokenize_atom_argument_subterm(T, Mode),
	tokenize_atom_argument_subterm_list(As, Mode).


tokenize_atom_argument_subterm(c(_), _Mode) -->
	[].

tokenize_atom_argument_subterm(s(_, Ts), Mode) -->
	tokenize_allocation_list(Ts, Mode).

tokenize_atom_argument_subterm(_=v(_), _Mode) -->
	[].


tokenize_allocation_list([], _Mode) -->
	[].

tokenize_allocation_list([c(_)|As], Mode) -->
	tokenize_allocation_list(As, Mode).

tokenize_allocation_list([(X=T)|As], Mode) -->
	tokenize_assignment(T, X, Mode),
	tokenize_allocation_list(As, Mode).


tokenize_assignment(s(F, Ts), X, query) -->
	tokenize_allocation_list(Ts, query),
	[X=s(F)],
	tokenize_structure_term_registers(Ts).

tokenize_assignment(s(F, Ts), X, program) -->
	[X=s(F)],
	tokenize_structure_term_registers(Ts),
	tokenize_allocation_list(Ts, program).

tokenize_assignment(v(_), _, _Mode) -->
	[].


tokenize_structure_term_registers([]) -->
	[].

tokenize_structure_term_registers([c(C)|Terms]) -->
	[c(C)],
	tokenize_structure_term_registers(Terms).

tokenize_structure_term_registers([X=T|Terms]) -->
	tokenize_structure_term_register(T, X),
	tokenize_structure_term_registers(Terms).


tokenize_structure_term_register(_, X) -->
	[X].
