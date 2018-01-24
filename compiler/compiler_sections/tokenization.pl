
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
	[x_a(X, A)].

tokenize_atom_argument_allocation(c(C), A) -->
	!,
	[c_a(C, A)].

tokenize_atom_argument_allocation(s(F, Ts), A) -->
	[s_x(F, A)],
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

tokenize_allocation_list([A|As], Mode) -->
	tokenize_allocation(A, Mode),
	tokenize_allocation_list(As, Mode).


tokenize_allocation(c(_), _) -->
	[].

tokenize_allocation(X=T, Mode) -->
	tokenize_assignment(T, X, Mode).


tokenize_assignment(s(Functor, Terms), X, Mode) -->
	tokenize_structure_assignment(Mode, s(Functor, Terms), X).

tokenize_assignment(v(_), _, _Mode) -->
	[].


tokenize_structure_assignment(query, s(Functor, Terms), X) -->
	tokenize_allocation_list(Terms, query),
	[s_x(Functor, X)],
	tokenize_structure_term_registers(Terms).

tokenize_structure_assignment(program, s(Functor, Terms), X) -->
	[s_x(Functor, X)],
	tokenize_structure_term_registers(Terms),
	tokenize_allocation_list(Terms, program).


tokenize_structure_term_registers([]) -->
	[].

tokenize_structure_term_registers([Term|Terms]) -->
	tokenize_structure_term_register(Term),
	tokenize_structure_term_registers(Terms).


tokenize_structure_term_register(c(C)) -->
	[c(C)].

tokenize_structure_term_register(X=_) -->
	[X].
