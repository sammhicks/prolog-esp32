
:- module(tokenization, [
	      tokenize_query_allocation//2,     % +Functor, +Allocation
	      tokenize_fact_allocation//1,      % +Allocation
	      tokenize_rule_allocation//4       % +Allocation, +Goals, +Permanent_Variables, +Already_Declared_Permanent_Variables
	  ]).

tokenize_query_allocation(Functor, Allocation) -->
	tokenize_atom_argument_subterm_list(Allocation, query),
	tokenize_atom_argument_allocation_list(Allocation),
	tokenize_call(Functor).


tokenize_fact_allocation(Allocation) -->
	tokenize_atom_argument_allocation_list(Allocation),
	tokenize_atom_argument_subterm_list(Allocation, program),
	[proceed].


tokenize_rule_allocation(Allocation, Goals, Permanent_Variables, Already_Declared_Permanent_Variables) -->
	[allocate(Frame_Size)],
	{
	    length(Permanent_Variables, Frame_Size)
	},
	tokenize_atom_argument_allocation_list(Allocation),
	tokenize_atom_argument_subterm_list(Allocation, program),
	tokenize_goals_allocation(Goals, Already_Declared_Permanent_Variables),
	[deallocate].


tokenize_goals_allocation([], []) -->
	[].

tokenize_goals_allocation([Goal|Goals], [Goal_Already_Declared_Permanent_Variables|Goals_Already_Declared_Permanent_Variables]) -->
	tokenize_goal_allocation(Goal, Goal_Already_Declared_Permanent_Variables),
	tokenize_goals_allocation(Goals, Goals_Already_Declared_Permanent_Variables).


tokenize_goal_allocation(goal(Functor, Allocation), Already_Declared_Permanent_Variables, [goal(Query_Tokens, Already_Declared_Permanent_Variables)|Tokens], Tokens) :-
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

tokenize_atom_argument_allocation(i(I), A) -->
	!,
	[i_a(I, A)].

tokenize_atom_argument_allocation(s(Functor, Terms), A) -->
	[s_x(Functor, A)],
	tokenize_structure_term_registers(Terms).

tokenize_atom_argument_allocation(l(Head, Tail), A) -->
	[l_x(A)],
	tokenize_structure_term_registers([Head, Tail]).

tokenize_atom_argument_subterm_list([], _Mode) -->
	[].

tokenize_atom_argument_subterm_list([_=T|As], Mode) -->
	tokenize_atom_argument_subterm(T, Mode),
	tokenize_atom_argument_subterm_list(As, Mode).


tokenize_atom_argument_subterm(c(_), _Mode) -->
	[].

tokenize_atom_argument_subterm(i(_), _Mode) -->
	[].

tokenize_atom_argument_subterm(s(_, Terms), Mode) -->
	tokenize_allocation_list(Terms, Mode).

tokenize_atom_argument_subterm(l(Head, Tail), Mode) -->
	tokenize_allocation_list([Head, Tail], Mode).

tokenize_atom_argument_subterm(_=v(_), _Mode) -->
	[].


tokenize_allocation_list([], _Mode) -->
	[].

tokenize_allocation_list([A|As], Mode) -->
	tokenize_allocation(A, Mode),
	tokenize_allocation_list(As, Mode).


tokenize_allocation(c(_), _) -->
	[].

tokenize_allocation(i(_), _) -->
	[].

tokenize_allocation(X=T, Mode) -->
	tokenize_assignment(T, X, Mode).


tokenize_assignment(s(Functor, Terms), X, Mode) -->
	tokenize_structure_assignment(Mode, Functor, Terms, X).

tokenize_assignment(l(Head, Tail), X, Mode) -->
	tokenize_list_assignment(Mode, [Head, Tail], X).

tokenize_assignment(v(_), _, _Mode) -->
	[].


tokenize_structure_assignment(query, Functor, Terms, X) -->
	tokenize_allocation_list(Terms, query),
	[s_x(Functor, X)],
	tokenize_structure_term_registers(Terms).

tokenize_structure_assignment(program, Functor, Terms, X) -->
	[s_x(Functor, X)],
	tokenize_structure_term_registers(Terms),
	tokenize_allocation_list(Terms, program).


tokenize_list_assignment(query, Items, X) -->
	tokenize_allocation_list(Items, query),
	[l_x(X)],
	tokenize_structure_term_registers(Items).

tokenize_list_assignment(program, Items, X) -->
	[l_x(X)],
	tokenize_structure_term_registers(Items),
	tokenize_allocation_list(Items, program).


tokenize_structure_term_registers([]) -->
	[].

tokenize_structure_term_registers([Term|Terms]) -->
	tokenize_structure_term_register(Term),
	tokenize_structure_term_registers(Terms).


tokenize_structure_term_register(c(C)) -->
	[c(C)].

tokenize_structure_term_register(i(I)) -->
	[i(I)].

tokenize_structure_term_register(X=_) -->
	[X].


tokenize_call(Functor, [Token|Tokens], Tokens) :-
	special_predicate(Functor, Token),
	!.

tokenize_call(Functor, [call(Functor)|Tokens], Tokens).


special_predicate((>)/2, >).
special_predicate((<)/2, <).
special_predicate((=<)/2, =<).
special_predicate((>=)/2, >=).
special_predicate((=:=)/2, =:=).
special_predicate((=\=)/2, =\=).
special_predicate((is)/2, is).
special_predicate(true/0, true).
special_predicate(false/0, fail).
special_predicate(fail/0, fail).
special_predicate((=)/2, =).
