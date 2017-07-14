
:- module(tokenization, [
	      tokenize_query_allocation//1,     % -Query_Allocation
	      tokenize_program_allocation//1    % -Program_Allocation
	  ]).

tokenize_query_allocation(a(Functor, Allocation)) -->
	tokenize_atom_argument_allocation_list(Allocation),
	tokenize_atom_argument_subterm_list(Allocation, query),
	[call(Functor/Arity)],
	{
	    length(Allocation, Arity)
	}.

tokenize_program_allocation(a(Functor, Allocation)) -->
	[label(Functor/Arity)],
	{
	    length(Allocation, Arity)
	},
	tokenize_atom_argument_allocation_list(Allocation),
	tokenize_atom_argument_subterm_list(Allocation, program),
	[proceed].


tokenize_atom_argument_allocation_list([]) -->
	[].

tokenize_atom_argument_allocation_list([(A=T)|As]) -->
	tokenize_atom_argument_allocation(T, A),
	tokenize_atom_argument_allocation_list(As).


tokenize_atom_argument_allocation(X=v(_), A) -->
	[xa(X, A)].

tokenize_atom_argument_allocation(s(F, Ts), A) -->
	[A=F/N],
	{
	    length(Ts, N)
	},
	tokenize_structure_term_registers(Ts).


tokenize_atom_argument_subterm_list([], _Mode) -->
	[].

tokenize_atom_argument_subterm_list([_=T|As], Mode) -->
	tokenize_atom_argument_subterm(T, Mode),
	tokenize_atom_argument_subterm_list(As, Mode).


tokenize_atom_argument_subterm(s(_, Ts), Mode) -->
	tokenize_allocation_list(Ts, Mode).

tokenize_atom_argument_subterm(_=v(_), _Mode) -->
	[].


tokenize_allocation_list([], _Mode) -->
	[].

tokenize_allocation_list([(X=T)|As], Mode) -->
	tokenize_assignment(T, X, Mode),
	tokenize_allocation_list(As, Mode).


tokenize_assignment(s(F, Ts), X, query) -->
	tokenize_allocation_list(Ts, query),
	[X=F/N],
	{
	    length(Ts, N)
	},
	tokenize_structure_term_registers(Ts).

tokenize_assignment(s(F, Ts), X, program) -->
	[X=F/N],
	{
	    length(Ts, N)
	},
	tokenize_structure_term_registers(Ts),
	tokenize_allocation_list(Ts, program).

tokenize_assignment(v(_), _, _Mode) -->
	[].


tokenize_structure_term_registers([]) -->
	[].

tokenize_structure_term_registers([x(X)=_|Terms]) -->
	[x(X)],
	tokenize_structure_term_registers(Terms).
