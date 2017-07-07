
:- module(ast_compiler,
	  [
	      compile_query_ast//1,    % + Query_AST
	      compile_program_ast//1   % + Program_AST
	  ]).

compile_query_ast(Query, Codes, Codes_Tail) :-
	allocate_term_registers(Query, Allocation),
	reverse(Allocation, Reverse_Allocation),
	tokenise_allocations(Reverse_Allocation, Tokens),
	convert_query_tokens(Tokens, [], Codes, Codes_Tail).


convert_query_tokens([], _) -->
	[].

convert_query_tokens([Token|Tokens], Rs0) -->
	convert_query_token(Token, Rs0, Rs1),
	convert_query_tokens(Tokens, Rs1).


convert_query_token(X=S, Rs, [X|Rs], [put_structure(S,X)|Codes], Codes).


convert_query_token(x(X), Rs, Rs, [set_value(x(X))|Codes], Codes) :-
	member(x(X), Rs),
	!.

convert_query_token(x(X), Rs, [x(X)|Rs], [set_variable(x(X))|Codes], Codes).


compile_program_ast(Program, Codes, Codes_Tail) :-
	allocate_term_registers(Program, Allocation),
	tokenise_allocations(Allocation, Tokens),
	convert_program_tokens(Tokens, [], Codes, Codes_Tail).


convert_program_tokens([], _) -->
	[].

convert_program_tokens([Token|Tokens], Rs0) -->
	convert_program_token(Token, Rs0, Rs1),
	convert_program_tokens(Tokens, Rs1).


convert_program_token(X=S, Rs, [X|Rs], [get_structure(S,X)|Codes], Codes).


convert_program_token(x(X), Rs, Rs, [unify_value(x(X))|Codes], Codes) :-
	member(x(X), Rs),
	!.

convert_program_token(x(X), Rs, [x(X)|Rs], [unify_variable(x(X))|Codes], Codes).



tokenise_allocations(Allocations, Tokens) :-
	tokenise_allocations(Allocations, Allocations, Tokens, []).

tokenise_allocations([], _) -->
	[].

tokenise_allocations([X=Term|Allocations], All_Allocations) -->
	tokenise_allocation(Term, X, All_Allocations),
	tokenise_allocations(Allocations, All_Allocations).


tokenise_allocation(s(F, Terms), X, All_Allocations) -->
	!,
	[X=F/N],
	{
	    length(Terms, N)
	},
	tokenise_structure_terms(Terms, All_Allocations).

tokenise_allocation(v(_), _, _) -->
	[].

tokenise_structure_terms([], _) -->
	[].

tokenise_structure_terms([Term|Terms], All_Allocations) -->
	[Register],
	{
	    lookup_allocation(Term, Register, All_Allocations)
	},
	tokenise_structure_terms(Terms, All_Allocations).


lookup_allocations([], [], _).

lookup_allocations([Term|Terms], [Register|Registers], Allocations) :-
	lookup_allocation(Term, Register, Allocations),
	lookup_allocations(Terms, Registers, Allocations).


lookup_allocation(Term, Register, Allocations) :-
	member(Register=Term, Allocations),
	!.


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