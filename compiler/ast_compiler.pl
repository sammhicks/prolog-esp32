
:- module(ast_compiler,
	  [
	      compile_query_ast//1,    % + Query_AST
	      compile_program_ast//1   % + Program_AST
	  ]).

% --- Query ---

compile_query_ast(Query, Codes, Codes_Tail) :-
	allocate_atom_registers(Query, Allocation),
	tokenize_query_allocation(Allocation, Tokens, []),
	convert_query_tokens(Tokens, [], Codes, Codes_Tail).


convert_query_tokens([], _) -->
	[].

convert_query_tokens([Token|Tokens], Rs0) -->
	convert_query_token(Token, Rs0, Rs1),
	convert_query_tokens(Tokens, Rs1).


convert_query_token(call(Atom), Rs, Rs) -->
	[call(Atom)].

convert_query_token(X=S, Rs, [X|Rs]) -->
	[put_structure(S,X)].

convert_query_token(xa(X, A), Rs, [X|Rs]) -->
	[put_value(X, A)],
	{
	    member(A, Rs),
	    !
	}.

convert_query_token(xa(X, A), Rs, [X|Rs]) -->
	[put_variable(X, A)].

convert_query_token(x(X), Rs, Rs) -->
	[set_value(x(X))],
	{
	    member(x(X), Rs),
	    !
	}.

convert_query_token(x(X), Rs, [x(X)|Rs]) -->
	[set_variable(x(X))].

% --- Program ---

compile_program_ast(Program, Codes, Codes_Tail) :-
	allocate_atom_registers(Program, Allocation),
	tokenize_program_allocation(Allocation, Tokens, []),
	convert_program_tokens(Tokens, [], Codes, Codes_Tail).


convert_program_tokens([], _) -->
	[].

convert_program_tokens([Token|Tokens], Rs0) -->
	convert_program_token(Token, Rs0, Rs1),
	convert_program_tokens(Tokens, Rs1).


convert_program_token(label(Label), Rs, Rs) -->
	[label(Label)].

convert_program_token(proceed, Rs, Rs) -->
	[proceed].

convert_program_token(X=S, Rs, [X|Rs]) -->
	[get_structure(S,X)].


convert_program_token(xa(X, A), Rs, Rs) -->
	[get_value(X, A)],
	{
	    member(X, Rs),
	    !
	}.

convert_program_token(xa(X, A), Rs, [X|Rs]) -->
	[get_variable(X, A)].


convert_program_token(x(X), Rs, Rs) -->
	[unify_value(x(X))],
	{
	    member(x(X), Rs),
	    !
	}.

convert_program_token(x(X), Rs, [x(X)|Rs]) -->
	[unify_variable(x(X))].


% --- Allocation Tokenisation ---

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


% --- Register Allocation ---

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

