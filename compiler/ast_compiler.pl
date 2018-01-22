
:- module(ast_compiler, [
	      compile_query_ast//1,     % +Query_AST
	      compile_program_ast//1    % +Program_AST
	  ]).

:- use_module(compiler_sections/register_allocation).
:- use_module(compiler_sections/tokenization).
:- use_module(compiler_sections/permanent_variables).

% --- Query ---

compile_query_ast(query(Functor, Terms), Codes, Codes_Tail) :-
	allocate_atom_registers(Terms, [], Allocation),
	tokenize_query_allocation(Functor, Allocation, Tokens, []),
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

compile_program_ast([]) -->
	[].

compile_program_ast([Clause|Clauses]) -->
	compile_program_clause_ast(Clause),
	compile_program_ast(Clauses).


compile_program_clause_ast(rule(head(Functor, Terms), Goals), Codes, Codes_Tail) :-
	allocate_permanent_variables(Terms, Goals, Permanent_Variables),
	allocate_atom_registers(Terms, Permanent_Variables, Terms_Allocation),
	allocate_goals_registers(Goals, Permanent_Variables, Goals_Allocation),
	tokenize_rule_allocation(Functor, Terms_Allocation, Goals_Allocation, Permanent_Variables, Tokens, []),
	convert_program_tokens(Tokens, [], Codes, Codes_Tail).

compile_program_clause_ast(fact(Functor, Terms), Codes, Codes_Tail) :-
	allocate_atom_registers(Terms, [], Allocation),
	tokenize_fact_allocation(Functor, Allocation, Tokens, []),
	convert_program_tokens(Tokens, [], Codes, Codes_Tail).


convert_program_tokens([], _) -->
	[].

convert_program_tokens([Token|Tokens], Rs0) -->
	convert_program_token(Token, Rs0, Rs1),
	convert_program_tokens(Tokens, Rs1).


convert_program_token(goal(Tokens), Rs, Rs) -->
	convert_query_tokens(Tokens, []).

convert_program_token(label(Label), Rs, Rs) -->
	[label(Label)].

convert_program_token(proceed, Rs, Rs) -->
	[proceed].

convert_program_token(allocate(N), Rs, Rs) -->
	[allocate(N)].

convert_program_token(deallocate, Rs, Rs) -->
	[deallocate].

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

convert_program_token(y(Y), Rs, Rs) -->
	[unify_value(y(Y))],
	{
	    member(y(Y), Rs),
	    !
	}.

convert_program_token(y(Y), Rs, [y(Y)|Rs]) -->
	[unify_variable(y(Y))].
