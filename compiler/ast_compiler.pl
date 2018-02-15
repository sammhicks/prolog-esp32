
:- module(ast_compiler, [
	      compile_query_ast//1,     % +Query_AST
	      compile_program_ast//1    % +Program_AST
	  ]).

:- use_module(library(lists)).

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

convert_query_token(c_a(C, A), Rs, Rs) -->
	[put_constant(C, A)].

convert_query_token(i_a(I, A), Rs, Rs) -->
	[put_integer(I, A)].

convert_query_token(s_x(S, X), Rs, Rs) -->
	[put_structure(S, X)].

convert_query_token(l_x(X), Rs, Rs) -->
	[put_list(X)].

convert_query_token(x_a(X, A), Rs, Rs) -->
	[put_value(X, A)],
	{
	    member(X, Rs),
	    !
	}.

convert_query_token(x_a(X, A), Rs, [X|Rs]) -->
	[put_variable(X, A)].

convert_query_token(c(C), Rs, Rs) -->
	[set_constant(C)].

convert_query_token(i(I), Rs, Rs) -->
	[set_integer(I)].

convert_query_token(x(X), Rs, Rs) -->
	[set_value(x(X))],
	{
	    member(x(X), Rs),
	    !
	}.

convert_query_token(x(X), Rs, [x(X)|Rs]) -->
	[set_variable(x(X))].

convert_query_token(y(Y), Rs, Rs) -->
	[set_value(y(Y))],
	{
	    member(y(Y), Rs),
	    !
	}.

convert_query_token(y(Y), Rs, [y(Y)|Rs]) -->
	[set_variable(y(Y))].

% --- Program ---

compile_program_ast([]) -->
	[].

compile_program_ast([Definition|Definitions]) -->
	compile_definition_ast(Definition),
	compile_program_ast(Definitions).


compile_definition_ast(fact(Functor, Arguments)) -->
	[label(Functor)],
	compile_fact_ast(Arguments).

compile_definition_ast(rule(head(Functor, Arguments), Goals)) -->
	[label(Functor)],
	compile_rule_ast(Arguments, Goals).

compile_definition_ast(disjunction(Functor, Clauses)) -->
	compile_disjunction_ast(Functor, Clauses).


compile_disjunction_ast(Functor, [Clause|Clauses]) -->
	[label(Functor)],
	disjunction_retry(Functor, Next_Functor),
	[try_me_else(Next_Functor)],
	compile_program_clause_ast(Clause),
	compile_disjunction_tail_ast(Next_Functor, Clauses).


compile_disjunction_tail_ast(Functor, [Clause]) -->
	!,
	[label(Functor)],
	[trust_me],
	compile_program_clause_ast(Clause).

compile_disjunction_tail_ast(Functor, [Clause|Clauses]) -->
	[label(Functor)],
	disjunction_retry(Functor, Next_Functor),
	[retry_me_else(Next_Functor)],
	compile_program_clause_ast(Clause),
	compile_disjunction_tail_ast(Next_Functor, Clauses).


compile_program_clause_ast(fact(Terms)) -->
	compile_fact_ast(Terms).

compile_program_clause_ast(rule(head(Terms), Goals)) -->
	compile_rule_ast(Terms, Goals).


compile_fact_ast(Terms, Codes, Codes_Tail) :-
	allocate_atom_registers(Terms, [], Allocation),
	tokenize_fact_allocation(Allocation, Tokens, []),
	convert_program_tokens(Tokens, [], Codes, Codes_Tail).


compile_rule_ast(Terms, Goals, Codes, Codes_Tail) :-
	allocate_permanent_variables(Terms, Goals, Permanent_Variables, Already_Declared_Permanent_Variables),
	allocate_atom_registers(Terms, Permanent_Variables, Terms_Allocation),
	allocate_goals_registers(Goals, Permanent_Variables, Goals_Allocation),
	tokenize_rule_allocation(Terms_Allocation, Goals_Allocation, Permanent_Variables, Already_Declared_Permanent_Variables, Tokens, []),
	convert_program_tokens(Tokens, [], Codes, Codes_Tail).


convert_program_tokens([], _) -->
	[].

convert_program_tokens([Token|Tokens], Rs0) -->
	convert_program_token(Token, Rs0, Rs1),
	convert_program_tokens(Tokens, Rs1).


convert_program_token(goal(Tokens, Already_Declared_Permanent_Variables), Rs, Rs) -->
	convert_query_tokens(Tokens, Already_Declared_Permanent_Variables).

convert_program_token(proceed, Rs, Rs) -->
	[proceed].

convert_program_token(allocate(N), Rs, Rs) -->
	[allocate(N)].

convert_program_token(deallocate, Rs, Rs) -->
	[deallocate].

convert_program_token(c_a(C, A), Rs, Rs) -->
	[get_constant(C, A)].

convert_program_token(i_a(I, A), Rs, Rs) -->
	[get_integer(I, A)].

convert_program_token(s_x(S, X), Rs, Rs) -->
	[get_structure(S, X)].

convert_program_token(l_x(X), Rs, Rs) -->
	[get_list(X)].

convert_program_token(x_a(X, A), Rs, Rs) -->
	[get_value(X, A)],
	{
	    member(X, Rs),
	    !
	}.

convert_program_token(x_a(X, A), Rs, [X|Rs]) -->
	[get_variable(X, A)].

convert_program_token(c(C), Rs, Rs) -->
	[unify_constant(C)].

convert_program_token(i(I), Rs, Rs) -->
	[unify_integer(I)].

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


disjunction_retry(Functor/Arity, retry(Functor)/Arity) -->
	[].
