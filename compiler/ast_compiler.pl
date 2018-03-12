
:- module(ast_compiler, [
	      compile_query_ast/2,      % +Query_AST, -Codes
	      compile_program_ast/2     % +Program_AST, -Codes
	  ]).

:- use_module(library(lists)).

:- use_module(compiler_sections/register_allocation).
:- use_module(compiler_sections/tokenization).
:- use_module(compiler_sections/permanent_variables).
:- use_module(compiler_sections/void_variables).
:- use_module(compiler_sections/last_call_optimisation).

% --- Query ---

compile_query_ast(query(Functor, Terms), Combined_Codes) :-
	allocate_atom_registers(Terms, [], Allocation),
	tokenize_query_allocation(Functor, Allocation, Tokens, []),
	convert_query_tokens(Tokens, [], Codes, []),
	combine_voids(Codes, Combined_Codes).


convert_query_tokens([], _) -->
	[].

convert_query_tokens([Token|Tokens], Rs0) -->
	convert_query_token(Token, Rs0, Rs1),
	convert_query_tokens(Tokens, Rs1).


convert_query_token(Token, Rs, Rs, [Token|Tokens], Tokens) :-
	unchanged_query_token(Token),
	!.

convert_query_token(x_a(X, A), Rs, Rs) -->
	[put_value(X, A)],
	{
	    member(X, Rs),
	    !
	}.

convert_query_token(x_a(X, A), Rs, [X|Rs]) -->
	[put_variable(X, A)].

convert_query_token(s_x(S, X), Rs, [X|Rs]) -->
	[put_structure(S, X)].

convert_query_token(l_x(X), Rs, [X|Rs]) -->
	[put_list(X)].

convert_query_token(c_a(C, A), Rs, Rs) -->
	[put_constant(C, A)].

convert_query_token(i_a(I, A), Rs, Rs) -->
	[put_integer(I, A)].

convert_query_token(v_a(A), Rs, Rs) -->
	[put_void(1, A)].

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

convert_query_token(c(C), Rs, Rs) -->
	[set_constant(C)].

convert_query_token(i(I), Rs, Rs) -->
	[set_integer(I)].

convert_query_token(void, Rs, Rs) -->
	[set_void(1)].


unchanged_query_token(call(_)).
unchanged_query_token(>).
unchanged_query_token(<).
unchanged_query_token(=<).
unchanged_query_token(>=).
unchanged_query_token(=\=).
unchanged_query_token(=:=).
unchanged_query_token(is).
unchanged_query_token(true).
unchanged_query_token(fail).
unchanged_query_token(=).
unchanged_query_token(digital_input).
unchanged_query_token(digital_output).
unchanged_query_token(digital_input_pullup).
unchanged_query_token(digital_input_pulldown).
unchanged_query_token(digital_read).
unchanged_query_token(digital_write).
unchanged_query_token(analog_input).
unchanged_query_token(configure_channel).
unchanged_query_token(analog_output).
unchanged_query_token(analog_read).
unchanged_query_token(analog_write).
unchanged_query_token(line_sensor).
unchanged_query_token(millis).
unchanged_query_token(delay).

% --- Program ---

compile_program_ast(Definitions, Codes) :-
	compile_definitions_ast(Definitions, Codes0),
	combine_voids(Codes0, Codes1),
	last_call_optimisation(Codes1, Codes).


compile_definitions_ast([], []).

compile_definitions_ast([Definition|Definitions], Codes) :-
	compile_definition_ast(Definition, Codes, Definitions_Codes),
	compile_definitions_ast(Definitions, Definitions_Codes).


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
	allocate_permanent_variables(Terms, Goals, Permanent_Variables, Already_Declared_Permanent_Variables, Trimmed_Variables),
	allocate_atom_registers(Terms, Permanent_Variables, Terms_Allocation),
	allocate_goals_registers(Goals, Permanent_Variables, Goals_Allocation),
	tokenize_rule_allocation(Terms_Allocation, Goals_Allocation, Permanent_Variables, Already_Declared_Permanent_Variables, Trimmed_Variables, Tokens, []),
	convert_program_tokens(Tokens, [], Codes, [end_of_rule|Codes_Tail]).


convert_program_tokens([], _) -->
	[].

convert_program_tokens([Token|Tokens], Rs0) -->
	convert_program_token(Token, Rs0, Rs1),
	convert_program_tokens(Tokens, Rs1).


convert_program_token(Token, Rs, Rs, [Token|Codes], Codes) :-
	unchanged_program_token(Token),
	!.

convert_program_token(x_a(X, A), Rs, Rs) -->
	[get_value(X, A)],
	{
	    member(X, Rs),
	    !
	}.

convert_program_token(x_a(X, A), Rs, [X|Rs]) -->
	[get_variable(X, A)].

convert_program_token(s_x(S, X), Rs, Rs) -->
	[get_structure(S, X)].

convert_program_token(l_x(X), Rs, Rs) -->
	[get_list(X)].

convert_program_token(c_a(C, A), Rs, Rs) -->
	[get_constant(C, A)].

convert_program_token(i_a(I, A), Rs, Rs) -->
	[get_integer(I, A)].

convert_program_token(v_a(a(_)), Rs, Rs) -->
	[].

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

convert_program_token(c(C), Rs, Rs) -->
	[unify_constant(C)].

convert_program_token(i(I), Rs, Rs) -->
	[unify_integer(I)].

convert_program_token(void, Rs, Rs) -->
	[unify_void(1)].

convert_program_token(trim(0), Rs, Rs) -->
	!,
	[].

convert_program_token(trim(N), Rs, Rs) -->
	!,
	[trim(N)].

convert_program_token(goal(Tokens, Already_Declared_Permanent_Variables), Rs, Rs) -->
	convert_query_tokens(Tokens, Already_Declared_Permanent_Variables).


unchanged_program_token(allocate(_)).
unchanged_program_token(deallocate).
unchanged_program_token(proceed).
unchanged_program_token(neck_cut).
unchanged_program_token(get_level(_)).
unchanged_program_token(cut(_)).


disjunction_retry(Functor/Arity, retry(Functor)/Arity) -->
	[].
