:- module(parser, [
	      definitions//1,	% -Definitions
	      query//1		% -Query
	  ]).

definitions([]) -->
	[].

definitions([Definition|Definitions]) -->
	spaces,
	definition(Definition),
	spaces,
	definitions(Definitions).


definition(definition(Functor, [Clause|Clauses])) -->
	program_clause(Functor, Clause),
	spaces,
	definition_tail(Functor, Clauses).


definition_tail(Functor, [Clause|Clauses]) -->
	program_clause(Functor, Clause),
	!,
	spaces,
	definition_tail(Functor, Clauses).

definition_tail(_Functor, []) -->
	[].


program_clause(Functor, Fact) -->
	fact(Functor, Fact),
	!.

program_clause(Functor, Rule) -->
	rule(Functor, Rule).


fact(Functor, fact(Terms)) -->
	structure(s(Functor, Terms)),
	spaces,
	":-",
	spaces,
	".".


fact(Functor, fact(Terms)) -->
	structure(s(Functor, Terms)),
	spaces,
	".".


rule(Functor, rule(head(Terms), [Goal|Goals])) -->
	structure(s(Functor, Terms)),
	spaces,
	":-",
	spaces,
	goal(Goal),
	spaces,
	goals(Goals),
	!.


goals([]) -->
	".".

goals([Goal|Goals]) -->
	",",
	spaces,
	goal(Goal),
	spaces,
	goals(Goals).


goal(goal(Functor, Terms)) -->
	structure(s(Functor, Terms)).


query(query(Functor, Terms)) -->
	structure(s(Functor, Terms)),
	spaces,
	".".


term(C) -->
	constant(C).

term(S) -->
	structure(S).

term(L) -->
	list(L).

term(V) -->
	variable(V).

constant(c(Constant)) -->
	structure(s(Constant/0, [])).


structure(s(Functor/Arity, Terms)) -->
	structure_functor(Functor),
	spaces,
	structure_bracket_terms(Terms),
	{
	    length(Terms, Arity)
	}.


structure_bracket_terms(Terms) -->
	"(",
	spaces,
	structure_terms(Terms),
	spaces,
	")",
	!.

structure_bracket_terms([]) -->
	[].


structure_terms([Term|Terms]) -->
	term(Term),
	comma_terms(Terms).

structure_terms([]) -->
	[].


structure_functor(A) -->
	identifier(prolog_atom_start, A).


list(c([])) -->
	"[",
	spaces,
	"]",
	!.

list(l(Head, Tail)) -->
	"[",
	term(Head),
	spaces,
	list_tail(Tail),
	spaces,
	"]".


list_tail(c([])) -->
	[].

list_tail(l(Head, Tail)) -->
	",",
	spaces,
	term(Head),
	spaces,
	list_tail(Tail).

list_tail(Tail) -->
	"|",
	spaces,
	term(Tail).


variable(v(V)) -->
	identifier(prolog_var_start, V).


identifier(Type, Identifier) -->
	type(H, Type),
	types(T, prolog_identifier_continue),
	!,
	{
	    atom_codes(Identifier, [H|T])
	}.


identifier(Type, Identifier) -->
	type(H, Type),
	{
	    atom_codes(Identifier, [H])
	}.

comma_terms([Term|Terms]) -->
	spaces,
	",",
	spaces,
	term(Term),
	comma_terms(Terms).

comma_terms([]) -->
	"".


spaces -->
	types(_, space).


type(Char, Type, [Char|Rest], Rest) :-
	code_type(Char, Type).


types([Char|Chars], Type) -->
	type(Char, Type),
	!,
	types(Chars, Type).

types([], _) -->
	[].
