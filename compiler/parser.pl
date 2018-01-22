:- module(parser, [
	      clauses//1,	% -Atoms
	      query//1		% -Query
	  ]).

clauses([]) -->
	[].

clauses([Clause|Clauses]) -->
	program_clause(Clause),
	spaces,
	!,
	clauses(Clauses).


program_clause(Fact) -->
	fact(Fact).

program_clause(Rule) -->
	rule(Rule).


rule(rule(head(Functor, Terms), [Goal|Goals])) -->
	structure(s(Functor, Terms)),
	spaces,
	":-",
	spaces,
	goal(Goal),
	spaces,
	goals(Goals).


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


fact(fact(Functor, Terms)) -->
	structure(s(Functor, Terms)),
	spaces,
	":-",
	spaces,
	".".


fact(fact(Functor, Terms)) -->
	structure(s(Functor, Terms)),
	spaces,
	".".


query(query(Functor, Terms)) -->
	structure(s(Functor, Terms)),
	spaces,
	".".


term(S) -->
	structure(S),
	!.

term(V) -->
	variable(V).


structure(s(Functor, Terms)) -->
	structure_functor(Functor),
	spaces,
	structure_bracket_terms(Terms).


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
	!,
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
