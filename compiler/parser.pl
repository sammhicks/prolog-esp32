:- module(parser, [
	      term//1,        % -Term
	      structure//1,   % -Structure
	      atom//1,	      % -Atom
	      variable//1     % -Variable
	  ]).


term(S) -->
	structure(S),
	!.

term(V) -->
	variable(V).


structure(s(Functor, Terms)) -->
	atom(a(Functor)),
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


atom(a(A)) -->
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
