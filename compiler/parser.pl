:- module(parser, [
	      term//1,        % -Term
	      structure//1,   % -Structure
	      atom//1,	      % -Atom
	      variable//1     % -Variable
	  ]).


term(S) -->
	structure(S).

term(V) -->
	variable(V).


structure(s(Name, [])) -->
	atom(a(Name)).


structure(s(Name, [])) -->
	atom(a(Name)),
	spaces,
	"(",
	spaces,
	")".

structure(s(Name, [Term|Terms])) -->
	atom(a(Name)),
	spaces,
	"(",
	spaces,
	term(Term),
	comma_terms(Terms),
	spaces,
	")".


atom(a(A)) -->
	identifier(prolog_atom_start, A).


variable(v(V)) -->
	identifier(prolog_var_start, V).


identifier(Type, Identifier) -->
	type(H, Type),
	types(T, prolog_identifier_continue),
	{
	    atom_codes(Identifier, [H|T])
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
