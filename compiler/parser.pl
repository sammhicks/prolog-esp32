:- module(parser, [
	      program/2,        % +Terms, -Definitions
	      query/2           % +Term, -Query
	  ]).

:- use_module(parser_sections/program_joiner).

program(Terms, Definitions) :-
	program_clauses(Terms, Clauses),
	!,
	join_program(Clauses, Definitions).

program(Terms, _) :-
	throw(not_a_program(Terms)).


query(Term, query(Functor, Arguments)) :-
	copy_term(Term, Copy),
	structure(Copy, s(Functor, Arguments)),
	numbervar_options(Options),
	numbervars(Functor, 0, End, Options),
	numbervars(Arguments, End, _, Options).


program_clauses([], []).

program_clauses([Term|Terms], [Clause|Clauses]) :-
	copy_term(Term, Copy),
	program_clause(Copy, Clause),
	numbervar_options(Options),
	numbervars(Copy, 0, _, Options),
	program_clauses(Terms, Clauses).


program_clause(Head_Term :- Body_Term, rule(head(Functor, Arguments), Body)) :-
	!,
	fact(Head_Term, fact(Functor, Arguments)),
	goals(Body_Term, Body).

program_clause(Term, Fact) :-
	fact(Term, Fact).


fact(Term, fact(Functor, Arguments)) :-
	structure(Term, s(Functor, Arguments)).


goals((Head_Term, Tail_Term), [Head|Tail]) :-
	!,
	goal(Head_Term, Head),
	goals(Tail_Term, Tail).

goals(Term, [Goal]) :-
	goal(Term, Goal).


goal(!, cut) :-
	!.

goal(Term, goal(Functor, Arguments)) :-
	structure(Term, s(Functor, Arguments)).


compound_arguments([], []).

compound_arguments([Term|Terms], [Argument|Arguments]) :-
	compound_argument(Term, Argument),
	compound_arguments(Terms, Arguments).


compound_argument(Term, Argument) :-
	variable(Term, Argument),
	!.

compound_argument(Term, Argument) :-
	constant(Term, Argument),
	!.

compound_argument(Term, Argument) :-
	integer(Term, Argument),
	!.

compound_argument(Term, Argument) :-
	list(Term, Argument),
	!.

compound_argument(Term, Argument) :-
	structure(Term, Argument),
	!.

compound_argument(Term, _) :-
	throw(not_supported_in_source(Term)).


variable(Term, Term) :-
	var(Term).


constant([], c([])).

constant(Term, c(Term)) :-
	atom(Term).


integer(Term, i(Term)) :-
	integer(Term).


list([Head|Tail], l(Head_Argument, Tail_Argument)) :-
	compound_argument(Head, Head_Argument),
	compound_argument(Tail, Tail_Argument).


structure(Term, s(Term/0, [])) :-
	atom(Term),
	!.

structure(Term, s(Functor/Arity, Arguments)) :-
	compound(Term),
	compound_name_arity(Term, Functor, Arity),
	compound_name_arguments(Term, Functor, Arguments_Terms),
	compound_arguments(Arguments_Terms, Arguments).


numbervar_options([functor_name(v), singletons(true)]).

