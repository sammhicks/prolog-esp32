:- module(parser, [
	      read_options/1,   % -Options
	      program/2,        % +Stream, -Definitions
	      query/3           % +Term, +Options, -Query
	  ]).

:- use_module(parser_sections/program_joiner).

read_options([backquoted_string(false), double_quotes(codes), singletons(_), variables(_), variable_names(_), term_position(_)]).


program(Stream, Definitions) :-
	read_terms(Stream, Terms, Options),
	program_clauses(Terms, Options, Clauses),
	join_program(Clauses, Definitions).


query(Term, Options, query(Functor, Arguments)) :-
	structure(Term, Options, s(Functor, Arguments)).


read_terms(Stream, Terms, Options) :-
	read_options(Current_Options),
	read_term(Stream, Current_Term, Current_Options),
	read_terms_tail(Stream, Current_Term, Current_Options, Terms, Options).


read_terms_tail(Stream, Current_Term, Current_Options, Terms, Options) :-
	read_options(New_Options),
	read_term(Stream, New_Term, New_Options),
	(   Current_Options == New_Options
	->  Terms = [],
	    Options = []
	;   Terms = [Current_Term|Remaining_Terms],
	    Options = [Current_Options|Remaining_Options],
	    read_terms_tail(Stream, New_Term, New_Options, Remaining_Terms, Remaining_Options)
	).


program_clauses([], [], []).

program_clauses([Term|Terms], [Options|Options_Tail], [Clause|Clauses]) :-
	program_clause(Term, Options, Clause),
	program_clauses(Terms, Options_Tail, Clauses).


program_clause(Head_Term :- Body_Term, Options, rule(head(Functor, Arguments), Body)) :-
	!,
	fact(Head_Term, Options, fact(Functor, Arguments)),
	goals(Body_Term, Options, Body).

program_clause(Term, Options, Fact) :-
	fact(Term, Options, Fact).


fact(Term, Options, fact(Functor, Arguments)) :-
	structure(Term, Options, s(Functor, Arguments)).


goals((Head_Term, Tail_Term), Options, [Head|Tail]) :-
	!,
	goal(Head_Term, Options, Head),
	goals(Tail_Term, Options, Tail).

goals(Term, Options, [Goal]) :-
	goal(Term, Options, Goal).


goal(Term, Options, goal(Functor, Arguments)) :-
	structure(Term, Options, s(Functor, Arguments)).


compound_arguments([], _, []).

compound_arguments([Term|Terms], Options, [Argument|Arguments]) :-
	compound_argument(Term, Options, Argument),
	compound_arguments(Terms, Options, Arguments).


compound_argument(Term, Options, Argument) :-
	variable(Term, Options, Argument),
	!.

compound_argument(Term, Options, Argument) :-
	constant(Term, Options, Argument),
	!.

compound_argument(Term, Options, Argument) :-
	integer(Term, Options, Argument),
	!.

compound_argument(Term, Options, Argument) :-
	list(Term, Options, Argument),
	!.

compound_argument(Term, Options, Argument) :-
	structure(Term, Options, Argument),
	!.


variable(Term, Options, v(Name)) :-
	var(Term),
	lookup_variable(Options, Term, Name).


constant([], _Options, c([])).

constant(Term, _Options, c(Term)) :-
	atom(Term).


integer(Term, _Options, i(Term)) :-
	integer(Term).


list([Head|Tail], Options, l(Head_Argument, Tail_Argument)) :-
	compound_argument(Head, Options, Head_Argument),
	compound_argument(Tail, Options, Tail_Argument).


structure(Term, _Options, s(Term/0, [])) :-
	atom(Term).

structure(Term, Options, s(Functor/Arity, Arguments)) :-
	compound_name_arity(Term, Functor, Arity),
	compound_name_arguments(Term, Functor, Arguments_Terms),
	compound_arguments(Arguments_Terms, Options, Arguments).


lookup_variable(Options, Variable, Name) :-
	memberchk(variable_names(Names), Options),
	member(Name=Current_Variable, Names),
	Variable==Current_Variable,
	!.

