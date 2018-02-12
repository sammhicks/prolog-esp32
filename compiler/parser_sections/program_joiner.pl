
:- module(program_joiner, [
	      join_program/2
	  ]).

join_program([], []).

join_program([Clause|Clauses], [Definition|Definitions]) :-
	disjunction([Clause|Clauses], Definition, Remaining_Clauses),
	!,
	join_program(Remaining_Clauses, Definitions).

join_program([Clause|Clauses], [Clause|Definitions]) :-
	join_program(Clauses, Definitions).


disjunction([Clause1,Clause2|Clauses], disjunction(Functor, [Head,Mid|Tail]), Remaining_Clauses) :-
	clause_functor(Clause1, Functor, Head),
	clause_functor(Clause2, Functor, Mid),
	disjunction_tail(Functor, Clauses, Tail, Remaining_Clauses).


disjunction_tail(Functor, [Clause|Clauses], [Head|Tail], Remaining_Clauses) :-
	clause_functor(Clause, Functor, Head),
	!,
	disjunction_tail(Functor, Clauses, Tail, Remaining_Clauses).

disjunction_tail(_Functor, Clauses, [], Clauses).


clause_functor(fact(Functor, Arguments), Functor, fact(Arguments)).

clause_functor(rule(head(Functor, Arguments), Goals), Functor, rule(head(Arguments), Goals)).
