
:- module(last_call_optimisation, [
	      last_call_optimisation/2
	  ]).

last_call_optimisation([], []).

last_call_optimisation([call(Functor), deallocate|Codes], [deallocate, execute(Functor)|Optimised_Codes]) :-
	!,
	last_call_optimisation(Codes, Optimised_Codes).

last_call_optimisation([Code|Codes], [Code|Optimised_Codes]) :-
	last_call_optimisation(Codes, Optimised_Codes).
