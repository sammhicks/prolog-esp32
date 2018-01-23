
:- module(permanent_variables, [
	      allocate_permanent_variables/3	% -Head_Terms, -Goals, +Permanent_Variables
	  ]).

:- use_module(library(lists)).

:- use_module(variables).


allocate_permanent_variables(Head_Terms, Goals, Sorted_Permanent_Variables) :-
	items_variables(Head_Terms, Head_Variables, []),
	goals_variables(Goals, Goals_Variables),
	permanent_variables([Head_Variables|Goals_Variables], [], Permanent_Variables),
	sort(Permanent_Variables, Sorted_Permanent_Variables).


permanent_variables([Goal_Variables|Goals_Variables], Vs, Vs_Final) :-
	select(V, Goal_Variables, Remaining_Goal_Variables),
	append(Goals_Variables, Other_Variables),
	member(V, Other_Variables),
	!,
	permanent_variables([Remaining_Goal_Variables|Goals_Variables], [V|Vs], Vs_Final).


permanent_variables([_|Goals_Variables], Vs, Vs_Final) :-
	permanent_variables(Goals_Variables, Vs, Vs_Final).


permanent_variables([], Vs, Vs).
