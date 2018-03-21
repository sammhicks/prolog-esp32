
:- module(variables, [
	      goals_variables/2,    % +Goals, -Variables
	      items_variables/2     % +Items, -Variables
	  ]).

:- use_module(library(ordsets)).


goals_variables([cut|Goals], [[]|Goals_Variables]) :-
	!,
	goals_tail_variables(Goals, Goals_Variables).

goals_variables(Goals, Goals_Variables) :-
	goals_tail_variables(Goals, Goals_Variables).


goals_tail_variables([], []).

goals_tail_variables([Goal|Goals], [Goal_Variables|Goals_Variables]) :-
	goal_variables(Goal, Goal_Variables),
	goals_tail_variables(Goals, Goals_Variables).


goal_variables(goal(_, Items), Variables) :-
	items_variables(Items, Variables).

goal_variables(cut, [cut]).


items_variables(Items, Variables) :-
	items_variables(Items, [], Variables).


items_variables([], Acc, Acc).


items_variables([Item|Items]) -->
	item_variables(Item),
	items_variables(Items).


item_variables(v(V), Acc, All) :-
	ord_add_element(Acc, v(V), All).

item_variables(s(_, Terms)) -->
	items_variables(Terms).

item_variables(l(Head, Tail)) -->
	items_variables([Head, Tail]).

item_variables(c(_)) -->
	[].

item_variables(i(_)) -->
	[].


