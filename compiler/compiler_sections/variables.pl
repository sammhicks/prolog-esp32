
:- module(variables, [
	      goals_variables/2,    % +Goals, -Variables
	      items_variables/2     % +Items, -Variables
	  ]).

:- use_module(library(ordsets)).

goals_variables([], []).

goals_variables([goal(_, Items)|Goals], [Item_Variables|Items_Variables]) :-
	items_variables(Items, Item_Variables),
	!,
	goals_variables(Goals, Items_Variables).


items_variables(Items, Variables) :-
	items_variables(Items, [], Variables).


items_variables([], Acc, Acc).


items_variables([Item|Items]) -->
	item_variables(Item),
	items_variables(Items).


item_variables(c(_)) -->
	[].

item_variables(s(_, Terms)) -->
	items_variables(Terms).

item_variables(l(Head, Tail)) -->
	items_variables([Head, Tail]).

item_variables(v(V), Acc, All) :-
	ord_add_element(Acc, v(V), All).
