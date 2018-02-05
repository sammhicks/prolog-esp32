
:- module(variables, [
	      goals_variables/2,    % +Goals, -Variables
	      items_variables//1,   % +Items
	      item_variables//1     % +Item
	  ]).

goals_variables([], []).

goals_variables([goal(_, Items)|Goals], [Item_Variables|Items_Variables]) :-
	items_variables(Items, Item_Variables, []),
	!,
	goals_variables(Goals, Items_Variables).


items_variables([]) -->
	[].

items_variables([Item|Items]) -->
	item_variables(Item),
	items_variables(Items).


item_variables(c(_)) -->
	[].

item_variables(s(_, Terms)) -->
	items_variables(Terms).

item_variables(l(Head, Tail)) -->
	items_variables([Head, Tail]).

item_variables(v(V)) -->
	[v(V)].
