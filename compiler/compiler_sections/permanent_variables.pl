
:- module(permanent_variables, [
	      allocate_permanent_variables/5    % +Head_Terms, +Goals, -Permanent_Variables, -Already_Declared_Permanent_Variables, -Goals_Trimmed_Variables
	  ]).

:- use_module(library(lists)).
:- use_module(library(ordsets)).

:- use_module(variables).


allocate_permanent_variables(Head_Terms, Goals, Ordered_Permanent_Variables, Already_Declared_Permanent_Variables, Goals_Trimmed_Variables) :-
	items_variables(Head_Terms, Head_Variables),
	goals_variables(Goals, Goals_Variables),
	permanent_variables([Head_Variables|Goals_Variables], [], Permanent_Variables),
	filter_permanent_variables(Head_Variables, Permanent_Variables, Head_Permanent_Variables),
	goals_permanent_variables(Goals_Variables, Permanent_Variables, Goals_Permanent_Variables),
	order_permanent_variables(Goals_Permanent_Variables, Ordered_Permanent_Variables),
	already_declared_permanent_variables(Goals_Permanent_Variables, Head_Permanent_Variables, Already_Declared_Permanent_Variables),
	goals_trimmed_variables(Goals_Permanent_Variables, Goals_Trimmed_Variables) .


permanent_variables([Goal_Variables|Goals_Variables], Vs, Vs_Final) :-
	select(V, Goal_Variables, Remaining_Goal_Variables),
	append(Goals_Variables, Other_Variables),
	member(V, Other_Variables),
	!,
	ord_add_element(Vs, V, VAll),
	permanent_variables([Remaining_Goal_Variables|Goals_Variables], VAll, Vs_Final).


permanent_variables([_|Goals_Variables], Vs, Vs_Final) :-
	permanent_variables(Goals_Variables, Vs, Vs_Final).


permanent_variables([], Vs, Vs).


goals_permanent_variables([], _, []).

goals_permanent_variables([Goal_Variables|Goals_Variables], Permanent_Variables, [Goal_Permanent_Variables|Goals_Permanent_Variables]) :-
	filter_permanent_variables(Goal_Variables, Permanent_Variables, Goal_Permanent_Variables),
	goals_permanent_variables(Goals_Variables, Permanent_Variables, Goals_Permanent_Variables).


filter_permanent_variables([], _, []).

filter_permanent_variables([Variable|Variables], Permanent_Variables, [Variable|Filtered_Variables]) :-
	member(Variable, Permanent_Variables),
	!,
	filter_permanent_variables(Variables, Permanent_Variables, Filtered_Variables).

filter_permanent_variables([_|Variables], Permanent_Variables, Filtered_Variables) :-
	filter_permanent_variables(Variables, Permanent_Variables, Filtered_Variables).


order_permanent_variables(Goals_Permanent_Variables, Ordered_Permanent_Variables) :-
	reverse(Goals_Permanent_Variables, Reverse_Goals_Permanent_Variables),
	append(Reverse_Goals_Permanent_Variables, All_Permanent_Variables),
	list_to_set(All_Permanent_Variables, Ordered_Permanent_Variables).


already_declared_permanent_variables([], _, []).


already_declared_permanent_variables([Goal|Goals], Acc, [Acc|Rolling_Acc]) :-
	ord_union(Goal, Acc, New_Acc),
	already_declared_permanent_variables(Goals, New_Acc, Rolling_Acc).


goals_trimmed_variables([], []).

goals_trimmed_variables([Goal_Variables|Goals_Variables], [Goal_Trimmed_Variables|Goals_Trimmed_Variables]) :-
	goal_trimmed_variables(Goal_Variables, Goals_Variables, Goal_Trimmed_Variables),
	goals_trimmed_variables(Goals_Variables, Goals_Trimmed_Variables).


goal_trimmed_variables(Goal_Variables, Goals_Variables, Goal_Trimmed_Variables_Count) :-
	ord_union(Goals_Variables, Required_Variables),
	ord_subtract(Goal_Variables, Required_Variables, Goal_Trimmed_Variables),
	length(Goal_Trimmed_Variables, Goal_Trimmed_Variables_Count).
