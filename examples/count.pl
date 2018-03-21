
count_list(Count, List) :-
	count_list(Count, 1, List).


count_list(0, _Start, []) :-
	!.

count_list(Count, Start, [Start|Tail]) :-
	Count > 0,
	Next_Count is Count - 1,
	Next_Start is Start + 1,
	count_list(Next_Count, Next_Start, Tail).
