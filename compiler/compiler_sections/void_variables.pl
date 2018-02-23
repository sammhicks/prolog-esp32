
:- module(void_variables, [
	      combine_voids/2
	  ]).


combine_voids([], []).

combine_voids([put_void(N1, a(Ai1)), put_void(N2, a(Ai2))|Codes], Combined_Codes) :-
	Ai2 is Ai1 + N1,
	!,
	N is N1 + N2,
	combine_voids([put_void(N, a(Ai1))|Codes], Combined_Codes).

combine_voids([set_void(N1), set_void(N2)|Codes], Combined_Codes) :-
	!,
	N is N1 + N2,
	combine_voids([set_void(N)|Codes], Combined_Codes).

combine_voids([unify_void(N1), unify_void(N2)|Codes], Combined_Codes) :-
	!,
	N is N1 + N2,
	combine_voids([unify_void(N)|Codes], Combined_Codes).

combine_voids([Code|Codes], [Code|Combined_Codes]) :-
	combine_voids(Codes, Combined_Codes).
