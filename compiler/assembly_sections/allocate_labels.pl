
:- module(allocate_labels, [
	      allocate_labels/2,                    % +Codes, -Labels
	      apply_labels/3                        % +Codes, +Labels, -Mapped_Codes
	  ]).

allocate_labels(Codes, Labels) :-
	codes_labels(Codes, Labels, []).


apply_labels(Codes, Labels, Mapped_Codes) :-
	map_codes(Codes, Mapped_Codes, Labels).


codes_labels([]) -->
	[].

codes_labels([Code|Codes]) -->
	code_labels(Code),
	codes_labels(Codes).


code_labels(label(L)) -->
	!,
	[L].

code_labels(_) -->
	[].


map_codes([], [], _Labels).

map_codes([Code|Codes], [Mapped_Code| Mapped_Codes], Labels) :-
	map_code(Code, Mapped_Code, Labels),
	map_codes(Codes, Mapped_Codes, Labels).


map_code(label(F/A), label(ID/A), Labels) :-
	nth0(ID, Labels, F/A),
	!.

map_code(call(L), call(ID), Labels) :-
	nth0(ID, Labels, L),
	!.

map_code(try_me_else(L), try_me_else(ID), Labels) :-
	nth0(ID, Labels, L),
	!.

map_code(retry_me_else(L), retry_me_else(ID), Labels) :-
	nth0(ID, Labels, L),
	!.


map_code(Code, Code, _Labels).
