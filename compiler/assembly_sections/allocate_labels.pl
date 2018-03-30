
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
	[L-_].

code_labels(_) -->
	[].


map_codes([], [], _Labels).

map_codes([Code|Codes], [Mapped_Code| Mapped_Codes], Labels) :-
	map_code(Code, Mapped_Code, Labels),
	map_codes(Codes, Mapped_Codes, Labels).


map_code(Code, Mapped_Code, Labels) :-
	has_map(Code, Mapped_Code, Labels),
	!.

map_code(Code, Code, _Labels).


has_map(call(F/A), call(PC, A), Labels) :-
	lookup_label(F/A, PC, Labels).

has_map(execute(F/A), execute(PC, A), Labels) :-
	lookup_label(F/A, PC, Labels).

has_map(try_me_else(L), try_me_else(PC), Labels) :-
	lookup_label(L, PC, Labels).

has_map(retry_me_else(L), retry_me_else(PC), Labels) :-
	lookup_label(L, PC, Labels).


lookup_label(Label, PC, Labels) :-
	memberchk(Label-PC, Labels),
	!.

lookup_label(Label, _PC, _Labels) :-
	throw(no_pc_for_label(Label)).
