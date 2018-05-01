
:- module(remove_labels, [
	      remove_labels/3       % +Bytes_With_Labels, -Bytes, +Labels
	  ]).


remove_labels(Bytes_With_Labels0, Bytes, Labels) :-
	nth0(PC, Bytes_With_Labels0, Label, Bytes_With_Labels1),
	nonvar(Label),
	Label = label(L),
	!,
	memberchk(L-PC, Labels),
	remove_labels(Bytes_With_Labels1, Bytes, Labels).

remove_labels(Bytes, Bytes, _Labels).
