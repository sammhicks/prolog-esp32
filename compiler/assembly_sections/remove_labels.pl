
:- module(remove_labels, [
	      remove_labels/4       % +Labels, -Label_Table, Bytes_With_Labels, Bytes
	  ]).


remove_labels(Bytes_With_Labels0, Bytes, Labels, Label_Table) :-
	nth0(PC, Bytes_With_Labels0, label(ID/Arity), Bytes_With_Labels1),
	!,
	nth0(ID, Label_Table, PC/Arity),
	remove_labels(Bytes_With_Labels1, Bytes, Labels, Label_Table).

remove_labels(Bytes, Bytes, _Labels, _Label_Table).
