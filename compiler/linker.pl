
:- module(linker, [
	      link_calls/2  % +Unlinked_Codes, -Linked_Codes
	  ]).

link_calls(Unlinked_Codes, Linked_Codes) :-
	link_calls(Unlinked_Codes, Linked_Codes, 0, Labels),
	check_call_links(Labels).


link_calls([], [], _Line, _Labels).

link_calls([call(L)|Unlinked_Codes], [call(L, At)|Linked_Codes], Line, Labels) :-
	member(L=At, Labels),
	!,
	succ(Line, Next_Line),
	link_calls(Unlinked_Codes, Linked_Codes, Next_Line, Labels).

link_calls([try_me_else(L)|Unlinked_Codes], [try_me_else(L, At)|Linked_Codes], Line, Labels) :-
	member(L=At, Labels),
	!,
	succ(Line, Next_Line),
	link_calls(Unlinked_Codes, Linked_Codes, Next_Line, Labels).

link_calls([retry_me_else(L)|Unlinked_Codes], [retry_me_else(L, At)|Linked_Codes], Line, Labels) :-
	member(L=At, Labels),
	!,
	succ(Line, Next_Line),
	link_calls(Unlinked_Codes, Linked_Codes, Next_Line, Labels).

link_calls([label(Label)|Unlinked_Codes], Linked_Codes, Line, Labels) :-
	member(Label=line(Current_Line), Labels),
	(   ground(Current_Line)
	->  !,
	    print_message(error, repeated_label(Label)),
	    fail
	;   Line = Current_Line
	),
	!,
	link_calls(Unlinked_Codes, Linked_Codes, Line, Labels).

link_calls([Code|Unlinked_Codes], [Code|Linked_Codes], Line, Labels) :-
	!,
	succ(Line, Next_Line),
	link_calls(Unlinked_Codes, Linked_Codes, Next_Line, Labels).


% Failure driven loop to check for calls to undeclared atoms
check_call_links(Labels) :-
	member(Label=Label_Line, Labels),
	(   ground(Label)	    % If Label is defined
	->  (	ground(Label_Line)  %
	    ->	fail		    % Keep looking
	    ;	!,                  % We can stop searching as we've found a hanging call
		fail)
	;   !).			    % We've reached the end, and no errors found
