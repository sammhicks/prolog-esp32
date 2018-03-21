
:- module(read_terms, [
	      read_terms_from_file/2,
	      read_terms/2
	  ]).

read_terms_from_file(File, Terms) :-
	open(File, read, Stream),
	read_terms(Stream, Terms),
	close(Stream).


read_terms(Stream, Terms) :-
	read_options(Current_Options),
	read_term(Stream, Current_Term, Current_Options),
	read_terms_tail(Stream, Current_Term, Current_Options, Terms).


read_terms_tail(Stream, Current_Term, Current_Options, Terms) :-
	read_options(New_Options),
	read_term(Stream, New_Term, New_Options),
	(   Current_Options == New_Options
	->  Terms = []
	;   Terms = [Current_Term|Remaining_Terms],
	    read_terms_tail(Stream, New_Term, New_Options, Remaining_Terms)
	).


read_options([backquoted_string(false), double_quotes(codes), term_position(_)]).
