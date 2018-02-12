
:- module(terminal, [
	      ping/0
	  ]).

:- use_module(library(socket)).
:- use_module(library(random)).

command(ping, 0).

ping :-
	tcp_connect(ip(192,168,167,1):1, Stream, []),
	command(ping, Header),
	random_between(0, 255, Body),
	format("~w\n", [Body]),
	put_bytes([Header,Body], Stream),
	flush_output(Stream),
	get_bytes([Response], Stream),
	(   Body = Response
	->  format("success\n")
	;   format("failure - ~w\n", [Body])),
	close(Stream).


put_bytes([], Stream) :-
	flush_output(Stream).

put_bytes([Code|Codes], Stream) :-
	put_byte(Stream, Code),
	put_bytes(Codes, Stream).


get_bytes([], _Stream).

get_bytes([Code|Codes], Stream) :-
	get_byte(Stream, Code),
	get_bytes(Codes, Stream).
