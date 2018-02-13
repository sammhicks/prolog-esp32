
:- module(terminal, [
	      open_connection/0,
	      close_connection/0,
	      reset_connection/0,
	      ping/1
	  ]).

:- use_module(library(socket)).
:- use_module(library(random)).

:- dynamic(current_connection/1).

open_connection :-
	current_connection(_),
	!.

open_connection :-
	tcp_connect(ip(192,168,167,1):1, Stream, []),
	assertz(current_connection(Stream)).


close_connection :-
	retract(current_connection(Stream)),
	!,
	close(Stream),
	close_connection.

close_connection.


reset_connection :-
	close_connection,
	open_connection.


ping(Result) :-
	current_connection(Stream),
	command(ping, Header),
	random_between(0, 255, Body),
	put_bytes([Header, Body], Stream),
	get_bytes([Response], Stream),
	(   Body = Response
	->  Result = success
	;   Result = failure(Body, Response)).


command(ping, 0).





put_bytes([], Stream) :-
	flush_output(Stream).

put_bytes([Code|Codes], Stream) :-
	put_byte(Stream, Code),
	put_bytes(Codes, Stream).


get_bytes([], _Stream).

get_bytes([Code|Codes], Stream) :-
	get_byte(Stream, Code),
	get_bytes(Codes, Stream).
