
:- module(bytes, [
	      uint8//1,	    % +N
	      uint16//1,    % +N
	      uint32//1     % +N
	  ]).

uint8(N) -->
	bytes(1, N).


uint16(N) -->
	bytes(2, N).


uint32(N) -->
	bytes(4, N).


bytes(Count, N, Codes, Tail) :-
	var(N),
	!,
	read_bytes(Count, N, Codes, Tail).

bytes(Count, N, Codes, Tail) :-
	integer(N),
	write_bytes(Count, N, Codes, Tail).


read_bytes(0, 0) -->
	!.

read_bytes(Count, N, [Digit|Digits], Tail) :-
	New_Count is Count - 1,
	read_bytes(New_Count, Major, Digits, Tail),
	N is 0xFF * Major + Digit.


write_bytes(0, _N) -->
	!.

write_bytes(Count, N, [Digit|Digits], Tail) :-
	divmod(N, 0xFF, New_N, Digit),
	New_Count is Count - 1,
	write_bytes(New_Count, New_N, Digits, Tail).
