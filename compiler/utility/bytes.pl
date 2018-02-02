
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

bytes(0, _N) -->
	!.

bytes(Count, N, [Digit|Digits], Tail) :-
	Digit is (N >> (8 * (Count - 1))) /\ 0xFF,
	New_Count is Count - 1,
	bytes(New_Count, N, Digits, Tail).
