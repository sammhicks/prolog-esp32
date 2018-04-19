
:- module(bytes, [
	      uint8//1,	    % +N
	      int8//1,      % +N
	      uint16//1,    % +N
	      int16//1,     % +N
	      uint32//1,    % +N
	      int32//1,     % +N
	      bytes//3	    % +Count, ?N, +Sign
	  ]).

uint8(N) -->
	bytes(1, N, unsigned).


int8(N) -->
	bytes(1, N, signed).


uint16(N) -->
	bytes(2, N, unsigned).


int16(N) -->
	bytes(2, N, signed).


uint32(N) -->
	bytes(4, N, unsigned).


int32(N) -->
	bytes(4, N, signed).


bytes(Count, N, Sign, Stream, Stream) :-
	var(N),
	is_stream(Stream),
	!,
	length(Bytes, Count),
	get_bytes(Bytes, Stream),
	read_bytes(Count, UN, Bytes, []),
	add_sign(Sign, Count, UN, N).

bytes(Count, N, Sign, Codes, Tail) :-
	var(N),
	var(Codes),
	!,
	length(N_Codes, Count),
	append(N_Codes, Tail, Codes),
	freeze(N, (remove_sign(Sign, Count, N, UN), write_bytes(Count, UN, N_Codes, []))).

bytes(Count, N, Sign, Codes, Tail) :-
	var(N),
	!,
	read_bytes(Count, UN, Codes, Tail),
	add_sign(Sign, Count, UN, N).

bytes(Count, N, Sign, Codes, Tail) :-
	integer(N),
	remove_sign(Sign, Count, N, UN),
	write_bytes(Count, UN, Codes, Tail).


get_bytes([], _Stream).

get_bytes([Code|Codes], Stream) :-
	get_byte(Stream, Code),
	get_bytes(Codes, Stream).


read_bytes(0, 0) -->
	!.

read_bytes(Count, N, [Digit|Digits], Tail) :-
	New_Count is Count - 1,
	read_bytes(New_Count, Major, Digits, Tail),
	N is 0x100 * Major + Digit.


write_bytes(0, _N) -->
	!.

write_bytes(Count, N, [Digit|Digits], Tail) :-
	divmod(N, 0x100, New_N, Digit),
	New_Count is Count - 1,
	write_bytes(New_Count, New_N, Digits, Tail).


add_sign(unsigned, _Count, N, N).

add_sign(signed, Count, N, N) :-
	signed_range(Count, Range),
	N < Range,
	!.

add_sign(signed, Count, UN, N) :-
	unsigned_range(Count, Range),
	N is UN - Range.


remove_sign(unsigned, Count, N, N) :-
	unsigned_range(Count, Range),
	(   N >= 0,
	    N < Range
	->  true
	;   throw(out_of_range(N, unsigned(Count)))
	).


remove_sign(signed, Count, N, N) :-
	N >= 0,
	!,
	signed_range(Count, Range),
	(   N < Range
	->  true
	;   throw(out_of_range(N, signed(Count)))
	).

remove_sign(signed, Count, N, UN) :-
	unsigned_range(Count, Range),
	UN is Range + N,
	(   UN >= 0
	->  true
	;   throw(out_of_range(N, signed(Count)))
	).


unsigned_range(ByteCount, Range) :-
	Range is 1 << (8 * ByteCount).


signed_range(ByteCount, Range) :-
	unsigned_range(ByteCount, UnsignedRange),
	Range is UnsignedRange >> 1.
