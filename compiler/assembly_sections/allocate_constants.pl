
:- module(allocate_constants, [
	      init_constants_state/1,       % -State
	      allocate_constants/4,         % +Codes, -Mapped_Codes, +Current_State, -Final_State
	      constant_allocation/2         % +State, -Allocation
	  ]).


init_constants_state(State) :-
	state(State, 0, []).


allocate_constants(Codes, Mapped_Codes, State0, State) :-
	codes_constants(Codes, Constants, []),
	add_constants(Constants, State0, State),
	map_codes(Codes, Mapped_Codes, State).


constant_allocation(State, Allocation) :-
	state(State, _, Allocation).


codes_constants([]) -->
	[].

codes_constants([Code|Codes]) -->
	code_constants(Code),
	codes_constants(Codes).


code_constants(put_constant(C, _)) -->
	!,
	[C].

code_constants(get_constant(C, _)) -->
	!,
	[C].

code_constants(set_constant(C)) -->
	!,
	[C].

code_constants(unify_constant(C)) -->
	!,
	[C].

code_constants(_) -->
	[].


add_constants([], State, State).

add_constants([Constant|Constants], State0, State) :-
	add_constant(Constant, State0, State1),
	add_constants(Constants, State1, State).


add_constant(Constant, State, State) :-
	state(State, _, Allocation),
	member(Constant-_, Allocation),
	!.

add_constant(Constant, Current_State, New_State) :-
	state(Current_State, Current_New_ID, Allocation),
	New_New_ID is Current_New_ID + 1,
	state(New_State, New_New_ID, [Constant-Current_New_ID|Allocation]).


map_codes([], [], _State).

map_codes([Code|Codes], [Mapped_Code| Mapped_Codes], State) :-
	map_code(Code, Mapped_Code, State),
	map_codes(Codes, Mapped_Codes, State).


map_code(put_constant(C, Ai), put_constant(ID, Ai), State) :-
	!,
	constant_id(C, ID, State).

map_code(get_constant(C, Ai), get_constant(ID, Ai), State) :-
	!,
	constant_id(C, ID, State).

map_code(set_constant(C), set_constant(ID), State) :-
	!,
	constant_id(C, ID, State).

map_code(unify_constant(C), unify_constant(ID), State) :-
	!,
	constant_id(C, ID, State).

map_code(Code, Code, _State).


constant_id(Functor, ID, State) :-
	state(State, _, Allocation),
	member(Functor-ID, Allocation),
	!.


state(constants(Next_ID, Allocation), Next_ID, Allocation).
