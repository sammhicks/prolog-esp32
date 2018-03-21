
:- module(allocate_structures, [
	      init_structures_state/1,      % -State
	      allocate_structures/4,        % +Codes, -Mapped_Codes, +Current_State, -Final_State
	      structure_allocation/2	    % +State, -Allocation
	  ]).

init_structures_state(State) :-
	state(Empty_State, 0, []),
	special_structures(Structures),
	add_structures(Structures, Empty_State, State).


allocate_structures(Codes, Mapped_Codes, State0, State) :-
	codes_structures(Codes, Structures, []),
	add_structures(Structures, State0, State),
	map_codes(Codes, Mapped_Codes, State).


structure_allocation(State, Allocation) :-
	state(State, _, Allocation).


codes_structures([]) -->
	[].

codes_structures([Code|Codes]) -->
	code_structures(Code),
	codes_structures(Codes).


code_structures(put_structure(F/_, _)) -->
	!,
	[F].

code_structures(get_structure(F/_, _)) -->
	!,
	[F].

code_structures(_) -->
	[].


add_structures([], State, State).

add_structures([Structure|Structures], State0, State) :-
	add_structure(Structure, State0, State1),
	add_structures(Structures, State1, State).


add_structure(Structure, State, State) :-
	state(State, _, Allocation),
	member(Structure-_, Allocation),
	!.

add_structure(Structure, Current_State, New_State) :-
	state(Current_State, Current_New_ID, Allocation),
	New_New_ID is Current_New_ID + 1,
	state(New_State, New_New_ID, [Structure-Current_New_ID|Allocation]).


map_codes([], [], _State).

map_codes([Code|Codes], [Mapped_Code| Mapped_Codes], State) :-
	map_code(Code, Mapped_Code, State),
	map_codes(Codes, Mapped_Codes, State).


map_code(put_structure(F/A, Ai), put_structure(ID/A, Ai), State) :-
	!,
	structure_id(F, ID, State).

map_code(get_structure(F/A, Ai), get_structure(ID/A, Ai), State) :-
	!,
	structure_id(F, ID, State).

map_code(Code, Code, _State).


structure_id(Functor, ID, State) :-
	state(State, _, Allocation),
	member(Functor-ID, Allocation),
	!.


special_structures([
    +,
    -,
    *,
    //,
    min,
    max,
    clamp
]).


state(structures(Next_ID, Allocation), Next_ID, Allocation).



