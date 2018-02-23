
:- module(read_solution, [
	      read_solution/4   % +Stream, +Query, Constants, Structures
	  ]).

:- use_module(value).
:- use_module(microcontroller_io).


read_solution(Stream, Query, _Constants, _Structures) :-
	atom(Query),
	!,
	get_byte(Stream, 0).

read_solution(Stream, Query, Constants, Structures) :-
	setup_state(Empty_State, Constants, Structures),
	get_byte(Stream, Arity),
	length(Arguments, Arity),
	fetch_registers(Arguments, Stream, Empty_State, Initial_State),
	fetch_compounds(Stream, Initial_State, _),
	compound_name_arguments(Query, _, Arguments).


setup_state(State, Constants, Structures) :-
	state(State, Constants, Structures, [], []).


fetch_registers([], _Stream, State, State).

fetch_registers([Value|Values], Stream) -->
	fetch_register(Value, Stream),
	fetch_registers(Values, Stream).


fetch_register(Value, Stream, Current_State, New_State) :-
	get_byte_block(Stream, Bytes),
	value(Wrapped_Value, Bytes, []),
	unwrap_value(Wrapped_Value, Value, Current_State, New_State).


fetch_compounds(_Stream, State, State) :-
	state(State, _Constants, _Structures, [], _Discovered_Values),
	!.

fetch_compounds(Stream) -->
	pop_item_to_read(Compound),
	!,
	fetch_compound(Compound, Stream),
	fetch_compounds(Stream).


fetch_compound(structure(H, Value), _Stream, State, State) :-
	state(State, _Constants, _Structures, _Items_To_Read, Discovered_Values),
	memberchk(structure(H, Value), Discovered_Values),
	!.

fetch_compound(structure(H, Value), Stream, State0, State) :-
	state(State0, _Constants, Structures, _Items_To_Read, _Discovered_Values),
	fetch_structure(Stream, H, ID, Wrapped_Arguments),
	memberchk(Name-ID, Structures),
	unwrap_values(Wrapped_Arguments, Arguments, State0, State1),
	compound_name_arguments(Value, Name, Arguments),
	push_discovered_item(structure(H, Value), State1, State).

fetch_compound(list(H, Value), _Stream, State, State) :-
	state(State, _Constants, _Structures, _Items_To_Read, Discovered_Values),
	memberchk(list(H, Value), Discovered_Values),
	!.

fetch_compound(list(H, Value), Stream, State0, State) :-
	state(State0, _Constants, _Structures, _Items_To_Read, _Discovered_Values),
	fetch_list(Stream, H, Wrapped_Arguments),
	unwrap_values(Wrapped_Arguments, [Head, Tail], State0, State1),
	Value = [Head|Tail],
	push_discovered_item(structure(H, Value), State1, State).


unwrap_values([], [], State, State).

unwrap_values([Wrapped_Value|Wrapped_Values], [Value|Values]) -->
	unwrap_value(Wrapped_Value, Value),
	unwrap_values(Wrapped_Values, Values).


unwrap_value(reference(H), Value, State, State) :-
	state(State, _, _, _, Discovered_Values),
	memberchk(H->Value, Discovered_Values),
	!.

unwrap_value(reference(H), Value, Current_State, New_State) :-
	state(Current_State, Constants, Structures, Items_To_Read, Discovered_Values),
	state(New_State, Constants, Structures, Items_To_Read, [(H->Value)|Discovered_Values]).

unwrap_value(structure(H), Value) -->
	push_item_to_read(structure(H, Value)).

unwrap_value(list(H), Value) -->
	push_item_to_read(list(H, Value)).

unwrap_value(constant(ID), C, State, State) :-
	state(State, Constants, _, _, _),
	memberchk(C-ID, Constants).

unwrap_value(integer(I), I, State, State).


pop_item_to_read(Item_To_Read, Current_State, New_State) :-
	state(Current_State, Constants, Structures, [Item_To_Read|Items_To_Read], Discovered_Values),
	state(New_State, Constants, Structures, Items_To_Read, Discovered_Values).


push_item_to_read(Item_To_Read, Current_State, New_State) :-
	state(Current_State, Constants, Structures, Items_To_Read, Discovered_Values),
	state(New_State, Constants, Structures, [Item_To_Read|Items_To_Read], Discovered_Values).


push_discovered_item(Discovered_Value, Current_State, New_State) :-
	state(Current_State, Constants, Structures, Items_To_Read, Discovered_Values),
	state(New_State, Constants, Structures, Items_To_Read, [Discovered_Value|Discovered_Values]).



state(state(Constants, Structures, Items_To_Read, Discovered_Values), Constants, Structures, Items_To_Read, Discovered_Values).


