
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
	value(Registers, Stream, Stream),
	unwrap_value(Registers, Arguments, Empty_State, Initial_State),
	fetch_values(Stream, Initial_State),
	compound_name_arguments(Query, _, Arguments).


setup_state(State, Constants, Structures) :-
	state(State, Constants, Structures, [], []).


fetch_values(_Stream, State) :-
	state(State, _, _, [], _),
	!.

fetch_values(Stream, State0) :-
	pop_value_to_read(Address, Value, State0, State1),
	fetch_value(Stream, Address, Wrapped_Value),
	unwrap_value(Wrapped_Value, Value, State1, State),
	fetch_values(Stream, State).


unwrap_value(reference(H), Value, State, State) :-
	state(State, _, _, _, Discovered_Values),
	memberchk(H->Value, Discovered_Values),
	!.

unwrap_value(reference(H), Value, Current_State, New_State) :-
	state(Current_State, Constants, Structures, Items_To_Read, Discovered_Values),
	state(New_State, Constants, Structures, Items_To_Read, [(H->Value)|Discovered_Values]).

unwrap_value(structure(ID, Subterm_Addresses), Structure, Current_State, New_State) :-
	state(Current_State, _, Structures, _, _),
	nth0(ID, Structures, Functor),
	push_values_to_read(Subterm_Addresses, Arguments, Current_State, New_State),
	compound_name_arguments(Structure, Functor, Arguments).

unwrap_value(list(Head_Address, Tail_Address), [Head|Tail]) -->
	push_value_to_read(Head_Address, Head),
	push_value_to_read(Tail_Address, Tail).

unwrap_value(constant(ID), C, State, State) :-
	state(State, Constants, _, _, _),
	memberchk(C-ID, Constants).

unwrap_value(integer(I), I, State, State).

unwrap_value(environment(Addresses), Values) -->
	push_values_to_read(Addresses, Values).


pop_value_to_read(Address, Value, Current_State, New_State) :-
	state(Current_State, Constants, Structures, [Address_Value|Items_To_Read], Discovered_Values),
	address_value(Address, Value, Address_Value),
	state(New_State, Constants, Structures, Items_To_Read, Discovered_Values).


push_values_to_read([], [], State, State).

push_values_to_read([Address|Addresses], [Value|Values]) -->
	push_value_to_read(Address, Value),
	push_values_to_read(Addresses, Values).


push_value_to_read(Address, Value, State, State) :-
	state(State, _Constants, _Structures, _Items_To_Read, Discovered_Values),
	address_value(Address, Value, Address_Value),
	memberchk(Address_Value, Discovered_Values),
	!.

push_value_to_read(Address, Value, Current_State, New_State) :-
	state(Current_State, Constants, Structures, Items_To_Read, Discovered_Values),
	address_value(Address, Value, Address_Value),
	state(New_State, Constants, Structures, [Address_Value|Items_To_Read], [Address_Value|Discovered_Values]).


address_value(Address, Value, Address->Value).

state(state(Constants, Structures, Items_To_Read, Discovered_Values), Constants, Structures, Items_To_Read, Discovered_Values).


