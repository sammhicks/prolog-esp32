
:- module(read_solution, [
	      read_solution/4   % +Stream, +Query, Constants, Structures
	  ]).

:- use_module(library(debug)).

:- use_module(microcontroller_io).
:- use_module(queue).
:- use_module(value).


read_solution(Stream, Query, Constants, Structures) :-
	atom(Query),
	!,
	compound_name_arity(Compound, Query, 0),
	read_solution(Stream, Compound, Constants, Structures).

read_solution(Stream, Query, Constants, Structures) :-
	compound(Query),
	setup_state(Empty_State, Constants, Structures),
	value(Registers, Stream, Stream),
	unwrap_value(Registers, Arguments, Empty_State, Initial_State),
	compound_name_arity(Query, Name, _),
	compound_name_arguments(Working_Query, Name, Arguments),
	fetch_values(Stream, Working_Query, Initial_State),
	debug(solution, "Full Solution -    ~w\n", [Working_Query]),
	Query = Working_Query.


setup_state(State, Constants, Structures) :-
	new_queue(Items_To_Read),
	state(State, Constants, Structures, Items_To_Read, []).


fetch_values(_Stream, _Working_Query, State) :-
	state(State, _, _, Items_To_Read, _),
	queue_empty(Items_To_Read),
	!.

fetch_values(Stream, Working_Query, State0) :-
	debug(solution, "Partial Solution - ~w\n", [Working_Query]),
	pop_value_to_read(Address, Value, State0, State1),
	fetch_value(Stream, Address, Wrapped_Value),
	unwrap_value(Wrapped_Value, Value, State1, State),
	fetch_values(Stream, Working_Query, State).


unwrap_value(reference(H), Value, State, State) :-
	state(State, _, _, _, Discovered_Values),
	memberchk(H->Value, Discovered_Values),
	!.

unwrap_value(reference(H), Value, Current_State, New_State) :-
	state(Current_State, Constants, Structures, Items_To_Read, Discovered_Values),
	state(New_State, Constants, Structures, Items_To_Read, [(H->Value)|Discovered_Values]).

unwrap_value(structure(ID, Subterm_Addresses), Structure, Current_State, New_State) :-
	state(Current_State, _, Structures, _, _),
	memberchk(Functor-ID, Structures),
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
	state(Current_State, Constants, Structures, Items_To_Read, Discovered_Values),
	queue_pop(Address_Value, Items_To_Read, Remaining_Items_To_Read),
	address_value(Address, Value, Address_Value),
	state(New_State, Constants, Structures, Remaining_Items_To_Read, Discovered_Values).


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
	queue_push(Address_Value, Items_To_Read, New_Items_To_Read),
	state(New_State, Constants, Structures, New_Items_To_Read, [Address_Value|Discovered_Values]).


address_value(Address, Value, Address->Value).

state(state(Constants, Structures, Items_To_Read, Discovered_Values), Constants, Structures, Items_To_Read, Discovered_Values).


