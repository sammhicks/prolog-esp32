
:- module(allocate_functors, [
	      init_functors_state/1,        % -State
	      allocate_functors/4           % +Codes, -Mapped_Codes, +Current_State, -Final_State
	  ]).

:- use_module(library(ordsets)).

init_functors_state(State) :-
	special_functors(State).


allocate_functors(Codes, Mapped_Codes, State0, State) :-
	codes_functors(Codes, Functors, []),
	add_functors(Functors, State0, State),
	map_codes(Codes, Mapped_Codes, State).


codes_functors([], Functors, Functors).

codes_functors([Code|Codes]) -->
	code_functors(Code),
	codes_functors(Codes).


code_functors(Code, [Functor|Functors], Functors) :-
	has_functor(Code, Functor),
	!.

code_functors(_, Functors, Functors).


has_functor(put_structure(F/_, _), F).
has_functor(put_constant(C, _), C).
has_functor(get_structure(F/_, _), F).
has_functor(get_constant(C, _), C).
has_functor(set_constant(C), C).
has_functor(unify_constant(C), C).


add_functors([], State, State).

add_functors([Functor|Functors]) -->
	add_functor(Functor),
	add_functors(Functors).


add_functor(Functor, State, State) :-
	member(Functor, State),
	!.

add_functor(Functor, Current_State, New_State) :-
	append(Current_State, [Functor], New_State).


map_codes([], [], _State).

map_codes([Code|Codes], [Mapped_Code| Mapped_Codes], State) :-
	map_code(Code, Mapped_Code, State),
	map_codes(Codes, Mapped_Codes, State).


map_code(put_structure(F/A, Ai), put_structure(ID/A, Ai), State) :-
	!,
	functor_id(F, ID, State).

map_code(put_constant(C, Ai), put_constant(ID, Ai), State) :-
	!,
	functor_id(C, ID, State).

map_code(get_structure(F/A, Ai), get_structure(ID/A, Ai), State) :-
	!,
	functor_id(F, ID, State).

map_code(get_constant(C, Ai), get_constant(ID, Ai), State) :-
	!,
	functor_id(C, ID, State).

map_code(set_constant(C), set_constant(ID), State) :-
	!,
	functor_id(C, ID, State).

map_code(unify_constant(C), unify_constant(ID), State) :-
	!,
	functor_id(C, ID, State).


map_code(Code, Code, _State).


functor_id(Functor, ID, State) :-
	nth0(ID, State, Functor),
	!.


special_functors([
    +,
    -,
    *,
    //,
    min,
    max,
    clamp
]).



