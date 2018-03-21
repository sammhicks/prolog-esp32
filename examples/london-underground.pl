

connected(bond_street, oxford_circus, central).
connected(oxford_circus, tottenham_court_road, central).
connected(bond_street, green_park, jubilee).
connected(green_park, charing_cross, jubilee).
connected(green_park, piccadilly_circus, piccadilly).
connected(piccadilly_circus, leicester_square, piccadilly).
connected(green_park, oxford_circus, victoria).
connected(oxford_circus, piccadilly_circus, bakerloo).
connected(piccadilly_circus, charing_cross, bakerloo).
connected(tottenham_court_road, leicester_square, northern).
connected(leicester_square, charing_cross, northern).


on_line(Station, Line) :-
	connected(Station, _, Line).

on_line(Station, Line) :-
	connected(_, Station, Line).


same_line(Station1, Station2, Line) :-
	on_line(Station1, Line),
	on_line(Station2, Line).


journey_to(From, To, Route) :-
	journey_to(From, To, Route, []).


journey_to(From, To, [(Line:(From-To))], Already_Visited) :-
	same_line(From, To, Line),
	not_member(Line, Already_Visited).

journey_to(From, To, [(Line:(From-Intermediate))|Route], Already_Visited) :-
	same_line(From, Intermediate, Line),
	not_member(Line, Already_Visited),
	journey_to(Intermediate, To, Route, [Line|Already_Visited]).


not_member(_Item, []) :-
	!.

not_member(Item, [Item|_]) :-
	!,
	fail.

not_member(Item, [_|Items]) :-
	not_member(Item, Items).
