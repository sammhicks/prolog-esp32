
:- module(queue, [
	      new_queue/1,      %
	      queue_empty/1,    %
	      queue_pop/3,      %
	      queue_push/3      %
	  ]).


new_queue(Items-Items).


queue_empty(List-Tail) :-
	List == Tail.


queue_pop(Item, List-Tail, New_List-New_Tail) :-
	not(queue_empty(List-Tail)),
	List = [Item|New_List],
	Tail = New_Tail.


queue_push(Item, List-[Item|Tail], List-Tail).
