
:- module(queue, [
	      new_queue/1,      % -Queue
	      queue_empty/1,    % +Queue
	      queue_pop/3,      % -Item, +Old_Queue, -New_Queue
	      queue_push/3      % +Item, +Old_Queue, -New_Queue
	  ]).

%!	new_queue(-Queue) is det
%
%	Creates a new queue
new_queue(Items-Items).


%!	queue_empty(+Queue) is det
%
%	Succeeds if Queue is empty
queue_empty(List-Tail) :-
	List == Tail.


%!	queue_pop(-Item, +Old_Queue, -New_Queue) is det.
%
%	Pops Item from the from of Old_Queue, leaving New_Queue
queue_pop(Item, List-Tail, New_List-New_Tail) :-
	not(queue_empty(List-Tail)),
	List = [Item|New_List],
	Tail = New_Tail.


%!	queue_push(+Item, +Old_Queue, -New_Queue) is det.
%
%	Push Item to the back of Old_Queue, leading to New_Queue
queue_push(Item, List-[Item|Tail], List-Tail).
