:- dynamic human/2.
:- dynamic orc/2.
:- dynamic array/1.
:- dynamic array_len/1.
:- dynamic r_s_way/2.
:- dynamic pass_possible/1.
:- dynamic touchdown/2.
:- dynamic wall_1/1. 	
:- dynamic arr_h/1.
:- dynamic arr_h_l/1.

wall_2(0).

map_1(_):-
	retractall(touchdown(X,U)),
	retractall(wall_1(Y)),
	retractall(orc(W,E)),
	retractall(human(Z,S)),

	assert(touchdown(3,7)), /*coordinates of touchdown*/
	
	assert(wall_1(11)), /*upper walls*/
	
	assert(orc(1,3)),	/*coordinates of orcs*/
	assert(orc(1,6)),
	assert(orc(11,7)),
	assert(orc(10,9)),
	assert(orc(1,5)),
	assert(orc(4,5)),
	assert(orc(0,1)),
	
	assert(human(0,0)),	/*coordinates of humans*/
	assert(human(1,1)).

map_2(_):-
	retractall(touchdown(X,U)),
	retractall(wall_1(Y)),
	retractall(orc(W,E)),
	retractall(human(Z,S)),

	assert(touchdown(3,2)),
	
	assert(wall_1(4)),

	assert(orc(3,1)),
	assert(orc(2,3)),
	assert(orc(2,2)),
	
	assert(human(0,0)),	
	assert(human(0,3)).
	
	
map_3(_):-
	retractall(touchdown(X,U)),
	retractall(wall_1(Y)),
	retractall(orc(W,E)),
	retractall(human(Z,S)),
	
	assert(touchdown(4,7)), 
	
	assert(wall_1(8)), 
	
	assert(orc(1,3)),	
	assert(orc(1,6)),
	assert(orc(2,7)),
	assert(orc(4,8)),
	assert(orc(4,5)),
	assert(orc(0,1)),
	
	assert(human(0,0)),	
	assert(human(3,0)),	
	assert(human(1,7)).
	
	
map_4(_):-
	retractall(touchdown(X,U)),
	retractall(wall_1(Y)),
	retractall(orc(W,E)),
	retractall(human(Z,S)),
	
	assert(touchdown(5,7)), 
	
	assert(wall_1(8)), 
	
	assert(orc(4,8)),
	assert(orc(4,5)),
	assert(orc(3,5)),
	
	assert(human(0,0)),	
	assert(human(3,0)).
	
	
map_5(_):-
	retractall(touchdown(X,U)),
	retractall(wall_1(Y)),
	retractall(orc(W,E)),
	retractall(human(Z,S)),
	
	assert(touchdown(3,2)),
	
	assert(wall_1(4)),
	
	assert(orc(0,1)),
	assert(orc(0,2)),
	assert(orc(0,3)),
	assert(orc(0,4)),
	assert(orc(1,0)),
	assert(orc(2,0)),
	assert(orc(3,0)),
	assert(orc(4,0)),
	assert(orc(2,1)),
	assert(orc(2,2)),
	assert(orc(2,3)),
	assert(orc(3,1)),
	assert(orc(3,3)),
	assert(orc(4,1)),
	
	assert(human(0,0)),	
	assert(human(1,1)).
	
	
map_6(_):-
	retractall(touchdown(X,U)),
	retractall(wall_1(Y)),
	retractall(orc(W,E)),
	retractall(human(Z,S)),
	
	assert(touchdown(3,2)),
	
	assert(wall_1(4)),
	
	assert(orc(3,1)),
	
	assert(human(0,0)).
	
	
	
is_safe(X,Y):-		/*check if there is no orc in the yard*/
	\+orc(X,Y).
	
win([X,Y]):-		/*check if yard is a touchdown*/
	touchdown(X,Y).
	
	
/*shows first found solution of backtracking algorithm, 
the shortest path among all the found in backtracking and one solution of random search*/	
main(_):-
	
	nb_setval(array_len,100000), /*further will compare length of array with it*/
	
	writeln(" ______________________________________________________________________________________________"),
	writeln("|                                                                                              |"),
	writeln("| Run 'map_i(_).' (where i is from 1 to 6) to create a map.                                    |"),
	writeln("|                                                                                              |"),
	writeln("| Run 'call_backtracking_search_first([0,0],[X,Y]).' to see first way found with backtracking. |"),
	writeln("| Run 'see_all_ways([0,0],[X,Y]).' to see all ways found with backtracking.                    |"),
	writeln("| Run 'call_search_shortest([0,0],[X,Y]).' to see shortest way found with backtracking.        |"),
	writeln("| Run 'call_random_search([0,0],[X,Y],0).' to see first way found with random search.          |"),
	writeln("| Run 'check_300_random([0,0],[X,Y],0).' to see 300 ways found with random search.             |"),
	writeln("| Run 'call_heuristic([0,0],[X,Y]).' to see way found with heuristic search.                   |"),
	writeln("|______________________________________________________________________________________________|").
	
		
	
		
call_backtracking_search_first(Start,Finish):-
	nb_setval(array_len,1000000),
	writeln("Backtracking first found path:"),
	statistics(runtime,[Start_t|_]), /*to count time*/
	(\+backtracking_search_first(Start,Finish) -> statistics(runtime,[Stop|_]),Runtime is Stop - Start_t,writeln("No possible way"),write(Runtime), writeln("ms"),nl; 
		(statistics(runtime,[Stop|_]),
		Runtime is Stop - Start_t,
		nb_getval(array, T), show_answer(T), nl, write("Number of steps: "), nb_getval(array_len, L), writeln(L),
				write(Runtime), writeln("ms"),
		nl)).
		
call_search_shortest(Start,Finish):-
	nb_setval(array_len,100000),
	statistics(runtime,[Start_t2|_]),
	(\+backtracking_search_shortest(Start,Finish) -> 
		statistics(runtime,[Stop2|_]),
		Runtime2 is Stop2 - Start_t2,
		(writeln("Backtracking shortest path:"), nb_getval(array, Te), show_answer(Te), nl, write(Runtime2), writeln("ms"),
		nl, write("Number of steps: "), nb_getval(array_len, Le), writeln(Le))),
	
	nl.
	
	
call_random_search(Start, Finish, Cycle):-
	writeln("Random search:"),
	statistics(runtime,[Start_t3|_]),
	(\+random_search(Start, Finish, Cycle) -> statistics(runtime,[Stop3|_]),
		Runtime3 is Stop3 - Start_t3,
		write(Runtime3), writeln("ms")).
	
	
call_heuristic(Start,Finish):-
	nb_setval(array_len,1000000),
	writeln("Heuristic path:"),
	statistics(runtime,[Start_t|_]), /*to count time*/
	(\+heuristic(Start,Finish) -> statistics(runtime,[Stop|_]),Runtime is Stop - Start_t,writeln("No possible way"),write(Runtime), writeln("ms"),nl; 
		(statistics(runtime,[Stop|_]),
		Runtime is Stop - Start_t,
		nb_getval(arr_h, T), show_answer(T), nl, write("Number of steps: "), nb_getval(arr_h_l, L), writeln(L),
				write(Runtime), writeln("ms"),
		nl)).	

	
	
backtracking(Start,Finish):-
	nb_setval(pass_possible, 0), /*allows to use pass*/
    dpth([Start],Finish,Way),
    find_shortest(Way).
	
	
/*shows all the solutions of backtracking*/
see_all_ways(Start,Finish):-
	nb_setval(pass_possible,0),
	statistics(runtime,[Start_t|_]),
	dpth([Start],Finish,Way),
	statistics(runtime,[Stop|_]),
	Runtime is Stop - Start_t,
	show_answer(Way),
	nl,
	write(Runtime), writeln("ms"),
	fail.
	
	
/*shows first found way in backtracking search*/
backtracking_search_first(Start,Finish):-
	backtracking(Start,Finish) -> true.

	
/*goes though the whole tree of solutions of backtracking and every iteration remember only the shortest path*/	
backtracking_search_shortest(Start,Finished):-
	backtracking(Start,Finish),
	fail.


find_shortest(Way):-
		length(Way, L),
		L1 is L-1,
		nb_getval(array_len,L2),
		L1<L2 -> (nb_setval(array, Way), nb_setval(array_len,L1)); true.		
		
prolong([Temp|Tail],[New,Temp|Tail]):-
	((left(Temp, [X,Y]), win([X,Y]));(left(Temp, [X,Y]),left([X,Y],[X2,Y2]), win([X2,Y2])) -> left(Temp, New);		/*if player is near the touchdown go to it*/
		((right(Temp, [X,Y]), win([X,Y]));(right(Temp, [X,Y]),right([X,Y],[X2,Y2]), win([X2,Y2])) -> right(Temp, New);
			((back(Temp, [X,Y]), win([X,Y]));(back(Temp, [X,Y]),back([X,Y],[X2,Y2]), win([X2,Y2])) -> back(Temp, New);
				((forward(Temp, [X,Y]), win([X,Y]));(forward(Temp, [X,Y]),forward([X,Y],[X2,Y2]), win([X2,Y2])) -> forward(Temp, New);
					(move(Temp,New)))))),
    \+member(New,[Temp|Tail]).
    
/*finish searching if touchdown is found*/
dpth([Finish|Tail],Finish,[Finish|Tail]):-
	win(Finish).


/*go in depth, remember way*/
dpth(TempWay,Finish,Way):-
    prolong(TempWay,NewWay), 
    dpth(NewWay,Finish,Way).
    
show_answer([_]):-!.

show_answer([A,B|Tail]):-
    show_answer([B|Tail]),
    nl,
    write(B),
    write(" -> "),
    write(A).
    
    
    
bdth([[Finish|Tail]|_],Finish,[Finish|Tail]):-
	win(Finish).
	
bdth([TempWay|OtherWays],Finish,Way):-
    findall(W,prolong(TempWay,W),Ways),
    append(OtherWays,Ways,NewWays),
    bdth(NewWays,Finish,Way).


search_bdth(Start,Finish):-
	nb_setval(pass_possible, 0),
    bdth([[Start]],Finish,Way),
    length(Way, L1),
    L2 is L1 - 1,
    nb_setval(arr_h_l,L2),
    nb_setval(arr_h,Way).

heuristic(Start, Finish):-
	search_bdth(Start, Finish) -> true.


	
random_search(Start, Finish, Cycle):-
	nb_setval(pass_possible, 0),
	(Cycle \= 100 -> true; (writeln("Number of tries is over"), false)),

	Cycle2 is Cycle + 1,
	random(0,12,R), 

	(R = 2 -> (left(Start,Finish) -> (write(Start), write("->"), writeln(Finish)); (writeln("Game over"),false));
	(R = 0 -> (right(Start,Finish)-> (write(Start), write("->"),writeln(Finish)); (writeln("Game over"),false));
	(R = 1 -> (forward(Start,Finish)-> (write(Start), write("->"),writeln(Finish)); (writeln("Game over"),false));
	(R = 3 -> (back(Start,Finish)-> (write(Start), write("->"),writeln(Finish)); (writeln("Game over"),false));
		(R = 4 -> (pass_right(Start,Finish)-> (write(Start), write("->"),writeln(Finish)); (writeln("Game over"),false));
		(R = 5 -> (pass_left(Start,Finish)-> (write(Start), write("->"),writeln(Finish)); (writeln("Game over"),false));
		(R = 6 -> (pass_back(Start,Finish)-> (write(Start), write("->"),writeln(Finish)); (writeln("Game over"),false));
		(R = 7 -> (pass_forward(Start,Finish)-> (write(Start), write("->"),writeln(Finish)); (writeln("Game over"),false));
			(R = 8 -> (pass_right_up_diag(Start,Finish)-> (write(Start), write("->"),writeln(Finish)); (writeln("Game over"),false));
			(R = 9 -> (pass_right_down_diag(Start,Finish)-> (write(Start), write("->"),writeln(Finish)); (writeln("Game over"),false));
			(R = 10 -> (pass_left_up_diag(Start,Finish)-> (write(Start), write("->"),writeln(Finish)); (writeln("Game over"),false));
			(R = 11 -> (pass_left_down_diag(Start,Finish)-> (write(Start), write("->"),writeln(Finish)); (writeln("Game over"),false)))))))))))))),
	(win(Finish) -> (writeln("You won the game!"), false);
		random_search(Finish, [X2,Y2],Cycle2)).
		
		
/*show solutions of random search for 300 trials*/
check_300_random(Start, Finish, Cycle):-
	statistics(runtime,[Start_t|_]),
	Cycle\=300,
	\+random_search(Start, Finish, Cycle),
	nl,
	statistics(runtime,[Stop|_]),
	Runtime is Stop - Start_t,
	write(Runtime), writeln("ms"),
	C2 is Cycle + 1,
	check_300_random(Start, Finish, C2).
				
	
			
pass_right([X,Y], [X2,Y2]):-
	right([X,Y], [X3,Y3]) -> (human(X3, Y3) -> (X2 is X3, Y2 is Y3); pass_right([X3, Y3], [X2,Y2])); false.
	
pass_left([X,Y], [X2,Y2]):-
	left([X,Y], [X3,Y3]) -> (human(X3, Y3) -> (X2 is X3, Y2 is Y3); pass_left([X3, Y3], [X2,Y2])); false.

pass_forward([X,Y], [X2,Y2]):-
	forward([X,Y], [X3,Y3]) -> (human(X3, Y3) -> (X2 is X3, Y2 is Y3); pass_forward([X3, Y3], [X2,Y2])); false.
	
pass_back([X,Y], [X2,Y2]):-
	back([X,Y], [X3,Y3]) -> (human(X3, Y3) -> (X2 is X3, Y2 is Y3); pass_back([X3, Y3], [X2,Y2])); false.
	
pass_right_up_diag([X,Y], [X2,Y2]):-
	(\+wall_1(X),	/*check if it is not in the corner*/
	\+wall_1(Y),
	X3 is X + 1,
	Y3 is Y + 1,
	is_safe(X3,Y3)) -> (human(X3, Y3) -> (X2 is X3, Y2 is Y3); pass_right_up_diag([X3, Y3], [X2,Y2])); false.
	
pass_right_down_diag([X,Y], [X2,Y2]):-
	(\+wall_1(X),
	\+wall_2(Y),
	X3 is X + 1,
	Y3 is Y - 1,
	is_safe(X3,Y3)) -> (human(X3, Y3) -> (X2 is X3, Y2 is Y3); pass_right_down_diag([X3, Y3], [X2,Y2])); false.
	
pass_left_up_diag([X,Y], [X2,Y2]):-
	(\+wall_2(X),
	\+wall_1(Y),
	X3 is X - 1,
	Y3 is Y + 1,
	is_safe(X3,Y3)) -> (human(X3, Y3) -> (X2 is X3, Y2 is Y3); pass_left_up_diag([X3, Y3], [X2,Y2])); false.
	
pass_left_down_diag([X,Y], [X2,Y2]):-
	(\+wall_2(X),
	\+wall_2(Y),
	X3 is X - 1,
	Y3 is Y - 1,
	is_safe(X3,Y3)) -> (human(X3, Y3) -> (X2 is X3, Y2 is Y3); pass_left_down_diag([X3, Y3], [X2,Y2])); false.

left([X,Y], [X2,Y2]):-
	\+wall_2(X),
	X2 is X-1,
	Y2 is Y,	
	is_safe(X2,Y2).

right([X,Y], [X2,Y2]):-
	\+wall_1(X),
	X2 is X+1,
	Y2 is Y,	
	is_safe(X2,Y2).

forward([X,Y], [X2,Y2]):-
	\+wall_1(Y),
	X2 is X,
	Y2 is Y+1,	
	is_safe(X2,Y2).
	
back([X,Y], [X2,Y2]):-
	\+wall_2(Y),
	X2 is X,
	Y2 is Y-1,	
	is_safe(X2,Y2).
	
move([X,Y], [X2,Y2]):-
	(nb_getval(pass_possible, T), T = 0) -> 
		(pass_right_up_diag([X,Y], [X2,Y2]), nb_setval(pass_possible, 1));false.
	
move([X,Y], [X2,Y2]):-
	(nb_getval(pass_possible, T), T = 0) -> 
		(pass_right_down_diag([X,Y], [X2,Y2]), nb_setval(pass_possible, 1));false.
	
move([X,Y], [X2,Y2]):-
	(nb_getval(pass_possible, T), T = 0) -> (pass_left_up_diag([X,Y], [X2,Y2]), nb_setval(pass_possible, 1));false.
		
move([X,Y], [X2,Y2]):-
	(nb_getval(pass_possible, T), T = 0) -> (pass_left_down_diag([X,Y], [X2,Y2]), nb_setval(pass_possible, 1));false.
	
move([X,Y], [X2,Y2]):-
	(nb_getval(pass_possible, T), T = 0) -> (pass_right([X,Y], [X2,Y2]), retractall(pass_possible), nb_setval(pass_possible, 1));false.
	
move([X,Y], [X2,Y2]):-
	(nb_getval(pass_possible, T), T = 0) -> (pass_left([X,Y], [X2,Y2]), retractall(pass_possible), nb_setval(pass_possible, 1));false.
	
move([X,Y], [X2,Y2]):-
	(nb_getval(pass_possible, T), T = 0) -> (pass_forward([X,Y], [X2,Y2]), retractall(pass_possible), nb_setval(pass_possible, 1));false.
	
move([X,Y], [X2,Y2]):-
	(nb_getval(pass_possible, T), T = 0) -> (pass_back([X,Y], [X2,Y2]), retractall(pass_possible), nb_setval(pass_possible, 1));false.
	
	
move([X,Y], [X2,Y2]):-
	left([X,Y], [X2,Y2]).
	
move([X,Y], [X2,Y2]):-
	right([X,Y], [X2,Y2]).
	
move([X,Y], [X2,Y2]):-
	forward([X,Y], [X2,Y2]).
	
move([X,Y], [X2,Y2]):-
	back([X,Y], [X2,Y2]).
	




