:- dynamic suspect/1.
:- dynamic room/1.
:- dynamic weapon/1.
:- dynamic hasCard/2.

%public
initializeCards :- initialSuspects(Los),
                    initialWeapons(Low),
                    initialRooms(Lor),
                    initializeSuspects(Los),
                    initializeWeapons(Low),
                    initializeRooms(Lor).

initialSuspects(ListOfSuspects) :- ListOfSuspects = ['Miss Scarlet', 'Colonel Mustard', 'Mrs White', 'Mr Green', 'Mrs Peacock', 'Professor Plum'].

initialWeapons(ListOfWeapons) :- ListOfWeapons = ['Candlestick', 'Knife', 'Revolver', 'Rope', 'Lead Pipe', 'Wrench'].

initialRooms(ListOfRooms) :- ListOfRooms = ['Kitchen', 'Ballroom', 'Conservatory', 'Dining Room', 'Billiard Room', 'Library', 'Lounge', 'Hall', 'Study'].


initializeSuspects([]).
initializeSuspects([H|T]) :- assert(suspect(H)), initializeSuspects(T).


initializeWeapons([]).
initializeWeapons([H|T]) :- assert(weapon(H)), initializeWeapons(T).


initializeRooms([]).
initializeRooms([H|T]) :- assert(room(H)), initializeRooms(T).

%public 
playerHasCard(P, C) :- assert(hasCard(P, C)).


%public
getRemainingWeapons(Remaining) :- initialWeapons(Initial), filter(Initial, Remaining).

%public
getRemainingRooms(Remaining) :- initialRooms(Initial), filter(Initial, Remaining).

%public
getRemainingSuspects(RemainingSuspects) :- initialSuspects(Initial),
                                           filterSuspects(Initial, RemainingSuspects).


filter([], []).
filter([H|T], [H|T2]) :- not(hasCard(P, H)),
                                    filter(T, T2).
filter([H|T], Result) :- filter(T, Result).




