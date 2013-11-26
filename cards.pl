:- module(cards, [initializeCards, playerHasCard/2, playerNotHasCard/2, getRemainingWeapons/1, 
                  getRemainingRooms/1, getRemainingSuspects/1, initialSuspects/1, 
                  initialWeapons/1, initialRooms/1]).

:- dynamic suspect/1.
:- dynamic room/1.
:- dynamic weapon/1.
:- dynamic hasCard/2.
:- dynamic noHasCard/2.

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
playerNotHasCard(P, C) :- assert(noHasCard(P,C)).

%public
getRemainingWeapons(Remaining) :- initialWeapons(Initial), filter(Initial, Remaining).
getRemainingRooms(Remaining) :- initialRooms(Initial), filter(Initial, Remaining).
getRemainingSuspects(Remaining) :- initialSuspects(Initial), filter(Initial, Remaining).


filter([], []).
filter([H|T], [H|T2]) :- not(hasCard(P, H)), filter(T, T2).
filter([H|T], Result) :- filter(T, Result).

