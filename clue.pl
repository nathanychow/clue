:- use_module(cards).


:- dynamic player/2.

startGame :- initializeCards, chooseHeroPlayer.


initialPlayers(Players) :- Players = ['Miss Scarlet', 'Colonel Mustard', 'Mrs White', 'Mr Green', 'Mrs Peacock', 'Professor Plum'].


chooseHeroPlayer :- initialPlayers(Players), ui:printOut('Choose Your Player', Players, Input),
                    assert(player(1, Input)).



getRemainingPlayers(RemainingSuspects) :- initialPlayers(Initial), filter(Initial, RemainingSuspects).

filter([], []).
filter([H|T], [H|T2]) :- not(player(P, H)),
filter(T, T2).
filter([H|T], Result) :- filter(T, Result).