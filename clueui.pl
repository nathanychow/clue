:- module(ui, []).


printOut(Header, Options, Input) :- write(Header), nl, printOptions(Options, 1), read(N), operateOnN(N, Options, Input).



operateOnN(N, Options, Input) :- (number(N), findN(N, Options, Input)).

% assume N is <= size of options
findN(1, [H|T], Input) :- Input = H.
findN(N, [H|T], Input) :- N1 is N - 1, findN(N1, T, Input).

printOptions([], N).
printOptions([H|T], N) :- write(N), write(': '), write(H), nl, X is N + 1, printOptions(T, X).

printList([]).
printList([H|T]) :- write(H), nl, printList(T).

sOut(Header) :- nl, write(Header), nl, write('------------------------'), nl.