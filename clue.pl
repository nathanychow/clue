:- use_module(clueui).
:- use_module(cards).
:- use_module(clueui).


:- dynamic player/2.
:- dynamic suggestion/3.
:- dynamic probability/3.

startGame :- initializeCards, chooseHeroPlayer, chooseOpponentPlayers(2), inputSuspectCards(1), inputWeaponCards(1), inputRoomCards(1), normalGameMenu.


initialPlayers(Players) :- Players = ['Miss Scarlet', 'Colonel Mustard', 'Mrs White', 'Mr Green', 'Mrs Peacock', 'Professor Plum'].


chooseHeroPlayer :- initialPlayers(Players), ui:printOut('Choose Your Player', Players, Input),
                    assert(player(1, Input)).

%%%%%
chooseOpponentPlayers(N) :- getRemainingPlayers(Remaining), ui:printOut('Choose Opponent Players', ['Done'|Remaining], Input), !,
    ((not(isDone(Input)), assert(player(N, Input)), N1 is N + 1, chooseOpponentPlayers(N1)) ; isDone('Done')).

isDone('Done') :- true.

getRemainingPlayers(RemainingSuspects) :- initialPlayers(Initial), filter(Initial, RemainingSuspects).

filter([], []).
filter([H|T], [H|T2]) :- not(player(P, H)), filter(T, T2).
filter([H|T], Result) :- filter(T, Result).
%%%%%


inputSuspectCards(N) :- getRemainingSuspects(Suspects), ui:printOut('Input Seen Suspect Card', ['Done'|Suspects], Input), !, ((not(isDone(Input)), playerHasCard(N, Input), inputSuspectCards(N)) ; isDone('Done')).

inputWeaponCards(N) :- getRemainingWeapons(Suspects), ui:printOut('Input Seen Weapon Card', ['Done'|Suspects], Input), !, ((not(isDone(Input)), playerHasCard(N, Input), inputWeaponCards(N)) ; isDone('Done')).

inputRoomCards(N) :- getRemainingRooms(Suspects), ui:printOut('Input Seen Room Card', ['Done'|Suspects], Input), !, ((not(isDone(Input)), playerHasCard(N, Input), inputRoomCards(N)) ; isDone('Done')).




%%%%%%%%

normalGameMenuList(List) :- List = ['I\'m making a suggestion', 'Another player is making a suggestion', 'Show me seen cards', 'Show me what\'s left', 'Should I make an accusation?', 'Exit Game'].

normalGameMenu :- normalGameMenuList(List), ui:printOut('Choose an action', List, Input), !, normalGameChoose(Input), normalGameMenu.


isNobody('Nobody') :- true.


%% record which cards we asked. 
normalGameChoose('I\'m making a suggestion') :- initialSuspects(Suspects), initialWeapons(Weapons), initialRooms(Rooms),
                                                ui:printOut('Which suspect?', Suspects, SuspectInput),
                                                ui:printOut('Which Weapon?', Weapons, WeaponInput),
                                                ui:printOut('Which Room?', Rooms, RoomInput),
                                                findall(X, player(P, X), Players),
                                                ui:printOut('Who showed you a card?', ['Nobody'|Players], ShowedCard), !,
                                                ((not(isNobody(ShowedCard)), ui:printOut('Which card?', [SuspectInput, WeaponInput, RoomInput], Card),
                                                    player(N, ShowedCard), playerHasCard(N, Card)) ; isNobody(ShowedCard)).

normalGameChoose('Show me seen cards') :- findall([X,P], cards:hasCard(X, P), Cards), ui:printList(Cards).

normalGameChoose('Show me what\'s left') :- getRemainingSuspects(Suspects), getRemainingWeapons(Weapons), getRemainingRooms(Rooms),
                                            ui:sOut('Suspects:'), ui:printList(Suspects),
                                            ui:sOut('Weapons:'), ui:printList(Weapons),
                                            ui:sOut('Rooms:'), ui:printList(Rooms).

normalGameChoose('Should I make an accusation?') :- getRemainingSuspects(Suspects), getRemainingWeapons(Weapons), getRemainingRooms(Rooms),
                                                    length(Suspects,Slen), length(Weapons,Wlen), length(Rooms,Rlen), !,
((Slen =:= 1, Wlen =:= 1, Rlen =:= 1, ui:sOut('Yes - make suggestion:'),ui:printList(Suspects),ui:printList(Weapons),ui:printList(Rooms)) ; (ui:sOut('Not yet'))).


normalGameChoose('Another player is making a suggestion') :- findall(X, player(P, X), [H|Players]),
                                                                ui:printOut('Which player?', Players, AskingPlayer),
                                                                initialSuspects(Suspects), initialWeapons(Weapons), initialRooms(Rooms),
                                                                ui:printOut('Which suspect?', Suspects, SuspectInput),
                                                                ui:printOut('Which Weapon?', Weapons, WeaponInput),
                                                                ui:printOut('Which Room?', Rooms, RoomInput),
                                                                ui:printOut('Who showed a card?', ['Nobody',H|Players], ShowedCard), !,
                                                                ((not(isNobody(ShowedCard)), player(ShowedCardPlayer, ShowedCard), TempList = [],
                                                                (cards:hasCard(P, SuspectInput), not(ShowedCardPlayer =:= P), append([], TempList, TempList1) ; append([SuspectInput], TempList, TempList1)),
                                                                (cards:hasCard(P2, WeaponInput), not(ShowedCardPlayer =:= P2), append([], TempList1, TempList2) ; append([WeaponInput], TempList1, TempList2)),
                                                                (cards:hasCard(P3, RoomInput), not(ShowedCardPlayer =:= P3), append([], TempList2, FinalList) ; append([RoomInput], TempList2, FinalList)),
                                                                length(FinalList, FLength),
                    ((FLength =:= 1, [FinalCard] = FinalList, playerHasCard(ShowedCardPlayer, FinalCard)) ; (FLength > 0, player(N, AskingPlayer), player(N2, ShowedCard), assert(suggestion(N, FinalList, N2))) ; true))
                                                                ; isNobody(ShowedCard)).



normalGameChoose('Exit Game') :- false.
