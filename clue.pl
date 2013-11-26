:- use_module(clueui).
:- use_module(cards).
:- use_module(clueui).


:- dynamic player/2.
:- dynamic suggestion/3.
:- dynamic probability/3.

startGame :- initializeCards, chooseHeroPlayer, chooseOpponentPlayers(2), inputSuspectCards(1), inputWeaponCards(1), inputRoomCards(1), !, normalGameMenu.


initialPlayers(Players) :- Players = ['Miss Scarlet', 'Colonel Mustard', 'Mrs White', 'Mr Green', 'Mrs Peacock', 'Professor Plum'].


chooseHeroPlayer :- initialPlayers(Players), ui:printOut('Choose Your Player', Players, Input),
                    assert(player(1, Input)).

%%%%%
chooseOpponentPlayers(N) :- getRemainingPlayers(Remaining), 
                            ui:printOut('Choose Opponent Players', 
                            ['Done'|Remaining], Input), 
                            !,
                           ((not(isDone(Input)), 
                             assert(player(N, Input)), 
                             N1 is N + 1, 
                             chooseOpponentPlayers(N1)) ; 
                            isDone('Done')).

isDone('Done').

getRemainingPlayers(RemainingSuspects) :- initialPlayers(Initial), filter(Initial, RemainingSuspects).
getPlayersName(PlayersName) :- findall(P, player(X, P), PlayersName).
getPlayersN(PlayersN) :- findall(X, player(X, P), PlayersN).

filter([], []).
filter([H|T], [H|T2]) :- not(player(P, H)), filter(T, T2).
filter([H|T], Result) :- filter(T, Result).
%%%%%


inputSuspectCards(N) :- getRemainingSuspects(Suspects), 
                        ui:printOut('Input Seen Suspect Card', ['Done'|Suspects], Input), 
                        !, 
                        
                        ((not(isDone(Input)), 
                          inputCard(N, Input), 
                          inputSuspectCards(N)) ; 
                         isDone('Done')).

inputWeaponCards(N) :- getRemainingWeapons(Suspects), 
                       ui:printOut('Input Seen Weapon Card', ['Done'|Suspects], Input), 
                       !, 
                       
                       ((not(isDone(Input)), 
                         inputCard(N, Input), 
                         inputWeaponCards(N)) ; 
                       isDone('Done')).

inputRoomCards(N) :- getRemainingRooms(Suspects), 
                    ui:printOut('Input Seen Room Card', ['Done'|Suspects], Input), 
                    !, 
                    
                    ((not(isDone(Input)), 
                      inputCard(N, Input), 
                      inputRoomCards(N)) ; 
                     isDone('Done')).



inputCard(N, Input) :- getPlayersN(Players), inputCard(N, Input, Players).

inputCard(N, Input, []).
inputCard(N, Input, [N|T]) :- playerHasCard(N, Input), inputCard(N, Input, T).
inputCard(N, Input, [H|T]) :- playerNotHasCard(H, Input), inputCard(N, Input, T).



%%%%%%%%

normalGameMenuList(List) :- List = ['I\'m making a suggestion', 
                                    'Another player is making a suggestion', 
                                    'Show me seen cards', 
                                    'Show me what\'s left', 
                                    'Should I make an accusation?', 
                                    'Exit Game'].

normalGameMenu :- normalGameMenuList(List), 
                  ui:printOut('Choose an action', List, Input), 
                  !, 
                  normalGameChoose(Input), 
                  normalGameMenu.


isNobody('Nobody').


%% record which cards we asked. 
normalGameChoose('I\'m making a suggestion') :- initialSuspects(Suspects), initialWeapons(Weapons), initialRooms(Rooms),
                                                ui:printOut('Which suspect?', Suspects, SuspectInput),
                                                ui:printOut('Which Weapon?', Weapons, WeaponInput),
                                                ui:printOut('Which Room?', Rooms, RoomInput),
                                                getPlayersName(Players),
                                                ui:printOut('Who showed you a card?', ['Nobody'|Players], Shower), 
                                                !,
                                                ((not(isNobody(ShowedCard)), 
                                                  ui:printOut('Which card?', [SuspectInput, WeaponInput, RoomInput], Card),
                                                  player(N, Shower), 
                                                  inputCard(N, Card)) ; 
                                                isNobody(Shower)).

normalGameChoose('Show me seen cards') :- findall([X,P], cards:hasCard(X, P), Cards), ui:printList(Cards).

normalGameChoose('Show me what\'s left') :- getRemainingSuspects(Suspects), getRemainingWeapons(Weapons), getRemainingRooms(Rooms),
                                            ui:sOut('Suspects:'), ui:printList(Suspects),
                                            ui:sOut('Weapons:'), ui:printList(Weapons),
                                            ui:sOut('Rooms:'), ui:printList(Rooms).

normalGameChoose('Should I make an accusation?') :- getRemainingSuspects(Suspects), getRemainingWeapons(Weapons), getRemainingRooms(Rooms),
                                                    length(Suspects,Slen), length(Weapons,Wlen), length(Rooms,Rlen), !,
													((Slen =:= 1, 
													  Wlen =:= 1, 
													  Rlen =:= 1, 
													  ui:sOut('Yes - make suggestion:'),
													  ui:printList(Suspects),
													  ui:printList(Weapons),
													  ui:printList(Rooms)) ; 
													(ui:sOut('Not yet'))).


normalGameChoose('Another player is making a suggestion') :- getPlayersName([H|Players]),
                                                             ui:printOut('Which player?', Players, AskingPlayer),
                                                             initialSuspects(Suspects), initialWeapons(Weapons), initialRooms(Rooms),
                                                             ui:printOut('Which suspect?', Suspects, SuspectInput),
                                                             ui:printOut('Which Weapon?', Weapons, WeaponInput),
                                                             ui:printOut('Which Room?', Rooms, RoomInput),
                                                             ui:printOut('Who showed a card?', ['Nobody',H|Players], Shower), 
                                                             !,
                                                             ((not(isNobody(Shower)), inferenceShowedCard(AskingPlayer, Shower, SuspectInput, WeaponInput, RoomInput)) ;
                                                              (isNobody(Shower), inferenceNobodyShowed(AskingPlayer,SuspectInput,WeaponInput,RoomInput))).
                                                        


normalGameChoose('Exit Game') :- false.


%%%%%%%%%%%% Inferences        
                                                
% Nobody showed, so none of remaining players has those cards
inferenceNobodyShowed(Player, Suspect, Weapon, Room) :- player(N, Player), 
                                                        getPlayersN(Players),
                                                        allNotHasCardsBut(N, Players, Suspect, Weapon, Room).
                                                        
                                       
% P2 showed P1 a card
% - find players between P1 and P2 - they do not have those cards
% - eliminate any of cards that we know P2 doesn't have
% - if list is 1 card, P2 has that card 
%       - add only if we don't already know that
% - if list is 2-3 cards, ... (no effect right now)
inferenceShowedCard(P1, P2, Suspect, Weapon, Room) :- player(PN1, P1),
                                                      player(PN2, P2),
                                                      playersBetweenDoNotHaveCards(PN1, PN2, Suspect, Weapon, Room),
                                                      removeDoesNotHave(PN2, [Suspect, Weapon, Room], Cards),
                                                      !,
                                                      Cards = [H|T],
                                                      length(Cards, CardLength),
                                                      ((CardLength =:= 1, 
                                                                (cards:hasCard(PN2,H) ; inputCard(PN2, H))) ;
                                                       (true)).
                                                       

allNotHasCardsBut(N, [], S, W, R).
allNotHasCardsBut(N, [N|T], S, W, R) :- !, allNotHasCardsBut(N, T, S, W, R).
allNotHasCardsBut(N, [H|T], S, W, R) :- !,
                                       (cards:noHasCard(H,S) ; playerNotHasCard(H,S)),
                                       (cards:noHasCard(H,W) ; playerNotHasCard(H,W)),
                                       (cards:noHasCard(H,R) ; playerNotHasCard(H,R)),
                                       allNotHasCardsBut(N, T, S, W, R).                                                       
                                                       
                                                       
playersBetweenDoNotHaveCards(PN1, PN2, S, W, R) :- playersBetween(PN1, PN2, [PN1|Between]),
          										   allNotHasCardsBut(PN1, Between, S, W, R).
          										 
playersBetween(N1, N2, PNBetween) :- getPlayersN(AllN),
                                     playersStartAtN(N1, AllN, StartN1),
                                     playersBeforeN(N2, StartN1, PNBetween).
                              
playersStartAtN(N, List, [N|T1]) :- !, append(L1, L2, List), append(L2, L1, [N|T1]).

playersBeforeN(N, [N|T], []).
playersBeforeN(N, [X|T1], [X|T2]) :- !, playersBeforeN(N, T1, T2).


removeDoesNotHave(P, [], []).
removeDoesNotHave(P, [H|T1], [H|T2]) :- not(cards:noHasCard(P,H)), !, removeDoesNotHave(P, T1, T2).
removeDoesNotHave(P, [H|T1], List) :- removeDoesNotHave(P, T1, List).



