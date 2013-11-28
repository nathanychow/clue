:- use_module(clueui).
:- use_module(cards).
:- use_module(clueui).


:- dynamic player/2.     % player(number, name)
:- dynamic suggestion/3. % suggestion(asking, suggested, [suspect, weapon, room])


%%%%% INITIALIZATION


startGame :- initializeCards, 
			 chooseHeroPlayer, 
			 chooseOpponentPlayers(2), 
			 inputSuspectCards(1), 
			 inputWeaponCards(1), 
			 inputRoomCards(1), 
			 initializeDoNotHaveOtherCards,
			 !, 
			 normalGameMenu.


initialPlayers(Players) :- Players = ['Miss Scarlet', 'Colonel Mustard', 'Mrs White', 'Mr Green', 'Mrs Peacock', 'Professor Plum'].


chooseHeroPlayer :- initialPlayers(Players), 
                    ui:printOut('Choose Your Player', Players, Input),
                    (player(1, Input) ; assert(player(1, Input))).


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

getRemainingPlayers(RemainingSuspects) :- initialPlayers(Initial), 
                                          filterRemainingPlayers(Initial, RemainingSuspects).



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
                     
                     
initializeDoNotHaveOtherCards :- getRemainingSuspects(Suspects), doNotHaveTheseCards(1, Suspects),
								 getRemainingWeapons(Weapons), doNotHaveTheseCards(1, Weapons),
								 getRemainingRooms(Rooms), doNotHaveTheseCards(1, Rooms).


%%%%%% OTHER HELPERS
getPlayersName(PlayersName) :- findall(P, player(X, P), PlayersName).
getPlayersN(PlayersN) :- findall(X, player(X, P), PlayersN).
getSuggestions(Suggestions) :- findall([P1, P2, Cards], suggestion(P1, P2, Cards), Suggestions).

getAllCards(AllCards) :- initialSuspects(Suspects), initialWeapons(Weapons), initialRooms(Rooms),
						 append(Suspects, Weapons, TwoList),
						 append(TwoList,Rooms,AllCards).
totalCards(Tot) :- getAllCards(List),
				   length(List, Tot).
				   
cardsPerPlayer(N) :- totalCards(Tot),
					 getPlayersN(Players),
					 length(Players, PlayersNum),
					 N is Tot // PlayersNum.

filterRemainingPlayers([], []).
filterRemainingPlayers([H|T], [H|T2]) :- not(player(P, H)), filterRemainingPlayers(T, T2).
filterRemainingPlayers([H|T], Result) :- filterRemainingPlayers(T, Result).

% filters out a list of cards from given list of cards
filterOut([],T,T).
filterOut([C|T],AllCards,Result) :- filterCard(C,AllCards,AllCardsMinusOne), filterOut(T,AllCardsMinusOne,Result).

filterCard(C,[],[]).
filterCard(C,[C|T1],T2) :- filterCard(C, T1, T2).
filterCard(C,[X|T1],[X|T2]) :- filterCard(C, T1, T2).

%%%%% ADD hasCard or noHasCard

doHaveTheseCards(P, []).
doHaveTheseCards(P, [Card|T]) :- inputCard(P,Card), doHaveTheseCards(P,T).

doNotHaveTheseCards(P, []).
doNotHaveTheseCards(P, [Card|T]) :- addNotHasCard(P,Card), doNotHaveTheseCards(P,T).

inputCard(N, Input) :- getPlayersN(Players), inputCard(N, Input, Players).

inputCard(N, Input, []).
inputCard(N, Input, [N|T]) :- addHasCard(N,Input), inputCard(N, Input, T).
inputCard(N, Input, [H|T]) :- addNotHasCard(H,Input), inputCard(N, Input, T).

addHasCard(P, C) :- cards:hasCard(P,C) ; playerHasCard(P,C).
addNotHasCard(P,C) :- cards:noHasCard(P,C) ; playerNotHasCard(P,C).


%%%%%%%% NORMAL GAME PLAY


normalGameMenu :- !,
                  normalGameMenuList(List), 
                  ui:printOut('Choose an action', List, Input), 
                  normalGameChoose(Input), 
                  !,
                  normalGameMenu.

normalGameMenuList(List) :- List = ['I\'m making a suggestion', 
                                    'Another player is making a suggestion', 
                                    'Show me seen cards', 
                                    'Show me what\'s left', 
                                    'Should I make an accusation?', 
                                    'Exit Game'].


isNobody('Nobody').


normalGameChoose('I\'m making a suggestion') :- initialSuspects(Suspects), initialWeapons(Weapons), initialRooms(Rooms),
                                                ui:printOut('Which suspect?', Suspects, SuspectInput),
                                                ui:printOut('Which Weapon?', Weapons, WeaponInput),
                                                ui:printOut('Which Room?', Rooms, RoomInput),
                                                getPlayersName(Players),
                                                ui:printOut('Who showed you a card?', ['Nobody'|Players], Shower), 
                                                !,
                                                ((not(isNobody(Shower)), 
                                                  ui:printOut('Which card?', [SuspectInput, WeaponInput, RoomInput], Card),
                                                  player(N, Shower), 
                                                  playersBetweenDoNotHaveCards(1, N, SuspectInput, WeaponInput, RoomInput),
                                                  inputCard(N, Card)) ; 
                                                (isNobody(Shower), inferenceNobodyShowed(1, SuspectInput, WeaponInput, RoomInput))),
                                                makeNewInferences.

normalGameChoose('Another player is making a suggestion') :- getPlayersName([H|Players]),
                                                             ui:printOut('Which player?', Players, AskingPlayer),
                                                             initialSuspects(Suspects), initialWeapons(Weapons), initialRooms(Rooms),
                                                             ui:printOut('Which suspect?', Suspects, SuspectInput),
                                                             ui:printOut('Which Weapon?', Weapons, WeaponInput),
                                                             ui:printOut('Which Room?', Rooms, RoomInput),
                                                             ui:printOut('Who showed a card?', ['Nobody',H|Players], Shower), 
                                                             !,
                                                             ((not(isNobody(Shower)), inferenceShowedCard(AskingPlayer, Shower, SuspectInput, WeaponInput, RoomInput)) ;
                                                              (isNobody(Shower), player(AskPlayer, AskingPlayer), inferenceNobodyShowed(AskPlayer,SuspectInput,WeaponInput,RoomInput))),
                                                             makeNewInferences.
                                                        


normalGameChoose('Show me seen cards') :- findall([X,P], cards:hasCard(X, P), Cards), !, ui:printList(Cards).

normalGameChoose('Show me what\'s left') :- getRemainingSuspects(Suspects), getRemainingWeapons(Weapons), getRemainingRooms(Rooms), !,
                                            ui:sOut('Suspects:'), ui:printList(Suspects),
                                            ui:sOut('Weapons:'), ui:printList(Weapons),
                                            ui:sOut('Rooms:'), ui:printList(Rooms).

normalGameChoose('Should I make an accusation?') :- findall(Card, cards:envelope(Cat, Card), Envelope), !,
													length(Envelope, ELen),
													((ELen =:= 3, ui:sOut('Yes - make suggestion:'), ui:printList(Envelope));
													 (ui:sOut('Not yet - only have:'), ui:printList(Envelope))).





normalGameChoose('Exit Game') :- false.




%%% MAKING SUGGESTIONS


inferenceNobodyShowed(N, Suspect, Weapon, Room) :- getPlayersN(Players),
                                                   allNotHasCardsBut(N, Players, [Suspect, Weapon, Room]).


allNotHasCardsBut(N, [], Cards).
allNotHasCardsBut(N, [N|T], Cards) :- !, allNotHasCardsBut(N, T, Cards).
allNotHasCardsBut(N, [H|T], Cards) :- !,
									  doNotHaveTheseCards(H,Cards),
                                      allNotHasCardsBut(N, T, Cards). 



% P2 showed P1 a card
% - find players between P1 and P2 - they do not have those cards
% - check if P2 has any of the cards
% - eliminate any of cards that we know P2 doesn't have
% - if list is 1 card, P2 has that card 
%       - add only if we don't already know that
% - if list is 2-3 cards, put all 3 cards in suggestion and keep going 
inferenceShowedCard(P1, P2, Suspect, Weapon, Room) :- player(PN1, P1),
                                                      player(PN2, P2),
                                                      playersBetweenDoNotHaveCards(PN1, PN2, Suspect, Weapon, Room),
                                                      inferenceP2Cards(PN1, PN2, [Suspect, Weapon, Room]).
                                                      
inferenceP2Cards(PN1, 1, Cards).
inferenceP2Cards(PN1, PN2, Cards) :- hasOneOfCards(PN2, Cards).
inferenceP2Cards(PN1, PN2, [Suspect, Weapon, Room]) :- removeDoesNotHave(PN2, [Suspect, Weapon, Room], Cards),
                                                      	!,
                                                      	Cards = [H|T],
                                                        length(Cards, CardLength),
                                                         !,
                                                        ((CardLength =:= 1, inputCard(PN2, H)) ;
                                                          (assert(suggestion(PN1, PN2, [Suspect, Weapon, Room])))).
                                                       

                                                      
                                                       
                                                       
playersBetweenDoNotHaveCards(PN1, PN2, S, W, R) :- playersBetween(PN1, PN2, [PN1|Between]),
          										   allNotHasCardsBut(PN1, Between, [S, W, R]).
          										 
playersBetween(N1, N2, PNBetween) :- getPlayersN(AllN),
                                     playersStartAtN(N1, AllN, StartN1),
                                     playersBeforeN(N2, StartN1, PNBetween).
                              
playersStartAtN(N, List, [N|T1]) :- !, append(L1, L2, List), append(L2, L1, [N|T1]).

playersBeforeN(N, [N|T], []).
playersBeforeN(N, [X|T1], [X|T2]) :- !, playersBeforeN(N, T1, T2).


removeDoesNotHave(P, [], []).
removeDoesNotHave(P, [H|T1], [H|T2]) :- not(cards:noHasCard(P,H)), !, removeDoesNotHave(P, T1, T2).
removeDoesNotHave(P, [H|T1], List) :- removeDoesNotHave(P, T1, List).









%%%%%%%%%%%% INFERENCE      


% at end of each turn:
% - go through all suggestions and see if you can make new inferences
% - go through each set of cards and see if they're envelope card (if all players don't have that card)
%           or a certain players card (not envelope card and all but one player don't have)


makeNewInferences :- getSuggestions(Suggestions),
                     inferencesSuggestions(Suggestions),
                     lookForEnvelopeCards,
                     lookForPlayerCards.
                     


% for each suggestion, 
% check to see if P2 owns a card in there

% eliminate cards that we know P2 doesn't have
% if list is 1 card, P2 has that card
%    - add only if we don't know already, then remove suggestion
% if list is 2-3 cards, then keep suggestion
inferencesSuggestions([]).
inferencesSuggestions([H|T]) :- inferencesSuggestion(H), inferencesSuggestions(T).

inferencesSuggestion([P1, P2, Cards]) :- hasOneOfCards(P2, Cards), retract(suggestion(P1, P2, Cards)).
inferencesSuggestion([P1, P2, Cards]) :- removeDoesNotHave(P2, Cards, RemainCards),
										 length(RemainCards,CardLen),
										 !,
										 ((CardLen =:= 1, 
										   retract(suggestion(P1,P2,Cards)),
										   RemainCards = [H],
										   inputCard(P2,H)) ;
										  (true)).

hasOneOfCards(P, [H|T]) :- cards:hasCard(P,H), !.
hasOneOfCards(P, [H|T]) :- !, hasOneOfCards(P,T).



% go through all known card facts and see if we can assign new player cards (all but our player)
%             if cards they don't have = total cards - cards per player, then we know which cards they have
%             if known cards = cards per player, then we know which cards they don't have
% - go through each set of cards and see if they're a certain players card 
%            (not envelope card and all but one player don't have)

lookForPlayerCards :- getPlayersN([H|Players]),
					  notDonePlayers(Players, LOP2),
					  notEnoughNotHasPlayers(LOP2, LOP3), 
					  lookForNewPlayerCards(LOP3).
					  
					  
% filter out all players that we know all cards for
notDonePlayers([], []).
notDonePlayers([P|T1], [P|T2]) :- not(enoughHasCards(P)), !, notDonePlayers(T1, T2).
notDonePlayers([P|T1], Players) :- !, notDonePlayers(T1, Players).


enoughHasCards(P) :- cardsPerPlayer(N),
					 findall(C, cards:hasCard(P,C), Cards),
					 length(Cards, Clen),
					 !,
					 N =< Clen,
					 doesNotHaveRest(Cards, P).
					 
doesNotHaveRest(HasCards, P) :- getAllCards(AllCards),
								filterOut(HasCards,AllCards,Result),
								!,
								doNotHaveTheseCards(P,Result).
								



% filter out all players that has enough cards we know they don't have
notEnoughNotHasPlayers([], []).
notEnoughNotHasPlayers([P|T1], [P|T2]) :- not(enoughHasNotCards(P)), !, notEnoughNotHasPlayers(T1, T2).
notEnoughNotHasPlayers([P|T1], Players) :- !, notEnoughNotHasPlayers(T1, Players).

enoughHasNotCards(P) :- totalCards(Tot), cardsPerPlayer(N),
						N2 is Tot - N,
						findall(C, cards:noHasCard(P,C), Cards),
						length(Cards, Clen),
						!,
						N2 =< Clen,
						doesHaveRest(Cards, P).
						
doesHaveRest(NotHasCards, P) :- getAllCards(AllCards),
								filterOut(NotHasCards,AllCards,Result),
								doHaveTheseCards(P,Result).
								



% given a list of all players we do not know all the cards for
% we want to see if can assign them any of the remaining cards
% for each category:
%     - get all remaining cards, go through each one
% for each card:
%     - check how many people do not have that card
%     - if it is envelope card, then you can't add
%     - if it is not envelope card, then should be N-1 players to add

lookForNewPlayerCards([]).
lookForNewPlayerCards(List) :- lookForPlayerCardsSuspect, 
                               lookForPlayerCardsWeapon, 
                               lookForPlayerCardsRoom.



lookForPlayerCardsSuspect :- getRemainingSuspects(Suspects), 
							 lookForPlayerCardsCategory('S', Suspects).
							  
lookForPlayerCardsWeapon :- getRemainingWeapons(Weapons),
							lookForPlayerCardsCategory('W', Weapons).
							  
lookForPlayerCardsRoom :- getRemainingRooms(Rooms),
  						  lookForPlayerCardsCategory('R', Rooms).		
							

lookForPlayerCardsCategory(Cat,[]).
lookForPlayerCardsCategory(Cat,[Card|T]) :- !, 
										getPlayersN(Players),
										findall(P, cards:noHasCard(P, Card), NoHasPlayers),
										lookForHandlePlayerCards(Players, NoHasPlayers, Card, Cat),
										lookForPlayerCardsCategory(Cat, T).


lookForHandlePlayerCards(Players, NoHasPlayers, Card, Cat) :- cards:envelope(Cat,Card), !.
lookForHandlePlayerCards(Players, NoHasPlayers, Card, Cat) :- cards:envelope(Cat, C), 
															  isAllButOnePlayer(Players, NoHasPlayers), !,
															  findRemainingPlayer(NoHasPlayers, Players, P),
														 	  inputCard(P,Card).
lookForHandlePlayerCards(Players, NoHasPlayers, Card, Cat).
														 
														 
isAllButOnePlayer(Players, NoHasPlayers) :- length(NoHasPlayers, L1),
											length(Players, L2),
											N is L1 + 1,
											N =:= L2.
	
findRemainingPlayer([], [H], H).										  
findRemainingPlayer([H1|T1], [H2|T2], H2) :- H1 =\= H2.
findRemainingPlayer([H|T1], [H|T2], P) :- findRemainingPlayer(T1, T2, P).
											  
         										
% go through each category
% if only one remaining card left and no envelope card, then that is card
% otherwise see if we can find a new envelope card (if all players don't have that card)
lookForEnvelopeCards :- lookForEnvelopeSuspect, !,
                        lookForEnvelopeWeapon, !,
                        lookForEnvelopeRoom.
                        
lookForEnvelopeSuspect :- cards:envelope('S',EnvS).
lookForEnvelopeSuspect :- getRemainingSuspects(Suspects), 
                          (onlyOneCardLeft('S', Suspects) ;
                          lookForEnvelopeCategory('S',Suspects)).
                                                  
lookForEnvelopeWeapon :- cards:envelope('W',EnvW).
lookForEnvelopeWeapon :- getRemainingWeapons(Weapons),
						 (onlyOneCardLeft('W', Weapons) ; lookForEnvelopeCategory('W', Weapons)).
						 
lookForEnvelopeRoom :- cards:envelope('R',EnvR).
lookForEnvelopeRoom :- getRemainingRooms(Rooms),
					   (onlyOneCardLeft('R', Rooms) ; lookForEnvelopeCategory('R', Rooms)).
                       

onlyOneCardLeft(Cat, [H]) :- newEnvelopeCard(Cat,H).  

newEnvelopeCard(Cat, Card) :- addEnvelopeCard(Cat, Card),
							  getPlayersN(Players),
							  addAllDoNotHaveCard(Players, Card).
							  
addAllDoNotHaveCard([], C).
addAllDoNotHaveCard([P|T], C) :- addNotHasCard(P,C), addAllDoNotHaveCard(T,C).

                       
lookForEnvelopeCategory(Cat, []).
lookForEnvelopeCategory(Cat, [H|T]) :- getPlayersN(Players), 
									   playersDoNotHaveCard(H, Players), 
									   addEnvelopeCard(Cat, H).
									   
lookForEnvelopeCategory(Cat, [H|T]) :- lookForEnvelopeCategory(Cat, T).

playersDoNotHaveCard(Card, []).
playersDoNotHaveCard(Card, [H|T]) :- cards:noHasCard(H,Card), playersDoNotHaveCard(Card, T).