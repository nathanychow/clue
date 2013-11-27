:- use_module(clueui).
:- use_module(cards).
:- use_module(clueui).


:- dynamic player/2.     % player(number, name)
:- dynamic suggestion/3. % suggestion(asking, suggested, [suspect, weapon, room])

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


chooseHeroPlayer :- initialPlayers(Players), ui:printOut('Choose Your Player', Players, Input),
                    (player(1, Input) ; assert(player(1, Input))).

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
getSuggestions(Suggestions) :- findall([P1, P2, Cards], suggestion(P1, P2, Cards), Suggestions).


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

initializeDoNotHaveOtherCards :- getRemainingSuspects(Suspects), doNotHaveTheseCards(Suspects),
								 getRemainingWeapons(Weapons), doNotHaveTheseCards(Weapons),
								 getRemainingRooms(Rooms), doNotHaveTheseCards(Rooms).
								 
doNotHaveTheseCards([]).
doNotHaveTheseCards([Card|T]) :- playerNotHasCard(1,Card), doNotHaveTheseCards(T).


inputCard(N, Input) :- getPlayersN(Players), inputCard(N, Input, Players).

inputCard(N, Input, []).
inputCard(N, Input, [N|T]) :- addHasCard(N,Input), inputCard(N, Input, T).
inputCard(N, Input, [H|T]) :- addNotHasCard(H,Input), inputCard(N, Input, T).

addHasCard(P, C) :- cards:hasCard(P,C) ; playerHasCard(P,C).
addNotHasCard(P,C) :- cards:noHasCard(P,C) ; playerNotHasCard(P,C).


%%%%%%%%

normalGameMenuList(List) :- List = ['I\'m making a suggestion', 
                                    'Another player is making a suggestion', 
                                    'Show me seen cards', 
                                    'Show me what\'s left', 
                                    'Should I make an accusation?', 
                                    'Exit Game'].

normalGameMenu :- !,
                  normalGameMenuList(List), 
                  ui:printOut('Choose an action', List, Input), 
                  normalGameChoose(Input), 
                  !,
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
                                                ((not(isNobody(Shower)), 
                                                  ui:printOut('Which card?', [SuspectInput, WeaponInput, RoomInput], Card),
                                                  player(N, Shower), 
                                                  playersBetweenDoNotHaveCards(1, N, SuspectInput, WeaponInput, RoomInput),
                                                  inputCard(N, Card)) ; 
                                                isNobody(Shower)),
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


normalGameChoose('Another player is making a suggestion') :- getPlayersName([H|Players]),
                                                             ui:printOut('Which player?', Players, AskingPlayer),
                                                             initialSuspects(Suspects), initialWeapons(Weapons), initialRooms(Rooms),
                                                             ui:printOut('Which suspect?', Suspects, SuspectInput),
                                                             ui:printOut('Which Weapon?', Weapons, WeaponInput),
                                                             ui:printOut('Which Room?', Rooms, RoomInput),
                                                             ui:printOut('Who showed a card?', ['Nobody',H|Players], Shower), 
                                                             !,
                                                             ((not(isNobody(Shower)), inferenceShowedCard(AskingPlayer, Shower, SuspectInput, WeaponInput, RoomInput)) ;
                                                              (isNobody(Shower), inferenceNobodyShowed(AskingPlayer,SuspectInput,WeaponInput,RoomInput))),
                                                             makeNewInferences.
                                                        


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
% - if list is 2-3 cards, put all 3 cards in suggestion and keep going 
inferenceShowedCard(P1, P2, Suspect, Weapon, Room) :- player(PN1, P1),
                                                      player(PN2, P2),
                                                      playersBetweenDoNotHaveCards(PN1, PN2, Suspect, Weapon, Room),
                                                      inferenceP2Cards(PN1, PN2, [Suspect, Weapon, Room], Cards).
                                                      
inferenceP2Cards(PN1, 1, List, Cards).
inferenceP2Cards(PN1, PN2, [Suspect, Weapon, Room], Cards) :- removeDoesNotHave(PN2, [Suspect, Weapon, Room], Cards),
                                                      		  !,
                                                      		  Cards = [H|T],
                                                              length(Cards, CardLength),
                                                              ((CardLength =:= 1, inputCard(PN2, H)) ;
                                                               (assert(suggestion(PN1, PN2, [Suspect, Weapon, Room])))).
                                                       

allNotHasCardsBut(N, [], S, W, R).
allNotHasCardsBut(N, [N|T], S, W, R) :- !, allNotHasCardsBut(N, T, S, W, R).
allNotHasCardsBut(N, [H|T], S, W, R) :- !,
                                       addNotHasCard(H,S),
                                       addNotHasCard(H,W),
                                       addNotHasCard(H,R),
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


% at end of each turn:
% - go through all suggestions and see if you can make new inferences
% - go through each set of cards and see if they're envelope card (if all players don't have that card)
%           or a certain players card (not envelope card and all but one player don't have)
makeNewInferences :- getSuggestions(Suggestions),
                     inferencesSuggestions(Suggestions),
                     lookForPlayerCards,
                     lookForEnvelopeCards.
                     


% for each suggestion, eliminate cards that we know P2 doesn't have
% if list is 1 card, P2 has that card
%    - add only if we don't know already, then remove suggestion
% if list is 2-3 cards, then keep suggestion
inferencesSuggestions([]).
inferencesSuggestions([H|T]) :- inferencesSuggestion(H), inferencesSuggestions(T).

inferencesSuggestion([P1, P2, Cards]) :- removeDoesNotHave(P2, Cards, RemainCards),
									     !,
										 length(RemainCards,CardLen),
										 ((CardLen =:= 1, 
										   retract(suggestion(P1,P2,Cards)),
										   RemainCards = [H],
										   inputCard(P2,H)) ;
										  (true)).


% go through all known card facts and see if we can assign new player cards
% - divide total cards with number of players to get cards per player
%             if cards they don't have = total cards - cards per player, then we know which cards they have
%             if known cards = cards per player, then we know which cards they don't have
% - go through each set of cards and see if they're a certain players card 
%            (not envelope card and all but one player don't have)
lookForPlayerCards :- haveEnoughPlayerCards, lookForPlayerCardsSuspect, !, lookForPlayerCardsWeapon, !, lookForPlayerCardsRoom.
 					  
lookForPlayerCardsSuspect :- not(cards:envelope('S',Env)), 
							 getRemainingSuspects(Suspects), 
							 lookForPlayerCardsCategory(Suspects).
lookForPlayerCardsSuspect.
							  
lookForPlayerCardsWeapon :- not(cards:envelope('W',Env)),
							getRemainingWeapons(Weapons),
							lookForPlayerCardsCategory(Weapons).
lookForPlayerCardsWeapon.
							  
lookForPlayerCardsRoom :- not(cards:envelope('R',Env)),
  						  getRemainingRooms(Rooms),
  						  lookForPlayerCardsCategory(Rooms).				
lookForPlayerCardsRoom.					
							
							
haveEnoughPlayerCards :- totalCards(Tot),
                        cardsPerPlayer(N),
                         getPlayersN([H|Players]),
						 haveEnoughPlayerCards(Players, Tot, N).	
							   
haveEnoughPlayerCards([], Tot, N).
haveEnoughPlayerCards([P|T], Tot, N) :- (enoughHasCards(P, N) ; enoughNotHasCards(P, Tot, N) ; true),
									    haveEnoughPlayerCards(T, Tot, N).

totalCards(Tot) :- initialSuspects(LOS), initialWeapons(LOW), initialRooms(LOR),
				   append(LOS,LOW, X1), append(X1,LOR, List),
				   length(List, Tot).
				   
cardsPerPlayer(N) :- totalCards(Tot),
					 getPlayersN(Players),
					 length(Players, PlayersNum),
					 N is Tot // PlayersNum.
					 
					 
enoughHasCards(P, N) :- findall(C, cards:hasCard(P,C), HasCards),
						length(HasCards, Len),
						Len =:= N,
						!,
						doesNotHaveRest(HasCards, P).
						
doesNotHaveRest(HasCards, P) :- initialSuspects(LOS), initialWeapons(LOW), initialRooms(LOR),
								append(LOS,LOW, X1), append(X1,LOR, List),
								doesNotHaveRest(HasCards,P,List).
doesNotHaveRest(HasCards,P,[]).
doesNotHaveRest([Card|T1],P,[Card|T2]) :- doesNotHaveRest(T1,P,T2).
doesNotHaveRest(HasCards,P,[C|T]) :- addNotHasCard(P,C), doesNotHaveRest(HasCards,P,T).

enoughNotHasCards(P, Tot, N) :- N2 is Tot - N,
									  findall(C, cards:noHasCard(P,C), NotHasCards),
									  length(NotHasCards, Len),
									  Len =:= N2,
									  !,
									  doesHaveRest(NotHasCards,P).
									  
doesHaveRest(NotHasCards, P) :- initialSuspects(LOS), initialWeapons(LOW), initialRooms(LOR),
								append(LOS,LOW, X1), append(X1,LOR, List),
								doesHaveRest(HasCards,P,List).
								
doesHaveRest(Cards,P,[]).
doesHaveRest([Card|T1],P,[Card|T2]) :- doesHaveRest(T1,P,T2).
doesHaveRest(Cards,P,[C|T]) :- addHasCard(P,C), doesHaveRest(Cards,P,T).

lookForPlayerCardsCategory([]).
lookForPlayerCardsCategory([Card|T]) :- !, getPlayersN(Players),
										findall(P, cards:noHasCard(P, Card), NoHasPlayers),
										lookForHandlePlayerCards(Players, NoHasPlayers, Card),
										lookForPlayerCardsCategory(T).

lookForHandlePlayerCards(Players, NoHasPlayers, Card) :- (isAllButOnePlayer(Players, NoHasPlayers),
														  !,
														  findRemainingPlayer(NoHasPlayers, Players, P),
														  inputCard(P,Card))
														 ;true.
														 
														 
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
                        
lookForEnvelopeSuspect :- cards:envelope('S',EnvS) ; (
                          getRemainingSuspects(Suspects), 
                          length(Suspects, Len),
                          ((Len =:= 1, Suspects = [H], addEnvelopeCard('S',H)) ; lookForEnvelopeCategory('S', Suspects))).
                          
lookForEnvelopeWeapon :- cards:envelope('W',EnvW);
                         (getRemainingWeapons(Weapons), 
                         length(Weapons, Len),
                         ((Len =:= 1, Weapons = [H], addEnvelopeCard('W',H)) ; lookForEnvelopeCategory('W', Weapons))).
    
lookForEnvelopeRoom :- cards:envelope('R', EnvR);
                       (getRemainingRooms(Rooms), 
                       length(Rooms, Len),
                       ((Len =:= 1, Rooms = [H], addEnvelopeCard('R',H)) ; lookForEnvelopeCategory('R', Rooms))).
                       
lookForEnvelopeCategory(Cat, []).
lookForEnvelopeCategory(Cat, [H|T]) :- getPlayersN(Players),
                                  	   ((playersDoNotHaveCard(H, Players), addEnvelopeCard(Cat, H));
                                  	    lookForEnvelopeCategory(Cat, T)).

playersDoNotHaveCard(Card, []).
playersDoNotHaveCard(Card, [H|T]) :- cards:noHasCard(H,Card), playersDoNotHaveCard(Card, T).