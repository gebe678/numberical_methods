!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! This program allows for a user to play a no-stakes simulated version
!! of Blackjack (the card game) against a computerized dealer.
!! Written by: Christian Santiago and Griffin Lehrer
!! Feb. 8, 2021
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine introPrint()
      !! WELCOME AND RULES USER TEXT
	  WRITE(*,*) 'Hello, and welcome to the Blackjack simulator!'         !! Welcomes user to blackjack simulator,
      WRITE(*,*) ''                                                       !! explains the basic rules of the game
      WRITE(*,*) 'To play this game, a player and a computerized dealer'  !! to them, explains the rules that  
      WRITE(*,*) 'will each be dealt 2 cards from a randomized 52 card'   !! the dealer must follow, and commences
      WRITE(*,*) 'deck. The player will then be able to take additional'  !! the game
      WRITE(*,*) 'cards from the deck (take hits) to try and get the'
      WRITE(*,*) 'values of the cards in their hand as close to 21 as'
      WRITE(*,*) 'possible without going over (going bust). After a'
      WRITE(*,*) 'player has signaled that they don''t want to hit' 
      WRITE(*,*) 'again (holding) or go bust, the dealer will then'
      WRITE(*,*) 'take hits until the values of the cards in their hand'
      WRITE(*,*) 'are at least 16 or they go bust. After both hands'
      WRITE(*,*) 'have been dealt, their values are compared.'
      WRITE(*,*) 'Whomever''s hand is closest to 21 without going bust'    
      WRITE(*,*) 'wins the hand. Additional hands will be played until'
      WRITE(*,*) 'the deck runs out of cards to be dealt, at which'
      WRITE(*,*) 'point the number of hands won are compared and a'
      WRITE(*,*) 'victor is determined for the game.'
      WRITE(*,*) ''
      WRITE(*,*) 'NOTE: FACE CARDS HAVE A VALUE OF 10, ACES CAN BE A 1'
      WRITE(*,*) 'OR 11, WHICHEVER MAKES THE BEST HAND WITHOUT BUSTING.'
      WRITE(*,*) 'THE LAST HAND OF A GAME WHERE THE CARDS IN THE DECK'
      WRITE(*,*) 'RUN OUT WILL NOT BE COUNTED.'
      WRITE(*,*) ''

      !! GAME COMMENCEMENT USER TEXT
      WRITE(*,*) '* - G A M E - S T A R T - *'
      WRITE(*,*) '==========================='

end subroutine introPrint

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine shuffleDeck(deck, cardValue)
      !! VARIABLES
      Integer, dimension(52) :: deck                                       
      Integer, dimension(52) :: cardValue

      !! FILLING DECK WITH CARDS, ASSIGNING CARDS WITH VALUES
      Integer :: count = 1                   !! - count will be used to assign card values

      do i=1, 52                             !! - loop will cycle through each card in the deck 
          deck(i) = 1                        !! - indicates that card "i" is present in the deck

          if(count .eq. 14) then             !! - checks if a suit has had all of its cards assigned values
              count = 1                      !!   if so, starts assigning values to cards in next suit using
          endif                              !!   count (A, 2, 3, 4, 5, 6, 7, 8, 9, 10, J, Q, K)

          if(count .gt. 10) then             !! - checks if card is a face card, if so, assign value of 10
              cardValue(i) = 10
          else if(count .eq. 1) then         !! - checks if card is an ace, if so, assign value of 11
              cardValue(i) = 11
          else
              cardValue(i) = count           !! - all other cards get the value of count
          endif

          count = count + 1                  !! - prepare count for loop to fill next card value
      enddo

end subroutine shuffleDeck

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine drawCard(card)
      !! VARIABLE
      INTEGER :: card, timeSeed

      !! creates a random time seed for the random number generator
      CALL SYSTEM_CLOCK(timeSeed)

      !! gives random number generator random time seed
      CALL SRAND(timeSeed)           
    
	  !! get a random number from 1 - 52
	  card = MOD(IRAND(),52)+1    
	
end subroutine drawCard

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine drawResultPrinter(playerType,card,cardValue)
      !! VARIABLES
      INTEGER                 :: card
      INTEGER, dimension (52) :: cardValue
      CHARACTER          (6)  :: playerType          !! - "Player" input for player draw, "Dealer" input for dealer draw (used to tell user who drew)

      CHARACTER          (5)  :: faceCardName        !! - gives king, queen, jack, ace names to appropriate cards
      CHARACTER          (8)  :: suitName            !!   when telling the user what card they drew
  
      !! DETERMINE CARD SUIT
      IF (card.LE.13) THEN                           !! - cards 1-13 are diamonds
          suitName = 'diamonds'
      ELSEIF (card.LE.26) THEN
          suitName = 'hearts'                        !! - cards 14-26 are hearts
      ELSEIF (card.LE.39) THEN
          suitName = 'clubs'                         !! - cards 27-39 are diamonds
      ELSE 
          suitName = 'spades'                        !! - cards 40-52 are spades
      ENDIF
	  
	  !! DETERMINE CARD NAME/VALUE AND PRINT DRAWN CARD TO USER
      IF (card.EQ.1 .or. card.eq.14 .or. card.eq.27 .or.&
      card.eq.40) THEN
          WRITE(*,*) playerType,' drew an ace of ', suitName                      !! - 1's of suits = ace

      ELSE IF (card.EQ.11 .or. card.eq.24 .or. card.eq.37 .or.&
      card.eq.50) THEN
          WRITE(*,*) playerType,' drew a jack of ', suitName                      !! - 11's of suits = jacks

      ELSE IF (card.EQ.12 .or. card.eq.25 .or. card.eq.38 .or.&
      card.eq.51) THEN
          WRITE(*,*) playerType,' drew a queen of ', suitName                     !! - 12's of suits = queens 

      ELSE IF (card.EQ.13 .or. card.eq.26 .or. card.eq.39 .or.&
      card.eq.52) THEN
          WRITE(*,*) playerType,' drew a king of ', suitName                      !! - 13's of suits = kings

      ELSE 
2         FORMAT (A,A,A,X,I2,A,A)
      WRITE(*,2)' ',playerType,' drew a',cardValue(card),' of ',suitName          !! - all other cards are normal number cards   
      ENDIF

end subroutine drawResultPrinter

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine dealerTurn(deck, cardValue, cardTotal, cardsInDeck)
      !! VARIABLES
      Integer, dimension(52) :: deck
      Integer, dimension(52) :: cardValue
      Integer                :: cardTotal
      Integer                :: cardsInDeck

      Integer, dimension(4)  :: aces                !! - counts number of aces in dealer's hand
      INTEGER                :: card                !! - placeholder for randomly withdrawn card from deck

      !! RESETTING ACES ARRAY FOR HAND TO EMPTY
      do i=1,4
          aces(i) = 0
      enddo

      !! START DEALER'S TURN
      do while(cardTotal .le. 16 .AND. cardsInDeck .ge. 0)            !! - dealer will hit so long as their hand is worth 16 or less
        
          !! DRAW CARD FROM DECK TO DEALER'S HAND		
          call drawCard(card)
          cardsInDeck = cardsInDeck - 1

          do while(deck(card) .eq. 0 .AND. cardsInDeck .ge. 0)        !! - if drawCard draws a card that has already
              call drawCard(card)                                     !!   been drawn, try again
          enddo

          !! DEALER DRAW RESULT NOTIFICATION
          CALL drawResultPrinter('Dealer', card, cardValue)     

          deck(card) = deck(card) - 1                                 !! - indicates that the drawn card no longer exists in the deck

          cardTotal = cardValue(card) + cardTotal                     !! - increases value of dealer's hand

          !! DEALING WITH ACES
          if(cardValue(card) .eq. 11) then                            !! - if dealer just pulled an ace, the aces 
              do i=1,4                                                !!   array is filled with one more value
                  if(aces(i) .eq. 0) then                             !!   that will be used (to lower an ace value 
                      aces(i) = 1                                     !!   to 1) if the dealer is going to bust
                      exit
                  endif
              enddo
          endif

          do i=1,4                                                    !! - if a dealer has an ace and their hand is 
              if(aces(i) .eq. 1) then                                 !!   going to bust, change an ace value from 11
                  if(cardTotal .ge. 22) then                          !!   to 1 and remove an ace from the aces array 
                      cardTotal = cardTotal - 10                      !!   (since they wouldn't bring the value back up to 11). 
                      aces(i) = 0                                     !!   Repeat for every ace the dealer has in their hand 
                  endif                                               !!   until their hand won't bust anymore.
              endif
          enddo

          !! CHECK IF DECK EMPTY, TERMINATE GAME IF SO      
4             FORMAT(A,X,I2,A)
          IF (cardsInDeck.eq.0) THEN
              WRITE(*,*) 
         WRITE(*,4)" Dealer's hand currently worth",cardTotal," points."
              WRITE(*,*) 'NO CARDS REMAIN IN THE DECK. ROUND IS VOID.'
              return                                                  !! referenced using return from https://community.intel.com/t5/Intel-Fortran-Compiler/Exit-from-a-subroutine-to-a-program/td-p/902871
          ENDIF
      enddo

      !! USER HAND VALUE OUTPUT
      WRITE(*,*)
      write(*,4)" Dealer's hand is currently worth",cardTotal," points."

      !! OUT OF SAVING ACES AND HAND VALUE HAS BUSTED        
      if(cardTotal .gt. 21) then
          write(*,*) "Dealer busted!"
      else 
          Write(*,*) "Dealer stays."
      endif
      WRITE(*,*)     "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

end subroutine dealerTurn

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine playerTurn(deck, cardValue, cardTotal, cardsInDeck)
      !! VARIABLES 
      Integer, dimension(52) :: deck
      Integer, dimension(52) :: cardValue
      Integer                :: cardTotal
      Integer                :: cardsInDeck

      Integer, dimension(4)  :: aces               !! - counts number of aces in player's hand
      INTEGER                :: card               !! - placeholder for randomly withdrawn card from deck
      Integer                :: userInput = 0
      Integer                :: startingHits = 0   !! - dealer must hit the user twice before they make any choices,
                                                   !1   this counts the number of preliminary hits a user has gotten
      !! SET NUMBER OF ACES IN HAND TO 0
      do i=1,4
          aces(i) = 0
      enddo

      !! START USER'S TURN
      do while(userInput.ne.1 .AND. cardsInDeck .ge. 0)       !! - 1 is the key value to stay

          !! DRAW CARD FROM DECK TO USER'S HAND
99        call drawCard(card)
          cardsInDeck = cardsInDeck - 1

          do while(deck(card) .eq. 0 .AND. cardsInDeck .ge. 0)
              call drawCard(card)                                !! - if drawCard draws a card that has already 
          enddo                                                  !!   been discarded from the deck, try again

          !! USER DRAW RESULT NOTIFICATION
          CALL drawResultPrinter('Player', card, cardValue)

          deck(card) = deck(card) - 1                            !! - indicates that the drawn card no longer exists in the deck

          cardTotal  = cardValue(card) + cardTotal               !! - increases value of player's hand

          !! DEALING WITH ACES
          if(cardValue(card) .eq. 11) then                       !! - if player just pulled an ace, the aces 
              do i=1,4                                           !!   array is filled with one more value
                  if(aces(i) .eq. 0) then                        !!   that will be used (to lower an ace value 
                      aces(i) = 1                                !!   to 1) if the player is going to bust
                      exit
                  endif
              enddo
          endif

          do i=1,4                                               !! - if a player has an ace and their hand is 
              if(aces(i) .eq. 1) then                            !!   going to bust, change an ace value from 11
                  if(cardTotal .ge. 22) then                     !!   to 1 and remove an ace from the aces array 
                      cardTotal = cardTotal - 10                 !!   (since they wouldn't bring the value backup to 11). 
                      aces(i) = 0                                !!   Repeat for every ace the player has in their hand 
                  endif                                          !!   until their hand won't bust anymore.
              endif
          enddo

          !! CHECK IF DECK EMPTY, TERMINATE GAME IF SO      
1         FORMAT (A,X,I2,A)
          IF (cardsInDeck.eq.0) THEN
          write(*,1)" Your hand's current worth:",cardTotal," points."
              WRITE(*,*) 'NO CARDS REMAIN IN THE DECK. ROUND IS VOID.'
              return
          ENDIF

          !! HAS USER GOTTEN PRELIMINARY 2 CARDS?
          startingHits = startingHits + 1
		  IF(startingHits.LT.2) GOTO 99                          !! - if no, make them get hit again before they can continue

          !! USER HAND VALUE OUTPUT
          write(*,*)
          write(*,1)" Your hand's current worth:",cardTotal," points."

          !! OUT OF SAVING ACES AND HAND VALUE HAS BUSTED        
          if(cardTotal .gt. 21) then
              write(*,*) "You busted!"
              exit
          endif

          !! USER INPUT TO STAY OR HIT AND OUTPUT TO VERIFY
          write(*,*) "Type a 1 to stay or any other number to hit."
          read(*,*) userInput
          if(userInput.eq.1) WRITE(*,*)'Player stays'
          If(userInput.ne.1) Write(*,*)'Player hits'
                             WRITE(*,*)'~~~~~~~~~~~~'
          
      enddo
      userInput = 0;
      startingHits = 0

end subroutine playerTurn

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program blackjack
      implicit none
  
      !! VARIABLES
      Integer, dimension(52) :: deck                 !! - indicates if a card in the deck has been dealt (0) or not (1)
      Integer, dimension(52) :: cardValue            !! - stores the numerical values for all cards in the deck (2-13, aces are assumed 11's until they cause a bust and are adjusted)
      INTEGER                :: card                 !! - placeholder for the location of a just-dealt card in the deck
      Integer                :: dealerHandScore = 0  !! - value of all cards in the dealer's hand
      Integer                :: playerHandScore = 0
      Integer                :: cardsInDeck = 52     !! - starting count of cards in the deck
      Integer                :: dealerWins = 0
      Integer                :: playerWins = 0
      Integer                :: i                    !! - looping variable

      !! SETUP
      call introPrint()
      call shuffleDeck(deck, cardValue)

      !! START/CONTINUE GAME
      do while(cardsInDeck .ge. 4)
          WRITE(*,*) '------------------------------'
          WRITE(*,*) '*PLAYER GO*'
          WRITE(*,*) '------------------------------'
          call playerTurn(deck, cardValue, playerHandScore, cardsInDeck)
          WRITE(*,*) '------------------------------'
          IF (cardsInDeck.eq.0) GOTO 6
          WRITE(*,*)'*DEALER GO*'
          WRITE(*,*)'------------------------------'
          call dealerTurn(deck, cardValue, dealerHandScore, cardsInDeck)
          WRITE(*,*)'------------------------------'
          IF (cardsInDeck.eq.0) GOTO 6

          !! PLAYER TEXT UPDATE OF STATE OF GAME
          WRITE(*,*) "*ROUND/HAND OVER*"
          write(*,*) "------------------------------"
3         FORMAT(A,X,I2)          
          write(*,3) " The dealer's hand is worth", dealerHandScore
          write(*,3) " The player's hand is worth", playerHandScore
          WRITE(*,*) 

          !! DETERMINING HAND WINNER
          if(playerHandScore.gt.21 .and. dealerHandScore.le.21) then
              write(*,*) "Player busts. The dealer wins this hand."                 !! - player busts
              dealerwins = dealerWins + 1

          else if(dealerHandScore.gt.21 .and. playerHandScore.le.21)then
              write(*,*) "Dealer busts. The player wins this hand."                 !! - dealer busts
              playerWins = playerWins + 1
          else if(dealerHandScore.gt.21 .AND. playerHandScore.gt.21)then
              write(*,*) "The dealer and player have busted,"&                      !! - dealer and player bust
              ," nobody wins the hand." 
            
          else if(dealerHandScore .gt. playerHandScore) then
              write(*,*) "The dealer wins this hand."                               !! - dealer beats player
              dealerwins = dealerWins + 1

          else if(playerHandScore .gt. dealerHandScore) then
              write(*,*) "The player wins this hand."                               !! - player beats dealer
              playerWins = playerWins + 1

          else
              write(*,*) "The dealer and the player have tied,"& 
             ," nobody wins hand."
          endif 

          !! RESETTING HANDS FOR NEXT ROUND
5         FORMAT(A,X,I2,A)
          write(*,*) '------------------------------'
          write(*,5) ' There are ', cardsInDeck, ' cards left in the deck.'
          WRITE(*,5) ' The dealer has won', dealerWins, ' hands.' 
          WRIte(*,5) ' The player has won', playerWins, ' hands.'

          dealerHandScore = 0
          playerHandScore = 0
      enddo

      !! DECK OUT OF CARDS DETERMINE WINNER
6     write(*,*) '=========================================================='
      WRITE(*,*) 'DECK DOES NOT CONTAIN ENOUGH CARDS TO CONTINUE. GAME OVER.'

      WRITE(*,5) ' The dealer has won', dealerWins, ' hands.' 
      WRIte(*,5) ' The player has won', playerWins, ' hands.'

      if(dealerWins .gt. playerWins) then
          write(*,*) "The dealer has won the game!"
      else if(playerWins .gt. dealerWins) then
          write(*,*) "The player has won the game!"
      else
          write(*,*) "The player and the dealer have tied!"
      endif

end program blackjack

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!