!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! This program allows for a user to play a no-stakes simulated version
!! of Blackjack (the card game) against a computerized dealer.
!! Written by: Christian Santiago and Griffin Lehrer
!! Feb. 8, 2021
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine introPrint()

          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !! PROGRAM START - WELCOME TEXT
    WRITE(*,*) 'Hello, and welcome to the Blackjack simulator!'
    WRITE(*,*) ''
    WRITE(*,*) 'To play this game, a player and a computerized dealer'
    WRITE(*,*) 'will each be dealt 2 cards from a randomized 52 card'
    WRITE(*,*) 'deck. The player will then be able to take additional'
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
    WRITE(*,*) ''

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! GAME START
    WRITE(*,*) '* - G A M E - S T A R T - *'
    WRITE(*,*) '==========================='

end subroutine introPrint

subroutine shuffleDeck(deck, cardValue)

    Integer, dimension(52) :: deck
    Integer, dimension(52) :: cardValue

    Integer :: count = 1

    do i=1, 52
        deck(i) = 1

        if(count .eq. 14) then
            count = 1
        endif

        if(count .gt. 10) then
            cardValue(i) = 10

        else if(count .eq. 1) then
            cardValue(i) = 11

        else
            cardValue(i) = count
        endif

        ! write(*,*) "count is " , count
        count = count + 1
    enddo

end subroutine shuffleDeck

subroutine drawCard(card)

    Real :: card

    ! create a random seed for the random number genertor
    call random_seed()

    ! get a random number between 0 and 1
    call random_number(card)

    ! transform the number from 0 - 1 to 1 - 52
    card = floor(card * 52) + 1

end subroutine drawCard

subroutine dealerTurn(deck, cardValue, cardTotal, cardsInDeck)

    Integer, dimension(52) :: deck
    Integer, dimension(52) :: cardValue
    Integer :: cardTotal
    Integer :: cardsInDeck

    Integer, dimension(4) :: aces

    Real :: card

    do i=1,4

        aces(i) = 0

    enddo

    do while(cardTotal .le. 16 .AND. cardsInDeck .ge. 0)

        call drawCard(card)
        cardsInDeck = cardsInDeck - 1

        do while(deck(int(card)) .eq. 0 .AND. cardsInDeck .ge. 0)
            call drawCard(card)
        enddo

        deck(int(card)) = deck(int(card)) - 1

        cardTotal = cardValue(int(card)) + cardTotal

        if(cardValue(int(card)) .eq. 11) then
            
            do i=1,4

                if(aces(i) .eq. 0) then

                    aces(i) = 1

                    exit

                endif

            enddo

        endif

        do i=1,4

            if(aces(i) .eq. 1) then

                if(cardTotal .ge. 22) then
                    cardTotal = cardTotal - 10
                    aces(i) = 0
                endif

            endif

        enddo

    enddo

end subroutine dealerTurn

subroutine playerTurn(deck, cardValue, cardTotal, cardsInDeck)

    Integer, dimension(52) :: deck
    Integer, dimension(52) :: cardValue
    Integer :: cardTotal
    Integer :: cardsInDeck
    Integer, dimension(4) :: aces

    Real :: card
    Integer :: userInput = 0

    do i=1,4

        aces(i) = 0

    enddo

    do while(userInput .ne. 1)

        call drawCard(card)
        cardsInDeck = cardsInDeck - 1

        do while(deck(int(card)) .eq. 0 .AND. cardsInDeck .ge. 0)
            call drawCard(card)
        enddo

        write(*,*) "You drew a card worth", cardValue(int(card)), " points!"

        deck(int(card)) = deck(int(card)) - 1

        cardTotal = cardValue(int(card)) + cardTotal

        if(cardValue(int(card)) .eq. 11) then
            
            do i=1,4

                if(aces(i) .eq. 0) then

                    aces(i) = 1

                    exit

                endif

            enddo

        endif

        do i=1,4

            if(aces(i) .eq. 1) then

                if(cardTotal .ge. 22) then
                    cardTotal = cardTotal - 10
                    aces(i) = 0
                endif

            endif

        enddo

        write(*,*) "Your hand is currently worth", cardTotal, " points"
        
        if(cardTotal .gt. 21) then
            write(*,*) "You busted!"
            exit
        endif

        write(*,*) "Type a 1 to stay or any other number to hit"
        read(*,*) userInput

    enddo
    userInput = 0;

end subroutine playerTurn

program blackjack
    implicit none

    Integer, dimension(52) :: deck
    Integer, dimension(52) :: cardValue
    Real :: card
    Integer :: dealerHandScore = 0
    Integer :: playerHandScore = 0
    Integer :: cardsInDeck = 52
    Integer :: dealerWins = 0
    Integer :: playerWins = 0

    Integer :: i

    call introPrint()
    
    call shuffleDeck(deck, cardValue)


    do while(cardsInDeck .ge. 4)
        call playerTurn(deck, cardValue, playerHandScore, cardsInDeck)
        call dealerTurn(deck, cardValue, dealerHandScore, cardsInDeck)

        write(*,*) "the dealers hand is ", dealerHandScore
        write(*,*) "the players hand is ", playerHandScore
        write(*,*) "cards left in the deck is ", cardsInDeck

        if(playerHandScore .gt. 21) then
            write(*,*) "The dealer wins this round"
            dealerwins = dealerWins + 1

        else if(dealerHandScore .gt. 21) then
            write(*,*) "The player wins this round"
            playerWins = playerWins + 1
        else if(dealerHandScore .gt. 21 .AND. playerHandScore .gt. 21)
            write(*,*) "You have both busted better luck next time"
            
        else if(dealerHandScore .gt. playerHandScore) then
            write(*,*) "The dealer wins this round"
            dealerwins = dealerWins + 1

        else if(playerHandScore .gt. dealerHandScore) then
            write(*,*) "The player wins this round"
            playerWins = playerWins + 1

        else
            write(*,*) "The dealer and the player have tied"
        endif 

        dealerHandScore = 0
        playerHandScore = 0
        write(*,*)
    enddo

    ! do i=1, 52
    !     write(*,*) i, cardValue(i), "is the card value"
    ! enddo

    ! do i=1, 52
    !     write(*,*) i, deck(i), "number of copies left in the deck"
    ! enddo

    if(dealerWins .gt. playerWins) then
        write(*,*) "The dealer has won the game!"

    else if(playerWins .gt. dealerWins) then
        write(*,*) "The player has won the game!"

    else
        write(*,*) "The player and the dealer have tied!"

    endif

end program blackjack