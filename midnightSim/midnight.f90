! subroutine that will get a random number between 1 - 6
! This simulates rolling a die
subroutine rollDice(x)
    implicit none
    
    ! variable that will hold the random number
    real :: x

    ! create a random seed for the random number genertor
    call random_seed()

    ! get a random number between 0 and 1
    call random_number(x)

    ! transform the number from 0 - 1 to 1 - 6
    x = floor(x * 6) + 1

end subroutine rollDice

! subroutine that containes logic for a players turn
subroutine playerTurn(playerBank)

    ! number of dice left to roll
    Integer :: diceToRoll = 6

    ! values of each die roll
    real, dimension(6) :: rolls

    ! values the player has banked from the die rolls
    Integer, dimension(6) :: playerBank

    ! number of dice the player has chosen to bank
    Integer :: numDiceToBank

    ! number of the die the player wants to bank
    Integer :: bankedDice

    ! index value of the die the player wants to bank
    Integer :: bankNumber = 1

    write(*,*) "the dice to roll is", diceToRoll

    ! run the simulation while the player still has to bank dice
    do while(diceToRoll .gt. 0)
        write(*,*) "You have ", diceToRoll, "dice left"

        ! roll dice 6 times and save the values into the rolls array
        do i=1,diceToRoll
            call rollDice(rolls(i))
        enddo

        ! print out the values to the user
        do i = 1, diceToRoll
            write(*,*) "you rolled", rolls(i)
        enddo

        write(*,*) "how many dice would you like to bank?"
        read(*,*) numDiceToBank

        ! check to make sure the player is banking a valid die
        do while(numDiceToBank .gt. diceToRoll .OR. numDiceToBank .le. 0)
            write(*,*) "You are trying to bank more dice then you have"
            write(*,*) "Enter a number greater than 0 and less than", diceToRoll

            read(*,*) numDiceToBank
        enddo

        ! get which dice the player wants to bank
        do i=1,numDiceToBank
            write(*,*) "what die would you like to bank?"
            write(*,*) "Enter a number between 1 and", diceToRoll

            read(*,*) bankedDice

            ! check to make sure the player is picking a valid die
            do while(bankedDice .gt. diceToRoll .OR. bankedDice .le. 0)
                write(*,*) "That is not a valid die"
                write(*,*) "Pick a number between 1 and", diceToRoll

                read(*,*) bankedDice
            enddo

            ! check to make sure that the player is not trying to bank the same die twice
            do while(rolls(bankedDice) .eq. 0)
                write(*,*) "You have already picked that dice"
                write(*,*) "pick another number"

                read(*,*) bankedDice
            enddo

            write(*,*) "You are banking a ", rolls(bankedDice)

            ! saved the banked dice values to the players bank
            playerBank(bankNumber) = rolls(bankedDice)

            ! set that dice value to 0 so the player can't choose it twice
            rolls(bankedDice) = 0

            ! increse the number of banked dice
            bankNumber = bankNumber + 1
        enddo

        ! reduce the number of dice to roll by the number of dice banked
        diceToRoll = diceToRoll - numDiceToBank
    enddo

    ! reset the dice to roll and the bank number values for the next player
    diceToRoll = 6
    bankNumber = 1

end subroutine playerTurn

! subroutine that calculates the players score
subroutine calculatePlayerScore(playerBank, playerScore)

    ! the players banked dice values
    Integer, dimension(6) :: playerBank

    ! the players score
    Integer :: playerScore

    ! variable that checks if the player has banekd a 4
    Integer :: player_4 = 0

    ! variable that checks if the player has banked a 6
    Integer :: player_1 = 0

    ! simulate the players turn
    call playerTurn(playerBank)

    ! loop through all the banked dice and calculate the scores
    do i=1, 6

        ! if the player has banked a 1 than update the 1 banked variable
        if(playerBank(i) .eq. 1 .AND. player_1 .eq. 0) then
            player_1 = 1

        ! if the player has banked a 4 than update the 4 banked variable
        else if(playerBank(i) .eq. 4 .AND. player_4 .eq. 0) then
            player_4 = 1

        else
            ! add all other values to the score
            playerScore = playerScore + playerBank(i)
        endif

    enddo

    write(*,*) player_1

    ! if the player has not banked a 1 and a 4 than set the player score to 0
    if(player_4 .eq. 0 .OR. player_1 .eq. 0) then
        playerScore = 0
    endif

    ! reset the player score values
    player_4 = 0
    player_1 = 0

end subroutine calculatePlayerScore

! main program
program midnight

    ! player 1 bank
    Integer, dimension(6) :: player1Bank

    ! player 2 bank
    Integer, dimension(6) :: player2Bank

    ! player 1 and 2 score variables
    Integer :: player1Score = 0
    Integer :: player2Score = 0

    ! set the player 1 and 2 banks to -1
    do i=1,6

        player1Bank(i) = -1
        player2Bank(i) = -1

    enddo

    ! simulate player 1's turn
    write(*,*) "Player 1's Turn"
    call calculatePlayerScore(player1Bank, player1Score)

    ! simulate player 2's turn
    write(*,*) "Player 2's Turn"
    call calculatePlayerScore(player2Bank, player2Score)

    ! output the banked values for player 1 and 2
    write(*,*) "the bank for player 1 is", player1Bank
    write(*,*) "the bank for player 2 is", player2Bank

    ! output the player scores
    write(*,*) "player 1 score is ", player1Score
    write(*,*) "player 2 score is ", player2Score

    ! find the winner based on who has the higher score
    if(player1Score .gt. player2Score) then
        write(*,*) "player 1 has won"

    else if(player1Score .eq. player2Score) then
        write(*,*) "the players have tied"

    else
        write(*,*) "player 2 has won"

    endif

end program midnight