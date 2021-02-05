subroutine rollDice(x)
    implicit none
    
    integer :: count
    real :: x

    call random_seed()
    call random_number(x)

    x = floor(x * 6) + 1

end subroutine rollDice

subroutine playerTurn(playerBank)

    Integer :: diceToRoll = 6
    real, dimension(6) :: rolls
    Integer, dimension(6) :: playerBank
    Integer :: numDiceToBank
    Integer :: bankedDice

    Integer :: bankNumber = 1

    write(*,*) "the dice to roll is", diceToRoll

    do while(diceToRoll .gt. 0)
        write(*,*) "You have ", diceToRoll, "dice left"

        do i=1,diceToRoll
            call rollDice(rolls(i))
        enddo

        do i = 1, diceToRoll
            write(*,*) "you rolled", rolls(i)
        enddo

        write(*,*) "how many dice would you like to bank?"
        read(*,*) numDiceToBank

        do while(numDiceToBank .gt. diceToRoll .OR. numDiceToBank .le. 0)
            write(*,*) "You are trying to bank more dice then you have"
            write(*,*) "Enter a number greater than 0 and less than", diceToRoll

            read(*,*) numDiceToBank
        enddo

        do i=1,numDiceToBank
            write(*,*) "what die would you like to bank?"
            write(*,*) "Enter a number between 1 and", diceToRoll

            read(*,*) bankedDice

            do while(bankedDice .gt. diceToRoll .OR. bankedDice .le. 0)
                write(*,*) "That is not a valid die"
                write(*,*) "Pick a number between 1 and", diceToRoll

                read(*,*) bankedDice
            enddo

            do while(rolls(bankedDice) .eq. 0)
                write(*,*) "You have already picked that dice"
                write(*,*) "pick another number"

                read(*,*) bankedDice
            enddo

            write(*,*) "You are banking a ", rolls(bankedDice)

            playerBank(bankNumber) = rolls(bankedDice)
            rolls(bankedDice) = 0
            bankNumber = bankNumber + 1
        enddo

        diceToRoll = diceToRoll - numDiceToBank
    enddo

    diceToRoll = 6
    bankNumber = 1

end subroutine playerTurn

subroutine calculatePlayerScore(playerBank, playerScore)

    Integer, dimension(6) :: playerBank
    Integer :: playerScore

    Integer :: player_4 = 0
    Integer :: player_1 = 0

    Integer :: diceToRoll

    call playerTurn(playerBank)

    do i=1, 6
        if(playerBank(i) .eq. 1 .AND. player_1 .eq. 0) then
            player_1 = 1

        else if(playerBank(i) .eq. 4 .AND. player_4 .eq. 0) then
            player_4 = 1

        else
            playerScore = playerScore + playerBank(i)
        endif

    enddo

    write(*,*) player_1

    if(player_4 .eq. 0 .OR. player_1 .eq. 0) then
        playerScore = 0
    endif

    player_4 = 0
    player_1 = 0

end subroutine calculatePlayerScore

program midnight

    Integer, dimension(6) :: player1Bank
    Integer, dimension(6) :: player2Bank

    Integer :: player1Score = 0
    Integer :: player2Score = 0

    Integer :: player2_4 = 0
    Integer :: player2_1 = 0

    do i=1,6

        player1Bank(i) = -1
        player2Bank(i) = -1

    enddo

    write(*,*) "Player 1's Turn"
    call calculatePlayerScore(player1Bank, player1Score)

    write(*,*) "Player 2's Turn"
    call calculatePlayerScore(player2Bank, player2Score)

    write(*,*) "the bank for player 1 is", player1Bank
    write(*,*) "the bank for player 2 is", player2Bank

    write(*,*) "player 1 score is ", player1Score
    write(*,*) "player 2 score is ", player2Score

    if(player1Score .gt. player2Score) then
        write(*,*) "player 1 has won"

    else if(player1Score .eq. player2Score) then
        write(*,*) "the players have tied"

    else
        write(*,*) "player 2 has won"

    endif

end program midnight