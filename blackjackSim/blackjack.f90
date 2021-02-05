subroutine createDeck(deck, cardValue)

    Integer, dimension(52) :: deck
    Integer, dimension(52) :: cardValue

    Integer :: count = 1

    do i=1, 52
        deck(i) = 4

        if(count .eq. 14) then
            count = 1
        endif

        if(count .gt. 10) then
            cardValue(i) = 10

        else
            cardValue(i) = count
        endif

        write(*,*) "count is " , count
        count = count + 1
    enddo

end subroutine createDeck

program blackjack
    implicit none

    Integer, dimension(52) :: deck
    Integer, dimension(52) :: cardValue

    Integer :: i
    
    call createDeck(deck, cardValue)

    do i=1, 52
        write(*,*) cardValue(i)
    enddo

end program blackjack