program zork
    implicit none;

    ! Input from the user to determin what coices will be made throughout
    ! the game
    Character(len=50):: userInput

    ! Game state will save which path the player is on and will add the correct output and choice
    ! to display to the user
    Integer:: state

    ! Start of the story write this initially to the screen
    write(*,*) "You wake up pleased "

    ! Get the first user input
    read(*,'(A)') userInput

    ! write the user input to the screen
    write(*,'(A)') userInput

    ! First choice branch

end program zork