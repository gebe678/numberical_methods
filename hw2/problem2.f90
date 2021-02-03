program problem2
    implicit none
    
    real :: age
    real :: carbonRemaining

    write(*,*) "enter a percentage of carbon remaining"
    write(*,*) "percentage should be between 0 and 1"

    read(*,*) carbonRemaining

    do while(carbonRemaining .gt. 1 .OR. carbonRemaining .lt. 0)
        write(*,*) "Incorrect Input please enter a number between 0 and 1"
        read(*,*) carbonRemaining
    enddo

    age = -log(carbonRemaining) / 0.0001216

    write(*,*) "the age is ", age

end program problem2