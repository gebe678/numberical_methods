subroutine rollDice(x)
    implicit none
    
    integer :: count
    real :: x

    call random_seed()
    call random_number(x)

    x = floor(x * 6) + 1

end subroutine rollDice

program midnight

    Integer, dimension(6) :: bank
    real :: one

    call rollDice(one)

    write(*,*) one
end program midnight