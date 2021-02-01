program quadraticFormula
    implicit none
    
    ! Variable declarations for the variables in the quadaratic formula
    Real a, b, c
    Real positive, negative, positiveSolution, negativeSolution

    ! instruction for the user
    write(*,*) "Enter the value for A"

    ! Read the value entered by the user
    read(*, *) a

    ! instruction for the user
    write(*,*) "Enter the value for B"

    ! Read the value entered by the user
    read(*, *) b

    ! instruction for the user
    write(*,*) "Enter the value for C"

    ! Read the value entered by the user
    read(*, *) c

    if((b ** 2) - (4 * a * c) .lt. 0) then
        write(*, *) "There is no solution to your problem imaginary number needed"
        STOP
    endif

    positive = -b + sqrt((b ** 2) - (4 * a * c))
    negative = -b - sqrt((b ** 2) - (4 * a * c))

    positiveSolution = positive / (2 * a)
    negativeSolution = negative / (2 * a)

    write(*,*) "x = ", positiveSolution, "and x = ", negativeSolution
    
end program quadraticFormula