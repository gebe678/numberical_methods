! Griffin Lehrer Exam 2 Problem 2

! Newton / Secant root finding problem

! Subroutine to evaluate a polynomial stored in an array at a specific value
subroutine evaluatePolynomial(polynomialLength, polynomial, x, answer)

    implicit none

    ! length of the polynomial this is user supplied input
    Integer :: polynomialLength

    ! an array containing polynomial data
    ! data stored as (coefficient, exponent) values
    real, dimension(polynomialLength * 2) :: polynomial

    ! variable that the function will be evaluated at
    real :: x

    ! the result of evaluating the polynomial at the x value
    real :: answer

    ! iterator for the do loop (needed due to implicit none)
    integer :: i

    ! iterator for the polynomial step needed to to the coefficient, exponent data stored
    integer :: polyIterator = 1

    ! array to hold the values for each term in the polynomial they then will be added together and
    ! saved in the answer variable
    real, dimension(polynomialLength) :: termCalculations

    ! will go through the polynomial term by term and calculate coefficient * x ^ exponent
    do i=1, polynomialLength

        ! get the coefficient from the polynomial array
        termCalculations(i) = polynomial(polyIterator)

        ! increment the polynomial index
        polyIterator = polyIterator + 1

        ! multiply the coefficient by x ^ exponent data
        termCalculations(i) = termCalculations(i) * (x ** polynomial(polyIterator))
        polyIterator = polyIterator + 1

    enddo

    ! will loop through all of the terms in the polynomial and add them together
    do i=1, polynomialLength

        answer = answer + termCalculations(i)

    enddo

    ! reset the polyIterator for the next polynomial to be evaluated
    polyIterator = 1

end subroutine evaluatePolynomial


! subroutine for finding the roots using Newtons Method
subroutine newtonsMethod(roots, polynomial)

    ! variable to hold a root of the equation
    real, dimension(2) :: roots

    ! array to hold the polynomial coefficient / power
    real, dimension(6) :: polynomial

    ! variable to hold the current x value
    real :: x

    ! variable to hold the evaluation of the polynomaial at x
    real :: answer = 0

    ! f(x) =  x^6 - x - 1
    ! f'(x) = 6x^5 - 1

    ! intialize both roots to -10 (outside the given range)
    roots(1) = -10
    roots(2) = -10

    write(*,*) "Please enter a beginning start value for x within the range -2, 2"
    read(*,*) x

    do while(x .lt. -2 .or. x .gt. 2)
        write(*,*) "X value ouside the given range -2, 2 please enter an x value within the range -2, 2"
        read(*,*) x
    enddo

    do i=1, 6

        

    enddo

    call evaluatePolynomial(3, polynomial, x, answer)

    write(*,*) "Polynomial evaluated at", x, "is", answer

end subroutine newtonsMethod

! subroutine for finding the roots using the secant method
subroutine secantMethod()

    write(*,*) "secant method"

end subroutine secantMethod

! beginning of the program
program newton_secant_roots

    implicit none

    ! type of method used to determine the roots of the equation
    integer :: method

    ! variable used for iterating through loops
    integer :: i

    ! array used to hold the roots of the equation
    real, dimension(2) :: roots

    ! polynomial array coefficient / exponent
    real, dimension(6) :: polynomial

    ! initialize the polynomial
    polynomial(1) = 1.0
    polynomial(2) = 6.0
    polynomial(3) = -1.0
    polynomial(4) = 1.0
    polynomial(5) = -1.0
    polynomial(6) = 0.0

    ! get user data to choose newton or secant method
    write(*,*) "Program to find the roots of the equation x^6 - x - 1"
    write(*,*)
    write(*,*) "Please enter 1 to use Newtons method or enter 2 to use the Secant method"
    read(*,*) method

    ! get correct data from the user if input other than 1 or 2 is entered
    do while(method .gt. 2 .or. method .lt. 1)

        write(*,*) "Invalid choice. Please enter 1 to use Newtons method or 2 to use the Secant method"
        read(*,*) method

    enddo

    ! user has selected Newtons Method
    if(method .eq. 1) then

        call newtonsMethod(roots, polynomial)

        do i=1, 2

            write(*,*) "roots are", roots(i)

        enddo

    ! user has selected the Secant Method
    else if(method .eq. 2) then

        call secantMethod

    endif

end program newton_secant_roots