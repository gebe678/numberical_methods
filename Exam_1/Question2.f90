! Griffin Lehrer Exam 1 Question 2

! Root Finder Program

! used following source to allocate an array size dynamically: 
! https://www.tutorialspoint.com/fortran/fortran_dynamic_arrays.htm

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

program rootFinder

    implicit none

    ! interator variable used in do loops due to implicit none
    integer :: i

    ! number value to choose if the user wants to use the bisection method or the false position method
    integer :: method

    ! Variables to get the polynomial from the user
    ! Polynomial parts include:
    ! Number of elements
    ! coefficient of the polynomial
    ! power of the term

    ! Number of elements the polynomial will contain
    integer :: numberOfElements

    ! variable to hold the current index of the polynomial
    integer :: polynomialIndex = 1

    ! polynomial array odd indicies hold the coefficient value
    ! even indicies hold the exponent value
    real, dimension(:), allocatable  :: polynomial

    ! variables to hold the range of the polynomial being searched and the midpoint (or c value if false position method is used)
    real :: lowerBound
    real :: upperBound
    real :: midpoint

    ! variable to hold the acceptable tolerance of the acual value
    real :: tolerance

    ! variable that holds the value of the polynomial calcuated at x
    real :: answer

    ! variable that will hold the last approximated value for the root
    ! this value will start at 0 so the tolerance must be less than 1
    ! or the method will only approximate the value once
    ! used to calculate the error
    real :: lastApproxValue = 0

    ! variable to hold the current error value
    ! initialized to 1 so the user must select a tolerance greater than 0 and less than 1
    real :: error = 1

    ! variables that store the value of the polynomial at the beginning (A), end (B), and midpoint
    ! (or c if the false position method is used) to find the roots of the funciton
    real :: answerA, answerB, answerC

    ! variable containing the number of steps (loops) to find the root within the tolerance range
    Integer :: steps = 1

    ! variable to check a halting condition for the false position method
    Integer :: stopper = 1

    ! Prompt the user for the length of the polynomial
    write(*,*) "How many terms would you like the polynomial to have?"
    write(*,*) "Please enter a whole number greater than 1"
    read(*,*) numberOfElements

    ! if the there are no elements in the polynomail ask the user for new input
    do while(numberOfElements .le. 0)
        write(*,*) "Invalid input enter a whole number greater than 0"
        read(*,*) numberOfElements
    enddo

    ! Prompt the user for the lower bound of the searched area
    write(*,*) "Enter the lower bound of the range to search"
    read(*,*) lowerBound

    ! Prompt the user for the upper bound of the searched area
    write(*,*) "Enter the upper bound of the range to search"
    read(*,*) upperBound

    ! check to make sure the lower bound is less than the upper bound
    ! prompt the user to enter an upper bound of the correct size if
    ! the above check is false
    do while(upperBound .le. lowerBound)

        write(*,*) "The upperbound can not be smaller than the lower bound."
        write(*,*) "Please enter an upper bound greater than", lowerBound
        read(*,*) upperBound

    enddo

    ! prompt the user for the tolerance of error
    write(*,*) "Please enter the tolerance of the acual value must select a value less than 1 and greater than 0"
    read(*,*) tolerance
    write(*,*) tolerance

    do while(tolerance .ge. 1 .OR. tolerance .le. 0)
        write(*,*) "Invalid tolerance. Tolerance selection must be less than 1 and greater than 0"
        read(*,*) tolerance
    enddo

    ! allocate the number of elements to the dynamic array
    ! one spot in the array will save the coefficient the next will save the power of the variable
    allocate (polynomial(numberOfElements * 2))

    ! get the elements of the polynomial from the user
    do i=1, numberOfElements

        ! prompt the user for the coefficient of the polynomial
        write(*,*) "please enter the coefficient of the polynomial"
        read(*,*) polynomial(polynomialIndex)
        polynomialIndex = polynomialIndex + 1

        ! prompt the user for the power of the exponent
        write(*,*) "please enter the power of the exponent for this term"
        read(*,*) polynomial(polynomialIndex)
        polynomialIndex = polynomialIndex + 1

    enddo

    ! print out all of the polynomial elements
    do i=1, numberOfELements * 2
        write(*,*) polynomial(i)
    enddo

    ! prompt the user to use the bisection method vs the false position method
    write(*,*) "press 1 to use the bisection method and 2 to use the false position method"
    read(*,*) method

    ! if the user selects incorrect method types
    do while(method .ne. 1 .AND. method .ne. 2) 
        write(*,*) "invalid selection plase press 1 to use the bisection method and 2 to use the false position method"
        read(*,*) method
    enddo

    ! evaluate the given polynomail at the lower bound and save the answer into
    ! answerA
    ! reset the answer before calling the evaluation
    answerA = 0
    call evaluatePolynomial(numberOfElements, polynomial, lowerbound, answerA)
    write(*,*) answerA, "function evaluated at the lower bound"

    ! evaluate the given polynomial at the upper bound and save the answer into
    ! answerB
    ! reset the answer before calling the evaluation
    answerB = 0
    call evaluatePolynomial(numberOfElements, polynomial, upperBound, answerB)
    write(*,*) answerB, "function evaluated at the upper bound"
    write(*,*)

    ! if statement will determine if a root exists in the given range
    if(answerA * answerB .lt. 0) then
        write(*,*) "A root has been dectected"
        
        ! print out the tolerance and the error to the user
        write(*,*) "Tolerance is", tolerance
        write(*,*) "Error is:", error
        write(*,*)

        ! keep searching for the root until the error is greater than the tolerance
        do while(error .ge. tolerance .or. stopper .eq. 1)
            ! if then statement to determine which method the user slected to find the root
            ! bisection method selected
            if(method .eq. 1) then
                ! find the midpoint of the bound
                midpoint = (lowerBound + upperBound) / 2
                stopper = 0

            ! false position method selected
            else if(method .eq. 2) then
                ! set the stopping condition to false
                stopper = 1
                !calculate the point to check based on the formula
                ! calculate the midpoint based on the updated bounds
                midpoint = ((lowerBound * answerB) - (upperBound * answerA)) / (answerB - answerA)

            ! Error code statment should be impossible to trigger as method should not be able to be anything other than 1 or 2
            else
                write(*,*) "ERROR invalid root finding method selected terminating program"
                STOP
            endif

            ! evaluate the polynomial at the midpoint
            ! reset the answer before calling the polynomial
            answerC = 0
            call evaluatePolynomial(numberOfElements, polynomial, midpoint, answerC)
            write(*,*) "f(c)", answerC, "function evalutated at the midpoint", midpoint

            ! if there is a root at the c value than print out the root value
            if(answerC .eq. 0) then

                write(*,*) "Root found at point", midpoint, "with", steps, "steps"
                stop

            endif

            ! the root is is between the lower bound and the midpoint
            ! reset the upper bound to the midpoint and recalculate the root
            if(answerA * answerC .lt. 0) then

                ! calculate the error if the false position method was selected
                if(method .eq. 2) then
                    error = upperBound - midpoint

                ! calculate the error if the bisection method was selected
                else if(method .eq. 1) then
                    error = upperbound - lowerBound
                endif

                ! reset the upperbound to the midpoint
                upperBound = midpoint

                ! reset f(b) to the f(c) value (c is the midpoint)
                answerB = answerC

                ! print out the interval to the user
                write(*,*) "the new interval is from", lowerBound, upperBound

                ! increment the steps taken to find the root
                steps = steps + 1
                
            ! the root is between the upper bound and the midpoint
            ! reset the lower bound to the midpoint
            else if(answerB * answerC .lt. 0) then

                ! calculate the error if the false position method was chosen
                if(method .eq. 2) then
                    error = midpoint - lowerBound

                ! calculate the error if the bisection method was chosen
                else if(method .eq. 1) then
                    error = upperbound - lowerBound
                endif

                ! update the lowerbound value to the midpoint
                lowerBound = midpoint

                ! update the f(a) value to the f(c) value (c is the midpoint)
                answerA = answerC

                ! print out the interval to the user
                write(*,*) "the new interval is from", lowerBound, upperBound
                steps = steps + 1

            endif

            ! if the false position method was chosen than this additional condition must also be true
            ! found this method at: https://ece.uwaterloo.ca/~dwharder/NumericalAnalysis/10RootFinding/falseposition/ 
            ! condidition is that f(a) or f(b) must be less than the tolerance for the false position method to stop
            if(method .eq. 2) then
                if(abs(answerA) .lt. tolerance)  then      
                     stopper = 0
                else if(abs(answerB) .lt. tolerance) then
                    stopper = 0
                 endif
            endif

            ! print out useful information to the user
            write(*,*) "lower bound", lowerBound
            write(*,*) "upper bound", upperBound
            write(*,*) "answerA", answerA
            write(*,*) "answerB", answerB
            write(*,*) "midpoint", midpoint
            write(*,*) "answerC", answerC
            write(*,*) "error is", error, tolerance
            write(*,*) "stopper is", stopper
            write(*,*)
        enddo

        ! print out the root and the midpoint if found
        write(*,*) "The root is at point", midpoint, "and was found in", steps, "steps"
        write(*,*) "the error is", error, "the stopper is", stopper

        ! write suggested next interval to search to the user
        write(*,*) "one possible root found to find more roots enter a new interval"
        write(*,*) "suggested endpoint for an interval is", midpoint + tolerance, "and", midpoint - tolerance

    ! if the root was found at f(b) or f(a) print that out to the user
    else if(answerA .eq. 0) then
        write(*,*) "root found at", lowerBound
    else if(answerB .eq. 0) then
        write(*,*) "root found at", upperBound
    ! No root exists in the given range
    else
        write(*,*) "There is an even number of roots or no roots in the given range"
        write(*,*) "Unable to detect the roots (if they exist)"

    endif

    ! deallocate the memory for the dynamic array
    ! this will free up memory for future use
    deallocate(polynomial)

end program rootFinder