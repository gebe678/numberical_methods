! Griffin Lehrer Exam 1 Question 2

! Root Finder Program

! used following source to allocate an array size dynamically: 
! https://www.tutorialspoint.com/fortran/fortran_dynamic_arrays.htm

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

    ! variables to hold the range of the polynomial being searched
    real :: lowerBound
    real :: upperBound

    ! variable to hold the acceptable tolerance of the acual value
    real :: tolerance

    ! variable used in the polynomial equation
    real :: x

    ! Prompt the user for the length of the polynomial
    write(*,*) "How many terms would you like the polynomial to have?"
    read(*,*) numberOfElements

    ! Prompt the user for the lower bound of the searched area
    write(*,*) "Enter the lower bound of the range to search"
    read(*,*) lowerBound

    ! Prompt the user for the upper bound of the searched area
    write(*,*) "Enter the upper bound of the range to search"
    read(*,*) upperBound

    ! prompt the user for the tolerance of error
    write(*,*) "Please enter the tolerance of the acual value"
    read(*,*) tolerance

    ! allocate the number of elements to the dynamic array
    ! one spot in the array will save the coefficient the next will save the power of the variable
    allocate (polynomial(numberOfElements * 2))

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

    do i=1, numberOfELements * 2
        write(*,*) polynomial(i)
    enddo

    ! prompt the user to use the bisection method vs the false position method

    write(*,*) "press 1 to use the bisection method and 2 to use the false position method"
    read(*,*) method

    do while(method .ne. 1 .AND. method .ne. 2) 
        write(*,*) "invalid selection plase press 1 to use the bisection method and 2 to use the false position method"
        read(*,*) method
    enddo

    ! if then statement to determine which method the user slected to find the root
    ! bisection method selected
    if(method .eq. 1) then

        

    ! false position method selectec
    else if(method .eq. 2) then

    ! Error code statment should be impossible to trigger as method should not be able to be anything other than 1 or 2
    else
        write(*,*) "ERROR invalid root finding method selected terminating program"

    endif

    ! deallocate the memory for the dynamic array
    ! this will free up memory for future use
    deallocate(polynomial)
end program rootFinder