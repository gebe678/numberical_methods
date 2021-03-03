! Griffin Lehrer
! Program to solve the differential equation y' = -2y +x^3 *e^(2x)

program diffSolver

    ! makes sure that all variables are explicitly declared
    implicit none

    ! declaration of x y and h
    ! x is the x value in the function
    ! y is the y value in the function
    ! h is the step size for the algorithm
    real :: x, y, h

    ! declaration of diffEquation
    ! diffEquation is the value of f(x, y)
    real :: diffEquation

    ! declaration of i and numSteps
    ! i is a variable used to interatate through the do loop
    ! numSteps is the number of times the algorithm is executed (number of times the do loop runs)
    integer :: i, numSteps

    ! prompts for the user
    write(*,*) "calculating the solution to the following differential equation"
    write(*,*)
    write(*,*) "y' = -2y +x^3 *e^(2x)"

    ! gets the step size for the user
    write(*,*)
    write(*,*) "Please choose a step size"
    read(*,*) h

    ! gets the number of steps from the user
    write(*,*) "Please enter a number of steps"
    read(*,*) numSteps

    ! initial values for x and y
    y = 2
    x = 0

    ! initial value of the differential equation
    diffEquation = x - y

    ! do loop to execute the algorithm numSteps times
    do i=1,numSteps

        ! find the an estimation of the differential for the ith step
        y = y + h * diffEquation

        ! increment the x value by the step size
        x = x + h

        ! increment the diffEquation value with the new x and y values
        diffEquation = x - y

        ! print the diffEquation to the user
        write(*,*) "the equation is ", diffEquation

        ! print the x value to the user
        write(*,*) "x value is ", x

        ! write the estimation of the equation to the user
        write(*,*) i, ": the value for y is ", y

    enddo

    write(*,*) "The value for the equation is ", y

end program diffSolver