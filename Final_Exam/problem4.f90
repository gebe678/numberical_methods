! Griffin Lehrer Final Exam
! Question 4 Euler - Trapeziod Predictor - Corrector

program predictor_corrector

    implicit none

    ! variable to hold the h value for the corrector method
    real :: h

    ! variable to hold the y value and x value (t) of the differential equation
    real :: y, t, t1

    ! arrays to hold the corrected values of the predictor corrector method
    real, dimension(:), allocatable :: t_values
    real, dimension(:), allocatable :: y_values

    ! varible to use for iterating needed due to implicit none
    integer :: i, j

    ! variable to hold the number of steps used
    integer :: steps = 0

    ! set the initial values for y and t
    y = 2
    t = 1

    ! give the user information about the program

    write(*,*)
    write(*,*) "Uses the Euler-Trapeziod method to integrate a function"
    write(*,*)
    write(*,*) "Function to integrate: y' = t + y"
    write(*,*)
    write(*,*) "Initial Conditions y(1) = 2 over the range [1,5]"
    write(*,*)

    write(*,*) "Please enter a value for h greater than 0 and less than 1"
    read(*,*)h

    ! make sure that the h value inputted by the user is valid
    ! valid if between 0 and 1 (1 included 0 excluded)
    do while(h .le. 0 .or. h .gt. 1)

        write(*,*) "Invalid choice for h please enter a number greater than 0 and less than 1"
        read(*,*)h

    enddo

    ! find the number of steps based on the user defined h value
    do while(t .le. 5)
        t = t + h
        steps = steps + 1
    enddo

    ! reset t
    t = 1

    ! allocate the arrays with the number of steps that are needed to get the range
    allocate(t_values(steps))
    allocate(y_values(steps))

    ! set the inital values for the corrected y and the t_values array
    t_values(1) = 1
    y_values(1) = 2

    i = 2
    do j = 1, steps

        t1 = t + h

        ! predict the value using the predict formula of th eeuler-trapezoid method
        y = y_values(i - 1) + (h * (t + y_values(i - 1)))

        ! correct the value using the correct formula and save it into the array
        y_values(i) = y_values(i - 1) + ( (.5 * h) * ( (t1 + y) + (t + y_values(i - 1)) ) )

        ! increment the t value for the next iteration
        t = t + h

        ! stop the loop if the t value becomes greater than 5
        if(t .gt. 5) then
            exit
        endif

        ! save the current t value into the array
        t_values(i) = t1

        t = t1
        ! increment the iterator
        i = i + 1
    enddo

    write(*,*)

    do j=1, steps

        write(*,*) "X: ", t_values(j), "Y: ", y_values(j)

    enddo

    ! deallocate the arrays
    deallocate(t_values)
    deallocate(y_values)

end program predictor_corrector