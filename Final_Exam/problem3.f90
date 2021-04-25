! Griffin Lehrer Final Exam
! Question 3 Runge-Kutta Method

program runge_kutta

    ! use runga kutta to evaluate y' + y = sin(4 * pi * t) where y(0) = 1 / 2 and h = 1/4 and 1/8

    ! used to make sure that there is no variables are assigned an implicit data type
    implicit none

    ! value for using do loops needed due to implicit none
    integer :: i

    ! variables for holding the k values needed for runge kutta
    real :: k1 = 0, k2 = 0, k3 = 0, k4 = 0

    ! variable to hold the y values for the estimations of the equation
    real :: y

    ! variable to hold the current x value for the equation
    real :: t

    ! variables to hold the caluclations of the y value and x value
    real :: y_value, x_value

    ! variable to hold the step (grid) size for the x values
    real :: h

    ! value to hold pi
    real :: pi = 4.0 * atan(1.0)

    ! variable to hold the number of steps to take for the runge kutta estimation
    integer :: steps

    ! initial value for y and t and h
    y = .5
    t = 0
    h = .25

    ! tell the user the purpose of the program
    write(*,*)
    write(*,*) "Uses Runge Kutta to estimate the value of a differential equation"
    write(*,*)
    write(*,*) "Differential equation:        y' + y = sin(4 * pi * t)"
    write(*,*)
    write(*,*) "Initial Conditions: y(0) = .5 and h = .25 and h = .125"
    write(*,*)

    ! get the number of estimations to make from the user
    write(*,*) "Please enter the amount of steps to take for the estimation"
    read(*,*) steps
    write(*,*)

    ! loop for calculating runge kutta with h = .25
    do i=1, steps

        ! find the first k value using the runge ktta formula
        k1 = h * (-y + sin(4 * pi * t))

        ! find the second k value using the runge kutta formula
        x_value = t + (.5 * h)
        y_value = y + (.5 * k1)

        ! plug the x and y values into the function to find the k2 value
        k2 = h * ( -y_value + sin(4 * pi * x_value) )

        ! find the third k value using the runge kutta formula
        x_value = t + (.5 * h)
        y_value = y + (.5 * k2)

        ! plug the x and y value into the function to find the k3 value
        k3 = h * ( -y_value + sin(4 * pi * x_value) )

        ! find the fourth k value using the runge kutta formula
        x_value = t + h
        y_value = y + k3

        k4 = h * (-y_value + sin(4 * pi * x_value))

        y = y + ( (1.0/6.0) * (k1 + (2.0 * k2) + (2.0 * k3) + k4) )

        t = t + h

        ! print out all of the steps to the user
        ! write(*,*)
        ! write(*,*) "n: ", i
        ! write(*,*)
        ! write(*,*) "t: ", t
        ! write(*,*)
        ! write(*,*) "h: ", h
        ! write(*,*) 
        ! write(*,*) "k1: ", k1
        ! write(*,*)
        ! write(*,*) "k2: ", k2
        ! write(*,*)
        ! write(*,*) "k3: ", k3
        ! write(*,*)
        ! write(*,*) "k4: ", k4
        ! write(*,*)
        ! write(*,*) "y: ", y

    enddo

    write(*,*) 
    write(*,*) "the value of the differential equation with h = .25 and", steps, "steps is: ", y
    write(*,*)

    ! reset the values for the runge kutta
    y = .5
    t = 0
    h = .125

    ! loop for calculating runge kutta with h = .125
    do i=1, steps

        ! find the first k value using the runge ktta formula
        k1 = h * (-y + sin(4 * pi * t))

        ! find the second k value using the runge kutta formula
        x_value = t + (.5 * h)
        y_value = y + (.5 * k1)

        ! plug the x and y values into the function to find the k2 value
        k2 = h * ( -y_value + sin(4 * pi * x_value) )

        ! find the third k value using the runge kutta formula
        x_value = t + (.5 * h)
        y_value = y + (.5 * k2)

        ! plug the x and y value into the function to find the k3 value
        k3 = h * ( -y_value + sin(4 * pi * x_value) )

        ! find the fourth k value using the runge kutta formula
        x_value = t + h
        y_value = y + k3

        k4 = h * (-y_value + sin(4 * pi * x_value))

        y = y + ( (1.0/6.0) * (k1 + (2.0 * k2) + (2.0 * k3) + k4) )

        t = t + h

        ! print out all of the steps to the user
        ! write(*,*)
        ! write(*,*) "n: ", i
        ! write(*,*)
        ! write(*,*) "t: ", t
        ! write(*,*)
        ! write(*,*) "h: ", h
        ! write(*,*) 
        ! write(*,*) "k1: ", k1
        ! write(*,*)
        ! write(*,*) "k2: ", k2
        ! write(*,*)
        ! write(*,*) "k3: ", k3
        ! write(*,*)
        ! write(*,*) "k4: ", k4
        ! write(*,*)
        ! write(*,*) "y: ", y

    enddo

    write(*,*) 
    write(*,*) "the value of the differential equation with h = .25 and", steps, "steps is: ", y
    write(*,*)

end program runge_kutta