! Griffin Lehrer Final Exam
! Question 3 Runge-Kutta Method

program runge_kutta

    ! use runga kutta to evaluate y' + y = sin(4 * pi * t) where y(0) = 1 / 2 and h = 1/4 and 1/8

    ! used to make sure that there is no variables are assigned an implicit data type
    implicit none

    ! value for using do loops needed due to implicit none
    integer :: i

    ! variables for holding the k values needed for runge kutta
    real :: k1, k2, k3, k4

    ! variable to hold the y values for the estimations of the equation
    real :: y

    ! variable to hold the current x value for the equation
    real :: t

    ! variable to hold the step (grid) size for the x values
    real :: h

    ! variable to hold the number of steps to take for the runge kutta estimation
    integer :: steps

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

    do i=1, steps

        write(*,*) i

    enddo

    ! loop for calculating runge kutta

end program runge_kutta