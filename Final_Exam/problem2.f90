! Griffin Lehrer Final Exam
! Question 2 Unique planetary bodies question

! subroutine to calculate the least squares fit on given points
subroutine least_squares(num_points, x_values, y_values, slope, intercept)

    implicit none

    ! value with the number of points that are in the function
    integer :: num_points

    ! real array containing all of the x points in the data
    real, dimension(num_points) :: x_values

    ! real array containing all of the y value in the data
    real, dimension(num_points) :: y_values

    ! variable to hold the slope after calculating the least squares
    real :: slope

    ! variable to hold the y-intercept after calculating the least squares
    real :: intercept

    ! counter variable for do loopes needed due to implicit none
    integer :: i

    ! variable to hold the x^2 values sum
    real :: x2_values_sum = 0

    ! variable to hold the x * y sum
    real :: xy_values_sum = 0

    ! variable to hold the x values sum
    real :: x_values_sum = 0

    ! variable to hold the y values sum
    real :: y_values_sum = 0

    ! loop through all of the x and y values to calculate the summations
    do i=1, num_points

        ! summation of the x ^ 2 values
        x2_values_sum = x2_values_sum + (x_values(i) ** 2)

        ! summation of the xy values
        xy_values_sum = xy_values_sum + (x_values(i) * y_values(i))

        ! summation of the x values
        x_values_sum = x_values_sum + x_values(i)

        ! summation of the y values
        y_values_sum = y_values_sum + y_values(i)
        
    enddo

    slope = ((num_points * xy_values_sum) - (x_values_sum * y_values_sum)) / ((num_points * x2_values_sum) - (x_values_sum ** 2))
    intercept = (y_values_sum - (slope * x_values_sum)) / (num_points)

    ! reset all of the summation variables
    x2_values_sum = 0
    xy_values_sum = 0
    x_values_sum = 0
    y_values_sum = 0

end subroutine least_squares

program uniquePlanetaryBody

    implicit none

    ! value to get the slope of the least squares run
    real :: slope

    ! variable to get the intercept of the least squares run
    real :: intercept

    ! array values for all planets
    real, dimension(8) :: x_values
    real, dimension(8) :: y_values

    ! array values for the first four planetary bodies (t planets)
    real, dimension(4) :: t_x_values
    real, dimension(4) :: t_y_values

    ! array values for the last four planetary bodies (g planets)
    real, dimension(4) :: g_x_values
    real, dimension(4) :: g_y_values

    ! initialize the points for all of the planetary bodies
    x_values(1) = 0.242
    x_values(2) = 0.616
    x_values(3) = 1.00
    x_values(4) = 1.881
    x_values(5) = 11.86
    x_values(6) = 29.33
    x_values(7) = 84.32
    x_values(8) = 164.79

    y_values(1) = 0.338
    y_values(2) = 0.724
    y_values(3) = 1.00
    y_values(4) = 1.542
    y_values(5) = 5.20
    y_values(6) = 9.51
    y_values(7) = 19.23
    y_values(8) = 30.10

    ! initialize the points for the first four planetary bodies
    t_x_values(1) = 0.242
    t_x_values(2) = 0.616
    t_x_values(3) = 1.00
    t_x_values(4) = 1.881

    t_y_values(1) = 0.338
    t_y_values(2) = 0.724
    t_y_values(3) = 1.00
    t_y_values(4) = 1.542

    ! initialize the points for the last four planetary bodies
    g_x_values(1) = 11.86
    g_x_values(2) = 29.33
    g_x_values(3) = 84.32
    g_x_values(4) = 164.79

    g_y_values(1) = 5.20
    g_y_values(2) = 9.51
    g_y_values(3) = 19.23
    g_y_values(4) = 30.10

    ! run the least squares fit on the full dataset
    call least_squares(8, x_values, y_values, slope, intercept)

    ! print out the line equation to the user for all of the planets
    write(*,*)
    write(*,*) "equation of the line for all of the planets"
    write(*,*) "y = ", slope, "x + ", intercept
    write(*,*)

    ! run the least squares fit on the t planets
    call least_squares(4, t_x_values, t_y_values, slope, intercept)

    ! print out the line equation to the user for the T planets
    write(*,*)
    write(*,*) "equation of the line for the T planets"
    write(*,*) "y = ", slope, "x + ", intercept
    write(*,*)

    ! run the least squares fit on the g planets
    call least_squares(4, g_x_values, g_y_values, slope, intercept)

    ! print out the line equation to the user for the G planets
    write(*,*)
    write(*,*) "equation of the line for the G planets"
    write(*,*) "y = ", slope, "x + ", intercept
    write(*,*)

end program uniquePlanetaryBody

