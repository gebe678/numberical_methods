! Griffin Lehrer Final Exam
! Question 1 Exoplanet Transit Modeling

program exoplanet_transit_modeling

    implicit none

    ! counter variable needed due to implicit none
    integer :: i

    ! number that models the percentage of total light from the M2 star
    ! number will be between 0 - 100 0 representing 0% brightness and 100 representing 100% brightness
    real :: star_brightness = 100

    ! variable to find the period of the exoplanet
    ! it is known that the exoplanet has a distance of 1 au from the m2 star
    ! P^2 = a^3
    ! a^3 = 1
    ! sqrt(p) = 1
    real :: period = 1

    ! variables to hold the x and y positions of the exoplanet each rotation
    real :: x, y

    ! variable to hold the theta (angle) for the planet
    real :: theta = 0

    ! pi
    real :: pi = acos(-1.0)

    ! open an output file for the x and y valeus
    open(1, file="x_values.txt")
    open(2, file="y_values.txt")

    ! find points for 3 orbits with 100 points each orbit
    do i = 1, (floor(period * 3) * 100)

        ! find the x and y values using theta as the angle
        x = cos(theta)
        y = sin(theta)

        ! find the new theta based on the planet we are looking for
        ! we want to move the planet by 1/100 of its full revolution
        ! we find this by finding 1/100 th of the unit ciricle (period is always 1 so no division is nescassary)

        write(1, *) x
        write(2, *) y
        
        theta = theta + ((1.0 / 100.0) * 2.0 * pi)

    enddo

    ! close the output files
    close(1)
    close(2)

end program exoplanet_transit_modeling