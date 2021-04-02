! Planet orbiting sun simulation
! Written by Griffin Lehrer and Jacob Buckelew

! 1: Venus
! 2: Earth
! 3: Mars
! 4: Jupiter
! 5: Saturn
! 6: Uranus
! 7: Neptune
program planetOrbit
    implicit none

    ! variable controlling what planet the user is simulating
    integer :: planet

    ! array that holds the planets distance from the sun (Radius)
    real, dimension(7) :: planetDistanceFromSun

    ! angle of the planets
    real :: theta = 0

    ! period of the planet
    real :: period

    ! x and y coordinates of the planet
    real :: x, y

    ! ingeter for do loops
    integer :: i

    ! pi
    real :: pi = acos(-1.0)

    ! open a file to write the x, y points to
    open(1, file = "values.dat")
    open(2, file="xvlaues")
    open(3, file="yvalues")

    write(*,*) "pi ", pi
    
    ! values for the distance from the planet to the sun
    ! distances units in AU

    ! Venus
    planetDistanceFromSun(1) = .723

    ! Earth
    planetDistanceFromSun(2) = 1

    ! Mars
    planetDistanceFromSun(3) = 1.542

    ! Jupiter
    planetDistanceFromSun(4) = 5.203

    ! Saturn
    planetDistanceFromSun(5) = 9.539

    ! Uranus
    planetDistanceFromSun(6) = 19.18

    ! Neptune
    planetDistanceFromSun(7) = 30.06

    write(*,*) "Please enter a planet to simulate"

    write(*,*) "Enter 1 - 7 for Venus through Neptune"
    read(*,*) planet

    do while(planet .gt. 7 .OR. planet .lt. 1)

        write(*,*) "Invalid planet please enter a number between 1 - 7 for Venus through Netpune"
        read(*,*) planet

    enddo

    ! find the perod of the planet
    period = planetDistanceFromSun(planet) ** 3
    period = sqrt(period)


    ! find 20 points for each orbit around the sun
    do i=1, (floor(period * 3) * 20)

        ! starting point for all planets is (1, 0)

        ! find the x and y values of the planet based on the current angle
        ! and the radius of the planet from the sun
        x = planetDistanceFromSun(planet) * cos(theta)
        y = planetDistanceFromSun(planet) * sin(theta)

        ! find the new theta based on the planet we are looking for
        ! we want to move the planet by 1/20 of its full revolution
        ! we find this by finding 1/20 th of the unit ciricle and dividing by the period of the planet in question
        
        theta = theta + (((1.0 / 20.0) * 2.0 * pi) / period)
        write(*,*) "theta", theta

        ! write the x and y value to the vlaues.dat file
        write(1, *) "x:", x, "y:", y
        write(2, *) x
        write(3, *) y

    enddo
    
    close(1)
    close(2)
    close(3)
end program planetOrbit