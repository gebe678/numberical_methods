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

    ! variable controlling what planet the user is simulating
    integer :: planet

    ! array that holds the planets distance from the sun (Radius)
    real, dimension(7) :: planetDistanceFromSun

    ! angle of the planets
    real :: theta = 0

    ! period of the planet
    real :: period
    
    ! values for the distance from the planet to the sun
    ! distances units in AU
    planetDistanceFromSun(1) = .723
    planetDistanceFromSun(2) = 1
    planetDistanceFromSun(3) = 1.542
    planetDistanceFromSun(4) = 5.203
    planetDistanceFromSun(5) = 9.539
    planetDistanceFromSun(6) = 19.18
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

    write(*,*) period
end program planetOrbit