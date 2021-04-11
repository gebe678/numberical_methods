! Exam 2 Question 3

! Planet orbiting sun simulation modified for eccentricity (earth, jupiter, neptune)
! Written by Griffin Lehrer

! used page https://en.wikipedia.org/wiki/Ellipse to find the ellipse equation
! also found page http://www.bogan.ca/orbits/kepler/e_anomly.html to help with the quesiton

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

    ! array that hold the eccentricity for the planets
    real, dimension(7) :: planetEccentricity

    ! angle of the planets
    real :: theta = 0

    ! period of the planet
    real :: period

    ! x and y coordinates of the planet
    real :: x, y

    ! ingeter for do loops
    integer :: i

    ! variables for using the ellipse formula
    real :: a, b

    ! pi
    real :: pi = acos(-1.0)

    ! open a file to write the x, y points to
    open(1, file = "values.dat")
    open(2, file="xvalues.dat")
    open(3, file="yvalues.dat")

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

    ! values for the planet eccentricities

    ! Venus
    planetEccentricity(1) = .007

    ! Earth
    planetEccentricity(2) = .017

    ! Mars
    planetEccentricity(3) = .093

    ! Jupiter
    planetEccentricity(4) = .048

    ! Saturn
    planetEccentricity(5) = .056

    ! Uranus
    planetEccentricity(6) = .047

    ! Neptune
    planetEccentricity(7) = .009

    write(*,*)
    write(*,*) "Program to simulate a planets orbit around the sun"
    write(*,*) "Output is x and y values of where the planet will be"
    write(*,*) "each point is where the planet will be when by incrementing 1/20th of earth rotation"
    write(*,*)

    write(*,*) "Please enter a planet to simulate"

    write(*,*) "Enter 1 - 7 for Venus through Neptune"
    read(*,*) planet

    do while(planet .gt. 7 .OR. planet .lt. 1)

        write(*,*) "Invalid planet please enter a number between 1 - 7 for Venus through Netpune"
        read(*,*) planet

    enddo

    if(planet .eq. 1) then
        write(*,*)
        write(*,*) "Simulating Venus"
        write(*,*)

    else if(planet .eq. 2) then
        write(*,*)
        write(*,*) "Simulating Earth"
        write(*,*)
        
    else if(planet .eq. 3) then
        write(*,*)
        write(*,*) "Simulating Mars"
        write(*,*)
        
    else if(planet .eq. 4) then
        write(*,*)
        write(*,*) "Simulating Jupiter"
        write(*,*)
        
    else if(planet .eq. 5) then
        write(*,*)
        write(*,*) "Simulating Saturn"
        write(*,*)
        
    else if(planet .eq. 6) then
        write(*,*)
        write(*,*) "Simulating Uranus"
        write(*,*)
        
    else if(planet .eq. 7) then
        write(*,*)
        write(*,*) "Simulating Neptune"
        write(*,*)

    endif
        
    ! find the perod of the planet
    period = planetDistanceFromSun(planet) ** 3
    period = sqrt(period)


    ! find 20 points for each orbit around the sun
    do i=1, (floor(period * 3) * 20)

        ! starting point for all planets is (1, 0)

        ! if the planet is Earth Jupiter or Neptune than use their eccentricity to find the ellipse orbit
        if(planet .eq. 2 .or. planet .eq. 4 .or. planet .eq. 7) then

            ! the points (x, y) = (a cos(theta), b sin(theta) ) for 0 .ge. theta .le. 2pi
            ! to find a and b use the formula e = c / a = sqrt( 1 - (b ** 2 / a ** 2) )
            ! assume that the height of the orbit does not change but the length does
            ! a = radius of the planet
            ! radius = distance of the planet from the sun

            ! find the b value of the ellipse using the formula sqrt((1 - e^2)a^2)
            a = planetDistanceFromSun(planet)
            b = sqrt((1 - planetEccentricity(planet)) * (a ** 2))

            write(*,*)
            write(*,*) "a:", a
            write(*,*) "b:", b
            write(*,*)

            x = (a * cos(theta))
            y = (b * sin(theta))

            write(*,*) "x:", x
            write(*,*)
            write(*,*) "y:", y
            write(*,*)

        else

            ! find the x and y values of the planet based on the current angle
            ! and the radius of the planet from the sun
            x = planetDistanceFromSun(planet) * cos(theta)
            y = planetDistanceFromSun(planet) * sin(theta)

            write(*,*) "x:", x
            write(*,*)
            write(*,*) "y:", y
            write(*,*)

        endif

        ! find the new theta based on the planet we are looking for
        ! we want to move the planet by 1/20 of its full revolution
        ! we find this by finding 1/20 th of the unit ciricle and dividing by the period of the planet in question

        theta = theta + (((1.0 / 20.0) * 2.0 * pi) / period)
        
        write(*,*)
        write(*,*) "theta", theta
        write(*,*)

        ! write the x and y value to the values.dat file
        write(1, *) "x:", x, "y:", y
        write(2, *) x
        write(3, *) y

    enddo
    
    write(*,*) "X values are located in xvalues.dat"
    write(*,*)
    write(*,*) "Y values are located in yvalues.dat"

    close(1)
    close(2)
    close(3)
end program planetOrbit