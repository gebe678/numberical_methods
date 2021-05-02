! Griffin Lehrer Final Exam
! Question 1 Exoplanet Transit Modeling

! --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

! sources:
! https://sites.uni.edu/morgans/astro/course/Notes/section2/spectraltemps.html
! https://verse-and-dimensions.fandom.com/wiki/Class_M_star
! https://en.wikipedia.org/wiki/Red_dwarf
! https://www.google.com/search?q=radius+of+mercury+in+solar+radius&sxsrf=ALeKk01m8hQNilBraDyDP4boCHyy7ZiBEg%3A1619798777389&source=hp&ei=-SqMYLioFZHhxgH3taDAAQ&iflsig=AINFCbYAAAAAYIw5CVGhuBXt_Gx0WOWib8qjHEy5ALwy&oq=radius+of+mercury+in+solar+radius&gs_lcp=Cgdnd3Mtd2l6EAM6BAgjECc6BQgAEJECOgQIABBDOgIIADoICC4QxwEQowI6CAgAELEDEIMBOggILhCxAxCDAToFCAAQsQM6BwgAELEDEEM6BAguEEM6CAgAELEDEJECOgcIABBGEPsBOgYIABAWEB46CAghEBYQHRAeOgcIIRAKEKABUOYCWKtxYM9yaAJwAHgAgAFxiAHdE5IBBDMzLjKYAQCgAQGqAQdnd3Mtd2l6&sclient=gws-wiz&ved=0ahUKEwj4iO3VrKbwAhWRsDEKHfcaCBgQ4dUDCAk&uact=5
! https://www.google.com/search?q=radius+of+earth+in+solar+radius&sxsrf=ALeKk03ZolaXO0Q5GVDXhkpV0zAY1gyisg%3A1619799230182&ei=viyMYLHOCt-TwbkP38OKkAE&oq=radius+of+earth+in+solar+radius&gs_lcp=Cgdnd3Mtd2l6EAMyBAgjECc6BwgAEEcQsAM6BwgjELACECc6BQgAEM0CUKnqAViA8QFgp_IBaAJwAngAgAF7iAHFA5IBAzYuMZgBAKABAaoBB2d3cy13aXrIAQjAAQE&sclient=gws-wiz&ved=0ahUKEwixpeOtrqbwAhXfSTABHd-hAhIQ4dUDCA4&uact=5
! https://www.google.com/search?q=radius+of+jupiter+in+solar+radius&sxsrf=ALeKk01149iB9A10BPocEFhf7xOVyc4p7A%3A1619799224685&ei=uCyMYNeuKcSVwbkPwuuSoAE&oq=radius+of+jupiter+in+solar+radius&gs_lcp=Cgdnd3Mtd2l6EAM6BwgAEEcQsAM6BQgAEM0COgQIIRAKUP0QWKwfYNYiaAJwAngAgAF2iAGgBZIBAzguMZgBAKABAaoBB2d3cy13aXrIAQjAAQE&sclient=gws-wiz&ved=0ahUKEwjX6pOrrqbwAhXESjABHcK1BBQQ4dUDCA4&uact=5
! https://www.paulanthonywilson.com/exoplanets/exoplanet-detection-techniques/the-exoplanet-transit-method/
! http://www.exoplanetes.umontreal.ca/transit-method/?lang=en
! https://exoplanets.nasa.gov/faq/31/whats-a-transit/
! https://www.sfu.ca/colloquium/PDC_Top/astrobiology/discovering-exoplanets/calculating-exoplanet-properties.html

! --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

! percentage of light vs the planet
! dependent on the radius of the planet vs the radius of the star
! noise should be +- 10%
! do a planet with mercury, earth, and saturn

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

    ! variable to hold a random noise level percentage
    real :: noise_level

    ! variable to decide if the random noise level is postive or negative
    real :: plus_minus_noise_level

    ! solar radius of the planets we are intersted in
    ! units (R0 solar radius)
    real, dimension(3) :: planet_radius
    real :: m2_star_radius = .44

    ! variable to hold the percentage decrease of star brighness during transit
    real :: transit_brightness_drop

    ! variable to hold the planet size from the user
    integer :: planet_size

    ! give the user information about the program
    write(*,*) "Program to calculate the brightness drop from an m2 star using the transit method"
    
    ! get the size of the planet from the user
    write(*,*)
    write(*,*) "Please enter the size of the planet you would like to simulate"
    write(*,*) "Enter 1 for a Mercury sized planet 2 for a Earth sized planet and 3 for a Jupiter sized planet"
    read(*,*) planet_size
    write(*,*)

    do while(planet_size .ne. 1 .and. planet_size .ne. 2 .and. planet_size .ne. 3)
        write(*,*) "Invalid planet choice"
        write(*,*) "Enter 1 for a Mercury sized planet 2 for a Earth sized planet and 3 for a Jupiter sized planet"
        read(*,*) planet_size
        write(*,*)
    enddo


    ! initialize the radai for the planets we are interested in
    ! 1 = mercury diameter
    ! 2 = earth diameter
    ! 3 = jupiter diameter

    planet_radius(1) = 0.0035068
    planet_radius(2) = 0.0091577
    planet_radius(3) = 0.10049

    ! open an output file for the x and y values
    open(1, file="x_values.txt")
    open(2, file="y_values.txt")
    open(3, file="time_points.txt")
    open(4, file="brightness_percentage_points.txt")

    ! find points for 3 orbits with 100 points each orbit
    do i = 1, (floor(period * 3) * 100)

        ! find the x and y values using theta as the angle
        x = cos(theta)
        y = sin(theta)

        ! write the x and y values to their respective files
        write(1, *) x
        write(2, *) y
        
        ! find the new theta based on the planet we are looking for
        ! we want to move the planet by 1/100 of its full revolution
        ! we find this by finding 1/100 th of the unit ciricle (period is always 1 so no division is nescassary)
        theta = theta + ((1.0 / 100.0) * 2.0 * pi)

        ! for every point in the orbit find a random percentage to reduce the noise level by
        ! get a random number between 1 and 10
        ! number represents the percentage to add or drop by
        call random_seed()
        call random_number(noise_level)
        call random_number(plus_minus_noise_level)

        ! make the noise level a number between 1 and 10
        noise_level = floor(noise_level * 10) + 1
        plus_minus_noise_level = floor(plus_minus_noise_level * 2) + 1

        ! add or subtract from the overall start brightness (simulates noise)
        ! adds if the plus_minus variable is 1
        ! subtracts if the plus_minus variable is 2
        if(plus_minus_noise_level .eq. 1) then
            star_brightness = star_brightness + noise_level
        
        else if(plus_minus_noise_level .eq. 2) then
            star_brightness = star_brightness - noise_level

        endif

        ! decrease the star brightness if the exoplanet is passing in front of it
        ! formula was found on https://www.sfu.ca/colloquium/PDC_Top/astrobiology/discovering-exoplanets/calculating-exoplanet-properties.html
        ! drop in brighness is found by r^2 / R^2 where r = planet radius and R = star radius
        transit_brightness_drop = (planet_radius(planet_size) ** 2) / (m2_star_radius ** 2)
        
        ! change the star_brightness percentage if the planet is in front of the star from the viewers perspective
        ! this happens if the star is below the line y = 0

        if(y .le. 0) then
            star_brightness = star_brightness - transit_brightness_drop
        endif

        ! write the time (i) value and brightness (star_brightness) to their respective files
        write(3, *) i
        write(4, *) star_brightness

        ! reset the star_brightness to 100% in prep for the next noise level calculation
        star_brightness = 100

    enddo

    write(*,*)
    write(*,*) "The drop in star brightness percentage for this planet was", transit_brightness_drop
    write(*,*)

    write(*,*) "Time values saved in time_points.txt"
    write(*,*) "Brighness values saved in brightness_percentage_points.txt"
    write(*,*) "X coordinates of the planet saved in x_values.txt"
    write(*,*) "Y coordinates of the planet saved in y_values.txt"

    ! close the output files
    close(1)
    close(2)
    close(3)
    close(4)

end program exoplanet_transit_modeling