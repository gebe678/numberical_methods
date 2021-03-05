! Griffin Lehrer Exam 1 Question 5

! Simulate a box and place a marble randomly within it

!           the box
!               12.7 cm length
!      ------------------------------
!      |              ^             | 
!      |              |             |
!      |   dist from length         |
!      |                            |  12.7 cm width
!      |              *             |
!      |                            |
!      |  dist from width --------> |
!      |                            |
!      ------------------------------

program marbleInBox

    implicit none

    ! variable i to use in the do loops needed due to implicit none
    integer :: i

    ! variable for the length of the box in centimeters
    real, parameter :: boxLength = 12.7

    ! variable for the width of the box in centimeters
    real, parameter :: boxWidth = 12.7
    
    ! variable for the diameter of the marble in centimeters
    real, parameter :: marbleDiameter = 1.5

    ! variable for the distance from the length the marble lands
    real :: marbleLengthDist

    ! variable for the distance form the width the marble lands
    real :: marbleWidthDist

    ! variable to hold the average of 100 random placements
    real :: placementAvg100Length = 0
    real :: placementAvg100Width = 0

    ! variable to hold the average of 10000 random placements
    real :: placementAvg10000Length = 0
    real :: placementAvg10000Width = 0

    ! random integer offset for the random number
    real :: randomInteger

    call random_seed()

    ! loop to find the average of 100 random placements
    do i=1, 100

        ! Get a random number for the distance of the marble from the length of the box

        ! get the point values randomly
        call random_number(marbleLengthDist)

        ! make sure the decimal part of the number is less than or equal to .7 as that is the size of the box
        do while(marbleLengthDist .gt. .7)
            call random_number(marbleLengthDist)
        enddo

        ! get the integer part of the random number
        call random_number(randomInteger)

        ! transform the number to be between 0 - 12
        randomInteger = floor((randomInteger * 12))

        ! add the integer and the decimal parts of the number together to get the distance form the length of the box
        marbleLengthDist = marbleLengthDist + randomInteger        

        ! get a random number for the distance of the marble from the width of the box
        call random_number(marbleWidthDist)

        call random_number(randomInteger)

        randomInteger = floor((randomInteger * 12))

        marbleWidthDist = marbleWidthDist + randomInteger

        placementAvg100Length = placementAvg100Length + marbleLengthDist
        placementAvg100Width = placementAvg100Width + marbleWidthDist

    enddo

        ! loop to find the average of 10000 random placements
    do i=1, 10000

        ! Get a random number for the distance of the marble from the length of the box

        ! get the point values randomly
        call random_number(marbleLengthDist)

        ! make sure the decimal part of the number is less than or equal to .7 as that is the size of the box
        do while(marbleLengthDist .gt. .7)
            call random_number(marbleLengthDist)
        enddo

        ! get the integer part of the random number
        call random_number(randomInteger)

        ! transform the number to be between 0 - 12
        randomInteger = floor((randomInteger * 12))

        ! add the integer and the decimal parts of the number together to get the distance form the length of the box
        marbleLengthDist = marbleLengthDist + randomInteger

        ! change the value of the random number from 0 - 1 to 0 - 12.7
        

        ! get a random number for the distance of the marble from the width of the box
        call random_number(marbleWidthDist)

        call random_number(randomInteger)

        randomInteger = floor((randomInteger * 12))

        marbleWidthDist = marbleWidthDist + randomInteger

        placementAvg10000Length = placementAvg10000Length + marbleLengthDist
        placementAvg10000Width = placementAvg10000Width + marbleWidthDist

    enddo

    placementAvg100Length = placementAvg100Length / 100
    placementAvg100Width = placementAvg100Width / 100
    placementAvg10000Length = placementAvg10000Length / 10000
    placementAvg10000Width = placementAvg10000Width / 10000


    write(*,*) "100 simulations: width:", placementAvg100Width, "length:", placementAvg100Length
    write(*,*) "10000 simulations: width:", placementAvg10000Width, "length:", placementAvg10000Length

    
end program marbleInBox