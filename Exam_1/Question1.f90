! Griffin Lehrer Numerical Methods Exam 1 Question 1

! Beam Deflection Program

program beamDeflection
    implicit none

    ! formatting statements to make the output look good
    ! use the parameter value to replace the formatting statment with a value
    ! this is not a variable as the name becomes a memonomic for the value
    character(len=*), parameter :: format1 = "(A8, 1X, I3, 2X, A15, F5.2)"
    character(len=*), parameter :: format2 = "(A31, 5X, A11)"
    character(len=*), parameter :: format3 = "(15X, I2, 20X, F10.2)"
    

    ! i is a variable used for interating in do loops
    ! Needed because of the useage of implicit none
    integer :: i

    ! value for inner loops used when nested do loops are needed
    ! needed because of the usage of implicit none
    integer :: j


    ! variables needed for the beam deflection algorithm
    ! L = load applied in pounds
    ! A = length from fixed end to applied load in feet
    ! E = elasticity of material

    ! I = moment of intertia computed by:
    ! I = (base * (height ** 3)) / 12

    ! deflection = value calcualted

    ! values taken from the dat file
    real :: length
    real :: base
    real :: height
    real :: elasticity
    real :: appliedLoad
    real :: deflection
    real :: intertia


    ! Number of intervals for each beam
    ! Intervals are calculated by the total number of feet minus one
    ! used to find the deflection foot by foot starting at foot 1
    integer :: numIntervals


    ! Arrays that hold the values to calculate beam analysis
    ! Each array will hold the values for one beam in the file
    ! Each array will hold all values for a different beam in the order they appear
    ! Row wise in the beam.dat file
    real, dimension(5) :: beam1Values
    real, dimension(5) :: beam2Values
    real, dimension(5) :: beam3Values
    real, dimension(5) :: beam4Values
    real, dimension(5) :: beam5Values


    ! beam deflection equation:
    !deflection = ((l * (a ** 2)) / (2 * e * i)) * (length - (a / 3.0))

    ! open the Beam.dat file so the values can be read into the program
    ! Row values are:
    ! Row 1: Length (first 10)
    ! Row 2: Base (first 4)
    ! Row 3: Height (first 15)
    ! Row 4 Elasticity (first 8)
    ! Row 5: App. Load (first 20)
    open(1, file = "Beam.dat")

    ! read the values from the dat file into arrays representing the values for each beam
    ! use a do loop to fill all of the values into the different arrays
    ! each array will contain the values to analyize one beam
    do i=1, 5
            
        read(1, *) beam1Values(i), beam2Values(i), beam3Values(i), beam4Values(i), beam5Values(i)

    enddo

    ! calculate the load of each beam at one foot intervals
    ! uses a double do loop the outer loop runs once for each beam and the inner loop calcuates
    ! the deflection for the load each foot from the end of the beam

    ! outer loop runs 5 times one for each beam
    do i=1, 5

        ! get the values for the current beam i
        if(i .eq. 1) then
            length = beam1Values(1)
            base = beam1Values(2)
            height = beam1Values(3)
            elasticity = beam1Values(4)
            appliedLoad = beam1Values(5)

        else if(i .eq. 2) then
            length = beam2Values(1)
            base = beam2Values(2)
            height = beam2Values(3)
            elasticity = beam2Values(4)
            appliedLoad = beam2Values(5)

        else if(i .eq. 3) then
            length = beam3Values(1)
            base = beam3Values(2)
            height = beam3Values(3)
            elasticity = beam3Values(4)
            appliedLoad = beam3Values(5)

        else if(i .eq. 4) then
            length = beam4Values(1)
            base = beam4Values(2)
            height = beam4Values(3)
            elasticity = beam4Values(4)
            appliedLoad = beam4Values(5)

        else if(i .eq. 5) then
            length = beam5Values(1)
            base = beam5Values(2)
            height = beam5Values(3)
            elasticity = beam5Values(4)
            appliedLoad = beam5Values(5)

        endif

        ! Print the beam number and length to the screen for the report
        write(*,*)
        write(*,format1) "Beam No.", i, "Total Length =", length
        write(*,*)

        ! Find the number of intervals from one foot of the beam to the end of the beam length
        ! this is just the floor of the beam length because we start one foot from the load end of the beam
        ! if the beam is just one foot we stop otherwise we coninue foot by foot down the lenght of the beam
        ! until we hit the end of the beam if the beam is an uneven than the floor brings us down to the nearest
        ! foot
        numIntervals = floor(length)

        ! print the header for the output
        write(*,format2) "Distance of Load From Fixed End ", " deflection"
        ! calculate deflection for every foot from the beam
        do j=1, numIntervals
            
            ! calculate the intertia for the deflection
            intertia = (base * (height ** 3)) / 12

            ! calculate the deflection for the beam
            deflection = (appliedLoad * ((j ** 2)) / (2 * elasticity * intertia)) * (length - (j / 3))
            write(*,format3) j, deflection

        enddo

    enddo

    ! close the opened Beam.dat file
    close(1)
end program beamDeflection