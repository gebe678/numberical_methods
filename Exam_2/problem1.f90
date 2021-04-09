! Exam 2 Problem 2 Griffin Lehrer
! Bacteria force curve fitting

program bacteria_force_curve_fitting

    implicit none

    ! variables to hold the displacement and force data
    real, dimension(281) :: displacementData, forceData

    ! variables to hold slope data and compaire similarity
    real :: slope1, slope2

    ! formatting statment to display all of the digits in the number read from the DisplacementData and ForceData
    character(len=20), PARAMETER :: displacementFormat = "(F20.12)"
    character(len=20), PARAMETER :: forceFormat = "(F20.16)"

    ! variables for controlling the file names
    integer :: displacement = 1
    integer :: force = 2

    ! variable for interating through loops
    ! needed due to implicit none
    ! counter used to keep the number of points that have been foudn
    integer :: i, count

    ! tolerance for the slopes to differ before stoping the interpolation
    real :: tolerance = .03

    ! allocatable arrays to hold the points found to be analyzed
    ! with least squares fit
    real, dimension(:), allocatable :: displacementPoints
    real, dimension(:), allocatable :: forcePoints

    ! open the file containing the force curve data
    open(displacement, file="DisplacementData.dat")
    open(force, file="ForceData.dat")

    ! There are 281 data points in the files
    ! loop can go 281 times to get all of the data points
    ! save the data points into an array
    do i=1, 281

        read(displacement, *) displacementData(i)
        read(force, *) forceData(i)

        ! write(*,*)
        ! write(*,*) "n:", i
        ! write(*,*)
        ! write(*,*) "displacement data", displacementData(i)
        ! write(*,*)
        ! write(*,*) "force data", forceData(i)
        ! write(*,*)

    enddo

    ! find the first slope of the dataset
    ! displacement as the x values and force as the y values
     slope1 = (forceData(281) - forceData(280)) / (displacementData(281) - displacementData(280))

    ! set the number of points found to 1
     count = 1

    ! loop through the data again and find the slopes between the points
    ! start at the end of the list and count down torwards the beginning
    do i=280, 2, -1

        ! calculate sequential slopes
        slope2 = (forceData(i) - forceData(i - 1)) / (displacementData(i) - displacementData(i - 1))

        ! print data
        write(*,*) "n:", i
        write(*,*)
        write(*,*) "x2:", displacementData(i)
        write(*,*)
        write(*,*) "x1:", displacementData(i - 1)
        write(*,*)
        write(*,*) "y2:", forceData(i)
        write(*,*)
        write(*,*) "y1:", forceData(i)
        write(*,*)
        write(*,*) "slope1", slope1
        write(*,forceFormat)slope1
        write(*,*)
        write(*,*)
        write(*,*) "slope2", slope2
        write(*,forceFormat)slope2
        write(*,*)
        write(*,*) "slope difference", ABS(slope1 - slope2)
        write(*, forceFormat) ABS(slope1 - slope2)

        ! check the current slope with the origional slope to check
        ! if they are within the linear tolerance
        ! if they arent than stop the loop
        if(  ABS(slope1 - slope2) .gt. tolerance) then

            write(*,*) "STOPPING"
            write(*,*)
            count = count + 1

            exit

        endif

        ! count the new point
        count = count + 1
        
    enddo

    write(*,*) count

    ! allocate the count of the points and add one for the extra point at the end
    allocate(displacementPoints(count))
    allocate(forcePoints(count))

    ! get the linear data from the files
    ! count backwards to make sure the data is in the correct order
    do i=count, 1, -1

        ! get the linear displacement points and the lienear force points
        displacementPoints(i) = displacementData(282 - i)
        forcePoints(i) = forceData(282 - i)

        ! write the points to the user
        write(*,*)
        write(*,*) "displacement", displacementPoints(i)
        write(*,*)
        write(*,*) "force", forcePoints(i)
        write(*,*)
        
    enddo

end program bacteria_force_curve_fitting