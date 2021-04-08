! Exam 2 Problem 2 Griffin Lehrer
! Bacteria force curve fitting

program bacteria_force_curve_fitting

    implicit none

    ! variables to hold the displacement and force data
    real, dimension(281) :: displacementData, forceData

    ! formatting statment to display all of the digits in the number read from the DisplacementData and ForceData
    character(len=20), PARAMETER :: displacementFormat = "(F20.12)"
    character(len=20), PARAMETER :: forceFormat = "(F20.16)"

    ! variables for controlling the file names
    integer :: displacement = 1
    integer :: force = 2

    ! variable for interating through loops
    ! needed due to implicit none
    integer :: i

    ! open the file containing the force curve data
    open(displacement, file="DisplacementData.dat")
    open(force, file="ForceData.dat")

    ! There are 281 data points in the files
    ! loop can go 281 times to get all of the data points
    ! save the data points into an array

    do i=1, 281

        read(displacement, *) displacementData(i)
        read(force, *) forceData(i)

        write(*,*)
        write(*,*) "n:", i
        write(*,*)
        write(*,*) "displacement data", displacementData(i)
        write(*,*)
        write(*,*) "force data", forceData(i)
        write(*,*)

    enddo

end program bacteria_force_curve_fitting