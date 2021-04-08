! Exam 2 Problem 2 Griffin Lehrer
! Bacteria force curve fitting

program bacteria_force_curve_fitting

    implicit none

    ! FIXME DELETE OR RENAME VARIABLE TO HOLD FILE DATA
    real :: data

    ! formatting statment to display all of the digits in the number read from the DisplacementData and ForceData
    character(len=20), PARAMETER :: displacementFormat = "(F20.12)"
    character(len=20), PARAMETER :: forceFormat = "(F20.16)"

    ! variables for controlling the file names
    integer :: displacement = 1
    integer :: force = 2

    ! open the file containing the force curve data
    open(displacement, file="DisplacementData.dat")
    open(force, file="ForceData.dat")

    ! read the data from the file
    read(displacement, *)data

    write(*, displacementFormat)data

    
    read(force, *) data
    write(*,forceFormat)data



end program bacteria_force_curve_fitting