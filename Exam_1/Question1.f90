! Griffin Lehrer Numerical Methods Exam 1 Question 1

! Beam Deflection Program

program beamDeflection
    implicit none

    ! variables needed for the beam deflection algorithm
    ! L = load applied in pounds
    ! A = length from fixed end to applied load in feet
    ! E = elasticity of material
    real :: L, A, E
    real :: num1, num2

    ! I = moment of intertia computed by:
    ! I = (base * (height ** 3)) / 12

    ! beam deflection equation:
    !deflection = ((l * (a ** 2)) / (2 * e * i)) * (length - (a / 3.0))

    ! open the Beam.dat file so the values can be read into the program
    open(1, file = "Beam.dat")

    read(1, *) num1, num2

    write(*,*) num1, num2

    ! close the opened Beam.dat file
    close(1)
end program beamDeflection