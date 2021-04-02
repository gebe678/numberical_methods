!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Jacob Buckelew and Griffin Lehrer
! This program interpolates data from Secretariat's performance in the 1973 Kentucky Derby
! The hermite interpolation is then used to estimate Secretariat's speed at the end of the race.

! Homework 7
! CMS 495, Fall 2021


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program Homework7_Q2

    ! Variable Declarations

    ! Declare a 2-d Array that will keep track of the divided differences table
    ! It will be of size 5x4 
    ! Each row corresponds to x, f(x), difference1, difference2, difference3
    ! 4 columns since there are 4 x values

    ! Integer Differences will keep track of how many sets of divided differences are being kept track of
    ! Num_values is simply an integer to represent num of x inputs(4). Will help with logic in the program
    ! i will be used for iteration of do loop later but also as an arg to subroutines

    ! Coefficients array will hold the 3 coefficients of the newton cubic polynomial
    ! Constants array will hold the 3 constant values that appear in the newton cubic polynomial(within parentheses)
    ! integer degree will keep track of the degree of the polynomial
    ! real speed will be Secretariat's speed at the end of the race
    ! integer term will keep track of which term we are deriving in the polynomial
    ! real input will be the input into the derived polynomial(119.4)

    Real, Dimension(5,4) :: diff_table
    Real, Dimension(3) :: coefficients
    real, Dimension(3) :: constants
    integer differences, num_values, i, degree, term
    real speed, input

    ! Format Statements
1   FORMAT(1X, F4.1, ' + ', F14.12, '(x -', F6.2, ') + ', F14.12, '(x -', F6.2, ')(x -', F6.2, ') + ', F14.12, '(x -', F6.2, &
        ')(x -', F6.2, ')(x -', F6.2, ')')

2   FORMAT(1X, "Estimation of Secretariat's Speed at the end of the race: ", F8.5, "mph")

    ! Exectuable Statements

    num_values = 4

    ! Initialize all of the values to 0 and then set the x, f(x) values to their correct values from the table
    diff_table(1,1) = 0.0
    diff_table(1,2) = 49.4
    diff_table(1,3) = 73.0
    diff_table(1,4) = 119.4
    diff_table(2,1) = 0.0
    diff_table(2,2) = 0.50
    diff_table(2,3) = 0.75
    diff_table(2,4) = 1.25



    ! Begin hermite interpolation of the data

    ! iterate through 3 sets of divided differences
    ! So from 1 to num_values - 1 
    do i = 1, num_values - 1
        ! differences will take on the value of num_values - i. Each subsequent row will require one less difference to be made
        ! differences will begin at 3 to indicate 3 differences are needed in the first run of interpolation
        differences = num_values - i
        call divided_difference(diff_table, differences, i)


    enddo

    ! Print Polynomial 
    Print *, "Newton Polynomial: "
    Write(*, 1) diff_table(2,1), diff_table(3,1), diff_table(1, 1), diff_table(4,1), diff_table(1,1), diff_table(1,2), &
        diff_table(5,1), diff_table(1,1), diff_table(1,2), diff_table(1,3)

    ! Figure out Secretariats speed at x= 119.4
    
    speed = 0.0
    degree = 3
    term = degree
    input = 119.4

    ! Setup matrices
    ! a1, a2, a3 coefficients will just be the first three values in rows 3-5
    ! In this case the a0 is just 0 so it's not significant to keep track of
    ! constants will be the first 3 values in the first row. Think of these like "b" values corresponding to the (x - b) terms
    ! in the interpolation

    coefficients(1) = diff_table(3,1)
    coefficients(2) = diff_table(4,1)
    coefficients(3) = diff_table(5,1)
    
    constants(1) = diff_table(1,1)
    constants(2) = diff_table(1,2)
    constants(3) = diff_table(1,3)

    ! Loop through each of the 4 mini functions within the newton polynomial
    ! First a_0 term will just derive to 0 so its unnecessary to count it
    ! So loop will derive 3 times instead of 4

    do n = 1, degree
        call derive(coefficients, constants, term, speed, input)
        term = term - 1

    enddo

    ! Take absolute value of speed and multiply by 3600 to get measure in mph instead of mps

    speed = abs(speed) * 3600
    Write(*,2) speed
stop 
end

! Subroutines

    ! The divided_difference subroutine will take an array called diff_table and an integer called differences as args
    ! It will also take an integer i which will be important for logic
    ! It will calculate a set of divided differences(dictated by differences arg) to be stored in the table
    subroutine divided_difference(diff_table, differences, i)

        ! Variable declarations
        ! row will be used to keep track of each row instead of using i
        Real, Dimension(5,4) :: diff_table
        integer differences
        integer i, row

        ! Need to perform hermite interpolation on the data in diff_table to calculate
        ! The desired set of divided differences dictated by differences variable

        ! initialize row to be i + 2
        ! Ex. starting at row 3 for the first round of differences
        row = i + 2

        ! Only accessing one row here
        ! so j will keep track of each column in the row( so basically x_k in formula)
        ! row will be used to access the previous rows data( to get f_j-1 values in the interpolation formula)
        ! i will be used to access the first x term in the denominator of the interpolation formula
        do j = 1, differences
            diff_table(row, j) = (diff_table(row - 1, j + 1) - diff_table(row - 1, j))/(diff_table(1, j + i) - diff_table(1,j))

        enddo
    end subroutine
    
    ! The derive subroutine will derive a single term from a newton cubic polynomial at a particular point using product rule
    ! Thus, it is basically interpreting each bit of the polynomial as its own differentiable part.
    ! subroutine takes arrays coefficients and constants as args as well as integer term, and reals speed and input
    subroutine derive(coefficients, constants, term, speed, input)
        ! Variable declarations
        ! Real output will just be used to calculate outputs as subroutine executes
        Real, Dimension(3) :: coefficients
        real, Dimension(3) :: constants
        integer term
        real output, speed, input


        ! will need to do either triple product rule, double product rule, or single linear differentiation to figure this out
        output = 0.0
        ! If term is 1, then we are working with a_1(x- b_1) which means derivative is just the coefficient a_1
        if(term .eq. 1) then
            output = output + coefficients(1)
        ! If term is 2, then we are working with a_2(x - b_1)(x- b_2) which means derivative is coefficient a_2 * double product
        ! rule result of two terms following it(evaluated at the input)
        elseif(term .eq. 2) then
            output = output + (coefficients(2) * ((input - constants(1)) + (input - constants(2))))
        
        ! if term is 3, we are working with the last term in the cubic polynomial so it will involve multiplying a_3
        ! By the triple product rule of the three (x - b) terms
        
        elseif(term .eq. 3) then
            output = output + (coefficients(3) * (((input - constants(1)) * (input - constants(2))) + ((input - constants(1)) &
                * (input - constants(3))) + ((input - constants(2)) * (input - constants(3)))))
        endif

        ! Add output to the speed
        speed = speed + output
        

    end subroutine