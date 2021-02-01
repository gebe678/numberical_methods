program problem1
    implicit none

    ! create 8 variables to hold the numbers from the file
    real :: num1, num2, num3, num4, num5, num6, num7, num8

    ! create variables to store the sum, average, and standard deviation
    real :: sum, average, stdDeviation

    ! open the files called values.txt and resutls.txt
    open(1, file="values.txt")
    open(2, file="results.txt")

    ! read values from the values.txt file and save them into variables
    read(1,*) num1, num2, num3, num4, num5, num6, num7, num8

    ! calculate the sum of the numbers from the file
    sum = num1 + num2 + num3 + num4 + num5 + num6 + num7 + num8

    ! calculate the average of the numbers from the file
    average = sum / 8

    ! calculate the standard deviation of the numbers from the file

    ! subtract the mean and square result and save back into origional variables
    num1 = (num1 - average) ** 2
    num2 = (num2 - average) ** 2
    num3 = (num3 - average) ** 2
    num4 = (num4 - average) ** 2
    num5 = (num5 - average) ** 2
    num6 = (num6 - average) ** 2
    num7 = (num7 - average) ** 2
    num8 = (num8 - average) ** 2

    ! find the mean of the new numbers
    stdDeviation = (num1 + num2 + num3 + num4 + num5 + num6 + num7 + num8) / 7

    ! take the square root to find the final standard deviation
    stdDeviation = sqrt(stdDeviation)

    ! write the results to the results.txt file
    write(2, *) "the sum is ", sum
    write(2, *) "the average is ", average
    write(2, *) "the standard deviation is ", stdDeviation

    ! close the files
    close(1)
    close(2)

end program problem1