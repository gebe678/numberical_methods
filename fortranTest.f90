subroutine test1(y)
    integer :: y
    integer :: x
    x = x + 5
    y = y + 10
end subroutine test1

program test
    integer :: dude = 0
    integer :: dude1 = 0

    call test1(dude)
    call test1(dude)

    write(*,*) dude, dude1
end program test