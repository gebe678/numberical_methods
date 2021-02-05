subroutine test1(x)

    integer :: x
    x = 20
end subroutine test1

program test
    integer :: dude = 0
    integer :: dude1 = 0

    call test1(dude)
    call test1(dude1)

    write(*,*) dude, dude1
end program test