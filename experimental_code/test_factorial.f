        program factorial

        implicit none

        integer :: argcount
        integer :: i, n, fact
        character(len=10) :: n_str
!       ------------------------------------------------------------------------
!       get arguments

        argcount = IArgC()

        call getarg(1,n_str)
        read(n_str,'(i10)') n

        fact = 1
        do i=1,n
         fact = i * fact
        end do

	write(6,'(a,i10)') ' '//trim(n_str)//'!=', fact
        stop 

        end program factorial
