	program test_do_while

	implicit none

	integer :: cycl = 1

	do while (cycl.le.10)
	 print*, cycl
	 cycl =  cycl + 1
	end do

	stop 
	end program test_do_while
