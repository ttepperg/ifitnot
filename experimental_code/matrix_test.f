        program matrix_test
	
	
	implicit none
	real :: ma(2,3)
	
	ma(1,1:3) = 0.0
	ma(2,1:3) = 1.0
	
	print*, ma(:,:)
	
	
	end program matrix_test
