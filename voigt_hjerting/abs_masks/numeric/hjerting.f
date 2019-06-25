	program hjerting
*	To check the numerical precision of the hjerting approximation
	implicit none
	
	intrinsic DEXP,DSQRT
	double precision x, x2, h0, h1, func,PI
	parameter (PI=3.141593D0)
	integer i
	
	x=0.0d0
	do i=1,1000000
	 x=x+1.0d-6
	 x2=x*x
	 h0 = DEXP(-x2)
	 h1 = 1.5d0/x2
	
	  func=1.0d0/x2*(h0*h0*(4.0d0*x2*x2+7.0d0*x2+4.0d0 + h1)-1.0d0-h1)
	 
	write(6,'(1pe14.6,1pe14.6)') x, func
	
	end do
	
	end program
	
	
