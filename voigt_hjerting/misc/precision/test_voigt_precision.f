        program test_voigt_precision
        
        
        implicit none

!       set precision of double precision reals
        integer, parameter :: doubleR = selected_real_kind(p=15,r=307)
        integer, parameter :: singleI = selected_int_kind(9)
        
        integer(kind=singleI) :: pixel
        integer(kind=singleI), parameter :: max_iter = 1000000

        real(kind=doubleR), parameter :: x_step = 1.0d0/dble(max_iter)
        real(kind=doubleR) :: a, x
        real(kind=doubleR) :: voigt_func
        real(kind=doubleR) :: voigt_correct

        external :: voigt_func

        a = 1.0d0

        x = 0.0d0
         
        write(6,'(a,f6.4)') '# a=', a
        
        do pixel=1,max_iter
        
         x=x+x_step
        
         write(6,'(31e12.4)') x, voigt_correct(a,x)
        
        end do ! pixel
        

        stop
        
        end program test_voigt_precision
!       ========================================================================


!       ========================================================================
        function voigt_correct(a,x)

!       NOTE:
!       computes the correction term to the purely Gaussian function to approxi
!       mate the Voigt-Hjerting function

!       See equation (23) in Tepper-Garcia, Thor (2006)
!       Monthly Notices of the Royal Astronomical Society; Volume 369, 
!       Issue 4, Page 2025

        implicit none

!       set precision of double precision reals
        integer, parameter :: doubleR = selected_real_kind(p=15,r=307)
        real(kind=doubleR) :: voigt_correct
        real(kind=doubleR) :: real_double

        real(kind=doubleR) :: a, x, x2, h0, Q
        real(kind=doubleR), parameter :: PI=3.141593d0
        
!       Check numerical precision
!       See ~/voigt_hjerting/code/precision (dir)

         x2 = x*x

!       Since h0*h0 = exp(-2x) is used below, check the exponent of
!       exp(-x); it must satisfy -x > 0.5*log(smallest machine number)

         if ((-1.0d0*x2).le.(5.0d-1*dlog(tiny(real_double)))) then
          h0 = 0.0d0
         else
         h0 = dexp(-1.0d0*x2)
         end if

         Q = 1.5d0 / x2

         voigt_correct = - (a / dsqrt(PI) / x2) *
     &   (h0 * h0 * (4.0d0 * x2 * x2 + 7.0d0 * x2 + 4.0d0 + Q) - 1.0d0 - Q)

        
!       ------------------------------------------------------------------------
        return

        end function voigt_correct
!       ========================================================================

