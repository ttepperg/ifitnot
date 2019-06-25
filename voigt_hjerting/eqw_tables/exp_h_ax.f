!       ----------------------------------------------------------------
!       Voigt-Profile Approximation
!       ----------------------------------------------------------------
        function exp_h_ax(wl)

!       Since a<<1 (where a is the damping parameter in the
!       Hjerting-Function H(a,v)), the line's profile can be approximated
!       by
!       
!       H(a,x) = exp(-x^2)*[1-(2a/SQRT(pi))*H_1(x)]
!       where
!       H_1(x)={(3+4x^2)(1+x^2)exp(-x^2)-(3+2x^2)sinh(x^2/(x^2))}/(2x^2)
!       with a = adamping and x = anu. 
!
!       ----------------------------------------------------------------
!       Declaring variables
!       ----------------------------------------------------------------
        use variables

        implicit none

             double precision :: adopplerinv, adamping, alphanu,
     +        x, x2 , h0, Q, P, voigt_hjerting, tau
             double precision :: wl, exp_h_ax

!       ----------------------------------------------------------------
!       Calculating absorption coefficent 'exp{-tau(lambda)}'
!       ----------------------------------------------------------------
        sqrtPI = dsqrt(PI)                !Defining constants
        sqrtPIinv = 1.0d0/dsqrt(PI)

        adopplerinv = (CLIGHT/(wl_central*b))*1.0d3
        adamping = (wl_central*gamma/(4.0d0*PI*b))*1.0d-13
        alphanu = (sqrtPI*ERADIUS*N_ion*(wl_central*wl_central)*
     1  fvalue*adopplerinv)*1.0d-16
        x = (wl)*adopplerinv*1.0d-8


!       Check numerical precision
!       See ~/voigt_hjerting/code/precision (dir)

        if (dabs(x).gt.4.0d-4) then ! numerically stable

         x2 = x*x

!       avoid underflow of exp(-x*x)
         if (dabs(x).lt.(dsqrt(-dlog(dble(tiny(1.0e0)))))) then
          h0 = dexp(-1.0d0*x2)
         else
          h0 = tiny(1.0e0)
         end if
        
!       avoid over/underflow of Q
         if ((dlog(x2) + dlog(dble(tiny(1.0e0)))).gt.dlog(1.5d0)) then
          Q = tiny(1.0e0)
         else
          Q = 1.5d0/x2
         end if
        
!       avoid over/underflow of P
         if ((dlog(x2) + dlog(dble(tiny(1.0e0)))).gt.
     &   (dlog(adamping)-dlog(sqrtPI))) then
          P = tiny(1.0e0)
         else
          P = adamping/sqrtPI/x2
         end if
        
         voigt_hjerting = h0 - P *
     &   (h0*h0*(4.0d0*x2*x2 + 7.0d0*x2 + 4.0d0 + Q) - 1.0d0 - Q)

        else                         !limit for x -> 0

         voigt_hjerting = 1.0d0 - 2.0d0*adamping/sqrtPI

        endif

!       Computing the absorption coefficent

!       avoid underflow of tau
        if (dlog(voigt_hjerting)+dlog(alphanu).gt.dlog(dble(tiny(1.0e0)))) then
         tau = alphanu * voigt_hjerting
        else
         tau = dble(tiny(1.0e0))
        end if

!       avoid underflow of dexp(-tau)
        if (tau.gt.(-1.0d0*dlog(dble(tiny(1.0d0))))) then
         exp_h_ax = 1.0d0
        else
         exp_h_ax = 1.0d0 - dexp(-1.0d0*tau)
        end if

        return
!       ----------------------------------------------------------------
        end function
!       ----------------------------------------------------------------
