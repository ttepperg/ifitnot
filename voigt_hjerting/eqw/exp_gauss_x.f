!       ----------------------------------------------------------------
!       Gaussian profile
!       ----------------------------------------------------------------

!       Returns the value of the Gaussian function as a function of
!       wavelength for a given Doppler parameter (in km/s), and central
!       wavelength (in Angstroem) of the corresponding transition.

        double precision function exp_gauss_x(wl)

        use variables
        
        implicit none

        double precision :: dopp, wl
        double precision :: x, tau

        sqrtPI = dsqrt(PI)
        
        dopp = (lambda_0*b/CLIGHT)*1.0d-3 !cm
        
        x = (wl/dopp)*1.0d-8
        
        tau_central = (sqrtPI*ERADIUS*N_ion*(lambda_0*lambda_0)*
     &  fvalue/dopp)*1.0d-16

!       avoid underflow of tau
        if (dabs(x).lt.
     &  (dsqrt(dlog(tau_central) - dlog(dble(tiny(1.0e0)))))) then
         tau = tau_central*dexp(-1.0*x*x)
        else
         tau = dble(tiny(1.0e0))
        end if

!       avoid underflow of dexp(-tau)
        if (tau.gt.(-1.0d0*dlog(dble(tiny(1.0e0))))) then
         exp_gauss_x = 1.0d0
        else
         exp_gauss_x = 1.0d0 - dexp(-1.0*tau)
        end if

!       print*, wl, x, tau, dopp, exp_gauss_x

        return
!       ------------------------------------------------------------------------
        end function exp_gauss_x
!       ------------------------------------------------------------------------
