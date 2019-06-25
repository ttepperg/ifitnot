!       ========================================================================
        function voigt_hjerting(a,x)

!       NOTE:
!       Returns the value of the Voigt-Hjerting function H(a,x) as a
!       function of x for a given a (damping) parameter, Doppler
!       parameter (in km/s), and central wavelength (in Angstroem) of
!       the corresponding transition.

!       Gamma =  natural line width (1 / sec)
!       gamma =  Gamma / 4 pi nu0
!       nu0 = transition frequency ( 1 / sec)
!       (Delta nu / nu0) = (Delta lambda / lambda0) = (b / c)
!       c -> light speed (in cm/s)
!       b -> Doppler parameter (in km/s)
!       lambda0 -> central wavelength (in Angstroem) of the corresponding
!       transition.
!
!       Algorithm is from Tepper-Garcia, Thor (2006)
!       Monthly Notices of the Royal Astronomical Society; Volume 369, 
!       Issue 4, Page 2025

        implicit none

!       set precision of double precision reals
        integer, parameter :: doubleR = selected_real_kind(p=15,r=307)
        real(kind=doubleR) :: voigt_hjerting

        real(kind=doubleR) :: a, x, x2, h0, Q
        real(kind=doubleR), parameter :: PI=3.141593d0
        
!       Check numerical precision
!       See ~/voigt_hjerting/code/precision (dir)

        if (dabs(x).gt.4.0d-4) then ! x > 0

         x2 = x*x

!       Since h0*h0 = exp(-2x) is used below, check the exponent of
!       exp(-x); it must satisfy -x > 0.5*log(smallest machine number)

         if ((-1.0d0*x2).le.(5.0d-1*dlog(tiny(real_double)))) then
          h0 = 0.0d0
         else
          h0 = dexp(-1.0d0*x2)
         end if

         Q = 1.5d0/x2

         voigt_hjerting = h0 - (a / dsqrt(PI) / x2) *
     &   (h0 * h0 * (4.0d0 * x2 * x2 + 7.0d0 * x2 + 4.0d0 + Q) - 1.0d0 - Q)

        else                         !limit for x -> 0

         voigt_hjerting = 1.0d0 - 2.0d0 * a / dsqrt(PI)

        endif
        
!       ------------------------------------------------------------------------
        return

        end function voigt_hjerting
!       ========================================================================
