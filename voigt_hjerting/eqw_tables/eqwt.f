        program eqw

!       (C) Thorsten Tepper Garcia 2005

!       Computes an equivalent width table for a given line.
!       Absorption Lines are calculated according to the
!       (correct) Line-Profile Method. Since a<<1 (where a is the
!       damping parameter in the Hjerting-Function H(a,v)), the line's
!       profile can be approximated by

!       H(a,x) = h0-a/sqrt(PI)/x^2*(h0^2*(4x^4 + 7x^2 + 4 + Q) - 1 - Q)
!       with h0=exp(-x^2), Q=3/2 x^2


!       ----------------------------------------------------------------
!       1. Declaring variables
!       ----------------------------------------------------------------
        use variables

        implicit none

        intrinsic :: DSQRT, DLOG, DLOG10, DEXP, DABS, MOD

        integer :: NH, DP
        integer, parameter :: maxNH=61,maxDP=31 
        integer :: i, j
        
        double precision :: width(maxNH,maxDP),widthaux!,eqwidth
        double precision :: exp_h_ax, id

        character(len=3) :: maxDP_str
        character(len=128) :: format_str

        logical :: flag

        external :: exp_h_ax, id
        
!       ----------------------------------------------------------------
!       2. Program start: Getting arguments
!       ----------------------------------------------------------------
!       Main input

        call read_parameter                        !from subroutines.h
!       ----------------------------------------------------------------
!       3. Generating wavelength baseline and flat spectrum
!       ----------------------------------------------------------------
        
        call wavebase                       !Generating wavelength baseline

!       ----------------------------------------------------------------
!       Define home directory

        homefile = '/home/volans/tepper/'

!       ----------------------------------------------------------------
!       Reading Atomic data (Morton+2003)
!       ----------------------------------------------------------------
!       Reads in restframe wavelength (in Angstroem), oscillator strength
!       and gamma-value (in Hz) of the corresponding transitions

        flag=.true.
        open(20, file = trim(homefile)//'/autovp/ions.dat')
         do 30
          read(20,*,end=40) ion_str, wl_central, fvalue, gamma
          if (trim(ion_str).eq.trim(ion_name)) then
           flag=.false.
           goto 40
          end if
  30         continue
  40         continue
        close(20)
        if (flag) then
         print*, 'Ion not found! STOP.'
         stop
        end if
        print*, trim(ion_str), wl_central, fvalue, gamma
!       ----------------------------------------------------------------
!       6.1 Computing eqw
!       ----------------------------------------------------------------
        sqrtPI = DSQRT(PI)                !Defining constants
        sqrtPIinv = 1/DSQRT(PI)

!       Initialising function

!       width(1,1)=eqwidth(flux,alambda,ncount)
        
        do NH=1,maxNH                        !Cycle for NHI
        
         do DP=1,maxDP                        !Cycle for b
         
         
          aNHI = 1.0d+01**(alogNHImin+(NH-1)*stepNHI)
          b = bmin+(DP-1)*stepb          


!       Precision (fourth argument of function 'integrate') is set
!       to 10^{-4} since this is the precision achieved by the
!       approximation to the Voigt-Hjerting function used in
!       exp_h_ax (see ~/voigt_hjerting/lyman/numeric)

!       Integrate the line profile, i.e. 1-exp(-tau), in the range
!       lambda=0-2e3

          call integrate(exp_h_ax,0.0d0,2.0d+03,1.0d-4,widthaux)
          print*, widthaux
          
          width(NH,DP) = widthaux

         end do
        end do
        
        write(maxDP_str,'(i3)') maxDP
        format_str='(A,'//trim(adjustl(maxDP_str))//'(F5.1,8X))'

        open(60,file='table_'//trim(ion_name)//'.eqw')
 
           write(60,trim(format_str)) '#               ',
     &         (bmin+(i-1)*stepb,i=1,maxDP)
         do i=1,maxNH
!         write(60,'(F6.2,1X,21(1pe12.6,1X))') alogNHImin+(i-1)*stepNHI,
!     &          (width(i,j)/wl_central, j=1,maxDP)
          write(60,'(32(1pe12.6,1X))') 1.0d+01**(alogNHImin+(i-1)*stepNHI),
     &          (width(i,j)*1.0d3, j=1,maxDP)
         end do
        
        close(60)

!       ----------------------------------------------------------------
!       12. End of program
!       ----------------------------------------------------------------
!       print*, 'Done.'
        end program
