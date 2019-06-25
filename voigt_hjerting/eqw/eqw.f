        program eqw

!       (C) Thorsten Tepper Garcia 2008

!       Returns the equivalent width (in mili Angstroem) for a given line
!       and given columndensity and doppler parameter.
!       Adapted from /eqw_tables/eqw
!       Absorption Lines are calculated according to the
!       (correct) Line-Profile Method. Since a<<1 (where a is the
!       damping parameter in the Hjerting-Function H(a,v)), the line's
!       profile can be approximated by
!
!       H(a,x) = h0-a/sqrt(PI)/x^2*(h0^2*(4x^4 + 7x^2 + 4 + Q) - 1 - Q)
!       with h0=exp(-x^2), Q=3/2 x^2
!

!       ----------------------------------------------------------------
!       1. Declaring variables
!       ----------------------------------------------------------------
        use variables
        
        implicit none

        double precision :: width
        double precision :: wl_range
        double precision :: exp_h_ax, exp_gauss_x, id
!        logical :: flag
        external :: exp_h_ax, exp_gauss_x, id
        
        character(len=256) :: errmsg, warnmsg, statmsg
        
!       ----------------------------------------------------------------
!       Define program error and warning messages;
!       The lines below are meant to print the different
!       messages in different colors

!       In red=31
        errmsg=CHAR(27)//'[1;31mERROR: eqw ...'//
     &  CHAR(27)//'[0m'
!       In yellow=33
        warnmsg=CHAR(27)//'[1;33mWARNING: eqw ...'//
     &  CHAR(27)//'[0m'
!       In green=32
        statmsg=CHAR(27)//'[1;32mSTATUS: eqw ...'//
     &  CHAR(27)//'[0m'

!       ----------------------------------------------------------------
!       2. Program start: Getting arguments
!       ----------------------------------------------------------------
!       Main input

        call read_parameter                        !from subroutines.h

!       ----------------------------------------------------------------
!       Getting home directory
        call system ('echo $HOME > homedir')
        open(60, file='homedir')
        read(60,'(a)') homedir
        close(60)
        call system ('rm -f homedir')        

!       ----------------------------------------------------------------
!       Reading Atomic data (Morton+2003)
!       ----------------------------------------------------------------
!       Reads in restframe wavelength (in Angstroem), oscillator strength
!       and gamma-value (in Hz) of the corresponding transitions
        
!        flag=.false.
!        open(20, file=trim(homedir)//'/ifitnot/ions.dat')
!         do 30
!          read(20,*,end=40) ion_str, lambda_0, fvalue, gamma
!          if (trim(ion_str).eq.trim(ion_name)) then
!           flag=.true.
!           goto 40
!          end if
!  30     continue
!  40    continue
!        close(20)
!        if (.not.flag) then
!         write(6,*) trim(errmsg)//'Ion '//trim(ion_name)//' not found!'
!         stop 1
!        end if

!       ----------------------------------------------------------------
!       BEWARE: the atomic data thus loaded might not be consistent with
!       abs_line_profile and ifitnot!

        call load_ion(ion_name)

!       ----------------------------------------------------------------
!       6.1 Computing eqw
!       ----------------------------------------------------------------
        N_ion = 1.0d1**(log_col_dens)
        b = b_value

!       Define wavelength range in Angstroem; chosen as 10^4 * FWHM
!       Recall:
!       FWHM = 2 b sqrt(ln 2)
!       dlambda = lambda_0 * (dv/c)

!       factor 1.d5 to transform b [km/s] -> b [cm/s]
        wl_range = lambda_0*(1.0d4*(2.0d0*1.d5*b*dsqrt(dlog(2.0d0))))/CLIGHT

!       Integrate the flux decrement, i.e. 1-exp(-tau), over lambda
!       from 0 to wl_range; equivalent width is given by 2x this integral

        width = 0.0d0
        call integrate(exp_h_ax,0.0d0,wl_range,1.0d-8,width)
        write(6,'(f6.2,2x,f6.2,2x,f14.4)') DLOG10(N_ion), b, 2.0*width*1.0d3
        width = 0.0d0
        call qromb(exp_h_ax,0.0d0,wl_range,width)
        write(6,'(f6.2,2x,f6.2,2x,f14.4)') DLOG10(N_ion), b, 2.0*width*1.0d3
        width = 0.0d0
        call qsimp(exp_h_ax,0.0d0,wl_range,width)
        write(6,'(f6.2,2x,f6.2,2x,f14.4)') DLOG10(N_ion), b, 2.0*width*1.0d3

!       ----------------------------------------------------------------
!       12. End of program
!       ----------------------------------------------------------------

        end program
