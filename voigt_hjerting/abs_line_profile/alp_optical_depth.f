!       ----------------------------------------------------------------
!       Voigt-Profile approximation to model absorption lines
!       ----------------------------------------------------------------
!       (C) Thorsten Tepper Garcia 2006
!       Based on the approximation to the Voigt-Hjerting function
!       published in:
!       Tepper Garcia, Thorsten
!       Monthly Notices of the Royal Astronomical Society; Volume 369, 
!       Issue 4, Page 2025
!       Please refer to the author when using this approximation.

	program alp_optical_depth

!       Absorption Lines are calculated according to the
!       approximation for a<<1 (where a is the damping parameter in the
!       Voigt-Hjerting function H(a,x)) given by (Tepper Garcia 2006)
!       
!       H(a,x) = h0-a/sqrt(PI)/x^2*(h0^2*(4x^4 + 7x^2 + 4 + Q) - 1 - Q)
!       with h0=exp(-x^2), Q=1.5 x^(-2)
*
!       The arguments of the program are the Doppler parameter bin (in
!       units km/s) and the logarithmic column density bin (in units of
!       log10(cm^{-2})). The output of the program is the optical depth
!	as a function of (log10 Nion, b).
!	This can be converted into a density map with Gnuplot
!
!       (see Tepper-Garcia 2006 for corresponding reference).
!       Please report any bugs or problems to:
!       tepper@astro.physik.uni-goettingen.de
!       ----------------------------------------------------------------
!       Variables declaration
!       ----------------------------------------------------------------
        implicit none
	include'parameters.f'

	intrinsic :: DEXP, DLOG, DSQRT

	integer :: argcount
	integer :: counter, pixels

	double precision :: tau_tot
	
	double precision :: tau_central, wavelength 
     	double precision :: wl_central, fvalue, gamma
     	double precision :: void, equiv_width_mA
     	double precision :: b_value, logNion, step_b_value, step_logNion,
     +	Nion, dopp_width, damping, alpha, tau_neg, sqrtPI
     	double precision, parameter :: b_value_max = 1.0d2
     	double precision, parameter :: logNion_max = 2.2d1
     	
	double precision, parameter :: b_value_min = 5.0d0
     	double precision, parameter :: logNion_min = 1.0d1

	double precision :: voigt_hjerting

	character(len=256) :: b_value_str, logNion_str, step_b_value_str,
     +	step_logNion_str
	character(len=256) :: ion_name, ion_str
	character(len=256) :: homedir
	character(len=256) :: outfile
	character(len=256) :: command

	logical :: flag
!       ----------------------------------------------------------------
!       Getting arguments
!       ----------------------------------------------------------------
	argcount = IArgC()
      	if (argcount .lt. 3) then
         print *,'USAGE: alp_optical_depth <transition> <step log10 Nion> '
     &   //'<step b (km/s)>'
	 STOP
      	end if
	
	call GetArg(1,ion_name)

	call GetArg(2,step_logNion_str)
	read (step_logNion_str,'(F5.2)') step_logNion

	call GetArg(3,step_b_value_str)
	read (step_b_value_str,'(F5.2)') step_b_value

!       ----------------------------------------------------------------
!       Computation
!       ----------------------------------------------------------------
!       Define home directory
	
	homedir = '/home/volans/tepper/'

!       ----------------------------------------------------------------
!       Reading Atomic data (Morton+2003)
!       ----------------------------------------------------------------
!       Reads in restframe wavelength (in Angstroem), oscillator strength
!       and gamma-value (in Hz) of the corresponding transitions

	flag=.true.
	open(20, FILE = 'ions.dat')
	 do 30
	  read(20,*,end=40) ion_str, wl_central, fvalue, gamma
	  if (trim(ion_str).eq.trim(ion_name)) then
	   flag=.false.
	   goto 40
	  end if
  30	 continue
  40	 continue
	close(20)
	if (flag) then
	 print*, 'Ion not found! STOP.'
	 stop
	end if
!       print*, ion_str, wl_central, fvalue, gamma
!
!       ----------------------------------------------------------------
!	Compute number of pixels; the value of 0.5 (in Angstroem) seems
!	to be a reasonable value for most lines

	pixels = int(0.5/resolution)

!       ----------------------------------------------------------------
!       START: loop over b-values and log10(Nion) values
!       ----------------------------------------------------------------
!	Define output filename and open

	outfile = 'alp_optical_depth.aod'
	
	open(50,file=trim(outfile))

	logNion = logNion_min

	do while (logNion.le.logNion_max)

	 b_value = b_value_min
	 
	 do while (b_value.le.b_value_max)
	  
!       ----------------------------------------------------------------
!       Calculating optical depth as a function of wavelength
!       ----------------------------------------------------------------
!	  Define constants
	  sqrtPI = DSQRT(PI)
	  Nion = 10.0D0**logNion				! in cm^{-2}

!	Initialize total optical depth and equivalent_width
	  tau_tot = 0.0d0

!       The following parameters are in cgs units      

	  dopp_width = (wl_central*b_value/CLIGHT)*1.0D-3
	  damping = (wl_central*gamma/(4.0D0*PI*b_value))*1.0D-13
	  alpha = (sqrtPI*ERADIUS*Nion*(wl_central*wl_central)*
     1    fvalue/dopp_width)*1.0D-16
	

	  do counter=-pixels,pixels

	   wavelength = wl_central + (counter)*resolution

	   tau_neg = (-alpha)*
     1	   voigt_hjerting(damping,b_value,wl_central,wavelength)

!	Central optical depth
	   if (counter.eq.0) tau_central = -tau_neg

!	Total optical depth
	   tau_tot = tau_tot -tau_neg

	  end do
	  
!	Average optical depth
	  tau_tot = tau_tot/(2*pixels+1)

!	----------------------------------------------------------------
!	Compute equivalent width using external function 'eqw'

!	Write parameters into string
	  write(logNion_str,'(f6.2)') logNion
	  write(b_value_str,'(f6.2)') b_value
	   
	  command = trim(homedir)//'/ifitnot/voigt_hjerting/eqw/eqw '//
     &    trim(ion_name)//' '//trim(logNion_str)//' '//trim(b_value_str)
     &    //' > eqw.dat'
	  call system(trim(command))

!	Read equivelent width from file eqw.dat
	  open(60, file='eqw.dat', status='old')
	   read(60,*) void, void, equiv_width_mA
	  close(60)
!	  print*, equiv_width_mA, eqw_aux


!	----------------------------------------------------------------
!	Write into file
	  write(50,*) logNion, b_value, tau_central, equiv_width_mA

!	Increase b_value
	  b_value = b_value + step_b_value

	 end do !loop over b_value

!	Write block separation needed by Gnuploy
	 write(50,*)

!	Increase logNion
	 logNion = logNion + step_logNion

	end do !loop over logNion


!	Close file
	close(50)

!       ----------------------------------------------------------------
	end program
!       ----------------------------------------------------------------

!	================================================================
!	Subroutines and Functions
!	================================================================

!	================================================================
	function voigt_hjerting(a,b,cwl,wl)
!       Returns the value of the Voigt-Hjerting function H(a,x) as a
!       function of wavelength for a given a-parameter, Doppler
!       parameter (in km/s), and central wavelength (in Angstroem) of
!       the corresponding transition.

	intrinsic DSQRT
	double precision voigt_hjerting
	double precision a, b, dopp, cwl, wl
	double precision x,x2,h0,Q,PI,CLIGHT
	parameter (PI=3.141593D0,CLIGHT=2.9979D+10)
	
	dopp = (cwl*b/CLIGHT)*1.0D-3
	x = ((wl-cwl)/dopp)*1.0D-8

	if (DABS(x).GT.4.0D-4) then		!arbitrarily set

	 x2 = x*x
	 h0 = DEXP(-x2)
	 Q = 1.5D0/x2

	 voigt_hjerting = h0 - a/DSQRT(PI)/x2*
     1	 (h0*h0*(4.0D0*x2*x2 + 7.0D0*x2 + 4.0D0 + Q) - 1.0D0 - Q)

	else					!limes for x -> 0

	 voigt_hjerting = 1.0d0 - 2.0D0*a/DSQRT(PI)

	endif
	
	return
!       ----------------------------------------------------------------
	end function
!       ----------------------------------------------------------------
