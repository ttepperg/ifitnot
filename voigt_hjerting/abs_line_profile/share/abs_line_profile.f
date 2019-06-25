!       ----------------------------------------------------------------
!       Voigt-Profile approximation to model absorption lines
!       ----------------------------------------------------------------
!       (C) Thorsten Tepper Garcia 2008
!       Based on the approximation to the Voigt-Hjerting function
!       published in:
!       Tepper Garcia, Thorsten
!       Monthly Notices of the Royal Astronomical Society; Volume 369, 
!       Issue 4, Page 2025
!       Please refer to the author when using this approximation.

	program abs_line_profile

!       Absorption Lines are calculated according to the
!       approximation for a<<1 (where a is the damping parameter in the
!       Voigt-Hjerting function H(a,x)) given by (Tepper Garcia 2006)
!       
!       H(a,x) = h0-a/sqrt(PI)/x^2*(h0^2*(4x^4 + 7x^2 + 4 + Q) - 1 - Q)
!       with h0=exp(-x^2), Q=1.5/x^2
!
!       The arguments of the program are a transition in the form e.g.
!	OVI_1031 (see ions.dat for a list of the avaible transitison),
!	the Doppler parameter 'b' (in units km/s) and the logarithmic
!	column density 'log10_Nion' (in units of log10(cm^{-2})).
!
!	The output of the program is:
!	a) a file named eqw.dat listing
!	log10_Nion | b | equivalent width (mili-Angstroem)
!	b) a plot of the line profile as a function of wavelength and
!	velocity (with respect to the line center). This plot should be
!	displayed automatically) 
!
!	All physical and other needed parameters are included in the file
!	'parameter.f'. The wavelength (and velocity) range is automatically
!	computed with a resolution of 0.001 A.
!
!       The line parameters (central wavelength of the transition,
!	oscillator strength, and gamma- (damping-) value) are contained
!	in the file 'ions.dat', and were taken from Morton's Compilation
!       (see Tepper-Garcia 2006 for corresponding reference).
!
!       NOTE: The author is not responsible for any modifications
!       done to this code in its original form.
!
!       Please report any bugs or problems to:
!       tepper@astro.physik.uni-goettingen.de
!       ----------------------------------------------------------------
!       Variables declaration
!       ----------------------------------------------------------------
        implicit none
	include'parameters.f'

	intrinsic :: DEXP, DLOG, DSQRT

	integer :: argcount
	integer :: prefix_index, counter, size

	double precision :: tau_tot, tau_neg, tau_central
	
	double precision, allocatable :: wavelength(:), velocity(:) 
     	double precision :: wl_min, wl_max, vel_min, vel_max
     	double precision :: wl_central, fvalue, gamma
     	double precision :: void, equiv_width_mA, eqw_aux
     	double precision :: b_value, logNion, Nion, dopp_width, damping,
     +	sqrtPI

	double precision :: voigt_hjerting

	character(len=256) :: b_value_str, logNion_str
	character(len=256) :: ion_name, ion_str
	character(len=256) :: infile, outfile, plotfile, psfile
	character(len=256) :: command

	logical :: ion_found
!       ----------------------------------------------------------------
!       Getting arguments
!       ----------------------------------------------------------------
	argcount = IArgC()
      	if (argcount .lt. 3) then
         print *,'USAGE: ans_line_profile <transition> <log10 Nion> '//
     &	 '<b (km/s)>'
	 stop 1
      	end if
	
	call GetArg(1,ion_name)

	call GetArg(2,logNion_str)
	read (logNion_str,'(F5.2)') logNion

	call GetArg(3,b_value_str)
	read (b_value_str,'(F5.2)') b_value

!       ----------------------------------------------------------------
!       Computation
!       ----------------------------------------------------------------

!       ----------------------------------------------------------------
!       Reading Atomic data (Morton+2003)
!       ----------------------------------------------------------------
!       Reads in restframe wavelength (in Angstroem), oscillator strength
!       and gamma-value (in Hz) of the corresponding transitions

	ion_found=.false.
	open(20, FILE = 'ions.dat')
	 do 30
	  read(20,*,end=40) ion_str, wl_central, fvalue, gamma
	  if (trim(ion_str).eq.trim(ion_name)) then
	   ion_found=.true.
	   goto 40
	  end if
  30	 continue
  40	 continue
	close(20)
	if (.not.ion_found) then
	 print*, 'Ion ', trim(ion_name),' not found.'
	 stop 1
	end if
!       print*, ion_str, wl_central, fvalue, gamma
!       ----------------------------------------------------------------
!       Calculating optical depth as a function of wavelength
!       ----------------------------------------------------------------
!	Define constants
	sqrtPI = DSQRT(PI)
	Nion = 1.0d1**logNion				! in cm^{-2}

!	Initialize total optical depth and equivalent_width
	tau_tot = 0.0d0
	eqw_aux = 0.0d0

!	Allocate memory for wavelength and velocity array

	size = int(0.5/resolution)
	allocate(wavelength(-size:size))
	allocate(velocity(-size:size))

!       The following parameters are in cgs units      

	dopp_width = (wl_central*b_value/CLIGHT)*1.0D-3
	damping = (wl_central*gamma/(4.0D0*PI*b_value))*1.0D-13
!	Central optical depth
	tau_central = (sqrtPI*ERADIUS*fvalue*(wl_central*wl_central)*
     1  Nion/dopp_width)*1.0D-16
	
!	Define output filename and open

	outfile = 'abs_line_profile.alp'
	
	open(50,file=trim(outfile))

	 do counter=-size,size

	  wavelength(counter) = wl_central + (counter)*resolution
	  velocity(counter) = CLIGHT/1.0d5*DLOG(wavelength(counter)/
     &wl_central)

	  tau_neg = (-tau_central)*
     1	  voigt_hjerting(damping,b_value,wl_central,wavelength(counter))

!	Total optical depth
	  tau_tot = tau_tot - tau_neg

!	Equivalent width
	  eqw_aux = eqw_aux + DEXP(tau_neg)*resolution

!	  write(6,*) CLIGHT/1.0d5*DLOG(wavelength/wl_central),
!     &	  wavelength-wl_central, DEXP(tau_neg)
!     &	  ((wavelength-wl_central)/dopp_width)*1.0D-8,dopp_width,
!     &	  tau_tot

	  write(50,*) CLIGHT/1.0d5*DLOG(wavelength(counter)/wl_central),
     &	  wavelength(counter)-wl_central, DEXP(tau_neg),
     &	  ((wavelength(counter)-wl_central)/dopp_width)*1.0D-8,dopp_width,
     &	  tau_tot
	
	 end do
	
	close(50)

!	Compute extremal values
	wl_min = minval(wavelength)-wl_central
	wl_max = maxval(wavelength)-wl_central
	vel_min = minval(velocity)
	vel_max = maxval(velocity)
	
!	Average optical depth
	tau_tot = tau_tot/(2*size+1)

!	Equivalent width in mA
	eqw_aux = (1 - eqw_aux/(wl_max -wl_min))*1.0d3

!	----------------------------------------------------------------
!	Compute equivalent width using external function 'eqw'

	command = './eqw '//trim(ion_name)//' '//trim(logNion_str)//
     &' '//trim(b_value_str)//' > eqw.dat'
	call system(trim(command))

!	Read equivelent width from file eqw.dat
	open(50, file='eqw.dat', status='old')
	 read(50,*) void, void, equiv_width_mA
	close(50)
!	print*, equiv_width_mA, eqw_aux

!	----------------------------------------------------------------
!	Create gnuplot file

!	Define psfile name
	prefix_index=index(outfile,'.alp')-1
	psfile=outfile(1:prefix_index)//'_alp.ps'

!	Define plotfile name
	plotfile=outfile(1:prefix_index)//'_alp.plot'

!	Define title
	infile = trim(ion_name)

	call create_plotfile(infile,outfile,plotfile,psfile,tau_tot,
     &tau_central,logNion,b_value,wl_min,wl_max,vel_min,vel_max,
     &equiv_width_mA,wl_central,fvalue,gamma)

!	Compile with gnuplot
	command='gnuplot '//trim(plotfile)
	call system(trim(command))

!	Open ps-file
	command='gv '//trim(psfile)//' &'
	call system(trim(command))

!       ----------------------------------------------------------------
	end program
!       ----------------------------------------------------------------

!	================================================================
!	Subroutines and Functions
!	================================================================

!	----------------------------------------------------------------
	subroutine create_plotfile(datfile,linefile,gpfile,psline,
     &	opt_depth,tau_0,col_dens,b_par,wlmin,wlmax,velmin,velmax,eqw,
     &	wl_0,fosc,damp)

!	To plot a 3D color map from a data file with three columns
!	(x:y:z)
!       where each 'iso_line' must be separated by a blank line
!
	implicit none
	
	include 'parameters.f'

	integer :: i

	integer,dimension(1) :: datetime(8)
	integer :: clock

	double precision, intent(in) :: opt_depth,col_dens,b_par,eqw,
     &	tau_0, wl_0, fosc, damp

	double precision, intent(in) :: wlmin, wlmax, velmin, velmax

	character(len=256), intent(in) :: linefile, gpfile, psline

	character(len=10), dimension(1) :: datetime_str(8)

	character(len=256) :: datfile, fontpath, fontfile, timestamp

	character(len=10) :: date, time, zone

!       ----------------------------------------------------------------
!	Get time stamp (include from fitfile to uniquely identify files)
	call date_and_time(date, time, zone, datetime)
	call system_clock(clock)

!	Write datime into a string (for later use)
	write(datetime_str(:),'(i5)') datetime(:)
	timestamp = trim(adjustl(datetime_str(1)))//'/'//
     &trim(adjustl(datetime_str(2)))//'/'//trim(adjustl(datetime_str(3)))
     &//'-'//trim(adjustl(datetime_str(4)))//'-'//
     &trim(adjustl(datetime_str(5)))//'.'//trim(adjustl(datetime_str(6)))
     &//'.'//trim(adjustl(datetime_str(7)))//'.'//
     &trim(adjustl(datetime_str(8)))

!	----------------------------------------------------------------
!	Redefine datfile name (to be used as title for the plot) to
!	to replace underscores by minuses
	do i=1,len_trim(datfile)
	 if (datfile(i:i).eq.'_') datfile(i:i)='-'
	end do

!	Open file
	open(40,file=trim(gpfile))

	 write(40,*) '# ------------------------------------------------'
	 write(40,*) '# line profile plot'
	 write(40,*) '# Generated by abs_line_profile'
	 write(40,*) '# ------------------------------------------------'
	 write(40,*)
	 write(40,*) '# Reset for a clean start'
	 write(40,*) 'reset'
	 write(40,*)
	 write(40,*) '# Set general plot options'
	 write(40,*)
	 write(40,*) '# Key'
	 write(40,*) 'set key bottom right'
	 write(40,*)

!	Define fontpath for fancy symbols
	 fontpath='/usr/share/texmf/fonts/type1/bluesky/cm/'
	 write(40,*) '# Set font path'
	 write(40,*) 'set fontpath '''//trim(fontpath)//''
	 write(40,*) '# Set term x11 and font cmsy10.pfb for special '
     &//'symbols (CMSY10 font) in OMS encoding'
	 write(40,*) '# (see table ~/ps_fontfile_doc.ps)'
	 write(40,*)

!	Define fontfile for fancy symbols
	 fontfile='cmsy10.pfb'
	 write(40,*) 'set term post enhanced color fontfile '''
     &//trim(fontfile)//''
	 write(40,*)
	 write(40,*) '# Set output filename'
	 write(40,*) 'set output "'//trim(psline)//'"'
	 write(40,*)
	 write(40,*) '# For fancy symbol fonts'
	 write(40,*) 'set encoding iso_8859_1'
	 write(40,*)

!	Set margins
	 write(40,*) '# Fixed left and right margins'
	 write(40,*) '#set bmargin 4'
	 write(40,*) '#set tmargin 6'
	 write(40,*) '#set rmargin 4'
	 write(40,*)

!	Set title of plot; size is modified via {/=<size> <text>}
	 write(40,*) '# Set title'
	 write(40,*) 'set title "{/=12 '//trim(datfile)//'}"'
	 write(40,*)

	 write(40,*) '# Set time stamp'
	 write(40,*) '#set label "',clock,'" at graph 0.8, 0.9'
	 write(40,*) '# centered, below title'
	 write(40,*) '#set label "{/=6 '//trim(timestamp)//'}" at '
     &//'graph 0.45, 1.02'
	 write(40,*) '# top right edge'
	 write(40,*) 'set label "{/=6 '//trim(timestamp)//'}" at '
     &//'graph 0.9, 1.08'
	 write(40,*)

	 write(40,*) '# Set text labels'
	 write(40,'(a,f7.2,a)') 'set label "{/=12 equivalent width '
     &//'[m \305] = ',real(eqw),'}" at graph 0.05, 0.28'
	 write(40,'(a,f9.4,a)') 'set label "{/=12 central optical depth = ',
     &real(dble(int(1.0d6*tau_0))/1.0d6),'}" at graph 0.05, 0.24'
	 write(40,'(a,f7.2,a)') 'set label "{/=12 average optical depth = ',
     &real(dble(int(1.0d4*opt_depth))/1.0d4),'}" at graph 0.05, 0.20'
	 write(40,'(a,f5.2,a)') 'set label "{/=12 log_{10} N_{ion} = ',
     &real(dble(int(1.0d2*col_dens))/1.0d2),'}" at graph 0.05, 0.16'
	 write(40,'(a,f5.2,a)') 'set label "{/=12 b-value = ',
     &real(dble(int(1.0d2*b_par))/1.0d2),'}" at graph 0.05, 0.12'
	 write(40,'(a,f7.2,a)') 'set label "{/=12 {/Symbol l}'
     &//'_{0} = ', real(wl_0),' \305}" at graph 0.8, 0.24'
	 write(40,'(a,e10.4,a)') 'set label "{/=12 f'
     &//'_{osc} = ', real(fosc),'}" at graph 0.8, 0.20'
	 write(40,'(a,e10.4,a)') 'set label "{/=12 {/Symbol G}'
     &//' = ', real(damp),' s^{-1}}" at graph 0.8, 0.16'
	 write(40,*)

	 write(40,*) '# tics format (default)'
	 write(40,*) 'set xtics nomirror format "{/=12% g}"'
	 write(40,*) 'set x2tics nomirror format "{/=12% g}"'
	 write(40,*) 'set ytics format "{/=12% g}"'
	 write(40,*)

!	Set minor tics on both axes
	 write(40,*) '# Set minor tics on all axes'
	 write(40,*) 'set mxtics'
	 write(40,*) 'set mx2tics'
	 write(40,*) 'set mytics'
	 write(40,*)

!	Set axes labels; size is modified via {/=<size> <text>}
	 write(40,*) '# Set axes label'
	 write(40,*) 'set xlabel "{/=10 {/Symbol l - l_{0}} [\305]"'
	 write(40,*) 'set x2label "{/=10 Velocity [km s^{ -1}]}"'
	 write(40,*) 'set ylabel "{/=10 Transmission}"'
	 write(40,*)

	 write(40,*) '# Set xy-range'
	 write(40,*) 'set xrange [',wlmin,':',wlmax,']'
	 write(40,*) 'set x2range [',velmin,':',velmax,']'
	 write(40,*) 'set yrange [-0.1:1.1]'
	 write(40,*)

!	Define constant function at transmission = 0
	 write(40,*) '# Define constant function at transmission = 0'
	 write(40,*) 't_0(x) = 0'

!	Plot in x-y scale
	 write(40,*) '# Plot data file'
	 write(40,*) 'plot '''//trim(linefile)//''' u ($2):($3) axes x1y1 '//
     &'w l lw 2 lc rgb "black" notitle \'
	 write(40,*) ', t_0(x) w l lt 2 lw 2 lc rgb "black" notitle \'
	 write(40,*) '#, '''//trim(linefile)//''' u ($1):($3) axes x2y1 '//
     &'w l lw 2 notitle \'
	 write(40,*)

!	Close file (do not forget!)
	close(40)

	end subroutine
!       ----------------------------------------------------------------

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
	x2 = x*x
	h0 = DEXP(-x2)
	Q = 1.5D0/x2

	if (DABS(x).GT.4.0D-4) then		!arbitrarily set

	 voigt_hjerting = h0 - a/DSQRT(PI)/x2*
     1	 (h0*h0*(4.0D0*x2*x2 + 7.0D0*x2 + 4.0D0 + Q) - 1.0D0 - Q)

	else					!limes for x -> 0

	 voigt_hjerting = 1.0d0 - 2.0D0*a/DSQRT(PI)

	endif
	
	return
!       ----------------------------------------------------------------
	end function
!       ----------------------------------------------------------------
