!       ------------------------------------------------------------------------
!       Voigt-Profile approximation to model absorption lines
!       ------------------------------------------------------------------------
!       (C) Thorsten Tepper Garcia 2008
!       Based on the approximation to the Voigt-Hjerting function
!       published in:
!       Tepper Garcia, Thorsten
!       Monthly Notices of the Royal Astronomical Society; Volume 369, 
!       Issue 4, Page 2025
!       Please refer to the author when using this approximation.

        program abs_line_profile_multicomp

!       Absorption Lines are calculated according to the
!       approximation for a<<1 (where a is the damping parameter in the
!       Voigt-Hjerting function H(a,x)) given by (Tepper Garcia 2006)
!       
!       H(a,x) = h0-a/sqrt(PI)/x^2*(h0^2*(4x^4 + 7x^2 + 4 + Q) - 1 - Q)
!       with h0=exp(-x^2), Q=1.5/x^2
!
!       The arguments of the program are a transition in the form e.g.
!       OVI_1031 (see ions.dat for a list of the avaible transitison),
!       the Doppler parameter 'b' (in units km/s) and the logarithmic
!       column density 'log10_Nion' (in units of log10(cm^{-2})).
!
!       The output of the program is:
!       a) a file named eqw.dat listing
!       log10_Nion | b | equivalent width (mili-Angstroem)
!       b) a plot of the line profile as a function of wavelength and
!       velocity (with respect to the line center). This plot should be
!       displayed automatically) 
!
!       Graphical output is supressed if plot = 'F'
!       All physical and other needed parameters are included in the file
!       'parameter.f'. The wavelength (and velocity) range is automatically
!       computed with a pixel size of 0.001 A.
!
!       The line parameters (central wavelength of the transition,
!       oscillator strength, and gamma- (damping-) value) are contained
!       in the file 'ions.dat', and were taken from Morton's Compilation
!       (see Tepper-Garcia 2006 for corresponding reference).
!
!       NOTE: The author is not responsible for any modifications
!       done to this code in its original form.
!
!       Please report any bugs or problems to:
!       tepper@astro.physik.uni-goettingen.de
!       ------------------------------------------------------------------------
!       Variables declaration
!       ------------------------------------------------------------------------
        implicit none

        include 'parameters.f'

        integer :: argcount
        integer :: prefix_index, counter, indx, size, nveloc, comp, components
        integer :: seed

        double precision :: tau_neg, tau_central, flux_central
        double precision :: fwhm_kms
        
        double precision, allocatable :: flux(:), flux_conv(:)

        double precision, allocatable :: gaussdev(:)

        double precision, allocatable :: wavelength(:), velocity(:)
        double precision, allocatable:: sigma(:), noise(:)
        double precision :: flux_noisy
        double precision :: profile_func

        double precision :: wl_min, wl_max, vel_min, vel_max, wl_range
        double precision :: line_centre, lambda_0, fvalue, gamma, m_ion
        double precision :: equiv_width_mA, eqw_aux_nc, eqw_aux_cv
        double precision :: void, temp
        double precision :: b_value, logNion, Nion, dopp_width, damping

        double precision :: voigt_hjerting, gauss
        double precision :: gasdev
        double precision :: minnoise=1.0d-3
        double precision :: signaltonoise

        double precision :: pixel_size_Ang !in Angstroem
        double precision :: vel_sep ! velocity separation in km/s
        double precision, parameter :: pixel_size_kms = 1.0d0 !in km/s

        character(len=256) :: b_value_str, logNion_str
        character(len=256) :: ion_name, ion_str
        character(len=256) :: homedir
        character(len=256) :: infile, outfile_alp, outfile_ifit,
     +  plotfile, psfile
        character(len=256) :: command
        character(len=10) :: signaltonoise_str, plot_str, fwhm_str
        character(len=5) :: profile_str, vel_sep_str, comp_str

        character(len=256) :: errmsg, warnmsg, statmsg
        
        logical :: ion_found, addnoise
        
!       ------------------------------------------------------------------------
!       Define program error and warning messages;
!       The lines below are meant to print the different
!       messages in different colors

!       In red=31
        errmsg=CHAR(27)//'[1;31mERROR: abs_line_profile_multicomp ...'//
     &  CHAR(27)//'[0m'
!       In yellow=33
        warnmsg=CHAR(27)//'[1;33mWARNING: abs_line_profile_multicomp ...'//
     &  CHAR(27)//'[0m'
!       In green=32
        statmsg=CHAR(27)//'[1;32mSTATUS: abs_line_profile_multicomp ...'//
     &  CHAR(27)//'[0m'

!       ------------------------------------------------------------------------
!       Getting arguments
!       ------------------------------------------------------------------------
        argcount = IArgC()
              if (argcount .ne. 9) then
         write(6,*) 'USAGE: abs_line_profile_multicomp <transition> <log10 Nion> '//
     &   '<b (km/s)> <noise (S/N or inf)> <profile (voigt | gauss) '//
     &   ' <FWHM (0. for no convolution)> <vel_sep> <components> <plot (T/F)>'
         stop 1
              end if
        
        call GetArg(1,ion_name)

        call GetArg(2,logNion_str)
        read (logNion_str,'(F6.0)') logNion

        call GetArg(3,b_value_str)
        read (b_value_str,'(F6.0)') b_value

        call GetArg(4,signaltonoise_str)
        
        if (trim(signaltonoise_str).eq.'inf') then
         signaltonoise=1.0d3
         addnoise=.false.
        else
         read (signaltonoise_str,'(f6.0)') signaltonoise
         addnoise=.true.
        end if

        call GetArg(5,profile_str)
        if ((profile_str.ne.'gauss').and.(profile_str.ne.'voigt')) then
         write(6,*) trim(errmsg)//'Non-valid profile : '//trim(profile_str)
         stop 1
        end if

        call GetArg(6,fwhm_str)
        read (fwhm_str,'(f5.0)') fwhm_kms

        call GetArg(7,vel_sep_str)
        read (vel_sep_str,'(f5.0)') vel_sep

        call GetArg(8,comp_str)
        read (comp_str,'(i2)') components
        
        call GetArg(9,plot_str)
        
!       ------------------------------------------------------------------------
!       Computation
!       ------------------------------------------------------------------------
!       Define home directory
        
!       homedir = '/home/volans/tepper/'
        call system('echo $HOME > homedir')
        open(5,file='homedir')
        read(5,'(a)') homedir
        close(5)
        
        homedir = trim(homedir)
!       print*, trim(homedir)
        
!       ------------------------------------------------------------------------
!       Reading Atomic data (Morton+2003)
!       ------------------------------------------------------------------------
!       Reads in restframe wavelength (in Angstroem), oscillator strength
!       and gamma-value (in Hz) of the corresponding transitions

        ion_found=.false.
        open(20, FILE = 'ions.dat')
         do 30
          read(20,*,end=40) ion_str, lambda_0, fvalue, gamma
          if (trim(ion_str).eq.trim(ion_name)) then
           ion_found=.true.
           goto 40
          end if
  30         continue
  40         continue
        close(20)
        if (.not.ion_found) then
         write(6,*) trim(errmsg)//'Ion ', trim(ion_name),' not found.'
         stop 1
        end if

!       ------------------------------------------------------------------------
!       Define wavelength range in terms of the velocity separation

!       Compute pixel size in Angstroem
        pixel_size_Ang = (pixel_size_kms * lambda_0) / (CLIGHT*1.0d-5)

!       Define wavelength range (5x the equivalent width in Angstroem)
        wl_range = 5.0d0 * (vel_sep / pixel_size_kms) * pixel_size_Ang

        write(6,*) 
     &  'ion    rest-wavelength    f_osc   damping      logNion    '//
     &  'b-value   pixel [km/s]  pixel [A]    profile'
        write(6,
     &  '(a,5x,f8.3,5x,f8.3,1pe12.4,1pe12.4,1pe12.4,1pe12.4,1pe12.4,5x,a)')
     &  trim(ion_str), lambda_0, fvalue, gamma, logNion, b_value,
     &  pixel_size_kms, pixel_size_Ang, trim(profile_str)

!       Allocate memory 
        size = nint(wl_range/pixel_size_Ang)
        nveloc = 2*size + 1
        allocate(flux(1:nveloc))
        allocate(flux_conv(1:nveloc))
        allocate(wavelength(-size:size))
        allocate(velocity(-size:size))
        allocate(sigma(-size:size))
        allocate(noise(-size:size))
        allocate(gaussdev(-size:size))
        flux(:) = 0.0d0
        wavelength(:) = 0.0d0
        velocity(:) = 0.0d0
        sigma(:) = 0.0d0
        noise(:) = 0.0d0
        gaussdev(:) = 0.0d0

!       ------------------------------------------------------------------------
!       Gas temperature [K], assuming width [km/s] to be purely thermal
        temp=1.0d0

!       Define ion mass
        if (ion_name(1:3).eq.'OVI') then
         m_ion = A_O*H_MASS
        else if (ion_name(1:6).eq.'NeVIII') then
         m_ion = A_Ne*H_MASS
        else if (ion_name(1:3).eq.'CIV') then
         m_ion = A_C*H_MASS
        else if (ion_name(1:2).eq.'HI') then
         m_ion = H_MASS
        else
         write(6,*) trim(warnmsg)//'m_ion not yet defined for other'
     &   //' ions than h1, o6, and ne8'
         stop 1
        end if
        
        temp=(b_value * b_value) / (2.0d0 * boltzmann * 1.0d-6 / m_ion)

!       ------------------------------------------------------------------------
!       Generate Gaussian random numbers (mean = 0, dispersion = 1)

!       Initialisation seed (has to be a negative integer)
        
        gaussdev(:) = 0.0d0
        
        if (addnoise) then
         seed= 0
         do counter=-size,size
          gaussdev(counter) = gasdev(seed)
         end do
        end if

!       The minimum noise value should be constrained by the given S/N:
!       Not sure about this...
!        minnoise = min(minnoise,1.0d-1/signaltonoise)

!       ------------------------------------------------------------------------
!       compute spectrum
!       ------------------------------------------------------------------------

!       initialise flux
        
        flux(:) = 1.0d0

!       loop over number of components
        
        DO comp=0,components-1
        
         indx = 0
         profile_func = 0.0d0

!       line parameters

!       velocity centroid (in Angstroem)
         line_centre = lambda_0 +
     &   (0.5*(1-components) + comp)*(vel_sep / pixel_size_kms)*pixel_size_Ang

!       column density, doppler width, damping (cgs units)
         Nion = 1.0d1**(logNion) ! in cm^{-2}
         dopp_width = (lambda_0*b_value/CLIGHT)*1.0d-3
         damping = (lambda_0*gamma/(4.0D0*PI*b_value))*1.0d-13

!       central optical depth
         tau_central = (dsqrt(PI)*ERADIUS*fvalue*(lambda_0*lambda_0)*
     &   Nion/dopp_width)*1.0d-16

!       flux corresponding to central optical depth
!       Check numerical precision
         if ((-1.0d0*tau_central).le.(dlog(tiny(1.0d0)))) then
          flux_central = 0.0d0
         else
          flux_central = dexp(-1.0d0*tau_central)
         end if


!       loop over pixel
        do counter=-size,size

         indx = indx + 1

         wavelength(counter) = lambda_0 + (counter)*pixel_size_Ang
         velocity(counter) = (CLIGHT/1.0d5)*dlog(wavelength(counter)/lambda_0)

!       optical depth; check for numerical precision

         if (profile_str.eq.'voigt')
     &    profile_func =
     &    voigt_hjerting(damping,b_value,lambda_0,line_centre,wavelength(counter))
         
         if (profile_str.eq.'gauss')
     &    profile_func = gauss(b_value,lambda_0,line_centre,wavelength(counter))
        
         if (profile_func.eq.0.0d0) then
          tau_neg = 0.0d0
         else if ((dlog10(tau_central)+dlog10(profile_func)).lt.
     &    dlog10(tiny(1.0d0))) then
          tau_neg = 0.0d0
         else
          tau_neg = (-tau_central)*profile_func
         end if

!       flux; check numerical precision
         if (tau_neg.le.dlog(dsqrt(tiny(1.0d0))*signaltonoise)) then
          flux(indx) = 0.0d0
         else
          flux(indx) = flux(indx) * dexp(tau_neg)
         end if

!       compute wavelength/velocity boundaries
         if (counter.eq.-size) wl_min = wavelength(counter)-lambda_0
         if (counter.eq.size) wl_max = wavelength(counter)-lambda_0
         if (counter.eq.-size) vel_min = velocity(counter)
         if (counter.eq.size) vel_max = velocity(counter)
        
        end do
        
        END DO
!       ------------------------------------------------------------------------
!       convolve spectrum
!       ------------------------------------------------------------------------

        flux_conv(:) = flux(:)

        if (fwhm_kms.gt.0.0d0)
     &  call convolve_spectrum(flux_conv,nveloc,pixel_size_kms,fwhm_kms)

!       ------------------------------------------------------------------------
!       OUTPUT 1: to be shown graphically
!       ------------------------------------------------------------------------

!       Define output filename and open

        outfile_alp = 'output_data/abs_line_profile_multicomp_'//trim(ion_str)//'_'//
     &  trim(logNion_str)//'_'//trim(b_value_str)//'_'//
     &  trim(signaltonoise_str)//'.alp'

!       ------------------------------------------------------------------------
!       OUTPUT 2: used for ifitnot benchmarking
!       ------------------------------------------------------------------------

!       Define output filename and open

        outfile_ifit = 'output_data/abs_line_profile_multicomp_'//trim(ion_str)//'_'//
     &  trim(logNion_str)//'_'//trim(b_value_str)//'_'//
     &  trim(signaltonoise_str)//'.ifit'

        open(60,file=trim(outfile_ifit))

        open(50,file=trim(outfile_alp))
         
         eqw_aux_nc = 0.0d0
         eqw_aux_cv = 0.0d0

         indx = 0
         do counter=-size,size

          indx = indx + 1

!       add noise; check numerical precision
          
          if (addnoise) then
           sigma(counter) = sqrt( minnoise**2.0 +
     &     (flux_conv(indx)/signaltonoise)**2.0 )
           noise(counter) = sigma(counter)*gaussdev(counter)
          else
           sigma(counter) = minnoise  
           noise(counter) = 0.0d0
          end if

          if (flux_conv(indx).le.(tiny(1.0d0) - noise(counter))) then
           flux_noisy = flux_conv(indx) + abs(noise(counter))
          else
           flux_noisy = flux_conv(indx) + noise(counter)
          end if

!       Equivalent width
          eqw_aux_nc = eqw_aux_nc + (1.0d0 - flux(indx))*pixel_size_Ang
          eqw_aux_cv = eqw_aux_cv + (1.0d0 - flux_conv(indx))*pixel_size_Ang

          write(50,*) velocity(counter), (wavelength(counter)-lambda_0),
     &    flux_noisy, flux(indx), flux_conv(indx)

          write(60,*) wavelength(counter), flux_noisy, sigma(counter)
        
         end do
        
        close(50)

        close(60)

!       Compute wquivalent width in mA to double-check
        eqw_aux_nc = eqw_aux_nc*1.0d3
        eqw_aux_cv = eqw_aux_cv*1.0d3
        equiv_width_mA = eqw_aux_cv
!       ----------------------------------------------------------------
!       Compare both measurements (should agree within 1%)
        write(6,*) 'Equivalent Width (non-conv) Equivalent Width (conv)'
        write(6,*) eqw_aux_nc, eqw_aux_cv

!       ----------------------------------------------------------------
!       Create gnuplot file
        
        if (plot_str.ne.'F') then

!       Define psfile name
        prefix_index=index(outfile_alp,'.alp')-1
        psfile=outfile_alp(1:prefix_index)//'_alp.ps'

!       Define plotfile name
        plotfile=outfile_alp(1:prefix_index)//'_alp.plot'

!       Define title
        infile = trim(ion_name)

        call create_plotfile(infile,outfile_alp,plotfile,psfile,
     &  tau_central,flux_central,logNion,b_value,wl_min,wl_max,vel_min,
     &  vel_max,equiv_width_mA,lambda_0,fvalue,gamma,temp,signaltonoise)

!       Compile with gnuplot
        command='gnuplot '//trim(plotfile)
        call system(trim(command))

!       Open ps-file
        command='gv '//trim(psfile)//' &'
        call system(trim(command))
        
        end if!plot

!       ------------------------------------------------------------------------
        end program
!       ------------------------------------------------------------------------

!       ========================================================================
!       Subroutines and Functions
!       ========================================================================

!       ------------------------------------------------------------------------
        double precision function voigt_hjerting(a,b,wl0,cwl,wl)

!       Returns the value of the Voigt-Hjerting function H(a,x) as a
!       function of wavelength for a given a-parameter, Doppler
!       parameter (in km/s), and central wavelength (in Angstroem) of
!       the corresponding transition.

        double precision :: a, b, dopp, wl0, cwl, wl
        double precision :: x, x2, h0, Q
        double precision, parameter :: PI=3.141593d0, CLIGHT=2.9979d+10
        
        dopp = (wl0*b/CLIGHT)*1.0d-3
        x = ((wl-cwl)/dopp)*1.0d-8

!       Check numerical precision
!       See ~/hjerting/lyman/numeric (dir)

        if (dabs(x).gt.4.0d-4) then

         x2 = x*x

!       Since h0*h0 = exp(-2x) is used below, check the exponent of
!       exp(-x); it must satisfy -x > 0.5*log(smallest number)

         if ((-1.0d0*x2).le.(5.0d-1*dlog(tiny(1.0d0)))) then
          h0 = 0.0d0
         else
          h0 = dexp(-1.0d0*x2)
         end if

         Q = 1.5d0/x2

         voigt_hjerting = h0 - a/dsqrt(PI)/x2*
     1         (h0*h0*(4.0d0*x2*x2 + 7.0d0*x2 + 4.0d0 + Q) - 1.0d0 - Q)

        else                                        !limit for x -> 0

         voigt_hjerting = 1.0d0 - 2.0d0*a/DSQRT(PI)

        endif
        
        return
!       ------------------------------------------------------------------------
        end function voigt_hjerting
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        double precision function gauss(b,wl0,cwl,wl)

!       Returns the value of the Gaussian function as a function of
!       wavelength for a given Doppler parameter (in km/s), and central
!       wavelength (in Angstroem) of the corresponding transition.

        double precision :: b, dopp, wl0, cwl, wl
        double precision :: x
        double precision, parameter :: CLIGHT=2.9979d+10
        
        dopp = (wl0*b/CLIGHT)*1.0d-3
        x = ((wl-cwl)/dopp)*1.0d-8

!       check for numerical precision to avoid floating exception
        if (dabs(x).lt.(dsqrt(-dlog(tiny(1.0d0))))) then
         gauss = dexp(-1.0d0*x*x)
        else
         gauss = 0.0d0
        end if

        return
!       ------------------------------------------------------------------------
        end function gauss
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       Numerical Recipes
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       GENERATES UNIFORMILY DISTRIBUTED RANDOM NUMBERS
              
        function ran1(idum)
        integer idum,IA,IM,IQ,IR,NTAB,NDIV
        double precision ran1,AM,EPS,RNMX
        parameter (IA=16807,IM=2147483647,AM=1.0D0/IM,IQ=127773,IR=2836,
     !  NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2D-7,RNMX=1.0D0-EPS)
        integer j,k,iv(NTAB),iy
        save iv,iy
        data iv /NTAB*0/, iy /0/
        if (idum.le.0.or.iy.eq.0) then
         idum=max(-idum,1)
         do 11 j=NTAB+8,1,-1
          k=idum/IQ
          idum=IA*(idum-k*IQ)-IR*k
          if (idum.lt.0) idum=idum+IM
          if (j.le.NTAB) iv(j)=idum
11       continue
         iy=iv(1)
         end if
         k=idum/IQ
         idum=IA*(idum-k*IQ)-IR*k
         if (idum.lt.0) idum=idum+IM
         j=1+iy/NDIV
         iy=iv(j)
         iv(j)=idum
         ran1=min(AM*iy,RNMX)
         return
         end function
C  (C) Copr. 1986-92 Numerical Recipes Software 5.W7.
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       GENERATES GAUSSIAN DISTRIBUTED RANDOM NUMBERS

        function gasdev(idum)
        integer idum
        double precision gasdev
CU      USES ran1
        integer iset
        double precision fac,gset,rsq,v1,v2,ran1
        SAVE iset,gset
        DATA iset/0/
        if (iset.eq.0) then
1        v1=2.0D0*ran1(idum)-1.0D0
         v2=2.0D0*ran1(idum)-1.0D0
         rsq=v1**2+v2**2
         if(rsq.ge.1.0D0.or.rsq.eq.0.0D0) goto 1
         fac=sqrt((-2.0D0)*dlog(rsq)/rsq)
         gset=v1*fac
         gasdev=v2*fac
         iset=1
        else
         gasdev=gset
         iset=0
        end if
        return
        end function
C  (C) Copr. 1986-92 Numerical Recipes Software 5.W7.
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       Stolen from SpecWizard
!       USES convlv

        subroutine convolve_spectrum(flx,num_px,px_size,fwhm)

!       flux = signal
!       nveloc = number of pixels
!       vpixsize = size of pixel in km/s
!       fwhm_kms = FWHM of Gaussian response in km/s

        implicit none

        double precision :: sigmakms, b, norm, fwhm, px_size
        integer :: i,j,off, nvpix, num_px
        double precision, allocatable, save :: gauss(:)
        double precision  :: flx(*)
        double precision, allocatable  :: convl_flux(:),
     &  convl_flux_convolved(:)

        write(*,*) 'Convolving with Gaussian PSF using fwhm = ', fwhm
        write(*,*)

!       determine number of pixels
        nvpix = num_px

!       Compute sigma in km/s
        sigmakms = fwhm / 2.35482

!       Compute sigma in units of pixels
        b = sigmakms / px_size
        
!       For convolution with instrumental PSF we need to Fourier 
!       transform, we thus need to increase the array so that it is a
!       power of 2.
        nvpix = int(2**(aint(dlog(dble(nvpix))/log(2.)) + 1))
        
!       Create normalized Gaussian in wrap-around order (required by
!       convlv)

        allocate(gauss(nvpix))
        norm = 1.d0 / (2.d0 * b * b)
        do i = 0, nvpix-1
           if (i .le. nvpix-1) then 
              if (i .le. nvpix/2) then
                 j = i
              else
                 j = i - nvpix
              endif
              if (abs(j) .lt. 10.*b) then
                 gauss(i+1) = exp(-(dble(j)**2.)*norm)
              else
                 gauss(i+1) = 0.d0
              endif
           else
              gauss(i+1) = 0.d0
           endif
        enddo

!       normalise Gaussian
        gauss  = gauss / sum(gauss)

        allocate(convl_flux(nvpix),convl_flux_convolved(2*nvpix))
        convl_flux(:) = 0.0d0
        convl_flux(1:num_px) = flx(1:num_px)

!       copy periodic copies of the flux signal into the zero buffer
!       to avoid aliasing (or end) effects
        do i=num_px+1,nvpix
           off = i-num_px
           if (off .lt. (nvpix-num_px)/2.) then
              convl_flux(i) = convl_flux(i-num_px)
           else
              convl_flux(i) = convl_flux(i-(nvpix-num_px))
           endif
        enddo
        convl_flux_convolved(:) = 0.0

        call convlv(convl_flux,nvpix,gauss,nvpix,1,convl_flux_convolved)

        flx(1:num_px) = convl_flux_convolved(1:num_px)

        deallocate(convl_flux,convl_flux_convolved)

        return
        end subroutine convolve_spectrum
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       Stolen from SpecWizard
!       USES twofft; realft

        subroutine convlv(data,n,respns,m,isign,ans)

        implicit none 

        integer, intent(in) :: n, m, isign
        double precision, intent(in)    :: data(n)
        double precision, intent(inout) :: respns(n)
        double complex, intent(out):: ans(2*n)

        double complex, allocatable :: fft(:)

        integer i,no2
        integer, save :: nfft=-1

        if(nfft .ne. n)then
           allocate(fft(n))
           nfft = n
        endif

        do  i=1,(m-1)/2
           respns(n+1-i)=respns(m+1-i)
        enddo
        do  i=(m+3)/2,n-(m-1)/2
           respns(i)=0.0
        enddo

        call twofft(data,respns,fft,ans,n)
        no2=n/2
        do i=1,no2+1
           if (isign.eq.1) then
              ans(i)=fft(i)*ans(i)/no2
           else if (isign.eq.-1) then
              if (abs(ans(i)).eq.0.0) then
                write(*,*) 'deconvolving at response zero in convlv'
                stop
              end if
              ans(i)=fft(i)/ans(i)/no2
           else
              stop 'no meaning for isign in convlv'
           endif
        enddo
        ans(1)=dcmplx(dble(ans(1)),dble(ans(no2+1)))
        call realft(ans,n,-1)

        return
        end subroutine convlv
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       Stolen from SpecWizard
!       USES four1

        subroutine twofft(data1,data2,fft1,fft2,n)

        implicit none 
        integer, intent(in) :: n
        double precision, intent(in) :: data1(n), data2(n)
        double complex :: fft1(n), fft2(n)

        ! local variables
        integer j,n2
        double complex ::  h1,h2,c1,c2

        c1=dcmplx(0.5,0.0)
        c2=dcmplx(0.0,-0.5)
        do  j=1,n
           fft1(j)=dcmplx(data1(j),data2(j))
        enddo
        call four1(fft1,n,1)
        fft2(1)=dcmplx(imag(fft1(1)),0d0)
        fft1(1)=dcmplx(dble(fft1(1)),0d0)
        n2=n+2
        do j=2,n/2+1
           h1=c1*(fft1(j)+conjg(fft1(n2-j)))
           h2=c2*(fft1(j)-conjg(fft1(n2-j)))
           fft1(j)=h1
           fft1(n2-j)=conjg(h1)
           fft2(j)=h2
           fft2(n2-j)=conjg(h2)
        enddo
        return
        end subroutine twofft
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       Stolen from SpecWizard
!       USES four1

        subroutine realft(data,n,isign)

        implicit none
        integer, intent(in) :: n, isign
        double precision, intent(inout) :: data(n)
        ! local variables
        integer i,i1,i2,i3,i4,n2p3
        double precision c1,c2,h1i,h1r,h2i,h2r,wis,wrs
        double precision theta,wi,wpi,wpr,wr,wtemp
        theta=3.141592653589793d0/dble(n/2)
        c1=0.5
        if (isign.eq.1) then
           c2=-0.5
           call four1(data,n/2,+1)
        else
           c2=0.5
           theta=-theta
        endif
        wpr=-2.0d0*sin(0.5d0*theta)**2
        wpi=sin(theta)
        wr=1.0d0+wpr
        wi=wpi
        n2p3=n+3
        do  i=2,n/4
           i1=2*i-1
           i2=i1+1
           i3=n2p3-i2
           i4=i3+1
           wrs=sngl(wr)
           wis=sngl(wi)
           h1r=c1*(data(i1)+data(i3))
           h1i=c1*(data(i2)-data(i4))
           h2r=-c2*(data(i2)+data(i4))
           h2i=c2*(data(i1)-data(i3))
           data(i1)=h1r+wrs*h2r-wis*h2i
           data(i2)=h1i+wrs*h2i+wis*h2r
           data(i3)=h1r-wrs*h2r+wis*h2i
           data(i4)=-h1i+wrs*h2i+wis*h2r
           wtemp=wr
           wr=wr*wpr-wi*wpi+wr
           wi=wi*wpr+wtemp*wpi+wi
        enddo
        if (isign.eq.1) then
           h1r=data(1)
           data(1)=h1r+data(2)
           data(2)=h1r-data(2)
        else
           h1r=data(1)
           data(1)=c1*(h1r+data(2))
           data(2)=c1*(h1r-data(2))
           call four1(data,n/2,-1)
        endif
        return
        end subroutine realft
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       Stolen from SpecWizard

        subroutine four1(data,nn,isign)

        implicit none
        integer, intent(in) :: nn, isign
        double precision, intent(inout) :: data(2*nn)
        ! local variables  
        integer i,istep,j,m,mmax,n
        double precision :: tempi,tempr
        double precision ::  theta,wi,wpi,wpr,wr,wtemp

        n=2*nn
        j=1
        do  i=1,n,2
           if(j.gt.i)then
              tempr=data(j)
              tempi=data(j+1)
              data(j)=data(i)
              data(j+1)=data(i+1)
              data(i)=tempr
              data(i+1)=tempi
           endif
           m=n/2
1         if ((m.ge.2).and.(j.gt.m)) then
              j=j-m
              m=m/2
              goto 1
         endif
         j=j+m
        enddo
        mmax=2
2        if (n.gt.mmax) then
        istep=2*mmax
        theta=6.28318530717959d0/(isign*mmax)
        wpr=-2.d0*sin(0.5d0*theta)**2
        wpi=sin(theta)
        wr=1.d0
        wi=0.d0
        do m=1,mmax,2
           do  i=m,n,istep
              j=i+mmax
              tempr=sngl(wr)*data(j)-sngl(wi)*data(j+1)
              tempi=sngl(wr)*data(j+1)+sngl(wi)*data(j)
              data(j)=data(i)-tempr
              data(j+1)=data(i+1)-tempi
              data(i)=data(i)+tempr
              data(i+1)=data(i+1)+tempi
           enddo
           wtemp=wr
           wr=wr*wpr-wi*wpi+wr
           wi=wi*wpr+wtemp*wpi+wi
        enddo
        mmax=istep
        goto 2
        endif
        return
        end subroutine four1
!       ------------------------------------------------------------------------

!       ----------------------------------------------------------------
        subroutine create_plotfile(datfile,linefile,gpfile,psline,
     &  tau_0,f_0,col_dens,b_par,wlmin,wlmax,velmin,velmax,eqw,
     &  wl_0,fosc,damp,temp,sn)

!       To plot a 3D color map from a data file with three columns
!       (x:y:z)
!       where each 'iso_line' must be separated by a blank line
!
        implicit none
        
        include 'parameters.f'

        integer :: i

        integer :: datetime(8)
        integer :: clock

        double precision :: col_dens,b_par,eqw,
     &  tau_0, f_0, wl_0, fosc, damp, temp, sn

        double precision :: wlmin, wlmax, velmin, velmax

        character(len=256), intent(in) :: linefile, gpfile, psline

        character(len=10) :: datetime_str(8)

        character(len=256) :: datfile, fontpath, fontfile, timestamp

        character(len=10) :: date, time, zone

!       ------------------------------------------------------------------------
!       Get time stamp (include from fitfile to uniquely identify files)
        call date_and_time(date, time, zone, datetime)
        call system_clock(clock)

!       Write datime into a string (for later use)
        write(datetime_str(:),'(i5)') datetime(:)
        timestamp = trim(adjustl(datetime_str(1)))//'/'//
     &  trim(adjustl(datetime_str(2)))//'/'//trim(adjustl(datetime_str(3)))
     &  //'-'//trim(adjustl(datetime_str(4)))//'-'//
     &  trim(adjustl(datetime_str(5)))//'.'//trim(adjustl(datetime_str(6)))
     &  //'.'//trim(adjustl(datetime_str(7)))//'.'//
     &  trim(adjustl(datetime_str(8)))

!       ----------------------------------------------------------------
!       Redefine datfile name (to be used as title for the plot) to
!       to replace underscores by minuses
        do i=1,len_trim(datfile)
         if (datfile(i:i).eq.'_') datfile(i:i)='-'
        end do

!       Open file
        open(40,file=trim(gpfile))

         write(40,*) '# ------------------------------------------------'
         write(40,*) '# line profile plot'
         write(40,*) '# Generated by abs_line_profile_multicomp'
         write(40,*) '# ------------------------------------------------'
         write(40,*)
         write(40,*) '# Reset for a clean start'
         write(40,*) 'reset'
         write(40,*)
         write(40,*) '# Set general plot options'
         write(40,*)
         write(40,*) '# Key'
         write(40,*) 'set key top right'
         write(40,*)

!       Define fontpath for fancy symbols
         fontpath='/usr/share/texmf/fonts/type1/bluesky/cm/'
         write(40,*) '# Set font path'
         write(40,*) '# set fontpath '''//trim(fontpath)//''
         write(40,*) '# Set term x11 and font cmsy10.pfb for special '
     &   //'symbols (CMSY10 font) in OMS encoding'
         write(40,*) '# (see table ~/ps_fontfile_doc.ps)'
         write(40,*)

!       Define fontfile for fancy symbols
         fontfile='cmsy10.pfb'
!        write(40,*) 'set term post enhanced color fontfile '''
!     &   //trim(fontfile)//''' dashlength 1'
         write(40,*) 'set term post enhanced color dashlength 1'
         write(40,*)
         write(40,*) '# Set output filename'
         write(40,*) 'set output "'//trim(psline)//'"'
         write(40,*)
         write(40,*) '# For fancy symbol fonts'
         write(40,*) 'set encoding iso_8859_1'
         write(40,*)

!       Set title of plot; size is modified via {/=<size> <text>}
         write(40,*) '# Set title'
         write(40,*) 'set title "{/=12 '//trim(datfile)//'}"'
         write(40,*)

         write(40,*) '# Set time stamp'
         write(40,*) '#set label "',clock,'" at graph 0.8, 0.9'
         write(40,*) '# centered, below title'
         write(40,*) '#set label "{/=6 '//trim(timestamp)//'}" at '
     &   //'graph 0.45, 1.02'
         write(40,*) '# top right edge'
         write(40,*) '#set label "{/=6 '//trim(timestamp)//'}" at '
     &   //'graph 0.9, 1.08'
         write(40,*)

         write(40,*) '# Set text labels'
         write(40,'(a,1pe8.2,a)') 'set label "{/=18 S/N=',
     &   sn,'}" tc rgb "royalblue" at graph 0.05, 0.9'

         write(40,'(a,1pe12.2,a)') 'set label "{/=12 equivalent width '
     &   //'[m \305] = ',eqw,'}" at graph 0.05, 0.32'

         write(40,'(a,1pe12.2,a)') 'set label "{/=12 central optical depth = ',
     &   tau_0,'}" at graph 0.05, 0.28'

         write(40,'(a,1pe10.2e3,a)') 'set label "{/=12 flux at central opt.depth = ',
     &   f_0,'}" at graph 0.05, 0.24'

         write(40,'(a,f5.2,a)') 'set label "{/=12 log_{10} N_{ion} = ',
     &   col_dens,'}" at graph 0.05, 0.16'

         write(40,'(a,f6.2,a)') 'set label "{/=12 b-value = ',
     &   b_par,'}" at graph 0.05, 0.12'

         write(40,'(a,f5.2,a)') 'set label "{/=18 log (T/K) =',
     &   dlog10(temp),'" left tc rgb "red" at graph 0.75, 0.32'

         write(40,'(a,f5.2,a)') 'set label "{/=18 log (N/b) =',
     &   col_dens-dlog10(b_par),'" left tc rgb "black" at graph 0.75, 0.24'

         write(40,'(a,e10.4,a)') '#set label "{/=12 f'
     &   //'_{osc} = ', fosc,'}" at graph 0.8, 0.20'

         write(40,'(a,e10.4,a)') '#set label "{/=12 {/Symbol G}'
     &   //' = ', damp,' s^{-1}}" at graph 0.8, 0.16'

         write(40,'(a,f7.2,a)') '#set label "{/=12 {/Symbol l}'
     &   //'_{0} = ', wl_0,' \305}" at graph 0.8, 0.24'

         write(40,'(a,e10.4,a)') '#set label "{/=12 f'
     &   //'_{osc} = ', fosc,'}" at graph 0.8, 0.20'

         write(40,'(a,e10.4,a)') '#set label "{/=12 {/Symbol G}'
     &   //' = ', damp,' s^{-1}}" at graph 0.8, 0.16'

         write(40,*)
         write(40,*) '# tics format (default)'
         write(40,*) 'set xtics nomirror format "{/=14% g}"'
         write(40,*) 'set x2tics nomirror format "{/=14% g}"'
         write(40,*) 'set ytics format "{/=14% g}"'
         write(40,*)

!       Set minor tics on both axes
         write(40,*) '# Set minor tics on all axes'
         write(40,*) 'set mxtics'
         write(40,*) 'set mx2tics'
         write(40,*) 'set mytics'
         write(40,*)

!       Set axes labels; size is modified via {/=<size> <text>}
         write(40,*) '# Set axes label'
         write(40,*) 'set xlabel "{/=14 {/Symbol l - l_{0}} [\305]"'
         write(40,*) 'set x2label "{/=14 Velocity [km s^{ -1}]}"'
         write(40,*) 'set ylabel "{/=14 Transmission}"'
         write(40,*)

         write(40,*) '# Set xy-range'
         write(40,*) 'set xrange [',wlmin,':',wlmax,']'
         write(40,*) 'set x2range [',velmin,':',velmax,']'
         write(40,*) 'set yrange [-0.1:1.4]'
         write(40,*)

!       Define constant function at transmission = 0
         write(40,*) '# Define constant function at transmission = 0/1'
         write(40,*) 't_0(x) = 0.0'
         write(40,*) 't_1(x) = 1.0'

!       Plot in x-y scale
         write(40,*) '# Plot data file'
         write(40,*) 'plot '''//trim(linefile)//''' u ($2):($4) axes x1y1 '//
     &   'w l lt 1 lw 2 lc rgb "web-green" title "flux" \'
         write(40,*) ', '''//trim(linefile)//''' u ($2):($5) axes x1y1 '//
     &   'w l lt 1 lw 2 lc rgb "red" title "flux + conv" \'
         write(40,*) ', '''//trim(linefile)//''' u ($2):($3) axes x1y1 '//
     &   'w histeps lt 1 lw 1 lc rgb "black" title "flux + conv + noise" \'
         write(40,*) ', t_0(x) w l lt 0 lw 2 lc rgb "black" notitle \'
         write(40,*) ', t_1(x) w l lt 0 lw 2 lc rgb "black" notitle \'
         write(40,*) '#, '''//trim(linefile)//''' u ($1):($3) axes x2y1 '//
     &   'w l lw 2 notitle \'
         write(40,*)

!       Close file (do not forget!)
        close(40)

        end subroutine create_plotfile
!       ------------------------------------------------------------------------
