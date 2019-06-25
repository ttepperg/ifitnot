!       ----------------------------------------------------------------
!       Voigt-Profile approximation to model Absorption lines
!       ----------------------------------------------------------------
!       (C) Thorsten Tepper Garcia 2009
!       Based on the approximation to the Voigt-Hjerting function
!       published in:
!       Tepper Garcia, Thorsten
!       Monthly Notices of the Royal Astronomical Society; Volume 369, 
!       Issue 4, Page 2025
!       Please refer to the author when using this approximation.

        program profiles_hdf5

!        Input parameters: <ion name> <spectrum file prefix> <profile flag>

!        See ~/ifitnot/ions.dat for a list of available ions
!        profile flags:
!        2 -> Gauss
!        3 -> Voigt-Hjerting

!        WORKS BEST WITH ifit

!        Reads in a line list '<prefix>.fit' in the form:

!        Nion [cm^{-2}] velocity[km/s] b_value[km/s] ->

!        dNion[cm^{-2}] dvelocity[km/s] db_value[km/s] wavelength[A] ->

!        equiv_width[A] shifted_velocity[km/s] shifted_wavelength[A]
!
!        NOTE: velocities and wavelengths refer to line centroids;
!        quantities with a 'd' prefix denote error (sigma) in the
!        corresponding quantity; 'shifted' velocities and wavelengths
!        refer to the line centroids with respect to the shifted spectrum
!        A spectrum is circularly shifted by ifit, if desired, to avoid
!        absorption features at the boundaries. This is allowed since the
!        synthetic spectra are obtained from simulations (OWLS) with
!        periodic boundary conditions.
!
!        A second file '<prefix>.dat' is read, which contains the original,
!        fitted spectrum, in the form:
!        wavelength [A] | flux (normalised) | sigma (noise)
!
!        The program uses the line parameters to compute a synthetic
!        spectrum using full Voigt-profiles (see below), and outputs two
!        files, '<prefix>_linepar.dat' and '<prefix>_SynSpec.dat'; the
!        latter is identical to '<prefix>.dat'. The former contains a
!        line-parameter list in the form
!
!        z_abs wl_line[A] LOG10(Nion) b_value[km/s] equiv_width[mA] ->

!        vel_line[km/s] dNion[cm^{-}] db_value[km/s] dvel_line[km/s]

!        in ascending order in z_abs, the absorption redshift.
!
!       Line profiles are calculated according to the approximation for
!        a<<1 (where a is the damping parameter in the Voigt-Hjerting
!        function H(a,x)) given by (Tepper Garcia 2006)
!       
!       H(a,x) = h0-a/sqrt(PI)/x^2*(h0^2*(4x^4 + 7x^2 + 4 + Q) - 1 - Q)
!       with h0=exp(-x^2), Q=1.5 x^(-2)
*
!       The arguments of the program are the Doppler parameter b (in
!       units km/s) and the logarithmic column density (in units of
!       log10(cm^{-2})). The output of the program is the Voigt-profile
!       as a function of wavelength for the given transitions.
!       All physical and other needed parameters are
!       included in the file 'parameter.h'. The resolution is 0.01 A. The
!       datablock corresponding to each line is separated by a '#'. The
!       output is standard output and can be redirected to any desired
!       file. The resolution, etc. can be modified if needed.
!       Furthermore, the author is not responsible for any modifications
!       done to this code in its original form.
!
!       The line parameters (central wavelength of the transition, osc.
!       strength, and gamma-value) are contained in the file
!       'lyman_series.dat', and were taken from Morton's Compilation
!       (see Tepper-Garcia 2006 for corresponding reference).
!
!       Please report any bugs or problems to:
!       tepper@astro.physik.uni-potsdam.de
!
!       Modified to read in ifit output
!
!       2009: Now modified to cope with HDF5 and related codes written
!       by me: read_spec_hdf5, write_fit_hdf5, and the ifit "interface"
!       auto_min_hdf5
!
!        11/2009 IMPORTANT: minimum flux set to 1.0d-99, since there was
!        some strange behaviour at I/O for small numbers of the order
!        1.0d-308
!
!        11/2010: modified to cope with spectra circularly shifted (by
!        ifit)
!
!        11/2010: modified to input/output files with UNIQUE names in
!        order to allow for simultanoeus calls
!
!       USES hdf5_wrapper module in:
!
!       /home/volans/tepper/hdf5/HDF5_Wrapper/lib/ 
!
!       Modules

!       use hdf5_wrapper !not yet in use
        use set_precision
        use physical
        use common_strings
        use common_nums

!       ----------------------------------------------------------------
!       Variables declaration
!       ----------------------------------------------------------------

        implicit none
        
        intrinsic :: DEXP, DSQRT

        integer :: argcount
        integer :: i, j, dummy_int
        integer :: size_of_file
        integer, dimension(1) :: dim
        
        double precision :: voigt_hjerting, gauss, x ! functions

        character(len=256) :: homedir, command, prefix
        character(len=10) :: dummy_str
        character(len=5) :: profile_str

        logical :: file_exists

!       ----------------------------------------------------------------
!       Getting arguments
!       ----------------------------------------------------------------
!       Define program error and warning messages
!        errmsg='ERROR: profiles_hdf5 ... '
!        warnmsg='WARNING: profiles_hdf5 ... '
!        statmsg='WARNING: profiles_hdf5 ... '

!        The lines below are meant to print the different
!        messages in different colors

!        In red=31
        errmsg=CHAR(27)//'[1;31mERROR (profiles_hdf5): '//
     &        CHAR(27)//'[0m'
!        In yellow=33
        warnmsg=CHAR(27)//'[1;33mWARNING (profiles_hdf5): '//
     &        CHAR(27)//'[0m'
!        In green=32
        statmsg=CHAR(27)//'[1;32mSTATUS (profiles_hdf5): '//
     &        CHAR(27)//'[0m'

        argcount = IArgC()
              if (argcount.ne.2) then
         write(6,*) trim(warnmsg)//'USAGE: profiles_hdf5 <ion_name> '
     &   //'<file prefix>'
         stop 1
              end if
        call GetArg(1,ion_name)
        call GetArg(2,prefix)

!       ----------------------------------------------------------------
!       Computation
!       ----------------------------------------------------------------
!       Define home directory
        
        command = 'echo $HOME > homedir_'//trim(ion_name)
        call system(trim(command))
        open(10,file='homedir_'//trim(ion_name))
        read(10,'(a)') homedir
        close(10)

!       ----------------------------------------------------------------
!       Reading line-fitting parameters from file '<prefix>.fit'
!       generated by ifit
!       ----------------------------------------------------------------

        infile=trim(prefix)//'.fit'

!       File exists?
         inquire(file=infile,exist=file_exists)

!       If file doesn't exist, stop cleanly
         if (.not.file_exists) then
          write(6,'(a)') trim(errmsg)//'File '//trim(infile)//' does not exist'
          stop 1
         end if

!       Read header containing redshift, average signal-to-noise ratio, instru-
!       mental broadening, average optical depth, number of identified lines,
!       chi^2 value, and spectra shift from file '<prefix>.fit'

        open (10,file=trim(infile), status='old')
         
         read(10,*) z_min, average_sn, fwhm_kms, profile_str
         read(10,*) dummy_str, nlines
         read(10,*) check_fit_str, average_chi_sq
         read(10,*) spectrum_shifted_str, spectrum_shift
         read(10,*) equiv_width_lim_mA, Nion_lim

        close(10)
        
        if (trim(spectrum_shifted_str).eq.'TRUE') write(6,'(a,i6,a)') 
     &        trim(warnmsg)//'Spectrum cycled by', spectrum_shift,' pixels'
        if (nlines.gt.0) write(6,'(a,f6.3,a,i5,a,f6.3)') trim(statmsg)//
     &        'z_min =', z_min, ' Lines:', nlines, ' average chi^2 = ',
     &        average_chi_sq
        
        if (trim(check_fit_str).eq.'TRUE') write(6,'(a)') trim(warnmsg)//
     &        'Possible bad fit; visual inspection required!'

        
        if ((trim(profile_str).ne.'gauss').and.
     &  (trim(profile_str).ne.'voigt')) then
         write(6,'(a)') trim(errmsg)//'Non-valid profile: '//
     &   trim(profile_str)
         stop 1
        end if

!       ----------------------------------------------------------------
!       Check if lines were detected; if not, write out empty files to
!       prevent write_fit_hdf5 from crashing

        if (nlines.le.0) then
         write(6,'(a)') trim(warnmsg)//
     &    'No lines to fit! Writing out empty files.'
         
         open(10,file=trim(prefix)//'_linepar.dat')
         close(10)

         open(15,file=trim(prefix)//'_SynSpec.dat')
         close(15)
         stop

        end if

!       Allocate memory for line parameter arrays

        allocate(Nion(nlines))

        allocate(dNion(nlines))

        allocate(vel_line(nlines))

        allocate(dvel_line(nlines))

        allocate(b_value(nlines))

        allocate(db_value(nlines))

        allocate(wl_line(nlines))

        allocate(equiv_width_mA(nlines))

!       Allocate memory for line redshift and corresponding indices

        allocate(z_abs(nlines))

        allocate(z_indx(nlines))

!       Allocate memory for velocity shift (in case spectrum has been cycled)
        allocate(vel_line_shift(nlines))

!       Allocate memory for velocity shift (in case spectrum has been cycled)
        allocate(wl_line_shift(nlines))

!       ----------------------------------------------------------------
!       Read line parameters from file '<prefix>.fit';
!       skip first 5 lines which were previously read

        open (10,file=trim(infile), status='old')
         
         read(10,*)
         read(10,*)
         read(10,*)
         read(10,*)
         read(10,*)
         
         do i=1,nlines
          read(10,*,end=20) dummy_int, Nion(i), vel_line(i), 
     &          b_value(i), dNion(i), dvel_line(i), db_value(i),
     &          wl_line(i), equiv_width_mA(i), vel_line_shift(i),
     &          wl_line_shift(i)
          end do
  20         continue
        close(10)

!       ----------------------------------------------------------------
!       BEWARE: the atomic data thus loaded might not be consistent with
!       abs_line_profile and ifitnot!

!       Reading Morton+2003 atomic data tables
        call load_ion(trim(ion_name))

!       ----------------------------------------------------------------
!       Determine size of spectrum in file ion_name.dat

        infile=trim(prefix)//'.dat'
        
!       File exists?
         inquire(file=infile,exist=file_exists)

!       If file doesn't exist, stop cleanly
         if (.not.file_exists) then
          write(6,*) trim(errmsg)//'File '//trim(infile)//
     &' does not exist'
          stop 1
         end if

!        Define array dimension
        dim(1) = size_of_file(infile)

!       Allocate memory for actual spectra ID list

        allocate(waveln(1:dim(1)))

        allocate(wl_aux(1:dim(1)))

        allocate(flux(1:dim(1)))

        allocate(fluxorig(1:dim(1)))

        allocate(sigma(1:dim(1)))

        allocate(vel(1:dim(1)))

        allocate(vel_aux(1:dim(1)))

!       ----------------------------------------------------------------
!       Calculating optical depth as a function of wavelength
!       ----------------------------------------------------------------

!       Reading wavelength, flux, and noise from file '<prefix>.dat'

        open(30,file=trim(infile),status='old')
        
         do i=1,dim(1)
          read(30,*) waveln(i), fluxorig(i), sigma(i)
         end do
        
        close(30)

!       ------------------------------------------------------------------------
!       Compute velocity (in observer's frame, i.e. redshifted!)

!       The line below assumes l=l_0*exp(v/c) or (1+z)=exp(v/c)
        vel(1:dim(1)) = CLIGHT * dlog(waveln(1:dim(1))/waveln(1)) * 1.0d-5 !km/s

!       ------------------------------------------------------------------------
!       determine average pixel size

!       need shifted versions of vel and wl_aux
!       NOTE: eoshift shifts an array 'shift' positions to the left; if shift is
!       negative, the shift is performed to the right

        wl_aux= eoshift(waveln, shift = 1)
        vel_aux = eoshift(vel, shift = 1)
        
        pixel_size_ang =
     &  sum(wl_aux(1:dim(1)-1) - waveln(1:dim(1)-1))/dble(dim(1) - 1)
        
        pixel_size_kms =
     &  sum(vel_aux(1:dim(1)-1) - vel(1:dim(1)-1))/dble(dim(1) - 1)

!       ----------------------------------------------------------------
!       Compute synthetic spectrum using line parameters

!       Initializing flux
        flux(:)=1.0d0

!        Spectra with periodic boundary conditions, not shifted
        if (spectrum_shifted_str.eq.'FALSE') then
        
         sqrtPI = DSQRT(PI)
         z_abs(:) = wl_line(:)/wl_central - 1.0d0

         do i=1,nlines        !loop over lines
         
!       The following parameters are in cgs units      

          dopp_width = (wl_central*b_value(i)/CLIGHT)*1.0D-3
          damping = (wl_central*gamma/(4.0D0*PI*b_value(i)))*1.0D-13
          tau_central = (sqrtPI*ERADIUS*Nion(i)*(wl_central*wl_central)*
     &    fvalue/dopp_width)*1.0D-16

          zplus1_inv=wl_central/wl_line(i)
         
          do j=1,dim(1)  !loop over wl range
     
!       Auxiliar wavelength variable for calling function
           wavelength = waveln(j)

           x = ((wl_central-wavelength*zplus1_inv)/dopp_width)*1.0d-8
 
!       profile function
           if (profile_str.eq.'gauss') then ! Gaussian profile

            phi = gauss(x)

           else if (profile_str.eq.'voigt') then ! Voigt-Hjerting profile

            phi = voigt_hjerting(damping,x)
         
           end if

!       check for numerical precision to avoid code crash;
!       log(tiny(real_double)) ~ 700, which corresponds to a HI column
!       density of log NHI >= 16 for b >= 10

!       avoid underflow of exp(-tau)
           if ((dlog(tau_central)+dlog(phi)).gt.
     &     dlog(-1.d0*dlog(dble(tiny(real_single))))) then
           
            tau = -1.d0*dlog(dble(tiny(real_single)))

!       avoid underflow of tau
           else
     &     if ((dlog(tau_central)+dlog(phi)).lt.
     &     dlog(dble(tiny(real_single)))) then
           
            tau = dble(tiny(real_single))

           else
           
            tau = tau_central * phi
        
           end if

!          avoid underflow of flux
           if ((dlog(flux(j)) - tau).gt.
     &     dlog(dble(tiny(real_single)))) then
            flux(j) = flux(j)*dexp(-1.0d0*tau)
           else
            flux(j) = dble(tiny(real_single))
           end if


          end do
        
         end do

!        Spectra with periodic boundary conditions, circularly shifted to avoid
!        features at boundaries
        else if (spectrum_shifted_str.eq.'TRUE') then

!        Use shifted line-centroid
         sqrtPI = DSQRT(PI)
         z_abs(:) = wl_line_shift(:)/wl_central - 1.0d0

         do i=1,nlines        !loop over lines
         
!       The following parameters are in cgs units      

          dopp_width = (wl_central*b_value(i)/CLIGHT)*1.0d-3
          damping = (wl_central*gamma/(4.0d0*PI*b_value(i)))*1.0d-13
          tau_central = (sqrtPI*ERADIUS*Nion(i)*(wl_central*wl_central)*
     &    fvalue/dopp_width)*1.0d-16

!        Use shifted line-centroid
          zplus1_inv=wl_central/wl_line_shift(i)
         
          do j=1,dim(1)  !loop over wl range
     
!       Auxiliar wavelength variable for calling function
           wavelength = waveln(j)

           x = ((wl_central-wavelength*zplus1_inv)/dopp_width)*1.0d-8
 
!       profile function
           if (profile_str.eq.'gauss') then ! Gaussian profile

            phi = gauss(x)

           else if (profile_str.eq.'voigt') then ! Voigt-Hjerting profile

            phi = voigt_hjerting(damping,x)
         
           end if

!       check for numerical precision to avoid code crash;
!       log(tiny(real_double)) ~ 700, which corresponds to a HI column
!       density of log NHI >= 16 for b >= 10

!       avoid underflow of exp(-tau)
           if ((dlog(tau_central)+dlog(phi)).gt.
     &     dlog(-1.d0*dlog(dble(tiny(real_single))))) then
           
            tau = -1.d0*dlog(dble(tiny(real_single)))

!       avoid underflow of tau
           else
     &     if ((dlog(tau_central)+dlog(phi)).lt.
     &     dlog(dble(tiny(real_single)))) then
           
            tau = dble(tiny(real_single))

           else
           
            tau = tau_central * phi
        
           end if

!          avoid underflow of flux
           if ((dlog(flux(j))- tau).gt.
     &     dlog(dble(tiny(real_single)))) then
            flux(j) = flux(j)*dexp(-1.0d0*tau)
           else
            flux(j) = dble(tiny(real_single))
           end if

          end do
        
         end do

!        Cycle spectrum back to original position
         flux = cshift(flux,-spectrum_shift)

        end if !periodic spectra?

!       ----------------------------------------------------------------
!       convolve model spectrum with instrumental LSF (if required)
          
        if (fwhm_kms.gt.0.0d0)
     &   call  convolve_gaussian(flux(:),dim(1),pixel_size_kms,fwhm_kms)
         
!       ------------------------------------------------------------------------

!       ----------------------------------------------------------------
!       Create ascii file with wavelength, flux (fit), noise

        open(40,file=trim(prefix)//'_SynSpec.dat')
        
          do j=1,dim(1)  !loop over wl range
           write(40,*) waveln(j), flux(j), sigma(j), fluxorig(j), vel(j)
          end do

        close(40)

!       ----------------------------------------------------------------
!       Create file with line parameters, ordered by z_abs

!        FIRST, compute the correct, i.e. not shifted redshift
        z_abs(:) = wl_line(:)/wl_central - 1.0d0

!       Create index array for zabs in ascending order
        call indexx(nlines,z_abs,z_indx)
        
        open(50,file=trim(prefix)//'_linepar.dat')

!       write file content info
        write(50,'(a)') '# file contains (in that order):'
        write(50,'(a)') '# lines  z_min  <S/N>  profile function'
        write(50,'(a)') '# possible-bad-fit flag   <chi2-value> '
        write(50,'(a)') '# spectrum-shift flag   shift [pixel]'
        write(50,'(a)') '# min. equivalent width [mA] min. col.dens (log10)'
        write(50,'(a)') '# z_abs | wl_abs | log10(N) | b-value | EW | v_abs | '
     &  //' err(N) | err(b) | err(v) | shifted wl_abs | shifted v_abs'

!       Equivalent width in mA
!       Column densities given in log10, associated errors in linear scale!

!        Number of identified lines; minimum redshift; average signal-to-noise
        write(50,*)  nlines, z_min, average_sn, profile_str

!        NEW: flag for good/bad fit; if true, then the reduced chi^2 is large,
!        indicating a bad fit
        write(50,*)  trim(check_fit_str), average_chi_sq

!        NEW: flag = true if spectrum has been circularly shifted (to avoid
!        features at boundaries), and shift (in pixel)
!        Note that shift is done ALWAYS to the left, i.e. backwards

        write(50,*)  trim(spectrum_shifted_str), spectrum_shift

!        Formal completeness limit
        write(50,*)  equiv_width_lim_mA, dlog10(Nion_lim)
         
        do i=1,nlines        !loop over lines
         write(50,*) z_abs(z_indx(i)), wl_line(z_indx(i)),
     &   DLOG10(Nion(z_indx(i))), b_value(z_indx(i)),
     &   1.0d3*equiv_width_mA(z_indx(i)),
     &   vel_line(z_indx(i)), dNion(z_indx(i)),
     &   db_value(z_indx(i)),dvel_line(z_indx(i)),
     &   wl_line_shift(z_indx(i)), vel_line_shift(z_indx(i)) 
        end do
        
        close(50)

        write(6,'(a)') trim(statmsg)//'Done.'

!       ----------------------------------------------------------------
        end program
!       ----------------------------------------------------------------
