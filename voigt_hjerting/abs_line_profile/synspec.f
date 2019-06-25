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

!       ========================================================================
        module set_precision

!       ------------------------------------------------------------------------
!       shamelessly stolen from SpecWizard (by Schaye, Booth, and Theuns)

!       set precision of single and real(kind=doubleR) real and integers
        integer, parameter :: singleR = selected_real_kind(p=6,r=37)
        integer, parameter :: doubleR = selected_real_kind(p=15,r=307)
        integer, parameter :: singleI = selected_int_kind(9)
        integer, parameter :: doubleI = selected_int_kind(18)

!       signal invalid values
        real(kind=doubleR), parameter :: invalid_R = -1.0d99
        integer(kind=singleI), parameter :: invalid_I = -1234567890
        character(len=20), parameter  :: invalid   = 'INVALID'

!       generic
        real(kind=singleR) :: real_single
        real(kind=doubleR) :: real_double

        end module set_precision
!       ========================================================================

!       ========================================================================
        module abs_lines
        use set_precision

!       ------------------------------------------------------------------------
!       atomic variables
!       derived (user-defined) type

        type single_t
         sequence             ! store data elements next to each other in memory
         real(kind=doubleR) :: lambda_0, f_osc, big_gamma ! fundamental quant.
         real(kind=doubleR) :: tau_0
        end type single_t

!       ------------------------------------------------------------------------
!       absorption component parameters;
!       derived (user-defined) type

        type absorption_line
         sequence
         real(kind=doubleR) :: vel_c, col_dens, b_value
         real(kind=doubleR) :: z_abs
         real(kind=doubleR) :: eq_width
        end type absorption_line
       
        type ion
         sequence
         character(len=10) :: name
         real(kind=doubleR) :: mass
         integer(kind=singleI) :: num_transitions
         type(single_t), allocatable :: transition(:)
         type(absorption_line), allocatable :: component(:)
        end type ion

        integer(kind=singleI) :: num_ions
        integer(kind=singleI) :: ION_INDEX

        type(ion), allocatable :: IONS(:)       ! main working structure

        integer(kind=singleI), allocatable :: strongest(:,:)

        real(kind=doubleR) :: fwhm_kms
        
        real(kind=doubleR) :: pixel_size_ang               !in Angstroem
        real(kind=doubleR) :: pixel_size_kms               !in km/s
        integer(kind=singleI) :: spectrum_size_px, spectrum_size_vel

        end module abs_lines
!       ========================================================================

!       ========================================================================
        module physical
        use set_precision

!        Physical parameters (cgs UNITS)

        real(kind=doubleR), parameter :: sigma0=6.3d-18
        real(kind=doubleR), parameter :: c_cms=2.9979d+10
        real(kind=doubleR), parameter :: c_kms=c_cms * 1.0d-5
        real(kind=doubleR), parameter :: radius_e=2.817d-13
        real(kind=doubleR), parameter :: pi=3.141593d0
        real(kind=doubleR), parameter :: charge_e=1.602d-19
        real(kind=doubleR), parameter :: mass_e=9.109d-34

        real(kind=doubleR), parameter :: atom_munit = 1.66053886e-24 ! g
        real(kind=doubleR), parameter :: boltzmann=1.3806504d-23 !J/K

        end module physical
!       ========================================================================


!       ------------------------------------------------------------------------
        program synspec
!       ------------------------------------------------------------------------

!       Absorption Lines are calculated according to the
!       approximation for a<<1 (where a is the damping parameter in the
!       Voigt-Hjerting function H(a,x)) given by (Tepper Garcia 2006)
!       
!       H(a,x) = h0-a/sqrt(pi)/x^2*(h0^2*(4x^4 + 7x^2 + 4 + Q) - 1 - Q)
!       with h0=exp(-x^2), Q=1.5/x^2
!
!       The arguments of the program are a transition in the form e.g.
!       OVI_1031 (see ions.dat for a list of the avaible transitison),
!       the Doppler parameter 'b' (in units km/s) and the logarithmic
!       column density 'log10_coldens' (in units of log10(cm^{-2})).
!
!       The output of the program is:
!       a) a file named eqw.dat listing
!       log10_coldens | b | equivalent width (mili-Angstroem)
!       b) a plot of the line profile as a function of wavelength and
!       velocity (with respect to the line center). This plot should be
!       displayed automatically) 
!
!       Graphical output is supressed if plot = 'F'
!       All physical and other needed parameters are included in the file
!       'parameter.f'. The wavelength (and velocity) range is automatically
!       computed with a pixel size of 0.001 A.
!
!       The line parameters (central wavelength of the transition, oscillator
!       strength, and gamma- (damping-) value) for each ion are stored in the
!       file 'atom_data.ion',
!
!       NOTE: The author is not responsible for any modifications done to this
!       code in its original form. 
!
!       Please report any bugs or problems to:
!       mail@thorsten.mx
!
!       IMPORTANT STUFF PENDING:
!
!       1) USE OF CONSTANT RESOLVING POWER R=lambda / delta_lambda INSTEAD OF
!          FIXED RESOLUTION
!
!       2) COMPARISON OF INTERPOLATION VS. BINNING:
!          DO THEY GIVE THE SAME RESULT?
!
!       3) GENERATE NON-NORMALISED SPECTRA AND SUBJECT THESE TO CONTINUUM
!          FITTING USING continuum_fit; IMPROVE THE LATER
!
!       ------------------------------------------------------------------------
!       Variables declaration
!       ------------------------------------------------------------------------
        use set_precision
        use abs_lines
        use physical

        implicit none

        integer(kind=singleI) :: pixel, pixel_aux
        integer(kind=singleI) :: spectrum_size_voc
        integer(kind=singleI) :: seed

        integer(kind=singleI) :: argcount

        integer(kind=singleI) :: i, j, k, m, n

        integer(kind=singleI) :: ionindex, trans_index

        real(kind=doubleR), allocatable :: wlength(:), v_over_c(:)
        real(kind=doubleR), allocatable :: wlength_binned(:)

        integer(kind=singleI), allocatable :: vector_aux(:)
 
        real(kind=doubleR) :: voverc
        integer(kind=singleI) :: voverc_indx, voverc_indx_min, voverc_indx_max

        real(kind=doubleR), allocatable :: tau(:,:,:), tau_tot(:)
        real(kind=doubleR), allocatable :: flux_voc(:), flux_conv(:)

        real(kind=doubleR), allocatable :: flux_binned(:), d2flux_dw2(:)

!       used to test difference between interpolating and binning:
        real(kind=doubleR), allocatable :: flux_binned_test(:)

        real(kind=doubleR) :: flux_noisy
        real(kind=doubleR), allocatable:: sigma(:), noise(:)
        real(kind=doubleR), allocatable :: gaussdev(:)

        real(kind=doubleR) :: profile_func

        real(kind=doubleR) :: vel_min, vel_max

        real(kind=doubleR) :: wl_min, wl_max, wl_range
        real(kind=doubleR) :: min_lambda_0_z = huge(real_single)
        real(kind=doubleR) :: max_lambda_0_z = 0.0d0

        real(kind=doubleR) :: tau_central
        real(kind=doubleR) :: coldens, bvalue, zabs
        real(kind=doubleR) :: bvalue_max = 0.0d0
        real(kind=doubleR) :: zabs_min = huge(real_single)
        real(kind=doubleR) :: zabs_max = 0.0d0

        real(kind=doubleR) :: lambda0, fosc, biggamma, tau0, damping
        real(kind=doubleR) :: m_ion
        real(kind=doubleR) :: temp

        real(kind=doubleR) :: voigt_hjerting, gauss
        real(kind=doubleR) :: gasdev
        real(kind=doubleR) :: minnoise=1.0d-3
        real(kind=doubleR) :: signaltonoise

        character(len=24) :: ion_name
        character(len=256) :: homedir

!       default input-parameter file
        character(len=256), parameter :: parfile_gen = 'synspec_param.gen'

!       default atomic data file prefix
        character(len=256), parameter ::
     &  atomdata_file_prefix='/ifitnot/atom_data.'

        character(len=256) :: infile, outfile_syn, outfile_ifit, outfile_tst,
     +  atomdata_file, plotfile, psfile, outfile_prefix
        character(len=256) :: command

        character(len=10) :: signaltonoise_str, fwhm_kms_str, profile_str,
     +  psplot_str, test_run_str

!       used when testing
        real(kind=doubleR) :: log_col_dens_test, b_value_test
        character(len=10) :: log_col_dens_test_str, b_value_test_str

        character(len=256) :: errmsg, warnmsg, statmsg
        
        logical :: test_run, psplot
        logical :: file_exists
        logical :: addnoise
        
!       ------------------------------------------------------------------------
!       Define program error and warning messages;
!       The lines below are meant to print the different
!       messages in different colors

!       In red=31
        errmsg=CHAR(27)//'[1;31mERROR: synspec ...'//
     &  CHAR(27)//'[0m'
!       In yellow=33
        warnmsg=CHAR(27)//'[1;33mWARNING: synspec ...'//
     &  CHAR(27)//'[0m'
!       In green=32
        statmsg=CHAR(27)//'[1;32mSTATUS: synspec ...'//
     &  CHAR(27)//'[0m'

!       ------------------------------------------------------------------------
!       input parameters (from command line; used for testing)
!       ------------------------------------------------------------------------
        argcount = IArgC()

        SELECT CASE(argcount)

!       ------------------------------------------------------------------------
        CASE(6)

         call GetArg(1,ion_name)

         call GetArg(2,log_col_dens_test_str)
         read (log_col_dens_test_str,'(F6.0)') log_col_dens_test

         call GetArg(3,b_value_test_str)
         read (b_value_test_str,'(F6.0)') b_value_test

         call GetArg(4,signaltonoise_str)
        
         if (trim(signaltonoise_str).eq.'0') then
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

         call GetArg(6,fwhm_kms_str)
         read (fwhm_kms_str,'(f5.2)') fwhm_kms

!       set derived parameters for test

         test_run = .true.       ! testing
         num_ions = 1            ! with only one ion
         psplot = .false.        ! and no graphical output

!       allocate ion structure
         allocate(IONS(num_ions))
         allocate(IONS(num_ions)%component(1))

!      store input parameter values

         IONS(num_ions)%name = trim(ion_name)
         IONS(num_ions)%component(1)%col_dens = 1.0d1**(log_col_dens_test)
         IONS(num_ions)%component(1)%b_value = b_value_test
         IONS(num_ions)%component(1)%z_abs = 0.0d0

!       read pixel size from input parameter file

!       check file
         inquire(file=trim(parfile_gen),exist=file_exists)

!       if file doesn't exist, stop cleanly
         if (.not.file_exists) then
          write(6,'(1x,a)') trim(errmsg)
          write(6,'(1x,a)') 'input-parameter file '//trim(parfile_gen)//
     &    ' does not exist'
          stop 1
         end if

         open(3,file=trim(parfile_gen))
          read(3,*) !comment line
          read(3,*) pixel_size_kms
          read(3,*) pixel_size_ang
         close(3)

!       ------------------------------------------------------------------------
        CASE(0)

!       input parameters (from file)

!       check file
         inquire(file=trim(parfile_gen),exist=file_exists)

!       if file doesn't exist, stop cleanly
         if (.not.file_exists) then
          write(6,'(1x,a)') trim(errmsg)
          write(6,'(1x,a)') 'input-parameter file '//trim(parfile_gen)//
     &    ' does not exist'
          stop 1
         end if

!       otherwise open and read parameter values:

         write(6,'(1x,a)') 'reading input-parameter file '//trim(parfile_gen)
         write(6,'(a)')

         open(3,file=trim(parfile_gen))

!       general parameters
          read(3,*) !comment line
          read(3,*) pixel_size_kms
          read(3,*) pixel_size_ang
          read(3,*) profile_str
          read(3,*) fwhm_kms
          read(3,*) signaltonoise
          read(3,*) test_run_str
          read(3,*) psplot_str

!       ion list
          read(3,*) !comment line
          read(3,*) !comment line
          read(3,*) !comment line
          read(3,*) !comment line
          read(3,*) num_ions
        
!       quick input-parameter check
         ionindex = 0
          do
           ionindex = ionindex + 1
           read(3,*,end=10)
          end do

  10      continue

         if (((ionindex-1).ne.num_ions)) then
          write(6,*) trim(errmsg)
          write(6,*)
     &   'Non-matching input number and list of ions in input parameter file!'
          stop 1
         end if

!       if ok, continue to read ion names:

!       rewind file by num_ions+1 lines
          do ionindex=1,num_ions+1
           backspace(3)
          end do

!       allocate ion structure
          allocate(IONS(num_ions))

          do ionindex=1,num_ions

           allocate(IONS(ionindex)%component(1))

           read(3,*) IONS(ionindex)%name,
     &     IONS(ionindex)%component(1)%col_dens,
     &     IONS(ionindex)%component(1)%b_value,
     &     IONS(ionindex)%component(1)%z_abs

!        transform logarithmic column density to linear scale

          IONS(ionindex)%component(1)%col_dens =
     &    1.0d1**(IONS(ionindex)%component(1)%col_dens)

          end do


!       continue input-parameter check (only line profile for now)

         if ((profile_str.ne.'gauss').and.(profile_str.ne.'voigt')) then
          write(6,*) trim(errmsg)
          write(6,*) 'Non-valid profile : '//trim(profile_str)
          stop 1
         end if

!       set derived parameter values

         if (signaltonoise.gt.0.0d0) then
          addnoise=.true.
          write (signaltonoise_str,'(f8.2)') signaltonoise
         else
          signaltonoise=1.0d3
          addnoise=.false.
          write (signaltonoise_str,'(i4)') 1000
         end if

         write (fwhm_kms_str,'(f6.2)') fwhm_kms

!       ------------------------------------------------------------------------
!       set derived parameters

        test_run = .false.
        if (trim(test_run_str).eq.'TRUE')  test_run = .true.

        if (test_run)  num_ions = 1

        psplot = .false.
        if ((trim(psplot_str).eq.'TRUE').and.(.not.test_run))  psplot = .true.

!       ------------------------------------------------------------------------
        CASE default

         write(6,*)
         write(6,*) 'to generate a synthetic spectrum using input parameter '//
     &   'file '//trim(parfile_gen)//':'
         write(6,*)
         write(6,*) './synspec'
         write(6,*)
         write(6,*) 'to generate a synthetic spectrum with parameters read '//
     &   'from command lines:'
         write(6,*)
         write(6,*) './synspec <ion> <log10 N> '//
     &   '<b (km/s)> <noise (S/N or 0)> <profile (voigt | gauss) '//
     &   ' <FWHM (or 0)>'
         write(6,*)
         stop 1

        END SELECT ! input arguments
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       Computation
!       ------------------------------------------------------------------------
!       Define home directory
        
        call system('echo $HOME > homedir')
        open(5,file='homedir')
        read(5,'(a)') homedir
        close(5)
        
!       ------------------------------------------------------------------------
        do i=1,num_ions

         ion_name = IONS(i)%name

!       set file name
         atomdata_file =
     &   trim(homedir)//trim(atomdata_file_prefix)//trim(ion_name)

!       check file
         inquire(file=trim(atomdata_file),exist=file_exists)

!       if file doesn't exist, stop cleanly
         if (.not.file_exists) then
          write(6,'(1x,a)') trim(errmsg)
          write(6,'(1x,a)') 'atomic data file '//trim(atomdata_file)//
     &    ' does not exist'
          stop 1
         end if

!       otherwise, open file and read data

         open(10,file=trim(atomdata_file))

          read(10,*) IONS(i)%name, IONS(i)%mass, IONS(i)%num_transitions

!       allocate and read data for each transition
          allocate(IONS(i)%transition(IONS(i)%num_transitions))

          do j=1,IONS(i)%num_transitions
           read(10,*)
     &     IONS(i)%transition(j)%lambda_0,
     &     IONS(i)%transition(j)%big_gamma,
     &     IONS(i)%transition(j)%f_osc
          end do

         close(10)

!       consistency check
         if (trim(ion_name).ne.trim(IONS(i)%name)) then
          write(6,*)
          write(6,*) 'Incorrect ion name '//trim(IONS(i)%name)//
     &    ' in file '//trim(atomdata_file_prefix)//trim(ion_name)
          write(6,*)
          stop 1
         end if


        end do ! over ions

!       ------------------------------------------------------------------------
!       arrange ionic data with increasing tau_0 ~(lambda_0 * osc. strength)

!       allocate array and auxiliary vector
        allocate(strongest(num_ions,maxval(IONS(:)%num_transitions)))
        allocate(vector_aux(maxval(IONS(:)%num_transitions)))

!       strongest(i,j) stores the indices of the transitions read
!       from in atomic data file of ion i such that the transitons are sorted 
!       in order of increasing oscillator strength when j runs from 1 to N,
!       where N is the number of transitions of that particular ion
 
        do i=1,num_ions

!        set central (constant) optica depth (in cm^2 km/s) for each transition

         IONS(i)%transition(:)%tau_0 =
     &   (dsqrt(pi) * radius_e * c_kms) *
     &   (IONS(i)%transition(:)%f_osc *
     &   IONS(i)%transition(:)%lambda_0) * 1.0d-8

!       arrange in *ASCENDING* order of tau_0
         call
     &   indexx(IONS(i)%num_transitions,IONS(i)%transition(:)%tau_0,vector_aux)

!       reverse vector to have it in *DESCENDING* order
         do j=1,IONS(i)%num_transitions
          strongest(i,j) = vector_aux(IONS(i)%num_transitions-j+1)
         end do

        end do ! i-loop over ions

!       free memory
        deallocate(vector_aux)

!       ------------------------------------------------------------------------
!       IMPORTANT:
!       override number of transitions when testing; use only strongest;
!       recall that only one ion is used, hence num_ions = 1:

        if (test_run) IONS(num_ions)%num_transitions = 1

!       ------------------------------------------------------------------------
        do i=1,num_ions

!       auxiliary index
         trans_index = IONS(i)%num_transitions

!       overal minimum / maximum observed absorption wavelengths
         min_lambda_0_z = min(min_lambda_0_z,
     &   (1.0d0 + IONS(i)%component(1)%z_abs) *
     &   minval(IONS(i)%transition(1:trans_index)%lambda_0)
     &   )

         max_lambda_0_z = max(max_lambda_0_z,
     &   (1.0d0 + IONS(i)%component(1)%z_abs) *
     &   maxval(IONS(i)%transition(1:trans_index)%lambda_0)
     &   )

!       set minimum / maximum absorption redshift
         zabs_min = min(zabs_min,IONS(i)%component(1)%z_abs)
         zabs_max = max(zabs_max,IONS(i)%component(1)%z_abs)

!       set maximum doppler parameter
         bvalue_max = max(bvalue_max,IONS(i)%component(1)%b_value)

        end do ! i-loop over ions

!       ------------------------------------------------------------------------
!       compute wavelength boundaries and range

        wl_min =
     &  min_lambda_0_z - max(5.0d0, 1.0d-1*(max_lambda_0_z - min_lambda_0_z))

        wl_max =
     &  max_lambda_0_z + max(5.0d0, 1.0d-1*(max_lambda_0_z - min_lambda_0_z))

        wl_range = wl_max - wl_min

!       compute size of spectrum

        spectrum_size_px = ceiling(wl_range/pixel_size_Ang)

        spectrum_size_vel =
     &  ceiling(dlog(wl_max/wl_min) *  (c_kms / pixel_size_kms))

!       make it an odd number; each component's centre at v=0
        spectrum_size_vel = 2*int(0.5d0 * dble(spectrum_size_vel) ) + 1

!       check that input pixel size in Angstroem is large enough

        if (pixel_size_Ang.lt.
     &  (wl_max * (dexp(pixel_size_kms / c_kms) - 1.0d0))) then
         write(6,*) 'pixel size [A] should be at least ',
     &   wl_max * (dexp(pixel_size_kms / c_kms) - 1.0d0)
         write(6,*) 'or pixel size [km/s] should be at most ',
     &   dlog( 1.0d0 + (pixel_size_Ang / wl_max)) * c_kms
         stop 1
        end if


!       minimum / maximum v/c indices

        voverc_indx_min = -spectrum_size_vel +
     &  floor( dlog(min_lambda_0_z/wl_min)  / (pixel_size_kms / c_kms) )

        voverc_indx_max = spectrum_size_vel +
     &  ceiling( dlog(max_lambda_0_z/wl_min) / (pixel_size_kms / c_kms) )

        spectrum_size_voc = voverc_indx_max - voverc_indx_min + 1


!       allocate and initialise vectors 

        allocate(tau(num_ions,maxval(IONS(:)%num_transitions),
     &  -spectrum_size_vel:spectrum_size_vel))

        allocate(v_over_c(-spectrum_size_vel:spectrum_size_vel))

        tau(:,:,:) = 0.0d0
        v_over_c(:) = 0.0d0

        allocate(tau_tot(voverc_indx_min:voverc_indx_max))
        allocate(flux_voc(1:spectrum_size_voc))
        allocate(flux_conv(1:spectrum_size_voc))
        allocate(wlength(1:spectrum_size_voc))

        wlength(:) = 0.0d0
        tau_tot(:) = 0.0d0
        flux_voc(:) = 0.0d0
        flux_conv(:) = 0.0d0

        allocate(flux_binned(1:spectrum_size_px))
        allocate(flux_binned_test(1:spectrum_size_px))
        allocate(wlength_binned(1:spectrum_size_px))
        allocate(sigma(1:spectrum_size_px))
        allocate(noise(1:spectrum_size_px))
        allocate(gaussdev(1:spectrum_size_px))

        flux_binned(:) = 0.0d0
        flux_binned_test(:) = 0.0d0
        wlength_binned(:) = 0.0d0
        sigma(:) = 0.0d0
        noise(:) = 0.0d0
        gaussdev(:) = 0.0d0

!       output info
        if (.not.test_run) then
         write(6,*)
     &   'SPECTRUM SIZE [PX]  PIXEL SIZE [A | km/s]   WAVELENGTH [A] MIN / MAX'
         write(6,'(6x,i6,6x,4f12.4)')
     &   spectrum_size_px, pixel_size_Ang, pixel_size_kms, wl_min, wl_max
         write(6,*)
        end if

!       ------------------------------------------------------------------------
!       calculate optical depth as a function of v/c
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       compute optical depth for each ion
!       ------------------------------------------------------------------------

!       loop over ions

        DO ionindex=1,num_ions

        if (.not.test_run)
     &   write(6,'(a)') 'computing optical depth for ion: '
     &   //trim(IONS(ionindex)%name)

!       component parameters

         coldens = IONS(ionindex)%component(1)%col_dens
         bvalue = IONS(ionindex)%component(1)%b_value
         zabs = IONS(ionindex)%component(1)%z_abs

!       loop over transitions, in descending order of strength (tau_0)
        
         DO trans_index=1,IONS(ionindex)%num_transitions
        
          profile_func = 0.0d0

!       transition parameters

          lambda0 =
     &    IONS(ionindex)%transition(strongest(ionindex,trans_index))%lambda_0
          fosc =
     &    IONS(ionindex)%transition(strongest(ionindex,trans_index))%f_osc
          biggamma =
     &    IONS(ionindex)%transition(strongest(ionindex,trans_index))%big_gamma
          tau0 =
     &    IONS(ionindex)%transition(strongest(ionindex,trans_index))%tau_0

!       damping parameter
          damping = (lambda0 * biggamma / (4.0d0*pi*bvalue))*1.0d-13

!       central optical depth
          tau_central = tau0 * (coldens/bvalue)

!       loop over dimensionless velocity scale [v/c] centred at current
!       transition

          do pixel=-spectrum_size_vel,spectrum_size_vel

!       positon in dimensionless vel. space v/c centred on current transition

           v_over_c(pixel) = pixel * (pixel_size_kms / c_kms)

!       optical depth; check for numerical precision

           if (profile_str.eq.'voigt')
     &      profile_func =
     &      voigt_hjerting(damping,bvalue,v_over_c(pixel))
         
           if (profile_str.eq.'gauss')
     &      profile_func = gauss(bvalue,v_over_c(pixel))
        
           if (profile_func.le.0.0d0) then ! compiler warns about this
            tau(ionindex,trans_index,pixel) = 0.0d0
           else if ((log10(tau_central)+dlog10(profile_func)).lt.
     &      log10(tiny(real_single))) then
            tau(ionindex,trans_index,pixel) = 0.0d0
           else
            tau(ionindex,trans_index,pixel) = tau_central * profile_func
           end if

          end do ! over pixel

         END DO ! over transitions

        END DO ! over ions

!       ------------------------------------------------------------------------
!       map component absorption onto spectrum in v/c space
!       ------------------------------------------------------------------------

!       compute total optical depth

        DO ionindex=1,num_ions

        if (.not.test_run)
     &   write(6,'(a)') 'computing spectrum for ion: '
     &   //trim(IONS(ionindex)%name)

!       loop over transitions in descending order of strength (tau_0)
        
         DO trans_index=1,IONS(ionindex)%num_transitions
        
!       transition parameters

         lambda0 = IONS(ionindex)%transition(strongest(ionindex,trans_index))%lambda_0
         zabs = IONS(ionindex)%component(1)%z_abs

!       loop over dimensionless velocity scale [v/c] centred at current
!       transition

          do pixel=-spectrum_size_vel,spectrum_size_vel

!       *true* position in v/c space of current transition

           v_over_c(pixel) = pixel * (pixel_size_kms / c_kms)
           voverc = v_over_c(pixel) + dlog(1.0d0 + zabs) + dlog(lambda0/wl_min)
           voverc_indx = ceiling(voverc / (pixel_size_kms / c_kms))

!       total optical depth
           tau_tot(voverc_indx) =
     &     tau_tot(voverc_indx) + tau(ionindex,trans_index,pixel)

          end do ! over pixel

         END DO ! over transitions

        END DO ! over ions

!       initialise flux
        
        flux_voc(:) = 1.0d0

!       transform optical depth into flux;
!       check for underflow

        do pixel=1,spectrum_size_voc
         pixel_aux = pixel + (voverc_indx_min - 1)
         if ((-1.0d0*tau_tot(pixel_aux)).lt.log(tiny(real_single))) then
          flux_voc(pixel) = 0.0d0
         else
          flux_voc(pixel) = dexp(-1.0d0 * tau_tot(pixel_aux))
         end if
        end do

!       ------------------------------------------------------------------------
!       convolve spectrum in v/c space
!       ------------------------------------------------------------------------

        flux_conv(:) = flux_voc(:)

        if (fwhm_kms.gt.0.0d0)
     &  call
     &  convolve_spectrum(flux_conv,spectrum_size_voc,pixel_size_kms,fwhm_kms)

!       ------------------------------------------------------------------------
!       rebin (i.e. interpolate) spectrum onto observed wavelength baseline
!       ------------------------------------------------------------------------
!       output info
        if (.not.test_run)
     &   write(6,'(a,1pe12.4)')
     &   'resampling (interpolating) spectrum onto pixels [A]: ', pixel_size_ang

!       initialise flux and wavelength baseline

        flux_binned(:) = 0.0d0
        wlength_binned(:) = 0.0d0
        wlength(:) = 0.0d0

!       compute current wavelength baseline
        
        do pixel=1,spectrum_size_voc
          pixel_aux = pixel + (voverc_indx_min - 1)
          wlength(pixel) = wl_min * dexp(pixel_aux * pixel_size_kms / c_kms)
        end do

!       ------------------------------------------------------------------------
!       interpolate *original* flux

        allocate(d2flux_dw2(1:spectrum_size_voc))
        d2flux_dw2(:) = 0.0d0

!       compute second-derivative of *convolved* flux at each wavelength
        call
     &  spline(wlength,flux_conv,spectrum_size_voc,1.0d31,1.0d31,d2flux_dw2)

        do pixel=1,spectrum_size_px       

!       compute new wavelength point
         wlength_binned(pixel) = wl_min + dble(pixel-1) * pixel_size_Ang

!       interpolate flux at new wavelength
         call
     &   splint(wlength,flux_conv,d2flux_dw2,spectrum_size_voc,
     &   wlength_binned(pixel),flux_binned(pixel))

        end do ! over pixels

        if (.not.test_run) write(6,'(a)') 'done.'

!       ------------------------------------------------------------------------
!       rebin (true rebinning) spectrum onto observed wavelength baseline
!       ------------------------------------------------------------------------
!       output info
        if (.not.test_run)
     &   write(6,'(a,1pe12.4)')
     &   'resampling (rebinning) spectrum onto pixels [A]: ', pixel_size_ang

!       initialise flux and wavelength baseline

        flux_binned_test(:) = 0.0d0

	call rebin(wlength,flux_conv,wlength_binned,flux_binned_test,
     &  spectrum_size_voc,spectrum_size_px,pixel_size_ang)

        if (.not.test_run) write(6,'(a)') 'done.'

!       ------------------------------------------------------------------------
!       Gaussian random numbers to add noise (mean = 0, dispersion = 1)
!       ------------------------------------------------------------------------
!       Initialisation seed (has to be a negative integer)
        
        gaussdev(:) = 0.0d0
        
        if (addnoise) then

!       seed to initialise Gaussian random number generator;
!       use a random seed
!         call system('echo $RANDOM > seed')
!         open(10,file='seed')
!         read(10,*) seed
!         close(10)
!         seed= -1*seed

         seed=0
         do pixel=1,spectrum_size_px
          gaussdev(pixel) = gasdev(seed)
         end do

        end if

!       minimum noise value should be constrained by the given S/N:
!       Not sure about this...
        minnoise = min(minnoise,1.0d-1/signaltonoise)

!       ------------------------------------------------------------------------
!       OUTPUT
!       ------------------------------------------------------------------------

!       Define common filename prefix

!       replace dots from strings name by 'p' to avoid confusion with dot
!       demarcating file extenstion

!       signal-to-noise ratio
        do i=1,len_trim(signaltonoise_str)
         if (signaltonoise_str(i:i).eq.'.')
     &    signaltonoise_str(i:i)='p'
        end do

!      instrumental FWHM
        do i=1,len_trim(fwhm_kms_str)
         if (fwhm_kms_str(i:i).eq.'.')
     &    fwhm_kms_str(i:i)='p'
        end do

         if (test_run) then

         outfile_prefix = 'output_data/synspec'//
     &   '_'//trim(adjustl(log_col_dens_test_str))//
     &   '_'//trim(adjustl(b_value_test_str))//
     &   '_'//trim(adjustl(signaltonoise_str))//
     &   '_'//trim(adjustl(profile_str))//
     &   '_'//trim(ion_name)

        else

         outfile_prefix = 'output_data/synspec_ions'//
     &   '_SN'//trim(adjustl(signaltonoise_str))//'_FWHM'//
     &   trim(adjustl(fwhm_kms_str))

        end if

!       OUTPUT 1: to be shown graphically

        outfile_syn = trim(outfile_prefix)//'.syn'

!       OUTPUT 2: used for ifitnot benchmarking

        outfile_ifit = trim(outfile_prefix)//'.ifit'

        open(60,file=trim(outfile_ifit))

        open(50,file=trim(outfile_syn))
         
         do pixel=1,spectrum_size_px

!       add noise; check numerical precision
          
          sigma(pixel) = sqrt( minnoise**2.0 +
     &   (flux_binned(pixel)/signaltonoise)**2.0 )
        
          noise(pixel) = sigma(pixel)*gaussdev(pixel)

          if (flux_binned(pixel).le.(tiny(real_single) - noise(pixel))) then
           flux_noisy = flux_binned(pixel) + abs(noise(pixel))
          else
           flux_noisy = flux_binned(pixel) + noise(pixel)
          end if

          write(50,*) pixel * pixel_size_kms, wlength_binned(pixel),
     &    flux_noisy, flux_binned(pixel), flux_binned_test(pixel)

          write(60,*)
     &    wlength_binned(pixel), flux_noisy, sigma(pixel), flux_binned(pixel)
        
         end do
        
        close(50)

        close(60)

!       OUTPUT 3: to test rebinning, convolution, etc

        outfile_tst = trim(outfile_prefix)//'.tst'

        open(40,file=trim(outfile_tst))

         do pixel=1,spectrum_size_voc
          
          pixel_aux = pixel + (voverc_indx_min - 1)
          write(40,*) wl_min * dexp(pixel_aux * pixel_size_kms / c_kms),
     &    flux_conv(pixel), flux_voc(pixel)
         end do

        close(40)

!       ----------------------------------------------------------------
!       Create gnuplot file 2
        
        if (psplot) then

!       Define psfile name
        psfile=trim(outfile_prefix)//'multi_syn.ps'

!       Define plotfile name
        plotfile=trim(outfile_prefix)//'multi_syn.plot'

!       Define title
        infile = trim(ion_name)

        call create_plotfile_multiplet(infile,outfile_ifit,outfile_tst,
     &  plotfile,psfile,signaltonoise)

!       Compile with gnuplot
        command='gnuplot '//trim(plotfile)
        call system(trim(command))

!       Open ps-file
        command='gv '//trim(psfile)//' &'
        call system(trim(command))
        
        end if ! plot

!       ------------------------------------------------------------------------
        end program
!       ------------------------------------------------------------------------

!       ========================================================================
!       Subroutines and Functions
!       ========================================================================

!       ------------------------------------------------------------------------
        real(kind=doubleR) function gauss(b,v_over_c)

!       Returns the value of the Gaussian function as a function of
!       v/c for a given Doppler parameter b (in km/s), and central
!       wavelength (in Angstroem) of the corresponding transition.

        use set_precision
        use physical, only: c_kms

        implicit none

        real(kind=doubleR), intent(in) :: b, v_over_c
        real(kind=doubleR) :: x
        
        x = (v_over_c / (b / c_kms))

!       check for numerical precision to avoid floating exception
        if (dabs(x).lt.(dsqrt(-dlog(dble(tiny(real_single)))))) then
         gauss = dexp(-1.0d0*x*x)
        else
         gauss = dble(tiny(real_single))
        end if

        return
!       ------------------------------------------------------------------------
        end function gauss
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        real(kind=doubleR) function voigt_hjerting(a,b,v_over_c)

!       Returns the value of the Voigt-Hjerting function H(a,x) as a
!       function of wavelength for a given a-parameter, Doppler
!       parameter (in km/s), and central wavelength (in Angstroem) of
!       the corresponding transition.

        use set_precision
        use physical, only: c_kms, pi

        implicit none

        real(kind=doubleR) :: a, b, v_over_c
        real(kind=doubleR) :: x, x2, h0, Q, P
        
        x = (v_over_c / (b / c_kms))

!       Check numerical precision
!       See ~/ifitnot/voigt_hjerting/code/precision (dir)

        if (dabs(x).gt.4.0d-4) then ! numerically stable

         x2 = x*x

!       avoid underflow of exp(-x*x)
         if (dabs(x).lt.(dsqrt(-dlog(dble(tiny(real_single)))))) then
          h0 = dexp(-1.0d0*x2)
         else
          h0 = tiny(real_single)
         end if
        
!       avoid over/underflow of Q
         if ((dlog(x2) + dlog(dble(tiny(real_single)))).gt.dlog(1.5d0)) then
          Q = tiny(real_single)
         else
          Q = 1.5d0/x2
         end if
        
!       avoid over/underflow of P
         if ((dlog(x2) + dlog(dble(tiny(real_single)))).gt.
     &   (dlog(a)-dlog(dsqrt(pi)))) then
          P = tiny(real_single)
         else
          P = a/dsqrt(pi)/x2
         end if
        
         voigt_hjerting = h0 - P *
     &   (h0*h0*(4.0d0*x2*x2 + 7.0d0*x2 + 4.0d0 + Q) - 1.0d0 - Q)

        else                         !limit for x -> 0

         voigt_hjerting = 1.0d0 - 2.0d0*a/dsqrt(pi)

        endif
        
        return
!       ------------------------------------------------------------------------
        end function voigt_hjerting
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       Numerical Recipes
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       GENERATES UNIFORMILY DISTRIBUTED RANDOM NUMBERS
              
        function ran1(idum)

        use set_precision

        implicit none

        integer(kind=singleI) :: idum
        real(kind=doubleR) :: ran1
        integer(kind=singleI), parameter :: IA=16807,IM=2147483647,IQ=127773,
     !  IR=2836, NTAB=32 ! edited by TTG
        real(kind=doubleR), parameter :: AM=1.0d0/dble(IM), ! edited by TTG
     !  NDIV=1.0d0+(dble(IM)-1.0d0)/dble(NTAB),EPS=1.2d-7,RNMX=1.0d0-EPS
        integer(kind=singleI) :: j,k,iv(NTAB),iy
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
         j=int(1+iy/NDIV) ! edited by TTG
         iy=iv(j)
         iv(j)=idum
         ran1=min(AM*iy,RNMX)

         if (ran1.lt.0.0d0) then
          print*, 'ran1: negative random number!'
          print*, AM, iy, RNMX
          stop 1
         end if

         return
         end function
C  (C) Copr. 1986-92 Numerical Recipes Software 5.W7.
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       GENERATES GAUSSIAN DISTRIBUTED RANDOM NUMBERS

        function gasdev(idum)

        use set_precision

        implicit none

        integer(kind=singleI) :: idum
        real(kind=doubleR) :: gasdev
CU      USES ran1
        integer(kind=singleI) :: iset
        real(kind=doubleR) :: fac,gset,rsq,v1,v2,ran1
        SAVE iset,gset
        DATA iset/0/
        if (iset.eq.0) then
1        v1=2.0D0*ran1(idum)-1.0D0
         v2=2.0D0*ran1(idum)-1.0D0
         rsq=v1**2+v2**2
         if(rsq.ge.1.0d0.or.rsq.eq.0.0d0) goto 1 ! compiler warns about this
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
!       spectrum_size_px = number of pixels
!       vpixsize = size of pixel in km/s
!       fwhm_kms = FWHM of Gaussian response in km/s

        use set_precision

        implicit none

        real(kind=doubleR) :: sigmakms, b, norm, fwhm, px_size
        integer(kind=singleI) :: i,j,off, nvpix, num_px
        real(kind=doubleR), allocatable, save :: gauss(:)
        real(kind=doubleR)  :: flx(*)
        real(kind=doubleR), allocatable  :: convl_flux(:),
     &  convl_flux_convolved(:)

!       determine number of pixels
        nvpix = num_px

!       Compute sigma in km/s
        sigmakms = fwhm / (2.0d0 * dsqrt( 2.0d0 * dlog(2.0d0)))

!       Compute sigma in units of pixels
        b = sigmakms / px_size
        
!       For convolution with instrumental PSF we need to Fourier 
!       transform, we thus need to increase the array so that it is a
!       power of 2.
        nvpix = int(2.0d0**(aint(dlog(dble(nvpix))/dlog(2.0d0)) + 1.0d0))
        
!       Create normalized Gaussian in wrap-around order (required by
!       convlv)

        allocate(gauss(nvpix))
        norm = 1.0d0 / (2.0d0 * b * b)
        do i = 0, nvpix-1
           if (i .le. nvpix-1) then 
              if (i .le. nvpix/2) then
                 j = i
              else
                 j = i - nvpix
              endif
              if (abs(j) .lt. ceiling(1.0d1*b)) then
                 gauss(i+1) = exp(-(dble(j)**2.0d0)*norm)
              else
                 gauss(i+1) = 0.0d0
              endif
           else
              gauss(i+1) = 0.0d0
           endif
        enddo

!       normalise Gaussian
        gauss  = gauss / sum(gauss)

        allocate(convl_flux(nvpix),convl_flux_convolved(2*nvpix))
        convl_flux(:) = 0.0d0
        convl_flux(1:num_px) = flx(1:num_px)

!       make periodic copies of the flux signal into the zero buffer
!       to avoid aliasing (or end) effects
!        do i=num_px+1,nvpix
!           off = i-num_px
!           if (off .lt. (nvpix-num_px)/2.0d0) then
!              convl_flux(i) = convl_flux(i-num_px)
!           else
!              convl_flux(i) = convl_flux(i-(nvpix-num_px))
!           endif
!        enddo

        convl_flux_convolved(:) = 0.0d0

        call convlv(convl_flux,nvpix,gauss,nvpix,1,convl_flux_convolved)

        flx(1:num_px) = convl_flux_convolved(1:num_px)

        deallocate(convl_flux,convl_flux_convolved)

!       ------------------------------------------------------------------------
!       output info

        write(6,'(a,f7.2)')
     &  'spectrum convolved with Gaussian LSF with FWHM [km/s]:', fwhm
        write(6,'(a,i8)')
     &  'over a wavelength range around line centre of width [pixel]',
     &  ceiling(1.0d1*b)
        write(6,*)
     &  'over a wavelength range around line centre of width [km/s]',
     &   1.0d1*b*px_size
        write(6,*)

!       ------------------------------------------------------------------------
        return

        end subroutine convolve_spectrum
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       Stolen from SpecWizard
!       USES twofft; realft

        subroutine convlv(data,n,respns,m,isign,ans)

        use set_precision

        implicit none

        integer, intent(in) :: n, m, isign
        real(kind=doubleR), intent(in)    :: data(n)
        real(kind=doubleR), intent(inout) :: respns(n)
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

        use set_precision

        implicit none

        integer, intent(in) :: n
        real(kind=doubleR), intent(in) :: data1(n), data2(n)
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

        use set_precision

        implicit none

        integer, intent(in) :: n, isign
        real(kind=doubleR), intent(inout) :: data(n)
        ! local variables
        integer i,i1,i2,i3,i4,n2p3
        real(kind=doubleR) c1,c2,h1i,h1r,h2i,h2r,wis,wrs
        real(kind=doubleR) theta,wi,wpi,wpr,wr,wtemp
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

        use set_precision

        implicit none

        integer, intent(in) :: nn, isign
        real(kind=doubleR), intent(inout) :: data(2*nn)
        ! local variables  
        integer i,istep,j,m,mmax,n
        real(kind=doubleR) :: tempi,tempr
        real(kind=doubleR) ::  theta,wi,wpi,wpr,wr,wtemp

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

!       ------------------------------------------------------------------------
        subroutine
     &  create_plotfile_multiplet(datfile,linefile,tstfile,gpfile,psline,sn)

!       To plot a 3D color map from a data file with three columns
!       (x:y:z)
!       where each 'iso_line' must be separated by a blank line
!
        use set_precision
        use abs_lines, only: IONS, num_ions, fwhm_kms,
     +  spectrum_size_vel, pixel_size_kms
        use physical

        implicit none

        real(kind=doubleR), intent(in) :: sn
        character(len=256), intent(inout) :: datfile
        character(len=256), intent(in) :: linefile, tstfile, gpfile, psline

        integer(kind=singleI) :: i, ionindex

        real(kind=doubleR) :: zabs
        real(kind=doubleR), allocatable :: wl_0(:), f_osc(:), damp(:)

        integer(kind=singleI) :: num_trans

        real(kind=doubleR) :: velmin, velmax

        real(kind=doubleR) :: margin

        character(len=256) :: fontpath, fontfile, timestamp

        integer(kind=singleI) :: datetime(8)
        integer(kind=singleI) :: clock
        character(len=10) :: date, time, zone
        character(len=10) :: datetime_str(8)

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
         write(40,*) '# Generated by synspec'
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
         write(40,*) '# symbols (CMSY10 font) in OMS encoding'
         write(40,*) '# (see table ~/ps_fontfile_doc.ps)'
         write(40,*)

!       Define fontfile for fancy symbols
         fontfile='cmsy10.pfb'

         write(40,*) 'set term post portrait enhanced color dashlength 1'
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

!       ------------------------------------------------------------------------
         write(40,*) '# Set xy-range'
         write(40,*) 'ckms=', c_kms
         velmin =  -1.0d0 * 200 * pixel_size_kms
         velmax =  +1.0d0 * 200 * pixel_size_kms
         write(40,*) 'set xrange [',velmin,':',velmax,']'
         write(40,*) 'set yrange [-0.1:1.4]'
         write(40,*)

!       Define constant function at transmission = 0
         write(40,*) '# Define constant function at transmission = 0/1'
         write(40,*) 't_0(x) = 0.0'
         write(40,*) 't_1(x) = 1.0'

!       ------------------------------------------------------------------------
        DO ionindex=1,num_ions
!       ------------------------------------------------------------------------

!       ion-specific quantities

!       absorption redshift
        zabs = IONS(ionindex)%component(1)%z_abs

!       ------------------------------------------------------------------------
!       for each ion a new multiplot environment

         write(40,'(a,e10.4,a)') '# multiplot environment'
         write(40,'(a,e10.4,a)') 'set multiplot'

!       ------------------------------------------------------------------------
!       panels

!       margin scale
        margin = 0

!       number of transitions per ion
        num_trans = IONS(ionindex)%num_transitions

        if (allocated(wl_0)) deallocate(wl_0)
        if (allocated(f_osc)) deallocate(f_osc)
        if (allocated(damp)) deallocate(damp)
        allocate(wl_0(1:num_trans))
        allocate(f_osc(1:num_trans))
        allocate(damp(1:num_trans))

        DO i=1,num_trans

!       transition-specific quantities

        wl_0(i) = IONS(ionindex)%transition(i)%lambda_0
        f_osc(i) = IONS(ionindex)%transition(i)%f_osc
        damp(i) = IONS(ionindex)%transition(i)%big_gamma

!       ------------------------------------------------------------------------
!       set margin scale
         margin =  (1.0d0 - 1.2d-1)/dble(num_trans)

!       ------------------------------------------------------------------------
         write(40,'(a,e10.4,a)') '# margins'
         write(40,'(a,e10.4,a)') 'set tmargin screen ', 0.95 - (i-1)*margin
         write(40,'(a,e10.4,a)') 'set bmargin screen ', 0.95 - (i)*margin


        if (i.gt.1)
     &   write(40,'(a,1pe8.2,a)') 'unset title'

!       ------------------------------------------------------------------------
         write(40,*) '# Set text labels'
         write(40,'(a,1pe8.2,a)') 'unset label'

        if (i.eq.1)
     &   write(40,'(a,1pe8.2,a)') 'set label "{/=12 S/N=',
     &   sn,'}" tc rgb "royalblue" at graph 0.05, 0.9'

        if (i.eq.1)
     &   write(40,'(a,1pe8.2,a)') 'set label "{/=12 S/N=',
     &   sn,'}" tc rgb "royalblue" at graph 0.05, 0.9'

         write(40,'(a,1pe10.4,a)') 'set label "{/=10 {/Symbol G}'
     &   //' = ', damp(i),' s^{-1}}" at graph 0.7, 0.16'

!       NOTE: the format statement 0p is necessary, otherwise the output after
!       the 1p directive is shifted by one decimal place!

         write(40,'(a,1pe10.4,0p,a,f5.3)') 'set label "{/=10 f'
     &   //'_{osc} = ', f_osc(i),'}" at graph 0.7, 0.16+', 0.02*num_trans

         write(40,'(a,f7.2,a,f5.3)') 'set label "{/=10 {/Symbol l}'
     &   //'_{0} = ', wl_0(i),' \305}" at graph 0.7, 0.16+', 0.04*num_trans

!       ------------------------------------------------------------------------
        if (i.eq.num_trans) then

!       axes number formatting
         write(40,*)
         write(40,*) '# tics format (default)'
         write(40,*) 'set xtics mirror format "{/=14% g}"'
         write(40,*) 'set ytics format "{/=14% g}"'
         write(40,*)

!       axes labels; size is modified via {/=<size> <text>}
         write(40,*) '# Set axes label'
         write(40,*) 'set xlabel "{/=14 vel [kms^{-1}]"'
         write(40,*) 'set ylabel "{/=14 Transmission}"'
         write(40,*)

        else

!       axes number formatting
         write(40,*)
         write(40,*) '# tics format (default)'
         write(40,*) 'set xtics mirror format " "'
         write(40,*) 'set ytics format "{/=14% g}"'
         write(40,*)

!       axes labels; size is modified via {/=<size> <text>}
         write(40,*) '# Set axes label'
         write(40,*) 'set xlabel ""'
         write(40,*) 'set ylabel "{/=14 Transmission}"'
         write(40,*)

        end if

!       Set minor tics on both axes
         write(40,*) '# Set minor tics on all axes'
         write(40,*) 'set mxtics'
         write(40,*) 'set mytics'
         write(40,*)


!       Plot in x-y scale
         write(40,*) '# Plot data file'
         write(40,*) 'plot '''//trim(linefile)//
     &   ''' u (ckms*log(($1)/(',wl_0(i),'*(1.+',zabs,')))):($2) '//
     &   'w histeps lt 1 lw 1 lc rgb "black" title "flux + conv + noise" \'
         write(40,*) ', '''//trim(linefile)//
     &   ''' u (ckms*log(($1)/(',wl_0(i),'*(1.+',zabs,')))):($4) '//
     &   'w histeps lt 1 lw 1 lc rgb "royalblue" title "flux + conv + nonoise" \'
         if (fwhm_kms.gt.0.0d0) then
         write(40,*) ', '''//trim(tstfile)//
     &   ''' u (ckms*log(($1)/(',wl_0(i),'*(1.+',zabs,')))):($3) '//
     &   'w histeps lt 1 lw 1 lc rgb "red" title "flux (no conv, no noise)" \'
         end if
         write(40,*) ', t_0(x) w l lt 0 lw 2 lc rgb "black" notitle \'
         write(40,*) ', t_1(x) w l lt 0 lw 2 lc rgb "black" notitle \'
         write(40,*)

        END DO ! over transitions

!       ------------------------------------------------------------------------
        write(40,'(a,e10.4,a)') '# exit multiplot environment'
        write(40,'(a,e10.4,a)') 'unset multiplot'

!       ------------------------------------------------------------------------
        END DO ! over ions
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       Close file (do not forget!)
        close(40)

        end subroutine create_plotfile_multiplet
!       ========================================================================

!       ----------------------------------------------------------------
!       Indexing and ranking of an array
!       ----------------------------------------------------------------

!       Indexes an array arr(1:n), i.e., outputs the array indx(1:n) such that
!       arr(indx(j)) is in ascending order for j = 1, 2, . . . , N .
!       The input quantities n and arr are left unchanged.

        SUBROUTINE indexx(n,arr,indx)

        use set_precision
        
        implicit none

        integer(kind=singleI), intent(in) :: n
        integer(kind=singleI), intent(inout) :: indx(n)
        real(kind=doubleR) :: arr(n)
        integer(kind=singleI), parameter :: M=7, NSTACK=50
        integer(kind=singleI) :: i,indxt,ir,itemp,j,jstack,k,l,istack(NSTACK)
        real(kind=doubleR) :: a

        do 11 j=1,n
          indx(j)=j
11      continue
        jstack=0
        l=1
        ir=n
1       if (ir-l.lt.M)then
          do 13 j=l+1,ir
            indxt=indx(j)
            a=arr(indxt)
            do 12 i=j-1,1,-1
              if (arr(indx(i)).le.a)goto 2
              indx(i+1)=indx(i)
12          continue
            i=0
2           indx(i+1)=indxt
13        continue
          if (jstack.eq.0)return
          ir=istack(jstack)
          l=istack(jstack-1)
          jstack=jstack-2
        else
          k=(l+ir)/2
          itemp=indx(k)
          indx(k)=indx(l+1)
          indx(l+1)=itemp
          if (arr(indx(l+1)).gt.arr(indx(ir)))then
            itemp=indx(l+1)
            indx(l+1)=indx(ir)
            indx(ir)=itemp
          endif
          if (arr(indx(l)).gt.arr(indx(ir)))then
            itemp=indx(l)
            indx(l)=indx(ir)
            indx(ir)=itemp
          endif
          if (arr(indx(l+1)).gt.arr(indx(l)))then
            itemp=indx(l+1)
            indx(l+1)=indx(l)
            indx(l)=itemp
          endif
          i=l+1
          j=ir
          indxt=indx(l)
          a=arr(indxt)
3         continue
            i=i+1
          if (arr(indx(i)).lt.a)goto 3
4         continue
            j=j-1
          if (arr(indx(j)).gt.a)goto 4
          if (j.lt.i)goto 5
          itemp=indx(i)
          indx(i)=indx(j)
          indx(j)=itemp
          goto 3
5         indx(l)=indx(j)
          indx(j)=indxt
          jstack=jstack+2
          if (jstack.gt.NSTACK) write(6,*) 'NSTACK too small in indexx'
          if (ir-i+1.ge.j-l)then
            istack(jstack)=ir
            istack(jstack-1)=i
            ir=j-1
          else
            istack(jstack)=j-1
            istack(jstack-1)=l
            l=i
          endif
        endif
        goto 1
        END SUBROUTINE indexx
!       ========================================================================

!       ========================================================================
        SUBROUTINE spline(x,y,n,yp1,ypn,y2)

!       Given arrays x(1:n) and y(1:n) containing a tabulated function, i.e.,
!       yi = f(xi), with x1 < x2 < .. . < xN, and given values yp1 and ypn for
!       the first derivative of the interpolating function at points 1 and n,
!       respectively, this routine returns an array y2(1:n) of length n which
!       contains the second derivatives of the interpolating function at the
!       tabulated points xi. If yp1 and/or ypn are equal to 1e30 or larger,
!       the routine is signaled to set the corresponding boundary condition for
!       a natural spline, with zero second derivative on that boundary.

!       IMPORTANT: the program spline is called only once to process an entire
!       tabulated function in arrays xi and yi

        use set_precision
        
        implicit none

        integer(kind=singleI), intent(in) :: n
        real(kind=doubleR) :: x(n),y(n),y2(n)
        real(kind=doubleR) :: yp1,ypn

        integer(kind=singleI) :: i,k
        real(kind=doubleR) :: p,qn,sig,un,u(n)

        if (yp1.gt..99d30) then
          y2(1)=0.
          u(1)=0.
        else
          y2(1)=-0.5
          u(1)=(3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
        endif
        do 11 i=2,n-1
          sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
          p=sig*y2(i-1)+2.
          y2(i)=(sig-1.)/p
          u(i)=(6.*((y(i+1)-y(i))/(x(i+
     *          1)-x(i))-(y(i)-y(i-1))/(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*
     *          u(i-1))/p
!       check for underflow
         if (u(i).lt.tiny(real_single)) u(i) = tiny(real_single)
  11    continue
        if (ypn.gt..99e30) then
          qn=0.
          un=0.
        else
          qn=0.5
          un=(3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
        endif
        y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
        do 12 k=n-1,1,-1
          y2(k)=y2(k)*y2(k+1)+u(k)
  12    continue
        return
        END
!       ========================================================================

!       ========================================================================
        SUBROUTINE splint(xa,ya,y2a,n,x,y)

!       Given the arrays xa(1:n) and ya(1:n) of length n, which tabulate a
!       function (with the xa_i in order), and given the array y2a(1:n), which
!       is the output from spline above, and given a value of x, this routine
!       returns a cubic-spline interpolated value y.
        
        use set_precision
        
        implicit none

        integer(kind=singleI), intent(in) :: n
        real(kind=doubleR) :: x,y,xa(n),y2a(n),ya(n)
        integer(kind=singleI) :: k,khi,klo
        real(kind=doubleR) :: a,b,h
        klo=1
        khi=n
  1     if (khi-klo.gt.1) then
          k=(khi+klo)/2
          if(xa(k).gt.x)then
            khi=k
          else
            klo=k
          endif
        goto 1
        endif
        h=xa(khi)-xa(klo)
        if (h.eq.0.) write(6,*) 'bad xa input in splint'
        a=(xa(khi)-x)/h
        b=(x-xa(klo))/h
        y=a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**
     *        2)/6.
        return
        END
!       ========================================================================

!       ========================================================================
        subroutine rebin(xdata_in,ydata_in,xdata_out,ydata_out,
     &  num_px_in,num_px_out,px_size_ang)

!       taken from SpecWizard (2012)

        use set_precision

        implicit none

        real(kind=doubleR), intent(in)  :: xdata_in(1:num_px_in),
     &  xdata_out(1:num_px_out), ydata_in(1:num_px_in)
        real(kind=doubleR), intent(in)  :: px_size_ang
        integer(kind=singleI), intent(in) :: num_px_in, num_px_out
        real(kind=doubleR), intent(out) :: ydata_out(1:num_px_out)
   
        ! local
        integer(kind=singleI) :: i,j,k,ninbin

        j = 1
        do i= 1,num_px_out

         ninbin = 0
         do while ((xdata_in(j).lt.xdata_out(i)-0.5*px_size_ang).and.
     &   (j.lt. num_px_in))
          j = j + 1
         enddo

         k = j
         do while ((xdata_in(k) .le. xdata_out(i)+0.5*px_size_ang).and.
     &   (k .le. num_px_in))
          ninbin = ninbin + 1
          ydata_out(i) = ydata_out(i) + ydata_in(k)
          k = k + 1
         enddo

         if (ninbin.eq.0) then

          if (i.lt.num_px_in) then 
             write(*,'("ERROR: Grid too coarse! xdata_out = ", f10.3)')
     &       xdata_out(i)
             stop 1
          endif

         else 
               ydata_out(i) = ydata_out(i) / dble(ninbin)

         endif

        enddo ! i-loop over binned pixels

        end subroutine rebin
!       ========================================================================
