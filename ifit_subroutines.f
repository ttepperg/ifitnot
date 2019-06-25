
!       ========================================================================
        subroutine start_program(routinemsg)

        use ifit_variables

        implicit none

        integer(kind=singleI) :: argcount

        character(len=*) :: routinemsg

!       ------------------------------------------------------------------------
!       Define program error and warning messages
!       The lines below are meant to print the different messages in different
!       colours

!       In red=31, yello=33, gree=32
        errmsg=trim(colour_errmsg_prefix)//'ERROR ('//trim(routinemsg)//'):'//
     &  trim(colour_msg_suffix)

        warnmsg=trim(colour_warnmsg_prefix)//'WARNING ('//trim(routinemsg)//
     &  '):'//trim(colour_msg_suffix)
        
        statmsg=trim(colour_statmsg_prefix)//'STATUS ('//trim(routinemsg)//
     &  '):'//trim(colour_msg_suffix)

!       ------------------------------------------------------------------------
!       program start

!       save starting time
        itime_start = time8()             !stores current time (in seconds)
        time_start = ctime(itime_start)   !stores current date+time in a string

!       ------------------------------------------------------------------------
!       print program name for information

        write(6,*)
        write(6,'(a)') trim(output_dashed_line)
        write(6,'(1x,a)') trim(colour_statmsg_prefix)//trim(routinemsg)//
     &  trim(colour_msg_suffix)//' COMPUTATION STARTED '//trim(time_start)
        write(6,'(a)') trim(output_dashed_line)
        write(6,*)

!       ------------------------------------------------------------------------
!       input arguments

        argcount = IArgC()
        if (argcount.ne.1) then
         write(6,*) trim(warnmsg)//'USAGE: ifitnot <specfile prefix>'
         stop 1
        end if

!       ------------------------------------------------------------------------
!       input data: spectrum file

        call getarg(1,specfile_in)

!       ------------------------------------------------------------------------
        return

        end subroutine start_program
!       ========================================================================

!       ========================================================================
        subroutine input_parameters()

        use set_precision
        use ifit_variables

        implicit none

        integer(kind=singleI) :: ionindex
        character(len=10) :: ion_str
        character(len=256) :: parfile_gen, parfile_ion
        character(len=64) :: routinemsg = 'input_parameters:'
        
        logical :: file_exists

!       ------------------------------------------------------------------------
!       print routine name for information
        write(6,'(1x,a)') trim(colour_statmsg_prefix)//trim(routinemsg)//
     &  trim(colour_msg_suffix)

!       ------------------------------------------------------------------------
!       read general input parameters
        parfile_gen='ifit_parameters.gen'
        
!       check file existence
        inquire(file=parfile_gen,exist=file_exists)

!       if file doesn't exist, stop cleanly
        if (.not.file_exists) then
         write(6,'(1x,a)') trim(errmsg)
         write(6,'(1x,a)') 'input-parameter file '//trim(parfile_gen)//
     &   ' does not exist'
         stop 1
        end if

        open(3,file=trim(parfile_gen))

!       general parameters
         read(3,*) !comment line
         read(3,*) profile_str
         read(3,*) fwhm_kms
         read(3,*) Nsigma
         read(3,*) scale_noise
         read(3,*) conf_hi
         read(3,*) conf_lo
         read(3,*) z_qso
         read(3,*) use_smooth_flux_str
         read(3,*) single_ion_fit_str
         read(3,*) spectrum_shifted_str
         read(3,*) visual_str
         read(3,*) test_run_str
         read(3,*) debug_str
         read(3,*) verbose_str
         read(3,*) quite_str
         read(3,*) psplot_str

!       ion list
         read(3,*) !comment line
         read(3,*) !comment line
         read(3,*) num_ions

!		  fit using smoothed or input flux?
         if (trim(use_smooth_flux_str).eq.'TRUE') then
          use_smooth_flux = .true.
         else
          use_smooth_flux = .false.
         end if

!       override number of ions to be considered if `single_ion_fit_str=TRUE'
!       note that the following parameter is per default set to .true.

         if (trim(single_ion_fit_str).eq.'FALSE') then
          single_ion_fit = .false.
         else
          single_ion_fit = .true.
          num_ions = 1
         end if


!       allocate ion name array
         allocate(IONS(num_ions))

!       read ion names
         do ionindex=1,num_ions
          read(3,*) IONS(ionindex)%name
         end do

        close(3)

!       ------------------------------------------------------------------------
!       initalise sweep flag
        sweep = 0

!       set instrumental broadening
        fwhm_kms_save = fwhm_kms

        if (fwhm_kms_save.gt.0.0d0) then         
         convolve = .true.
        else
         convolve = .false.
        end if

!       set logical flags according to corresponding input values:

!       note that the following parameters are per default set to .false.
        if (trim(spectrum_shifted_str).eq.'TRUE') then
         spectrum_shifted = .true.
        else
         spectrum_shifted = .false.
        end if

        if (trim(visual_str).eq.'TRUE') then
         visual = .true.
        else
         visual = .false.        
        end if
        
        if (trim(psplot_str).eq.'TRUE') then
         psplot = .true.
        else
         psplot = .false.        
        end if
        
        if (trim(verbose_str).eq.'TRUE') then
         verbose = .true.
        else
         verbose = .false.
        end if

        if (trim(debug_str).eq.'TRUE') then ! override verbose
         debug = .true.
         verbose = .true.
        else
         debug = .false.
        end if

        if (trim(quite_str).eq.'TRUE') then ! override verbose and debug
         quite = .true.
         debug = .false.
         verbose = .false.
        else
         quite = .false.
        end if

!       test_run = true implies visual = true at run-time; however, if input
!       value of visual = false, the correspoding files will be written but not
!       gnuplot-loaded
!       
        if (trim(test_run_str).eq.'TRUE') then
         test_run = .true.
        else
         test_run = .false.
        end if

!       ignore qso redshift input value if testing
        if (test_run) z_qso = 1.0d2

!       ------------------------------------------------------------------------
!       allocate vector with file names (one for each ion)
        allocate(ifit_file_prefix(num_ions))
        allocate(ifit_lines(num_ions))

!       allocate ions (only one ion with one transition for now)
        allocate(col_dens_min(num_ions))
        allocate(col_dens_max(num_ions))
        allocate(b_value_min(num_ions))
        allocate(b_value_max(num_ions))

!       ------------------------------------------------------------------------
!       read ion-dependent input parameters
        
        do ionindex=1,num_ions

!       define parameter file
         parfile_ion='ifit_parameters.'//trim(IONS(ionindex)%name)
        
!       check file existence
         inquire(file=parfile_ion,exist=file_exists)

!       if file doesn't exist, stop cleanly
         if (.not.file_exists) then
          write(6,'(1x,a)') trim(errmsg)
          write(6,'(1x,a)') 'input-parameter file '//trim(parfile_ion)//
     &    ' does not exist'
          stop 1
         end if

         open(3,file=trim(parfile_ion))

          read(3,*) !comment line
          read(3,*) ion_str
!       consistency check
          if (trim(ion_str).ne.IONS(ionindex)%name) then
           write(6,*) trim(warnmsg)
           write(6,*) 'Non-matching ion name in input parameter '
     &     //'file '//trim(parfile_ion)//': '//trim(ion_str)
           stop 1
          end if

          read(3,*) col_dens_min(ionindex)
          read(3,*) col_dens_max(ionindex)
          read(3,*) b_value_min(ionindex)
          read(3,*) b_value_max(ionindex)

         close(3)

        end do ! over ions

!       ------------------------------------------------------------------------
!       output some relevant parameter values

         write(6,'(a,8x,i5)') 'total ion(s): ', num_ions

!       ion-dependent parameters
         do ionindex=1,num_ions
          write(6,'(a,19x,a)') 'ion: ', trim(IONS(ionindex)%name)
          write(6,'(a,1x,f5.2)') 'min. col.dens (log10): ',
     &    dlog10(col_dens_min(ionindex))
          write(6,'(a,1x,f5.2)') 'max. col.dens (log10): ',
     &    dlog10(col_dens_max(ionindex))
          write(6,'(a,10x,f8.2)') 'min. b-value: ', b_value_min(ionindex)
          write(6,'(a,10x,f8.2)') 'max. b-value: ', b_value_max(ionindex)
          write(6,*)
         end do

!       general parameters
         write(6,'(a,15x,a)') 'profile: ', profile_str
         write(6,'(a,12x,f8.2)') 'FWHM (LSF): ', fwhm_kms
         write(6,'(a,9x,f8.2)') 'significance: ', Nsigma
         write(6,'(a,9x,f8.4)') 'goodness (hi): ', conf_hi
         write(6,'(a,8x,f8.4)') 'goodness (low): ', conf_lo
         write(6,'(a,14x,f8.3)') 'source z: ', z_qso
         write(6,'(a,13x,a)') 'SINGLE ION (transition): ',
     &   trim(single_ion_fit_str)
         write(6,'(a,17x,a)') 'SHIFT: ',trim(spectrum_shifted_str)
         write(6,'(a,18x,a)') 'TEST: ', trim(test_run_str)
         write(6,*)
        
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       IMPORTANT:

!       NOTE:
!       Perform sanity check over all input parameters -> TO DO!
!       For now:

!       abort if doing a test run with more than one ion
        if (test_run.and.(.not.single_ion_fit)) then
         write(6,'(1x,a)') trim(errmsg)
         write(6,'(1x,a)') ' Test run modus (test_rung) and mulitplet search '//
     &   '(single_ion_fit) both on!'
         stop 1
        end if


        if ((conf_hi.gt.1.0d0).or.(conf_lo.gt.1.0d0).or.(conf_hi.gt.conf_lo))
     &  then
         write(6,'(1x,a)') trim(errmsg)
         write(6,'(1x,21pe8.2)') 
     &   ' Non-valid confindence input value(s): ', conf_hi, conf_lo
         stop 1
        end if

        if ((profile_str.ne.'gauss').and.(profile_str.ne.'voigt')) then
         write(6,'(1x,a)') trim(errmsg)
         write(6,'(1x,a)') ' Non-valid profile input value:', profile_str
         stop 1
        end if

!       Make sure user know that Gaussian profiles are not suited for high
!       column densities
!       NOTE:
!       Need to make this ion dependent; take for now HI-Lya as reference;
!       Should be log N(HI)_max ~ 15 - 16

!       ion-dependent parameters
        do ionindex=1,num_ions

         write(6,'(1x,a)') 'Checking input parameters for ion: '
     &   //trim(IONS(ionindex)%name)

         if ((profile_str.eq.'gauss').and.(col_dens_max(ionindex).gt.1.0d20)) then
          write(6,'(1x,a)') trim(warnmsg)
          write(6,'(1x,a,f6.2)')
     &    ' Make sure that Gaussian profiles are suited for maximum '
     &    //'input column density (logarithmic): ',
     &    dlog10(col_dens_max(1))
         stop 1
         end if
        
!       compute instrumental broadening in terms of b:

!       (b [km/s])^2 >= (1/ln 2)*(0.5 FWHM [km/s])^2
!
!       where FWHM corresponds to the instrumental Line Spread Function (LSF)
!       print a warning if not;
        
         b_instrumental = (0.5*fwhm_kms)/dsqrt(dlog(2.0d0))

         if (b_value_min(ionindex).lt.b_instrumental) then
          write(6,'(1x,a)') trim(colour_warnmsg_prefix)//'BE AWARE!'//
     &    trim(colour_msg_suffix)
          write(6,'(a,f6.2)')
     &    'instrumental broadening (b_inst [km/s]):', b_instrumental
          write(6,'(a,f6.2)')
     &    'minimum input b-value (b_value_min [kms]):', b_value_min(ionindex)
         end if

        end do ! over ions

        if (test_run.and.spectrum_shifted) then
         write(6,'(1x,a)') trim(errmsg)
         write(6,'(1x,a)')
     &   'Possible non-compatible input parameter values for "test_run_str"'
     &   //' and "spectrum_shifted_str"!'
         stop 1
        end if
        
        if (z_qso.lt.0.0d0) then
         write(6,'(1x,a,f6.3)') trim(errmsg)
         write(6,'(1x,a,f6.3)') ' Wrong input QSO redshift: ', z_qso
         stop 1
        end if

!       ------------------------------------------------------------------------
!       allocate array to follow chi^2 evolution
        
        if (visual) then
         
         allocate(chi2_evolution(0:iter_max*4))
         allocate(component_number_evolution(0:iter_max*4))
         chi2_evolution(:) = 0.0
         component_number_evolution(:) = 0
        
        end if
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        return

        end subroutine input_parameters
!       ========================================================================

!       ========================================================================
        subroutine load_ions(numions)

!       NOTE:
!       Multiplet transitions do not be to be given in any particular order;
!       they will be order by decreasing strenght (f_osc * lambda_0) once loaded

!       NOTE:
!       object IONS allocated by routine `input_parameters'

        use set_precision
        use ifit_variables, only: IONS, strongest, single_ion_fit
        use constants

        implicit none

        integer(kind=singleI) :: i, j
        integer(kind=singleI), intent(in) :: numions
        integer(kind=singleI), allocatable :: vector_aux(:)

        character(len=64), parameter :: atomdata_file_prefix='atom_data.'
        character(len=10) :: ion_name

!       ------------------------------------------------------------------------
        do i=1,numions

         ion_name = IONS(i)%name

!       open file and read data

         open(10,file=trim(atomdata_file_prefix)//trim(ion_name))

          read(10,*) IONS(i)%name, IONS(i)%mass, IONS(i)%num_transitions

!		  override input number of transitions for single ion fit
		    if (single_ion_fit) then
		     IONS(i)%num_transitions = 1
			 end if

!       allocate and read data for each transition
          allocate(IONS(i)%transition(IONS(i)%num_transitions))

          do j=1,IONS(i)%num_transitions
           read(10,*) IONS(i)%transition(j)%lambda_0,
     &     IONS(i)%transition(j)%big_gamma,
     &     IONS(i)%transition(j)%f_osc
          end do

         close(10)

!       consistency check
         if (trim(ion_name).ne.trim(IONS(i)%name)) then
          write(6,*) 'Incorrect ion name '//trim(IONS(i)%name)//
     &    ' in file '//trim(atomdata_file_prefix)//trim(ion_name)
          stop 1
         end if

        end do ! over ions

!       ------------------------------------------------------------------------
!       allocate array and auxiliary vector
        allocate(strongest(numions,maxval(IONS(:)%num_transitions)))
        allocate(vector_aux(maxval(IONS(:)%num_transitions)))

        do i=1,numions

!       compute optical depth and related constants for strongest
!       transition (see model, model_at_px, model_local)

!       constant central optical depth (at x = 0); the *actual* central optical
!       depth is given in general by tau_0 * (N/b) and is *dimensionless*;
!       tau_0 below has the thus the appropriate dimensions cm^2 km/s,
!       hence the factor 1.0d-8; in short [tau_0] = cm^2 km/s

         IONS(i)%transition(:)%tau_0 = (dsqrt(pi) * radius_e * c_kms) *
     &   IONS(i)%transition(:)%f_osc * 
     &   (IONS(i)%transition(:)%lambda_0 * 1.0d-8) ! in cm

!       ratio of transition's lifetime to resonant frequency (dimensionless,
!       hence the factor 1.0d-13)

         IONS(i)%transition(:)%gamma_over_nu =
     &   ((IONS(i)%transition(:)%big_gamma *
     &   IONS(i)%transition(:)%lambda_0) *
     &   1.0d-13) / (4.0d0 * pi * c_kms)

!       arrange ionic data with increasing tau_0 ~(lambda_0 * osc. strength)

!       strongest(i,j) stores the indices of the transitions read
!       from in atomic data file of ion i such that the transitons are sorted 
!       in order of increasing oscillator strength when j runs from 1 to N,
!       where N is the number of transitions of that particular ion
 
!       arrange in *ASCENDING* order of tau_0
         call
     &   indexx(IONS(i)%num_transitions,IONS(i)%transition(:)%tau_0,vector_aux)

!       reverse vector to have it in *DESCENDING* order
         do j=1,IONS(i)%num_transitions
          strongest(i,j) = vector_aux(IONS(i)%num_transitions-j+1)
         end do

        end do ! over loaded ions

!       convert atomic mass from amu to gram

        IONS(:)%mass = IONS(:)%mass * atom_munit

!       free memory
        deallocate(vector_aux)

!       ------------------------------------------------------------------------
        return

        end subroutine load_ions
!       ========================================================================

!       ========================================================================
        logical function do_sweep(cycl)
        
        use set_precision
        use ifit_variables, only: IONS, fwhm_kms, fwhm_kms_save,
     +  strongest, single_ion_fit,
     +  colour_statmsg_prefix, colour_msg_suffix 

        implicit none

        integer(kind=singleI), intent(inout) :: cycl

        character(len=12) :: routinemsg = 'do_sweep:'

!       ------------------------------------------------------------------------
        do_sweep = .false.

        select case(cycl)

        case(0)

         cycl = 1
         fwhm_kms = 0.0d0
         do_sweep = .true.

!       output info
         write(6,'(a,i4)') trim(colour_statmsg_prefix)//trim(routinemsg)//
     &   trim(colour_msg_suffix), cycl
         write(6,'(a,i3,a)')
     &   'single ion (transition) fit ignoring instrumental broadening'
         write(6,'(a,f8.2)')
     &   'using (first listed ion | strongest transition ): '//
     &   trim(IONS(1)%name), IONS(1)%transition(strongest(1,1))%lambda_0
         write(6,*)


!       ------------------------------------------------------------------------
        case(1)
        
         cycl = 2
         do_sweep = .true.

!       set instrumental FWHM to input value
         fwhm_kms = fwhm_kms_save

         if (fwhm_kms.gt.0.0d0) then

!       output info
           write(6,'(a,i4)') trim(colour_statmsg_prefix)//trim(routinemsg)//
     &     trim(colour_msg_suffix), cycl
           write(6,'(a,i3,a)')
     &     'single ion (transition) fit including instrumental broadening'

          else if (single_ion_fit) then ! this was done during sweep=1

           cycl = 3
           do_sweep = .false.

          end if ! fwhm_kms > 0

!       ------------------------------------------------------------------------
        case(2)
        
         cycl = 3

         if (.not.single_ion_fit) then ! this was done during sweep=1,2

          do_sweep = .true.

!       set instrumental FWHM to input value
          fwhm_kms = fwhm_kms_save

!       output info
          write(6,'(a,i4)') trim(colour_statmsg_prefix)//trim(routinemsg)//
     &    trim(colour_msg_suffix), cycl

          if (fwhm_kms.gt.0.0d0) then

           write(6,'(a,i3,a)')
     &     'multiple ion/transition final fit including instrumental broadening'
           write(6,*)

          else

           write(6,'(a,i3,a)')
     &     'multiple ion/transition final fit ignoring instrumental broadening'

          end if ! fwhm_kms > 0
         
         else

          do_sweep = .false.

         end if ! not single ion fit


!       ------------------------------------------------------------------------
        case(3)

         do_sweep = .false.

        end select

!       ------------------------------------------------------------------------
        return

        end function do_sweep
!       ========================================================================

!       ========================================================================
        subroutine set_filename(cycl)
        
        use set_precision
        use ifit_variables, only: IONS, ION_INDEX, fwhm_kms, specfile_in,
     +  ifit_file_prefix, colour_statmsg_prefix, colour_msg_suffix 

        implicit none
        integer(kind=singleI), intent(inout) :: cycl

        character(len=13) :: routinemsg = 'set_filename:'

!       ------------------------------------------------------------------------
!       set common filename base; remove file extension

        ifit_file_prefix(ION_INDEX) =
     &  trim(specfile_in(1:scan(specfile_in,'.',.true.)-1))

        select case(cycl)

        case(1)

          ifit_file_prefix(ION_INDEX) = trim(ifit_file_prefix(ION_INDEX))//'_'//
     &    trim(IONS(ION_INDEX)%name)//'_noconv'

        case(2)

          ifit_file_prefix(ION_INDEX) = trim(ifit_file_prefix(ION_INDEX))//'_'//
     &    trim(IONS(ION_INDEX)%name)//'_conv'

        case (3)

         if (fwhm_kms.gt.0.0d0) then

          ifit_file_prefix(ION_INDEX) = trim(ifit_file_prefix(ION_INDEX))//
     &    '_conv'

         else

          ifit_file_prefix(ION_INDEX) = trim(ifit_file_prefix(ION_INDEX))//
     &    '_noconv'

         end if

        end select

!       ------------------------------------------------------------------------
!       print routine name for information
        write(6,'(a)') trim(colour_statmsg_prefix)//
     &  trim(routinemsg)//trim(colour_msg_suffix)//' base '//
     &  trim(ifit_file_prefix(ION_INDEX))

!       ------------------------------------------------------------------------
        return

        end subroutine set_filename
!       ========================================================================

!       ========================================================================
        subroutine read_spectrum(cycl)

!       input argument controls informative output to screen; if called from
!       ifitnot, output to screen is supressed

!       reads in from file input spectrum file in the following format:

!       wavelength  |  flux  |  noise

!       also sets modelflux = 1 everywhere before model calculation
!       and resid (the residual flux = flux_orig-modelflux+1) to flux_orig

        use set_precision
        use ifit_variables
        use constants

        implicit none

        integer(kind=singleI), intent(in) :: cycl
        integer(kind=singleI) :: i
        integer(kind=singleI) :: size_of_file
        
        character(len=64) :: routinemsg = 'read_spectrum: '

        logical :: file_exists

!       ------------------------------------------------------------------------
!       print routine name for information
        write(6,'(a)',advance='no') trim(colour_statmsg_prefix)//
     &  trim(routinemsg)//trim(colour_msg_suffix)//' '//trim(specfile_in)//
     &  ' ...'

!       ------------------------------------------------------------------------
!       check file existence
        inquire(file=trim(specfile_in),exist=file_exists)

!       If file doesn't exist, stop cleanly
        if (.not.file_exists) then
         write(6,*)
         write(6,*)
         write(6,'(1x,a)') trim(errmsg)
         write(6,'(1x,a)') ' file '//trim(trim(specfile_in))//' does not exist'
         stop 1
        end if

!       determine size of spectrum (in pixel); given as the number of records in
!       file
        spectrum_size_px = size_of_file(trim(specfile_in))

!       the following may be modified in fitting wavelength range is reduced
        spectrum_size_px_effective = spectrum_size_px

!       check for empty file
        if (spectrum_size_px.eq.0) then
         write(6,*)
         write(6,*)
         write(6,*) trim(errmsg)
         write(6,*) 'empty input spectrum file: '//trim(trim(specfile_in))
         stop 1
        end if

!       allocate spectrum arrays
        call alloc_spec(spectrum_size_px)

!       ------------------------------------------------------------------------
        open(2,file=trim(specfile_in),status='old')
        
        do i=1,spectrum_size_px
         read(2,*,end=10) wlength(i), flux(i), sigma(i)
        end do
        
 10     close(2)

!       ------------------------------------------------------------------------
!       create an exact copy of the input spectrum file, with a prefix common to
!       all ifit output files during first sweep (=1)

        if (cycl.eq.1) then

         open(2,file=trim(ifit_file_prefix(ion_index))//'.isp')
        
         do i=1,spectrum_size_px
          write(2,*) wlength(i), flux(i), sigma(i)
         end do

         close(2)

        end if
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       output info
        write(6,*) 'done.'
        write(6,'(a,i8)') 'spectrum size [pixel]:', spectrum_size_px

!       ------------------------------------------------------------------------
!       EXTREMELY IMPORTANT:

        neg_noise = .false.
        neg_noise_counts = 0
        
        neg_flux = .false.
        neg_flux_counts = 0
        
!       re-define negative/zero noise values in order to avoid crash
        if (any(sigma.le.0.0d0)) then

         neg_noise = .true.
         neg_noise_counts = count(sigma.le.0.0d0)
        
         write(6,'(1x,a)') trim(warnmsg)
         write(6,'(a,i5)')
     &   'Negative/zero noise values in input spectrum: ', neg_noise_counts
         write(6,*) 'Will set these to 1.0d0'

        where(sigma.le.0.0d0) sigma = 1.0d0

        end if ! noise < 0 anywhere?

!       re-define the value of pixels with negative flux values to their
!       interpolated (smoothed) value in order to avoid too large AOD-error
!       values (~ 1/f) and a potential crash

        if (any(flux.le.0.0d0)) then

         neg_flux = .true.
         neg_flux_counts = count(flux.le.0.0d0)
        
         write(6,'(1x,a)') trim(warnmsg)
         write(6,'(a,i5)')
     &   'Negative/zero flux values in input spectrum: ', neg_flux_counts
         write(6,*)
     &   'Will set these to thee maximum of their interpolated value and '//
     &   'corresponding noise'

!       set array default value
         flux_smooth = 0.0d0

!       use a linear (1) but narrow (min_component_res_px) smoothing window to
!       avoid negative and `too wrong' interpolated values

         call
     &   smooth_sg(flux_smooth,flux,1,spectrum_size_px,1,0,min_component_res_px,
     &   1.0d0,debug)

C         where(flux.le.0.0d0) flux = max(sigma,flux_smooth)
         where(flux.le.0.0d0) flux = abs(flux_smooth)

!       double-check
         if (any(flux.lt.0.0d0)) then

          neg_flux_counts = count(flux.le.0.0d0)
        
          write(6,'(1x,a)') trim(warnmsg)
          write(6,'(a,i5)')
     &   'Negative/zero flux values in input spectrum: ', neg_flux_counts
          write(6,'(a,i6)')
     &    'after linear smoothing with window of size [pixel]:',
     &    min_component_res_px
          write(6,*) 'Please check spectrum and correct if necessary'

          stop 1

         end if ! flux < 0 anywhere?

!       restore auxiliary array with default value
         flux_smooth = 0.0d0

        end if ! flux < 0 anywhere?

!       ------------------------------------------------------------------------
!       compute velocity in observer's frame

!       The line below assumes l=l_0*exp((v-v_0)/c) or (1+z)=exp((v-v_0)/c)

        velocity(1:spectrum_size_px) =
     &  c_kms*dlog(wlength(1:spectrum_size_px)/wlength(1))

!       allocate redshift vector and initialise
        if (allocated(z_min)) deallocate(z_min)
        allocate(z_min(1:num_ions,maxval(IONS(:)%num_transitions)))
        z_min(:,:) = 0.0d0

!       compute lowest redshift for *each* transition;
!       this quantity is required to get the correct absorption redshift for
!       each component, since the velocity scale is chosen such that v_0=0

        do i=1,num_ions
         z_min(i,1:IONS(i)%num_transitions) =
     &   (real(wlength(1)) /
     &   real(IONS(i)%transition(1:IONS(i)%num_transitions)%lambda_0)) - 1.0
        end do
!       ------------------------------------------------------------------------
!       determine average pixel size

!       NOTE: eoshift shifts an array 'shift' positions to the left; if shift is
!       negative, the shift is performed to the right

        velocity_aux = eoshift(velocity, shift = 1)
        wlength_aux = eoshift(wlength, shift = 1)
        
        pixel_size_kms =
     &  sum(velocity_aux(1:spectrum_size_px-1) -
     &  velocity(1:spectrum_size_px-1))/dble(spectrum_size_px - 1)

        pixel_size_ang =
     &  sum(wlength_aux(1:spectrum_size_px-1) -
     &  wlength(1:spectrum_size_px-1))/dble(spectrum_size_px - 1)

!       NEED TO CORRECT THE LINE BELOW TO ADAPT FOR ALL IONS

        if ((fwhm_kms_save.le.0.0d0).and.
     &  (b_value_min(1).le.0.6*pixel_size_kms)) then
!         write(6,'(1x,a)') trim(errmsg)
         write(6,'(1x,a)') trim(warnmsg)
         write(6,'(1x,a,f6.2)')
     &   'Input Doppler parameter equal / smaller than allowed:',
     &   0.6*pixel_size_kms
         write(6,'(1x,a)') 'Note that the min allowed b-value is set by '//
     &   'the average pixel size'
!         stop 1
        end if

        if ((fwhm_kms.gt.0.0d0).and.(fwhm_kms.lt.pixel_size_kms)) then
         write(6,'(1x,a)') trim(errmsg)
         write(6,'(a,2f6.2)')
     &   ' Instrumental resolution (FWHM) smaller'//
     &   ' than (average) pixel size:', fwhm_kms, pixel_size_kms
         write(6,*)
         stop 1
        end if
        
!       define width of scanning window use to detect absorption profiles which
!       are significant in absorption;
!       this parameter is used by absorption_profiles and insert_component
!       NOTE:
!       the following paramter is equal to the size of a resolution
!       element in pixel; but should be at least 'min_component_res_px' pixel in
!       size
        
        resol_elem_px = min_component_res_px
!     &  max(min_component_res_px,ceiling(fwhm_kms/pixel_size_kms))

!       NOTE: 'resol_elem_px' sets the window width used in absorption_profiles
!       to detect absorption profiles that are significant in absorption

!       ------------------------------------------------------------------------
!       cycle spectra to avoid features at boundaries
!       NOTE:
!       this is allowed ONLY for simulations with periodic boundary conditions
!       absorption profile boundaries and component centroids, both in velocity
!       and wavelength space, are shifted back before output (see
!       absorption_profiles and ifitnot.f)
        
!       Initialise array shift
        spectrum_shift_px  = 0

!       Cycle arrays flux and sigma
        call shift_spectrum(spectrum_shifted)

!       if non shifting required, limit the wavelength range to that relevant
!       for the given ion
        if ((.not.spectrum_shifted).and.(.not.test_run)) then
         
!       set wavelength boundaries (NEED TO DO THIS PROPERLY)

         wl_min = wlength(1)
!     &   minval(IONS(ION_INDEX)%transition(:)%lambda_0) - wlength_tolerance

         wl_max = wlength(spectrum_size_px)
!     &   maxval(IONS(ION_INDEX)%transition(:)%lambda_0) * (1.0d0+z_qso) +
!     &   wlength_tolerance

         if (((wl_min.gt.wlength(1)).and.(wl_min.lt.wlength(spectrum_size_px)))
     &   .or.(wl_max.lt.wlength(spectrum_size_px))) then


          write(6,'(1x,a)') trim(warnmsg)
          write(6,'(a,2(f8.2),a,2(f10.2))')
     &    'Constraining fitting wavelength range:',
     &    wlength(1), wlength(spectrum_size_px), ' to ', wl_min, wl_max

          where((wlength.lt.wl_min).or.(wlength.gt.wl_max)) flux = 1.0d0

          spectrum_size_px_effective = 
     &    count((wlength.ge.wl_min).and.(wlength.le.wl_max))

!       output info
          write(6,'(a,i8)') 'effective spectrum size [pixel]:',
     &    spectrum_size_px_effective

!       if spectrum_size_px_effective=0, then there are no lines in the spectrum
!       corresponding to the ion intended to be fitted
          if (spectrum_size_px_effective.eq.0) then

           write(6,'(1x,a)') trim(errmsg)
           write(6,'(1x,a)') 'ion '//trim(IONS(ION_INDEX)%name)//
     &     ' is not present in spectral wavelenght range! Causes:'
           write(6,'(1x,a)') '1) z_qso in input parameter file is incorrect'
           write(6,'(1x,a)') '2) spectrum does not contain absorption by '
     &     //trim(IONS(ION_INDEX)%name)
           write(6,*)
           stop 1

          end if

         end if ! wlength range

        end if ! not shifting, no test run

        write(6,*)

!       ------------------------------------------------------------------------
!       Initialise arrays

        aod_data(1:spectrum_size_px) = -1.0*dlog(flux(1:spectrum_size_px))
        noise(1:spectrum_size_px) = sigma(1:spectrum_size_px)
        modelflux(1:spectrum_size_px) = 1.0d0
        modelflux_convolved(1:spectrum_size_px) = 1.0d0

!       ------------------------------------------------------------------------
!       rescale noise (it's better not to, unless there is a good reason...)
        
        noise(:) = scale_noise * noise(:)

!       ------------------------------------------------------------------------
!       EXTREMELY IMPORTANT: get rid of glitches / bad pixels, etc.

        bad_pixel = .false.
        bad_pixel_counts = 0

!       use a linear (1) but narrow (min_component_res_px) smoothing window to
!       avoid negative and `too wrong' interpolated values

         call
     &   smooth_sg(flux_smooth,flux,1,spectrum_size_px,1,0,min_component_res_px,
     &   1.0d0,debug)

         if (any(abs(flux_smooth-flux).gt.(sigma_bad_pixel*noise))) then

          bad_pixel = .true.
          bad_pixel_counts =
     &    count(abs(flux_smooth-flux).gt.(sigma_bad_pixel*noise))

!		 may lead to significant but narrow absorption profiles which are then discarded;
!		 need a better solution to the problem of bad pixels than the following:
C          where(abs(flux_smooth-flux).gt.(sigma_bad_pixel*noise))
C     &    noise = 1.0d0

          write(6,'(1x,a)') trim(warnmsg)
          write(6,'(a,i5)') 'found some bad pixels: ', bad_pixel_counts

         end if ! bad pixels

!       restore auxiliary array with default value
         flux_smooth = 0.0d0


!       ------------------------------------------------------------------------
        return

        end subroutine read_spectrum
!       ========================================================================

!       ========================================================================
        subroutine shift_spectrum(shift)

!       Locate maximum flux pixel and cycle spectra to avoid having fea-
!       tures at boundaries; this is allowed since simulations have
!       periodic boundary conditions

        use set_precision
        use ifit_variables
        
        implicit none

        integer(kind=singleI), dimension(1) :: max_flux_pos
        character(len=15) :: routinemsg = 'shift_spectrum:'
        logical, intent(in) :: shift

!       ------------------------------------------------------------------------
!       return if no shifting required
        
        if (.not.shift) then

         write(6,*)
         write(6,'(1x,a)') trim(colour_statmsg_prefix)//
     &   trim(routinemsg)//trim(colour_msg_suffix)
         write(6,'(a,i6,a)') 'spectrum will not be cyclically shifted'

         return

        end if

!       ------------------------------------------------------------------------
!       Get position of pixel with maximum flux
!       maxloc(array) returns the location of the maximum value of array
!       cshift(array,shift) cycles array by shift positions to the left;
!       hence, if location of maximum flux value is pos, cshift(flux,pos-1)
!       shifts the elements of the array such that max_flux is at the 1st
!       position of array

        max_flux_pos = maxloc(flux)-1

!       Store shift amount to shift arrays back for output
        spectrum_shift_px = max_flux_pos(1)

        write(6,'(1x,a)') trim(colour_warnmsg_prefix)//
     &  trim(routinemsg)//trim(colour_msg_suffix)
        write(6,'(1x,a,i6,a)') 'Shifting flux and noise vectors'
     &  //' by', -1*spectrum_shift_px, ' pixels'
        write(6,*)
        write(6,'(1x,a,i6,a)') 
     &  ' Shift corresponds to (approximately): ',
     &  int(-1.0d0*spectrum_shift_px*pixel_size_kms), ' km/s'
        write(6,*)

!       Shift input spectrum arrays correspondingly

        flux(1:spectrum_size_px) =
     &  cshift(flux(1:spectrum_size_px),spectrum_shift_px)

        sigma(1:spectrum_size_px) =
     &  cshift(sigma(1:spectrum_size_px),spectrum_shift_px)
        
!       ------------------------------------------------------------------------
        return
        
        end subroutine shift_spectrum
!       ========================================================================

!       ========================================================================
        subroutine get_absorption_profiles()

!       identifies and counts absorption profiles, sets its boundaries, and
!       initialises several relevant fit variables

        use set_precision
        use ifit_variables
        
        implicit none

        integer(kind=singleI) :: absorption_profiles

!       ------------------------------------------------------------------------
!       determine the number of absorption profiles to allocate necessary arrays
!       this number is highly overestimated since the line below gives the
!       number of pixels above Nsigma; limit to 1 to avoid code crash; code will
!       simply produce an empty fit in this case

        call
     &  smooth_sg(flux_smooth,flux,1,spectrum_size_px,4,0,2*resol_elem_px,1.0d0,
     &  debug)

        call linear_chi2_spectrum(flux_smooth,noise,spectrum_size_px)       

        max_abs_profiles =
     &  max(1,count(linear_chi2_spec(1:spectrum_size_px).gt.Nsigma))

!       de- and re-allocate arrays
        if (allocated(abs_profile_bounds)) deallocate(abs_profile_bounds)
        if (allocated(abs_profile_tag)) deallocate(abs_profile_tag)
        allocate(abs_profile_bounds(max_abs_profiles,2))
        allocate(abs_profile_tag(max_abs_profiles))
        abs_profile_tag(:) = 0

!       ------------------------------------------------------------------------
!       smooth data; note that this is used *only* to identify the detection
!       absorption profiles; the fit is done on the actual data (array flux,
!       *not* resid)

!       fifth-to-last parameter in smooth_sg is order of fitting polynomial:
!       0 =  constant (box-car window)
!       1 =  linear, ...
!       fourth-to-last parameter in smooth_sg is order of derivative
!       0 = no derivative (i.e., smoothed function)
!       1 =  first derivative of function
!       third-to-last parameter is window size (in pixel)
!       second-to-last parameter is padding value to handle boundaries,
!       e.g. 1.0d0 for an ideally continuum-normalised spectrum
!       last parameter signals that file with input and smoothed flux be written
!       a box-car window is used, which implies that the position of the
!       individual components is conserved; see smooth_sg for more info

        call
     &  smooth_sg(flux_smooth,flux,1,spectrum_size_px,4,0,2*resol_elem_px,1.0d0,
     &  debug)

!       re-compute initial number of detection absorption profiles
!       last parameter signals that absorption profiles will be expandend and
!       overlapping absorption profiles combined; save original number (in case
!       any gets split)

        num_abs_profiles = 0
        
        num_abs_profiles =
     &  absorption_profiles(1,spectrum_size_px,abs_profile_bounds,flux_smooth)

        num_abs_profiles_save = num_abs_profiles

!       activate all detected absorption profiles
        abs_profile_tag(1:num_abs_profiles) = 1

!       ------------------------------------------------------------------------
!       total number of processed and splitted absorption profiles
        processed_abs_profiles = 0
        splitted_abs_profiles = 0
        empty_abs_profiles = 0

!       total number of fitted, inserted, discarded, processed and unidentified
!       components
        fitted_components = 0
        discarded_components = 0
        inserted_components = 0
        unidentified_components = 0

!       ------------------------------------------------------------------------
!       average, minimum and maximum reduced chi^2 value;
!       min_chi2 and max_chi2 each store the minimum/maximum chi^2 value among
!       all absorption profiles and the absorption profile ID;
!       all values written to standard output when fit is finished

        mean_chi2 = 0.0d0
        min_chi2(1) = huge(real_double)
        min_chi2(2) = 0.0d0
        max_chi2(:) = 0.0d0
       
!       ------------------------------------------------------------------------
        return
        
        end subroutine get_absorption_profiles
!       ========================================================================

!       ========================================================================
        subroutine spectrum_characteristics(cycl)

        use set_precision
        use ifit_variables
        use constants

        implicit none

        integer(kind=singleI), intent(in) :: cycl

        integer(kind=singleI) :: ionindex

        character(len=64) :: routinemsg = 'spectrum_characteristics:'

!       ------------------------------------------------------------------------
!       skip after first sweep
        if (cycl.gt.1) return

!       ------------------------------------------------------------------------
!       output info

        if (verbose) then

         write(6,'(1x,a,3i5)') trim(colour_statmsg_prefix)//trim(routinemsg)//
     &   trim(colour_msg_suffix)//
     &   'absorption profiles and components (actual | max):',
     &   num_abs_profiles, count(abs(IONS(ION_INDEX)%component(:)%status).ne.0),
     &   max_components

        end if

!       ------------------------------------------------------------------------
!       formal detection limit in terms of the rest-frame equivalent
!       width (in mili-Angstroem; see eqs. (2) and (3), and
!       eqs. (4) and (5), respectively, of
!       Lanzetta, Turnshek, and Wolfe (1987; LTW87),
!       
!       W_lim [mA] = Nsigma * sqrt(n) * (dv/c) * lambda_0 * <sigma>
!
!       where, Nsigma is an input parameter (see input_parameters.f), n is the
!       window size in pixel, dv is the pixel size in velocity units,
!       c is the speed of light, lambda_0 is the rest-frame wavelength,
!       and <sigma> ~ (S/N)^{-1} is the average noise

!       NOTE:
!       use only pixels in *non-absorbed* absorption profiles
!       these pixels are flaged by the array `unabsorbed_pixel' defined in
!       the routine `absorption_profiles'
!       if the entire spectrum has significant absorption, print a warning
!       and redefine array unabsorbed_pixel
        
       if (count(unabsorbed_pixel.eq.1).eq.0) then
         write(6,'(1x,a,f9.4)') trim(warnmsg)//trim(routinemsg)//
     &   ' Spectrum displays significant absorption everywhere!'
         unabsorbed_pixel(1:spectrum_size_px) = 1
         write(6,*)
        end if

!       global average signal-to-noise ratio
        average_sn_global =
     &  (sum(abs((flux(1:spectrum_size_px) * 
     &  unabsorbed_pixel(1:spectrum_size_px)) / noise(1:spectrum_size_px)))
     &  /count(unabsorbed_pixel.eq.1))

!       global average noise
        average_noise_global =
     &  (sum((noise(1:spectrum_size_px)*unabsorbed_pixel(1:spectrum_size_px)))
     &  /count(unabsorbed_pixel.eq.1))

!       NOTE: pixel size in Angstroem and km/s are computed in read_spectrum

!       print routine name for information
        write(6,'(1x,a)') trim(colour_statmsg_prefix)//
     &  trim(routinemsg)//trim(colour_msg_suffix)
        write(6,*)

        write(6,'(1x,a,20x,f9.4)')
     &  'average pixel size [Ang]:', pixel_size_ang
         write(6,'(1x,a,19x,f9.4)')
     &  'average pixel size [km/s]:', pixel_size_kms
        write(6,'(1x,a,19x,i3)')
     &  'resolution element [pixel]:', resol_elem_px
        write(6,'(1x,a,15x,i3)')
     &  'min. abs. profile size [pixel]:', min_absprofile_size_px


        write(6,'(1x,a,32x,1pe8.2)')
     &  'average noise:', average_noise_global
        write(6,'(1x,a,32x,f8.2)') 
     &  'average S/N:', average_sn_global
        write(6,*)

        write(6,'(1x,a,18x,2(2x,1pe12.4))')
     &  'minimum / maximum noise:', minval(noise,noise.ne.1.0d0),
     &  maxval(noise,noise.ne.1.0d0)
        write(6,'(1x,a,20x,2(2x,1pe12.4))')
     &  'minimum / maximum S/N:', minval(flux/noise,noise.ne.1.0d0),
     &  maxval(flux/noise,noise.ne.1.0d0)
        write(6,*)


!       ------------------------------------------------------------------------
!       compute lower detection limit for the SECOND strongest transition of
!       each ion (assuming there is a second one; otherwise use the only
!       transition available);
!       the idea is that at least two transitions are required to marginally
!       identify the absorption by a given ion 

!       the following two depend only on the spectrum properties and are hence
!       identical for all ions:

!       lower limit on central optical depth

         tau_central_lim = Nsigma * average_noise_global

!       the following limit is given by simple integration over the minimum
!       allowed size of an absorption profile

        equiv_width_lim_mA = 1.0d3 * ! in mA
     &  Nsigma * dble(min_absprofile_size_px) *
     &  (1.0d0 - dexp(-1.0d0*tau_central_lim)) *
     &  (pixel_size_ang)

        write(6,'(1x,a,6x,f8.2)')
     &  'Formal detection limit (exp[-tau]):', dexp(-1.0d0*tau_central_lim)

        write(6,'(1x,a,6x,f8.2)')
     &  'Formal detection limit (W_lim [mA]):', equiv_width_lim_mA
        write(6,*)

!       these are all ion(transition)-dependent

        write(6,'(1x,a)') 
     &  'the following values are estimated using the SECOND strongest '//
     &  'transition of each ion (if existent); the idea is that at least two '
     &  //'transitions are required to marginally identify the absorption by a'
     &  //' given ion'
        write(6,*)

        do ionindex=1,num_ions

!       lower limit on absorption strength (N/b)

         absorption_strength_lim = tau_central_lim /
     &   IONS(ionindex)%transition(
     &   strongest(ionindex,min(2,IONS(ionindex)%num_transitions)))%tau_0

!       compute threshold (minimum) column density (assuming linear
!       curve-of-growth)

         col_dens_lim = 1.0d5 * ! in cm^{-2}
     &   equiv_width_lim_mA /
     &   (pi * radius_e *
     &   IONS(ionindex)%transition(
     &   strongest(ionindex,min(2,IONS(ionindex)%num_transitions)))%f_osc *
     &   IONS(ionindex)%transition(
     &   strongest(ionindex,min(2,IONS(ionindex)%num_transitions)))%lambda_0 *
     &   IONS(ionindex)%transition(
     &   strongest(ionindex,min(2,IONS(ionindex)%num_transitions)))%lambda_0)

!       output to screen only of called from autofit
        
         write(6,'(1x,a)') 'ion: '//trim(IONS(ionindex)%name)
         write(6,*)

         write(6,'(1x,a,i4)') 'transition: ',
     &   strongest(ionindex,min(2,IONS(ionindex)%num_transitions))
         write(6,*)

         write(6,'(1x,a,6x,f8.2)')
     &   'Formal detection limit (log10[N/b]):', dlog10(absorption_strength_lim)

         write(6,'(1x,a,6x,f8.2)')
     &   'Formal detection limi (log10[N]):', dlog10(col_dens_lim)

         write(6,*)

!       check that limit below minimum allowed column density
         if (dlog10(col_dens_lim).gt.dlog10(col_dens_min(ionindex))) then
          
          write(6,'(1x,a)') trim(warnmsg)//trim(routinemsg)
          write(6,'(1x,a,f8.2)')
     &    'Formal column density limit higher than minimum input '//
     &    'column density (log10[N_min]):',
     &    dlog10(col_dens_min(ionindex))
          write(6,*)

         end if

        end do ! over ions

!        stop 1
!       ------------------------------------------------------------------------

!       check that detection limit is at least 1-sigma:
         if ((Nsigma * average_noise_global).lt.
     &   (1.0/average_sn_global)) then

          write(6,'(1x,a)')  trim(errmsg)//trim(routinemsg)//
     &    'Detection limit below average noise!'
          write(6,*) 'Increase Nsigma by, at least, ',
     &    ceiling(1.0d0/((Nsigma * average_noise_global)*average_sn_global))
          stop 1
          write(6,*)

         end if

        return
        
        end subroutine spectrum_characteristics
!       ========================================================================

!       ========================================================================
        function pick_abs_profile(n_abs_profiles)

!       NOTE:
!       identifies absorption profile with lowest average flux and returns
!       corresponding ID 

        use set_precision
        use ifit_variables, only: abs_profile_tag, flux, abs_profile_bounds,
     +  average_flux_local

        implicit none
        
        integer(kind=singleI) :: pick_abs_profile

        integer(kind=singleI) :: i
        integer(kind=singleI) :: n_abs_profiles
        integer(kind=singleI) :: absprofile_id(1), absprofile_id_aux
        real(kind=doubleR) :: flux_average, min_flux_inter

!       pick out first working absorption profile;
!       'maxloc' picks out the position of the largest element in array (and
!       the first occurrence if there are more than one elements with the same
!       highest value)
         
         absprofile_id = maxloc(abs_profile_tag)
         pick_abs_profile = absprofile_id(1)
         absprofile_id_aux = absprofile_id(1)
        
!       start with average flux in first working absorption profile
         flux_average =
     &   sum(
     &   flux(abs_profile_bounds(absprofile_id_aux,1):
     &   abs_profile_bounds(absprofile_id_aux,2))) / 
     &   (abs_profile_bounds(absprofile_id_aux,2) -
     &   abs_profile_bounds(absprofile_id_aux,1) + 1 )

!       loop over all absorption profiles and pick out pixel with minimum
!       average flux

         do i=1,n_abs_profiles
          
          min_flux_inter =
     &    sum(flux(abs_profile_bounds(i,1):abs_profile_bounds(i,2))) /
     &    (abs_profile_bounds(i,2) - abs_profile_bounds(i,1) + 1 )

!       find absorption profile with minimum average flux; skip processed
!       absorption profile 
          if ((min_flux_inter.lt.flux_average).and.(abs_profile_tag(i).gt.0))
     &    then
           flux_average = min_flux_inter
           pick_abs_profile = i
          end if
         
         end do

!       save average flux value (for output puroposes)
        average_flux_local = flux_average

!       ------------------------------------------------------------------------
        return

        end function pick_abs_profile
!       ========================================================================

!       ========================================================================
        subroutine
     &  smooth_sg(arr_out,arr_in,px_min,px_max,order_poly,order_der,wsize,
     &  padding,output)

!       smooths the spectrum by convolution with a Savitzky-Golay filter
!       using a polynomial of order m = 4, a derivative of order n = 0,
!       and a window with a full width equal to 9 pixel

!       NOTE: order of fitting polynomial indicates the highest moment
!       conserved during smoothing:
!       n=0 -> area (equivalent width) is conserved
!       n=1 -> area and position are conserved
!       n=2 -> area, position and width are conserved
!       etc.

!       IMPORTANT: strictly, SG filtering should only be applied to
!       equally spaced data points!

!       Best results are obtained for m = 4 and widht 1-2 FWHM of the data

!       See chapter 14.8 in the Numerical Recipes for Fortran 77 for more info

        use set_precision
        use ifit_variables, only: ifit_file_prefix, ION_INDEX, noise,
     +  colour_statmsg_prefix, colour_msg_suffix

        implicit none

        integer(kind=singleI), intent(in) :: px_min, px_max, wsize
        integer(kind=singleI), intent(in) :: order_poly, order_der
        real(kind=doubleR), intent(in) :: padding
        real(kind=doubleR), intent(inout) :: arr_out(px_min:px_max)
        real(kind=doubleR), intent(in) :: arr_in(px_min:px_max)

        integer(kind=singleI) :: i, j, size
        integer(kind=singleI) :: run_indx
        integer(kind=singleI) :: nl, nr !points to the left/right wrt point
        real(kind=doubleR) :: signal
        real(kind=doubleR), allocatable :: coeff(:)

        character(len=256) :: outfile
        character(len=64) :: routinemsg = 'smooth_sg:'
        
        logical :: first_call = .true.
        logical :: output

	integer(kind=doubleI) :: fact

!       ------------------------------------------------------------------------
!       set parameter values

        nl = wsize
        nr = wsize
        size = nl + nr + 1

!       output
        if (first_call) then
         first_call = .false.
         write(6,*)
         write(6,'(1x,a)') trim(colour_statmsg_prefix)//
     &   trim(routinemsg)//trim(colour_msg_suffix)
         write(6,'(a,i3)')
     &   'smoothing spectrum with window of size (pixel): ', size
        end if

!       initialise array
        arr_out(px_min:px_max) = 0.0d0

        if (allocated(coeff)) deallocate(coeff)
        allocate(coeff(1:size))
        coeff(1:size) = 0.0d0
        signal = 0.0d0

        call savgol(coeff,size,nl,nr,order_der,order_poly)

!       sg output is in wrap-around order; cyclally shift array to
!       get entries in 'right' order

        coeff(1:size) = cshift(coeff(1:size),-nr)

!       multiply with k! to obtain correct coefficients when order_der > 1

!       initialise
	fact = 1
	do i=1,order_der
	 fact = fact * i
	end do
        coeff(1:size) = fact * coeff(1:size)

!       do i=px_min, px_max
!       do j=size, 1, -1
!       run_indx = nr + 1 - j
!       write(6,*) i, i+run_indx, run_indx, coeff(j)
!       end do
!       print*
!       end do

!       convolve data with filter;
!       note that filter response is returned in wrap-around order
        do i=px_min, px_max

         do j=size, 1, -1
          
          run_indx = i + nr + 1 - j

!       NOTE:
!       the next lines take care of boundary conditions
!       "zero"-padding done with values equal to the input value, e.g
!       1.0d0 for an ideal continuum

          if (run_indx.lt.px_min) then
           signal = padding
          else if (run_indx.gt.px_max) then
           signal = padding
          else
           signal = arr_in(run_indx)
          endif

          arr_out(i) = arr_out(i) + coeff(j)*signal
         
         end do
         
        end do

!       output original and smoothed array for visualisation and comparison
!       noise is output as well for completeness
        if (output) then

         if (order_der.eq.0) outfile=trim(ifit_file_prefix(ION_INDEX))//'.ssg'
         if (order_der.eq.1) outfile=trim(ifit_file_prefix(ION_INDEX))//'.fde'
         if (order_der.eq.2) outfile=trim(ifit_file_prefix(ION_INDEX))//'.sde'

         open(unit=22,file=trim(outfile))

!       Read until end of file ...
         do
          read(22,*,end=55)
         end do
  55     continue
         backspace(22) ! avoid end-of-file (EOF) marker

         write(22,*)
         write(22,*)
         
         do i=px_min,px_max
          write(22,*) arr_in(i), arr_out(i), noise(i)
         end do
        
         close(22)
        
        end if
!       ------------------------------------------------------------------------
        return

        end subroutine smooth_sg
!       ========================================================================

!       ========================================================================
        subroutine model_spectrum(m,n,px_min,px_max)

!       computes value of flux in absorption profile (px_min,px_max) including
!       all components numbered m through n.

        use set_precision
        use ifit_variables
        use constants

        implicit none

        integer(kind=singleI) :: m, n, px_min, px_max
        integer(kind=singleI) :: i, j
        real(kind=doubleR) coldens, bvalue, doppwidth, x, damping
        real(kind=doubleR) :: tau_central, tau
        real(kind=doubleR) :: optical_depth   ! function

        real(kind=doubleR) :: vel_centre

!       initialise
        tau = 0.0d0

        do i=m,n

!       column density
         coldens = IONS(true_ion(i))%component(i)%col_dens

!       Doppler parameter
         bvalue = IONS(true_ion(i))%component(i)%b_value

!       component velocity centre (in v/c space)
         vel_centre = IONS(true_ion(i))%component(i)%vel_c

!       Doppler width (in Angstroem)
         doppwidth =
     &   (IONS(true_ion(i))%transition(true_transition(i))%lambda_0 *
     &   bvalue) / c_kms

!       damping parameter
         damping =
     &   (IONS(true_ion(i))%transition(true_transition(i))%gamma_over_nu /
     &   doppwidth) *
     &   IONS(true_ion(i))%transition(true_transition(i))%lambda_0 

!       central optical depth
         tau_central =
     &   IONS(true_ion(i))%transition(true_transition(i))%tau_0 *
     &   (coldens/bvalue)

!       NOTE:
!       'gamma_over_nu' is the natural line width (`big_gamma') divided by
!       4 pi nu and is calculated in read_spectrum.f;
!       'damping' is then the damping parameter, usually denoted 'a' as 
!       in H(a,x), where H(a,x) is the Voigt-Hjerting function, and calculated
!       as big_gamma / 4 pi Delta_nu, where Delta_nu = nu (b/c)

         do j=px_min,px_max

!       distance to component centre (in v/c space) in of doppler units

          x = (dexp((velocity(j)-vel_centre)/c_kms)-1.0d0) * (c_kms / bvalue)

!       Compute flux at point x(j)
          
          tau = optical_depth(profile_str,tau_central,damping,x)

!		  avoid underflow
			 if ((log(modelflux(j)) - tau).gt.log(tiny(real_single))) then
          	modelflux(j) = modelflux(j) * dexp(-1.0d0*tau)
			 else
			 	modelflux(j) = tiny(real_single)
			 end if          
         
         end do !j-loop; over absorption profile
        
        end do !i-loop; over components

!       ------------------------------------------------------------------------
        return

        end subroutine model_spectrum
!       ========================================================================

!       ========================================================================
        function model_at_px(par_val,numpar,veloc,abstype)

!       computes value of flux at a single velocity veloc including
!       of (numpar/3) components with parameters stored in par_val
!       recall that the parameters associated to component i are:
!       par_val(3(i-1)+1) => velocity centre
!       par_val(3(i-1)+2) => column density
!       par_val(3i)       => b-value


        use set_precision
        use ifit_variables
        use constants

        implicit none

        real(kind=doubleR) :: model_at_px

        integer(kind=singleI) :: numpar
        integer(kind=singleI) :: i, window, window_conv, pixel, central_px

        integer(kind=singleI) :: comp_id

        real(kind=doubleR) :: par_val(numpar), veloc, vel_px
        real(kind=doubleR) :: coldens, bvalue, doppwidth,
     +  x, damping, tau_central, tau, sigmakms, sigmapx
        real(kind=doubleR), allocatable ::  model(:),
     +  model_convolved(:)

        real(kind=doubleR) :: vel_centre

        real(kind=doubleR) :: optical_depth   ! function

        character(len=*) :: abstype

        character(len=64) :: routinemsg = 'model_at_px:'

!       initialise
        tau = 0.0d0

!       compute Gaussian width of convolution kernel (in km/s)
        sigmakms = fwhm_kms / (2.0d0 * dsqrt( 2.0d0 * dlog(2.0d0)))

!       express Gaussian width (in pixels)
        sigmapx = sigmakms / pixel_size_kms_local

!       define window size in pixel (for convolution, set by non-zero width of
!       of convolution kernel in routine gauss_conv)
        
        if (fwhm_kms.gt.0.0d0) then
        
         window = min(ceiling(0.5*spectrum_size_px),ceiling(1.0d1*sigmapx) + 1)
         window_conv = 2*window + 1
        
        else
         
         window = 0
         window_conv = 2*window + 1        

        end if ! convolution?

!       allocate arrays
        if (allocated(model)) deallocate(model)
        if (allocated(model_convolved))
     &  deallocate(model_convolved)

        allocate(model(-window:window))
        allocate(model_convolved(1:window_conv))

        model(:) = 1.0d0
        model_convolved(:) = 1.0d0

!       compute model flux at pixel corresponding to given velocity

        central_px = ceiling(0.5d0*window_conv)

        do i=1,numpar,3

!       component ID
         comp_id = component_id(i)

!       component velocity centre
         vel_centre = par_val(i)

!       column density
         coldens = par_val(i+1)

!       Doppler parameter
         bvalue = par_val(i+2)

!       Doppler width (in Angstroem)
         doppwidth =
     &   (IONS(true_ion(comp_id))%transition(true_transition(comp_id))%lambda_0
     &   * bvalue) / c_kms

!       damping parameter
         damping =
     &   (IONS(true_ion(comp_id))%transition(true_transition(comp_id))%gamma_over_nu /
     &   doppwidth) *
     &   IONS(true_ion(comp_id))%transition(true_transition(comp_id))%lambda_0 

!       central optical depth
         tau_central =
     &   IONS(true_ion(comp_id))%transition(true_transition(comp_id))%tau_0 *
     &   (coldens/bvalue)

!       NOTE:
!       'gamma_over_nu' is the natural line width (capital gamma) divided by
!       4 pi nu and is calculated in read_spectrum.f;
!       'damping' is then the damping parameter, usually denoted 'a' as in
!       H(a,x), where H(a,x) is the Voigt-Hjerting function, and calculated
!       as big_gamma / 4 pi Delta_nu, where Delta_nu = nu (b/c)

         do pixel = -window, window
         
          vel_px = veloc + pixel*pixel_size_kms_local

!       distance to component centre (in v/c space) in doppler units

          x = (dexp((vel_px-vel_centre)/c_kms)-1.0d0) * (c_kms / bvalue)

!       model flux at x(pixel)
          
          tau = optical_depth(profile_str,tau_central,damping,x)

          model(pixel) = dlog(model(pixel)) - tau

!       avoid underflow          
          model(pixel) = max(dble(tiny(real_single)), dexp(model(pixel)))

         end do ! over pixel around veloc

        end do ! i-loop ; over parameters

!       store model flux without convolution yet
        model_convolved(:) = model(:)

!       convolve model spectrum (if required, i.e. if fwhm_kms > 0)
        
        call
     &  gauss_conv(model_convolved,1,window_conv,pixel_size_kms_local,fwhm_kms)

!       compute final optical depth at input pixel;
!       depending on model adopted

!       check for zero values
        where(model_convolved.lt.tiny(real_double))
     &  model_convolved = tiny(real_double)
        
        if (trim(abstype).eq.'sat') then
         
         model_at_px = model_convolved(central_px)
        
        else if (trim(abstype).eq.'nonsat') then
         
         model_at_px = -1.0d0*dlog(model_convolved(central_px))
        
        else
         write(6,*) trim(errmsg)//trim(routinemsg)//
     &   'Non-valid absorption type: ', trim(abstype)
         stop 1
        end if

!       impose a lower limit to avoid underlflow operations elsewhere
        
        if (model_at_px.lt.tiny(real_single))
     &  model_at_px = dble(tiny(real_single))

!       ------------------------------------------------------------------------
        return

        end function model_at_px
!       ========================================================================

!       ========================================================================
        subroutine model_local(par_val,i,px_min,px_max)

!       computes value of flux in absorption profile bound by [px_min,px_max]
!       given the velocity centre, column density and b-value of a single
!       component 

        use set_precision
        use ifit_variables
        use constants

        implicit none

        integer(kind=singleI) :: px_min, px_max
        integer(kind=singleI) :: i, j

        integer(kind=singleI) :: comp_id

        real(kind=doubleR) coldens, bvalue, doppwidth, x, damping
        real(kind=doubleR) :: tau_central, tau
        real(kind=doubleR) :: optical_depth ! function

        real(kind=doubleR) :: par_val(*)

        real(kind=doubleR) :: vel_centre

!       initialise
        tau = 0.0d0

!       component ID
        comp_id = component_id(i)

!       component velocity centre
	vel_centre = par_val(i)

!       column density
        coldens = par_val(i+1)

!       Doppler parameter
        bvalue = par_val(i+2)

!       Doppler width (in Angstroem)
        doppwidth =
     &  (IONS(true_ion(comp_id))%transition(true_transition(comp_id))%lambda_0 *
     &  bvalue) / c_kms

!       damping
        damping =
     &  (IONS(true_ion(comp_id))%transition(true_transition(comp_id))%gamma_over_nu /
     &  doppwidth) *
     &  IONS(true_ion(comp_id))%transition(true_transition(comp_id))%lambda_0 

!       central optical depth
        tau_central =
     &  IONS(true_ion(comp_id))%transition(true_transition(comp_id))%tau_0 *
     &  (coldens/bvalue)

!       NOTE:
!       'gamma_over_nu' is the natural line width ('big_gamma') divided by
!       4 * pi * frequency [nu] and is calculated in routine `read_spectrum';
!       'damping' is then the damping parameter, usually denoted 'a' as in
!       H(a,x), where H(a,x) is the Voigt-Hjerting function, and calculated
!       as big_gamma / 4 pi Delta_nu, where Delta_nu = nu (b/c)

!       initialise flux
        modelflux_local_aux(1:spectrum_size_px) = 1.0d0

        do j=px_min,px_max

!       distance to component centre (in v/c space) in doppler units

         x = (dexp((velocity(j)-vel_centre)/c_kms)-1.0d0) * (c_kms / bvalue)


!       compute flux at point j
         
         tau = optical_depth(profile_str,tau_central,damping,x)

         modelflux_local_aux(j) = modelflux_local_aux(j) * dexp(-1.0d0*tau)
         
        end do !j-loop; over absorption profile
         
!       ------------------------------------------------------------------------
        return

        end subroutine model_local
!       ========================================================================

!       ========================================================================
        function optical_depth(profile_type,tau_c,damp,x)

!       computes the optical depth at pixel x (in Doppler parameter units with
!       respect to line centre) assuming a given line profile


        use set_precision

        implicit none

        real(kind=doubleR) :: optical_depth
        real(kind=doubleR) :: x, damp, tau_c
        character(len=*) :: profile_type

        real(kind=doubleR) :: phi
        real(kind=doubleR) :: voigt_hjerting, gauss              ! functions

!       initialise
        optical_depth = 0.0d0
        phi=0.0d0

!       profile function
        if (profile_type.eq.'gauss') then ! Gaussian profile

         phi = gauss(x)

        else if (profile_type.eq.'voigt') then ! Voigt-Hjerting profile

         phi = voigt_hjerting(damp,x)
         
        end if

!       check for numerical precision to avoid code crash;
!       log(tiny(real_double)) ~ 700, which corresponds to a HI column
!       density of log NHI >= 16 for b >= 10

!       avoid underflow of exp(-optical_depth)
        if ((dlog(tau_c)+dlog(phi)).gt.
     &  dlog(-1.d0*dlog(dble(tiny(real_single))))) then
         optical_depth = -1.d0*dlog(dble(tiny(real_single)))

!       avoid underflow of optical_depth
        else if ((dlog(tau_c)+dlog(phi)).lt.dlog(dble(tiny(real_single)))) then
         optical_depth = dble(tiny(real_single))

        else
         optical_depth = tau_c * phi
        
        end if

!       ------------------------------------------------------------------------
        return

        end function optical_depth
!       ========================================================================

!       ========================================================================
!        function flux_functions(par_val,numpar,veloc,flag)

!       NOTE:
!       NOTE YET FINISHED AND USED

!       Absorption-component profiles can be modeled as Gauss or Voigt profiles.
!       The parameter 'profile_str' is a program's input parameter
!       (see input_parameters.f)
!
!       If N is the column density, b the Doppler parameter, vel_c the component
!       centroid and a the damping parameter, then the (normalised) model flux
!       is given by
!
!       f = exp(-phi)
!
!       with
!
!       phi = g * exp(-x*x) -> Gauss
!
!       or
!
!       phi = g * H(a,x) -> Voigt
!
!       g = g_0 * (N/b) ; g_0 = (sqrt(pi) e^2 / m c) * f_osc * lambda_0
!
!       x = (c/b)^2 * [exp(dv/c) - 1 ]^2
!
!       dv = v - vel_c
!
!       Note that exp(dv/c) - 1 = (v - vel_c)/c and thus x = [(v - vel_c)/b]^2
!       for small dv
!
!       The derivatives of f with respect to vel_c, N, and b can be conveniently
!       expressed in terms of the functions f, phi, and h
!
!       The present function returns the value of each such function at
!       v=veloc, depending on the value of the parameters 'profile' and 'flag':
!       1 -> f
!       2 -> phi
!       3 -> x
!
!        use set_precision
!        use ifit_variables
!        use constants

!        implicit none


!       ------------------------------------------------------------------------
!        return

!        end function flux_functions
!       ========================================================================

!       ========================================================================
        function voigt_hjerting(a,x)

!       Returns the value of the Voigt-Hjerting function H(a,x) for a given
!       damping parameter a = Gamma / 4 pi Delta_nu, and reduced wavelength
!       x = (lambda - lambda_0)/ Delta_lambda, with:
!
!       Gamma =  natural line width
!       (Delta_nu / nu0) = (Delta_lambda / lambda_0) = (b / c)
!       b -> Doppler parameter (in km/s)
!       lambda_0 -> central wavelength (in Angstroem) of the corresponding
!       transition.
!
!       Algorithm is from Tepper-Garcia, Thorsten 2006
!       Monthly Notices of the Royal Astronomical Society; Volume 369, 
!       Issue 4, Page 2025
        
        use set_precision
        use constants, only: pi

        implicit none

        real(kind=doubleR) :: voigt_hjerting

        real(kind=doubleR) :: a
        real(kind=doubleR) :: x, x2, h0, Q, P
        
!       Check numerical precision
!       See ~/voigt_hjerting/code/precision (dir)

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
        
         voigt_hjerting = h0 - P*
     &   (h0*h0*(4.0d0*x2*x2 + 7.0d0*x2 + 4.0d0 + Q) - 1.0d0 - Q)

        else                         !limit for x -> 0

         voigt_hjerting = 1.0d0 - 2.0d0*a/dsqrt(pi)

        endif
        
!       ------------------------------------------------------------------------
        return

        end function voigt_hjerting
!       ========================================================================

!       ========================================================================
        function gauss(x)

!       Returns the value of the Gaussian function as a function of
!       wavelength for a given Doppler parameter (in km/s), and central
!       wavelength (in Angstroem) of the corresponding transition.
        
        use set_precision
        
        implicit none

        real(kind=doubleR) :: gauss

        real(kind=doubleR) :: x

!       avoid underflow of exp(-x*x)
        if (dabs(x).lt.(dsqrt(-1.0d0*dlog(dble(tiny(real_single)))))) then
         gauss = dexp(-1.0d0*x*x)
        else
         gauss = tiny(real_single)
        end if

!       ------------------------------------------------------------------------
        return

        end function gauss
!       ========================================================================

!       ========================================================================
        function absorption_profiles(minpix,maxpix,abs_pro_bounds,flux_aux)

        use set_precision
        use ifit_variables
        
        implicit none

        integer(kind=singleI) :: absorption_profiles

        integer(kind=singleI) :: abs_pro_bounds(max_abs_profiles,2)
        integer(kind=singleI) :: i, j, k, m
        integer(kind=singleI) :: left_bound, right_bound
        integer(kind=singleI) :: total_abs_profiles, total_abs_profiles_save
        integer(kind=singleI) :: minpix, maxpix, abs_profile_size

        integer(kind=singleI) :: lo_indx, hi_indx

        real(kind=doubleR) :: flux_average
        real(kind=doubleR) :: flux_aux(*)
        real(kind=doubleR) :: signif
        real(kind=doubleR) :: significance(minpix:maxpix)
        real(kind=doubleR) :: average_flux_min

        character(len=64) :: routinemsg = 'absorption_profiles: '
        
        logical :: abs_profile_detected

!       ------------------------------------------------------------------------
!       print routine name for information
        write(6,'(1x,a)',advance='no') trim(colour_statmsg_prefix)//
     &  trim(routinemsg)//trim(colour_msg_suffix)

!       ------------------------------------------------------------------------
!       initialise number of detected absorption profiles
        total_abs_profiles = 0

!       initialise all pixels as 'unabsorbed'
        unabsorbed_pixel(:) = 1

!       initialise detection measure given by *linear* chi^2 value
        significance(minpix:maxpix) = 0.0d0

!       ------------------------------------------------------------------------
!       compute the detection spectrum
        
        abs_profile_size = maxpix - minpix + 1
        call linear_chi2_spectrum(flux_aux,noise,abs_profile_size)
        
        significance(minpix:maxpix) = linear_chi2_spec(minpix:maxpix)
        
        total_abs_profiles = count(significance(minpix:maxpix).gt.Nsigma)

!       output detection spectrum
        if (visual.or.(debug.or.verbose)) then

         write(6,'(a,i5)') 
     &   'maximum initial number of absorption profiles:', total_abs_profiles

         open(10,file=trim(ifit_file_prefix(ION_INDEX))//'.det')
          do i=minpix,maxpix
           write(10,*) i, significance(i), Nsigma
          end do
         close(10)
        
        end if

!       skip if no absorption profiles were detected
        if (total_abs_profiles.eq.0) then
         write(6,*) 'No significant absorption detected'
         absorption_profiles = total_abs_profiles
         return
        end if

!       ------------------------------------------------------------------------
!       determine boundaries of detected absorption profiles

!       initialise boundaries (i.e. min/max pixel)
        abs_pro_bounds(:,1) = minpix
        abs_pro_bounds(:,2) = maxpix

!       initialise flag and number of detected absorption profiles
        abs_profile_detected = .false.
        total_abs_profiles = 0

        do i=minpix,maxpix
         if (.not.abs_profile_detected) then
          if (significance(i).gt.Nsigma) then
           abs_profile_detected = .true.
           total_abs_profiles = total_abs_profiles + 1
           abs_pro_bounds(total_abs_profiles,1) = i
          end if
         else
          if (significance(i).lt.Nsigma) then
           abs_profile_detected = .false.
           abs_pro_bounds(total_abs_profiles,2) = i
          end if
         end if
        end do !i-loop

!       ------------------------------------------------------------------------
!       output info

        if (.not.quite) then
         
         write(6,'(a,i5)') 
     &   'initial number of absorption profiles:', total_abs_profiles
        
         if (debug) then
          write(6,*)
          do k=1,total_abs_profiles
           write(6,*) k, abs_pro_bounds(k,:)
          end do
          write(6,*)
         end if

        end if

!       ------------------------------------------------------------------------
!       discard any absorption profile with size below resolution limit;
!       the reason is two-fold: 1) to have a robust estimate of the minimum
!       equivalent width detectable; 2) the degrees of freedom for chi^2
!       minimisation is given by absorption profile size (pixel) - number of
!       component parameters. The last number is equal to 3xcomponents and thus
!       implies that a absorption profile should be at least of size 4 pixel,
!       even better if it is at least larger  

         if (verbose)
     &    write(6,'(a,i3)')
     &    'minimum acceptable absorption profile size (in pixel):',
     &    min_absprofile_size_px

         do i=1,total_abs_profiles
          
          if (abs(abs_pro_bounds(i,2)-abs_pro_bounds(i,1)+1).lt.
     &    (min_absprofile_size_px)) then
           
           if (verbose)
     &      write(6,'(a,i3,2f12.3,i2)') 
     &      'Discard narrow absorption profile (ID | vel range | width):',
     &      i, velocity(abs_pro_bounds(i,1)), velocity(abs_pro_bounds(i,2)),
     &      abs_pro_bounds(i,2)-abs_pro_bounds(i,1)+1
           
           significance(abs_pro_bounds(i,1):abs_pro_bounds(i,2)) = -1.0d2
          
          end if

         end do ! over absorption profiles

!       ------------------------------------------------------------------------
!      determine number of valid absorption profiles

        total_abs_profiles_save = total_abs_profiles

!       initialise counter
        m = 1

        do while (m.le.total_abs_profiles)

!       discarded absorption profile
         if
     &   (all(significance(abs_pro_bounds(m,1):abs_pro_bounds(m,2)).lt.0.0d0))
     &   then

          total_abs_profiles = total_abs_profiles-1
        
!       overwrite absorption profile
          abs_pro_bounds(m:total_abs_profiles_save,:) =
     &    eoshift(abs_pro_bounds(m:total_abs_profiles_save,:),
     &    shift=1,boundary=0,dim=1)

         else ! valid absorption profile

!       increase counter
          m = m+1

         end if ! valid absorption profile

        end do !do-while
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       output info

        if (.not.quite) then
         
         write(6,'(a,i5)')
     &   'number of significant absorption profiles:', total_abs_profiles
        
         if (debug) then
          write(6,*)
          do k=1,total_abs_profiles
           write(6,*) k, abs_pro_bounds(k,:)
          end do
          write(6,*)
         end if

        end if

!       ------------------------------------------------------------------------
!       NOTE:
!       mark unabsorbed pixels
!       mark invalid pixels (those originally with negative error values, set
!       to 1.0d0 in `read_spectrum')
         
        where(significance(1:spectrum_size_px).ge.Nsigma) unabsorbed_pixel = 0
        where(sigma.eq.1.0d0) unabsorbed_pixel = 0
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       expand absorption profiles to increase number of pixels to be taken into account
!       when minimising chi^2, which imposes tighter constraint on fit

!       restore detection spectrum

        significance(minpix:maxpix) = linear_chi2_spec(minpix:maxpix)
        
!       ------------------------------------------------------------------------
!       method: expand absorption profile size to pixels with detection just
!       above the continuum (significance > 0)

!       ------------------------------------------------------------------------

         if (verbose) then
          write(6,*)
          write(6,'(1x,a)')
     &    'ABSORPTION PROFILES WILL BE EXPANDED'
          write(6,'(1x,a)')
     &    'OVELAPPING ABSORPTION PROFILES WILL BE COMBINED'
          write(6,*)
         end if
        
         do i=1,total_abs_profiles

!       left boundary          
          abs_profile_detected = .true.
          left_bound = abs_pro_bounds(i,1)
          do k=left_bound,minpix,-1
           if ((abs_profile_detected).and.
     &     (significance(k).gt.0.0d0)) then
            abs_pro_bounds(i,1) = max(minpix,k)
            else
            abs_profile_detected = .false.
           end if
          end do

!       right boundary          
          abs_profile_detected = .true.
          right_bound = abs_pro_bounds(i,2)
          do k=right_bound,maxpix
           if ((abs_profile_detected).and.
     &     (significance(k).gt.0.0d0)) then
            abs_pro_bounds(i,2) = min(k,maxpix)
           else
            abs_profile_detected = .false.
           end if
          end do
        
         end do ! over absorption profiles
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       check for minimum average flux in each absorption profile; this will
!       affect saturated regions in particular, expanding the fitting range thus
!       increasing the number of anchor points for chi^2 minimisation; usually
!       this aids in finding a good fit, quickly

!       IMPORTANT: the minimum accepted average flux (1/e) is somewhat arbitrary
!       improve this by justifying this value or include as user defined (input)
!       parameter

         average_flux_min = 1.0d0/dexp(1.0d0)

         do k=1,total_abs_profiles

!        average_noise =
!     &   dsqrt(
!     &   sum((flux(minpix:maxpix) - flux_aux(minpix:maxpix))**2.0d0)/
!     &   dble(maxpix-minpix + 1))

          flux_average =
     &    sum(flux(abs_pro_bounds(k,1):abs_pro_bounds(k,2))) / 
     &    (abs_pro_bounds(k,2) - abs_pro_bounds(k,1) + 1 )

!         do while (flux_average.lt.(Nsigma*average_noise))
          do while ((flux_average.lt.average_flux_min).and.
     &              ((abs_pro_bounds(k,1).gt.minpix).and.
     &               (abs_pro_bounds(k,2).lt.maxpix))
     &             )

!       expand absorption profile symmetrically to increase average flux

           abs_pro_bounds(k,1)=max(minpix,abs_pro_bounds(k,1)-1)
           abs_pro_bounds(k,2)=min(maxpix,abs_pro_bounds(k,2)+1)

           flux_average =
     &     sum(flux(abs_pro_bounds(k,1):abs_pro_bounds(k,2))) / 
     &     (abs_pro_bounds(k,2) - abs_pro_bounds(k,1) + 1 )

          end do !while

         end do ! over absorption profiles

!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       output info

         if (verbose) then
         
          write(6,'(a,i5)')
     &    'properties of expanded absorption profiles:',
     &    total_abs_profiles
        
          if (debug) then
           write(6,*)
           do k=1,total_abs_profiles
            write(6,*) k, abs_pro_bounds(k,:)
           end do
           write(6,*)
          end if

         end if

!       ------------------------------------------------------------------------
!       since some absorption profiles can overlap find them and combine them
!       while book-keeping properly

!       store original number of absorption profiles before combining them
        
         total_abs_profiles_save = total_abs_profiles

!       Initialise counter
         m = 1

         do while (m.lt.total_abs_profiles)

!       overlap >= 1/2 min_absprofile_size_px
          if (abs_pro_bounds(m,2)+int(0.5*min_absprofile_size_px).ge.
     &    abs_pro_bounds(m+1,1)) then

!       Re-define absorption profile's upper boundary
           abs_pro_bounds(m,2) = abs_pro_bounds(m+1,2)
  
           do k=m+1,total_abs_profiles-1
            abs_pro_bounds(k,1) = abs_pro_bounds(k+1,1)
            abs_pro_bounds(k,2) = abs_pro_bounds(k+1,2)
           end do
         
           total_abs_profiles = total_abs_profiles-1
        
          else

!       Increase counter
           m = m+1

          end if

         end do !do-while

!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       output info

        write(6,'(a,i5)')
     &  'final number of (expanded/combined) absorption profiles:',
     &  total_abs_profiles
        write(6,*)
        
        if (debug) then
         write(6,*)
         write(6,'(a)') trim(output_dashed_line)
         write(6,'(6x,a)') 'ABS.PROFILE ID          PIXEL RANGE'
         do k=1,total_abs_profiles
           write(6,*) k, abs_pro_bounds(k,:)
         end do
         write(6,*)
        end if

!       ------------------------------------------------------------------------
!       now write the absorption profiles for record keeping and further
!       processing
        open(unit=3,file=trim(ifit_file_prefix(ION_INDEX))//'.abs')

        do i=1,total_abs_profiles

         signif=0.0d0
         do j=abs_pro_bounds(i,1),abs_pro_bounds(i,2)
          if (signif.lt.significance(j)) signif=significance(j)
         end do

!       Output info for detected absorption profiles (this was done also before);
!       check whether spectrum has been circularly shifted and adjust
!       absorption profile boundaries accordingly
 
         if (.not.spectrum_shifted) then
        
          write(3,*) i,abs_pro_bounds(i,1),abs_pro_bounds(i,2), 
     &    wlength(abs_pro_bounds(i,1)), wlength(abs_pro_bounds(i,2)),
     &    velocity(abs_pro_bounds(i,1)),velocity(abs_pro_bounds(i,2)), signif

         else
!       If spectrum has been shifted, then shift output values (not the array
!       itself, since the actual bounds are needed for the fit!) to original
!       position (see read_spectrum/shift_spectrum)

!       Define lower and upper index bounds
          lo_indx = mod(abs_pro_bounds(i,1)+spectrum_shift_px,spectrum_size_px)
          hi_indx = mod(abs_pro_bounds(i,2)+spectrum_shift_px,spectrum_size_px)

!       If any of the above happens to be zero (an infortunate consequence
!       of the use of mod()), re-define:
          if (lo_indx.eq.0) lo_indx = spectrum_size_px
          if (hi_indx.eq.0) hi_indx = spectrum_size_px
        
!       Check that absorption profiles are not splitted across the spectrum
!       boundary; if so, define two new absorption profiles, and SHOULD increase
!       number of identified absorption profiles
         
          if (lo_indx.gt.hi_indx) then

           write(3,*) i, lo_indx, spectrum_size_px,
     &     wlength(lo_indx), wlength(spectrum_size_px),
     &     velocity(lo_indx), velocity(spectrum_size_px),
     &     signif
           write(3,*) i+1, 1, hi_indx,
     &     wlength(1), wlength(hi_indx),
     &     velocity(1), velocity(hi_indx),
     &     signif

          else

           write(3,*) i, lo_indx, hi_indx,
     &     wlength(lo_indx), wlength(hi_indx),
     &     velocity(lo_indx), velocity(hi_indx),
     &     signif
         
          end if
         
         end if !spectrum shifted?

        end do !i-loop over total absorption profiles
      
        close(3)
!       ------------------------------------------------------------------------

!       Assign function value to return
        absorption_profiles = total_abs_profiles

!       ------------------------------------------------------------------------
        return

        end function absorption_profiles
!       ========================================================================

!       ========================================================================
        function split_abs_profile(absprofile_id,new_abs_profiles)

!       if absorption profile inter has too many components, split at a location
!       somewhere
!       in the middle where the (normalised) flux is highest, and above
!       an given value; this is done to avoid splitting saturated absorption
!       absorption profiles

!       truncates the absorption profile absprofile_id, and adds a new absorption
!       profile to end of list.

        use set_precision
        use ifit_variables
        
        implicit none

        integer(kind=singleI), intent(inout) :: new_abs_profiles
        integer(kind=singleI) :: dof

        integer(kind=singleI) :: split_px(1)
        integer(kind=singleI) :: px_min, px_max
        integer(kind=singleI) :: absprofile_id
        
        integer(kind=singleI), allocatable :: abs_profile_aux(:,:),
     &  abs_profile_tag_aux(:)

!       maximum flux in absorption profile
        real(kind=doubleR), allocatable :: flux_sm(:)
        
        logical :: split_abs_profile

        character(len=64) :: routinemsg = 'split: '

!       ------------------------------------------------------------------------
!       initialise
         split_abs_profile = .false.

!       allocate auxiliary arrays
         if (allocated(abs_profile_aux)) deallocate(abs_profile_aux)
         allocate(abs_profile_aux(max_abs_profiles,2))
         if (allocated(abs_profile_tag_aux)) deallocate(abs_profile_tag_aux)
         allocate(abs_profile_tag_aux(max_abs_profiles))

!       define absorption profile boundaries
         px_min = abs_profile_bounds(absprofile_id,1)
         px_max = abs_profile_bounds(absprofile_id,2)

!       output info
         if (.not.quite) then
          write(6,*)
          write(6,'(a)') trim(output_dashed_line)
          write(6,'(1x,a,i5)') trim(colour_statmsg_prefix)//
     &    trim(routinemsg)//trim(colour_msg_suffix)//
     &    'trying to split absorption profile:', absprofile_id
         end if

!       split at pixel with maximum flux in absorption profile; but only
!       if the flux is close to the continuum (within the spectral S/N)
!       to avoid splitting saturated absorption profiles or complex, strong absorption
!       features 

!       smooth flux prior to splitting

         if (allocated(flux_sm)) deallocate(flux_sm)
         allocate(flux_sm(px_min:px_max))
         call smooth_sg(flux_sm(px_min:px_max),flux(px_min:px_max),
     &   px_min,px_max,4,0,2*resol_elem_px,1.0d0,.false.)

         split_px = 0

!       determine relative splitting pixel at maximum flux below the detection
!       threshold

         split_px = 
     &   maxloc(flux_sm(px_min:px_max),
     &   flux_sm(px_min:px_max).gt.dexp(-1.0d0*Nsigma*average_noise_global))

!       absolute splitting pixel
         split_px(1) = split_px(1) + px_min - 1

!       no splitting at or outside absorption profile boundaries
         if (.not.quite) then
          if ((split_px(1).le.px_min).or.(split_px(1).ge.px_max)) then
          write(6,'(1x,a,i10)')
     &    'no suitable splitting pixel:', split_px(1)
          return
          end if
         end if
         
!       no splitting if either new sub-absorption profile too small
         if (.not.quite) then
          if (((split_px(1) - px_min).le.(2*min_absprofile_size_px)).or.
     &    ((px_max - split_px(1)).le.(2*min_absprofile_size_px))) then
           write(6,'(a,i10,f10.2,f8.5)')
     &     'too small new sub-absorption profile; will not split at:',
     &     split_px(1), velocity(split_px(1)), flux_sm(split_px(1))
           return
          end if
         end if
         
!       no splitting if number of degrees of freedom (dof) in any absorption
!       profile is <= 0; 
!       dof is given by absorption profile size (in pixel) - number of
!       parameters (3x components) 

!       lower absorption profile
         dof = (split_px(1) - px_min)
         if (.not.quite) then
          if (dof.le.min_dof) then
          write(6,'(1x,a)')
     &    'vanishing number of degrees of freedom in new absorption profile!'
          return
          end if
         end if

!       upper absorption profile
         dof = (px_max - split_px(1))
         if (.not.quite) then
          if (dof.le.min_dof) then
           write(6,'(1x,a)')
     &     'vanishing number of degrees of freedom in sub-absorption profile!'
           return
          end if
         end if

!       otherwise increase number of absorption profiles
         new_abs_profiles = new_abs_profiles + 1

!       count number of splitted absorption profiles
         splitted_abs_profiles = splitted_abs_profiles + 1

!       update flag
         split_abs_profile = .true.

!       ------------------------------------------------------------------------
!       do if new number of absorption profiles larger than allocated number,
!       de/re-allocate
         if (new_abs_profiles.gt.max_abs_profiles) then

!       store original arrays
          abs_profile_aux(:,:) = abs_profile_bounds(:,:)
          abs_profile_tag_aux(:) = abs_profile_tag(:)

!       re-allocate arrays
          if (allocated(abs_profile_bounds)) deallocate(abs_profile_bounds)
          allocate(abs_profile_bounds(new_abs_profiles,2))
          if (allocated(abs_profile_tag)) deallocate(abs_profile_tag)
          allocate(abs_profile_tag(new_abs_profiles))

!       store new absorption profile and corresponding flag
          abs_profile_bounds(1:max_abs_profiles,:) =
     &    abs_profile_aux(1:max_abs_profiles,:)
          abs_profile_bounds(new_abs_profiles,1) = split_px(1)+1
          abs_profile_bounds(new_abs_profiles,2) =
     &    abs_profile_aux(absprofile_id,2)
          abs_profile_bounds(absprofile_id,2) = split_px(1)

          abs_profile_tag(1:max_abs_profiles) =
     &    abs_profile_tag_aux(1:max_abs_profiles)
          abs_profile_tag(new_abs_profiles) = 1

!       redefine maximum number of absorption profiles
          max_abs_profiles = new_abs_profiles

         else
         
          abs_profile_bounds(new_abs_profiles,1) = split_px(1)+1
          abs_profile_bounds(new_abs_profiles,2) =
     &    abs_profile_bounds(absprofile_id,2)
          abs_profile_bounds(absprofile_id,2) = split_px(1)
          abs_profile_tag(new_abs_profiles) = 1
        
         end if !new_abs_profiles > max_abs_profiles
        
!       info output

         if (.not.quite) then
          write(6,'(1x,a,2i5)')
     &    'increasing total number of absorption profiles (from | to):'//
     &    trim(colour_msg_suffix), new_abs_profiles-1, new_abs_profiles
          write(6,*)
         end if

!       update number of absorption profiless
!        num_abs_profiles = new_abs_profiles
         
         if (.not.quite) then

          write(6,'(1x,a)')
     &    'ABS.PROFILE    PIXEL       VEL [km/s]    COMPONENTS    '//
     &    'SPLIT (pixel | vel | flux)'
          write(6,'(1x,3(i6),f8.2,2x,f8.2,2x,i6,2(f8.2))') absprofile_id,
     &    abs_profile_bounds(absprofile_id,1),
     &    abs_profile_bounds(new_abs_profiles,2),
     &    velocity(abs_profile_bounds(absprofile_id,1)),
     &    velocity(abs_profile_bounds(new_abs_profiles,2)),
     &    split_px(1), velocity(split_px(1)), flux_sm(split_px(1))

          write(6,'(1x,3(i6),f8.2,2x,f8.2)') absprofile_id,
     &    abs_profile_bounds(absprofile_id,1),
     &    abs_profile_bounds(absprofile_id,2),
     &    velocity(abs_profile_bounds(absprofile_id,1)),
     &    velocity(abs_profile_bounds(absprofile_id,2))

          write(6,'(1x,3(i6),f8.2,2x,f8.2)') new_abs_profiles,
     &    abs_profile_bounds(new_abs_profiles,1),
     &    abs_profile_bounds(new_abs_profiles,2),
     &    velocity(abs_profile_bounds(new_abs_profiles,1)),
     &    velocity(abs_profile_bounds(new_abs_profiles,2))
         
         end if

!       ------------------------------------------------------------------------
        return

        end function split_abs_profile
!       ========================================================================

!       ========================================================================
        function chi2_function(x,y,sigfit,px_min,px_max,par_val,np)

!       NOTE:
!       computes the chi^2 function; consistent with the computation in mrqcof

        use set_precision
        use ifit_variables, only: absorption_type, chi2_terms

        implicit none

        real(kind=doubleR) :: chi2_function

        integer(kind=singleI) :: px_min, px_max, np, i
        real(kind=doubleR) :: par_val(*)
        real(kind=doubleR) :: x(*), y(*), sigfit(*)
        real(kind=doubleR) :: model_at_px, dy, sig_inv2

        chi2_function = 0.0d0
        chi2_terms(:) = 0.0d0

        do i=px_min,px_max
          dy = y(i) - model_at_px(par_val,np,x(i),absorption_type)
          sig_inv2 = 1.0d0/sigfit(i)/sigfit(i)
          chi2_terms(i) = dy*dy*sig_inv2
          chi2_function = chi2_function + dy*dy*sig_inv2
        end do

!       ------------------------------------------------------------------------
        return

        end function chi2_function
!       ========================================================================

!       ========================================================================
        function chi2_reduced_min(y,yy,sigfit,px_min,px_max,dof)

!       NOTE:
!       computes the *reduced* chi^2 value comparing the smoothed flux (y) to
!       the raw (input) flux (yy)

        use set_precision
        use ifit_variables, only: colour_warnmsg_prefix, colour_msg_suffix,
     +  quite, average_sn_local

        implicit none

        real(kind=doubleR) :: chi2_reduced_min

        integer(kind=singleI) :: px_min, px_max, i, dof
        real(kind=doubleR) :: y(*), yy(*), sigfit(*)
        real(kind=doubleR) :: dy, sig_inv2

        chi2_reduced_min = 0.0d0

!       get rid of glitches produced by smoothing;
!       note that these are edited such that they contribute each with 1 unit
!       to the chi^2-value

        where(abs(y(px_min:px_max)-yy(px_min:px_max)).gt.sigfit(px_min:px_max))
     &   y(px_min:px_max) = yy(px_min:px_max) + sigfit(px_min:px_max)

        do i=px_min,px_max
          dy = y(i) - yy(i)
          sig_inv2 = 1.0d0/sigfit(i)/sigfit(i)
          chi2_reduced_min = chi2_reduced_min + dy*dy*sig_inv2
        end do

        chi2_reduced_min = (chi2_reduced_min/dble(dof))

!       check that minimum chi2-value is reasonable;
!       too large a value may signal tha smoothed flux is off by a significant
!       amount at some pixel(s), which usually happens in saturated absorption
!       profiless 

        if (chi2_reduced_min.gt.1.0d0) then
         
         if (.not.quite) then
          write(6,'(1x,a,f6.2,i5)') trim(colour_warnmsg_prefix)//
     &    'chi2_reduced_min:'//trim(colour_msg_suffix)//
     &    ' too large minimum reduced chi2-value (dof):', 
     &    chi2_reduced_min, dof
          write(6,'(20x,a)') 
     &    'will be set to 1.0d0 (conservative)'
          write(6,*)
         end if

!       use a conservative approach
          chi2_reduced_min = min(1.0d0,(average_sn_local / dble(dof)))

        end if

!       ------------------------------------------------------------------------
        return
        end function chi2_reduced_min
!       ========================================================================

!       ========================================================================
        subroutine linear_chi2_spectrum(y,err,nd)

!       NOTE:
!       computes the chi^2 spectrum, i.e., chi^2 value at each pixel with
!       respect to an ideal normalised flux (f(v) = 1)
        
        use set_precision
        use ifit_variables, only: linear_chi2_spec

        implicit none

        integer(kind=singleI) :: i, nd
        real(kind=doubleR) :: y(*), err(*)

        linear_chi2_spec(:) = 0.0d0

        do i=1,nd
          linear_chi2_spec(i) = (1.0d0 - y(i))/err(i)
        end do

!       ------------------------------------------------------------------------
        return
        end subroutine linear_chi2_spectrum
!       ========================================================================

!       ========================================================================
        subroutine set_chi2_range()

!       computes various chi2-values:
!
!       1) absolut minimum, defined as the chi^2-value resulting from the 
!         differencebetween actual and smoothed fluxes, constraining this to
!         the amplitude of the local noise, such such that
!         (chi2_min_abs_local <= 1) everywhere (see function chi2_reduced_min)
!
!       2) optimum (chi2good_reduced), corresponding to the lower input P-value,
!         conf_hi 
!
!       2) tolerable (chi2bad_reduced), corresponding to the higher input
!         P-value, conf_lo, in addition to the local absolut minimum value
!
!       RECALL: A *lower* P-value (cumulative chi^2-probability) for a given
!       (reduced) chi^2-value corresponds to a *higher* Q-value (= 1 - P), which
!       implies that it is more likely that the given chi^2-value be exceeded by
!       chance; in other words, the `observed' (low) chi^2-value is most likely
!       indicating a good fit 

        use set_precision
        use ifit_variables

        implicit none

        real(kind=doubleR) :: chi2_function !function
        real(kind=doubleR) :: chi2_reduced_min !function
        real(kind=doubleR) :: reduced_chi2 !function

!       ------------------------------------------------------------------------
!       absolut minimum chi^2 value across absorption profile

        chi2_min_abs_local =
     &  chi2_reduced_min(flux_smooth,flux,noise,pixel_min,pixel_max,
     &  degrees_of_freedom)

!       optimum and maximum tolerable reduced chi^2-value, determined by the
!       input low and high P-values, conf_hi and conf_lo, respectively 

        chi2good_reduced =
     &  reduced_chi2(degrees_of_freedom,conf_hi)

        chi2bad_reduced =
     &  reduced_chi2(degrees_of_freedom,conf_lo) + chi2_min_abs_local

!       compute starting chi^2 value in absorption profile

        chi2_current =
     &  chi2_function(xdata_in,ydata_in,yerror_in,1,absprofile_size_px,
     &  trial_param,num_param)

!       store initial chi^2-value
        chi2_start = chi2_current

!       store best reduced chi^2 value so far
        best_reduced_chi2 = min(chi2_current/dble(degrees_of_freedom),
     &  chi2_discard_save/dble(degrees_of_freedom_save),
     &  best_chi2_add/dble(degrees_of_freedom_save),
     &  chi2_previous/dble(degrees_of_freedom_save))

!       output info
        if (.not.quite) then

         write(6,'(a,15x,1pe13.4,i5)')
     &   ' minimum (reduced chi^2 | dof): ',
     &   chi2_min_abs_local, degrees_of_freedom

         write(6,'(a,13x,2f6.3)')
     &   ' optimum | tolerable reduced chi^2: ',
     &   chi2good_reduced, chi2bad_reduced

         write(6,'(a,16x,1pe13.4,i5)')
     &   ' initial reduced chi^2 | dof: ',
     &   (chi2_current/dble(degrees_of_freedom)), degrees_of_freedom
        
        end if
        
!       compute number of outliers, i.e. pixels with highest/lowest chi^2 values
!       prints info to screen

        call chi2_outliers(absprofile_size_px)

!       output info
        if (verbose.or.debug) then
         write(6,'(a)') trim(output_dashed_line)
         write(6,'(1x,a)') trim(output_mrqmin_iteration_1_debug)
         write(6,'(1x,a)') trim(output_mrqmin_iteration_2_debug)
        else if (.not.quite) then
         write(6,'(a)') trim(output_dashed_line)
         write(6,'(1x,a)') trim(output_mrqmin_iteration_1_short)
         write(6,'(1x,a)') trim(output_mrqmin_iteration_2_short)
        end if

!       ------------------------------------------------------------------------
        return

        end subroutine set_chi2_range
!       ========================================================================

!       ========================================================================
!       subroutine template(x,nd,px_size)
        
!       use set_precision

!       implicit none


!       ------------------------------------------------------------------------
!       return

!       end subroutine template
!       ========================================================================

!       ========================================================================
!       function template(x,nd,px_size)
        
!       use set_precision

!       implicit none


!       ------------------------------------------------------------------------
!       return

!       end function template
!       ========================================================================

!       ========================================================================
        subroutine parameter_boundaries(par_val,np)

!       keeps the parameter values within the physically meaningful boundaries

        use set_precision
        use ifit_variables

        implicit none

        real(kind=doubleR) :: par_val(*)
        integer(kind=singleI) :: i,np

        do i=1,np,3

!       recall that parameters with *current* indices i,i+1,i+2 are linked to
!       component with *current* index i which has *absolute* ID index
!       component_id(i); and true_ion(j) gives the ion to which component j
!       corresponds to

         if (par_val(i).lt.velocity_min) then
          par_val(i) = velocity_min
         end if

         if (par_val(i).gt.velocity_max) then
          par_val(i) = velocity_max
         end if

         if (par_val(i+1).lt.col_dens_min(true_ion(component_id(i)))) then
          par_val(i+1) = col_dens_min(true_ion(component_id(i)))
         end if 
         
         if ((par_val(i+1)).gt.col_dens_max(true_ion(component_id(i)))) then
          par_val(i+1) = col_dens_max(true_ion(component_id(i)))
         end if 

         if (par_val(i+2).lt.b_value_min(true_ion(component_id(i)))) then
          par_val(i+2) = b_value_min(true_ion(component_id(i)))
         end if

!      limit component width to absorption profiles size
!      IMPORTANT: place a more meaningful limit on b, in particular for
!      saturated components

         if (par_val(i+2).gt.min((velocity_max-velocity_min),
     &   b_value_max(true_ion(component_id(i)))))
     &   then
          par_val(i+2) = min((velocity_max-velocity_min),
     &    b_value_max(true_ion(component_id(i))))
         end if

        end do

!       ------------------------------------------------------------------------
        return
        end subroutine parameter_boundaries
!       ========================================================================

!       ========================================================================
        subroutine fit_goodness(cases,dof,cycl)

!       computes values of parameters that characterise the goodness of a fit
!       and which determine whether a better solution should be looked for
!       see also function stop_iteration()


        use set_precision
        use ifit_variables, only: strong_convergence_criterium,
     +  convergence_criterium, verbose, debug, quite,
     +  IONS, num_abs_profiles, output_dashed_line, processed_abs_profiles, 
     +  velocity_min, velocity_max, sweep,
     +  success_mrqmin_iteration, chi2_current_succ,
     +  chi2_current, best_reduced_chi2_succ, best_reduced_chi2, chi2_previous,
     +  chi2_decrease_rate_diff, array_aux, chi2_decrease_rate_average_array,
     +  chi2_decrease_rate_average, chi2_decrease_iteration,
     +  log_lambda_mrqmin_oscillation, lambda_mrqmin,
     +  log_lambda_mrqmin_oscillation_diff, mrqmin_iteration,
     +  fit_attempt, fitted_components, num_param,
     +  output_fit_results_debug, output_fit_results_short,
     +  abs_profile_id, component_id, trial_param, trial_param_err,
     +  ION_INDEX, num_ions

        implicit none
        
        integer(kind=singleI), intent(in) :: cases, dof, cycl

        integer(kind=singleI) :: ionindex, num_comps

!       ------------------------------------------------------------------------
        select case(cases)

!       ------------------------------------------------------------------------
!       output during mrqmin iteration        
        case(1)

!       output fit parameter (format)
 100    format(a,i5,i5,2(1pe12.3),2(1pe12.3),2(2x,i6))
 101    format(a,i5,6x,i5,1x,2(1pe14.4),2(1pe14.4))
 102    format(a,i5,6x,i5,1x,2(1pe14.4),1pe14.4,i10,6x)

!       initialise values for first call
        if (success_mrqmin_iteration.eq.0) then
          chi2_current_succ = chi2_current
          best_reduced_chi2_succ = best_reduced_chi2
         end if

!       store values of a successful iteration (i.e. if chi^2 decreases)
        if (chi2_current.lt.chi2_previous) then

         success_mrqmin_iteration = success_mrqmin_iteration + 1
         chi2_current_succ = chi2_current
         best_reduced_chi2_succ = best_reduced_chi2
        
!       compute differential, relative decrease in chi^2 value
         chi2_decrease_rate_diff =
     &   (chi2_previous - chi2_current)/chi2_previous

!       compute average relative decrease in chi^2 value (over N iterations)
!       USAGE of eoshift(array, shift, boundary, dim)
!       if shift>0, shift is done to the left; otherwise right
!       values shift out are replaced by 'boundary' if given, otherwise zero

!       shift array by one position to the left; fill last position with new
!       value 'chi2_decrease_rate_diff'
         array_aux =
     &   eoshift(chi2_decrease_rate_average_array,1,
     &   chi2_decrease_rate_diff,1)

!       assign new value to array
         chi2_decrease_rate_average_array(:) =  array_aux(:)

!      compute average, i.e. arithmetic mean over numver of current
!      iterations, limited to 'chi2_decrease_iteration'

         chi2_decrease_rate_average =
     &   sum(chi2_decrease_rate_average_array) / 
     &   min(mrqmin_iteration,chi2_decrease_iteration)

        end if ! new chi^2 < old chi^2, i.e. a *successful* iteration

!       re-compute differential, relative decrease in chi^2 value
!       only for output purposes

        chi2_decrease_rate_diff =
     &  (chi2_previous - chi2_current)/chi2_previous

!       compute maximum oscillation amplitude (over N iterations)
!       of lambda parameter
             
        array_aux =
     &  eoshift(log_lambda_mrqmin_oscillation,1,
     &  dlog10(lambda_mrqmin),1)

        log_lambda_mrqmin_oscillation(:) =  array_aux(:)

        log_lambda_mrqmin_oscillation_diff =
     &  int(abs(maxval(log_lambda_mrqmin_oscillation) -
     &  minval(log_lambda_mrqmin_oscillation)))

        if (debug) then

         write(6,100)
     &   ' ',
     &   success_mrqmin_iteration, mrqmin_iteration,
     &   best_reduced_chi2_succ,
     &   (chi2_current_succ/dble(dof)),
     &   chi2_decrease_rate_diff,
     &   chi2_decrease_rate_average,
     &   int(dlog10(lambda_mrqmin)),
     &   int(log_lambda_mrqmin_oscillation_diff)
        
        else if (verbose) then

         write(6,100,advance='no') '\r ',
     &   success_mrqmin_iteration, mrqmin_iteration,
     &   best_reduced_chi2_succ,
     &   (chi2_current_succ/dble(dof)),
     &   chi2_decrease_rate_diff,
     &   chi2_decrease_rate_average,
     &   int(dlog10(lambda_mrqmin)),
     &   int(log_lambda_mrqmin_oscillation_diff)
        
        else if (.false.) then ! alternative output
        
         write(6,101,advance='no') '\r ',
     &   success_mrqmin_iteration, mrqmin_iteration,
     &   best_reduced_chi2_succ,
     &   (chi2_current_succ/dble(dof)),
     &   chi2_decrease_rate_diff,
     &   chi2_decrease_rate_average
        
        else if (.not.quite) then
        
         write(6,102,advance='no') '\r ',
     &   success_mrqmin_iteration, mrqmin_iteration,
     &   best_reduced_chi2_succ,
     &   (chi2_current_succ/dble(dof)),
     &   chi2_decrease_rate_average,
     &   int(dlog10(lambda_mrqmin))
        
        end if ! verbose

!       ------------------------------------------------------------------------
!       output after mrqmin iteration        
        case(2)

!       compute number of valid components
        if (cycl.le.2) then ! during sweep=1,2

         num_comps = count(abs(IONS(ION_INDEX)%component(:)%status).eq.1)

        else ! during sweep=3

         num_comps = 0
         do ionindex=1,num_ions
          num_comps = num_comps +
     &    count(abs(IONS(ionindex)%component(:)%status).eq.1)
         end do

        end if

         if (.not.quite) write(6,*)
         if (.not.quite) write(6,*)
         write(6,'(a)') trim(output_dashed_line)

!       NOTE: the format statement 0p is necessary, otherwise the output after
!       the 1p directive is shifted by one decimal place!

         if (debug) then
          write(6,'(1x,a)') trim(output_fit_results_debug)
          write(6,'(i8,5x,1pe12.4,0p,2i8,5x,2(f10.2),4x,3i10)')
     &    fit_attempt, (chi2_current/dble(dof)), abs_profile_id,
     &    num_abs_profiles-processed_abs_profiles, velocity_min, velocity_max,
     &    ceiling(dble(num_param)/3.0d0), fitted_components, num_comps

         else

!       NOTE: the format statement 0p is necessary, otherwise the output after
!       the 1p directive is shifted by one decimal place!

          write(6,'(1x,a)') trim(output_fit_results_short)
          write(6,'(i8,5x,1p,e12.4,0p,2x,2i8,3x,2f10.2,4x,i2)')
     &    fit_attempt, (chi2_current/dble(dof)), abs_profile_id,
     &    num_abs_profiles-processed_abs_profiles, velocity_min, velocity_max,
     &    sweep
         end if

         write(6,'(a)') trim(output_dashed_line)
         if (.not.quite) write(6,*)
        
        end select

!       ------------------------------------------------------------------------
!       print more info if so required

        call verbose_output(verbose,trial_param,trial_param_err,component_id)

!       ------------------------------------------------------------------------
        return

        end subroutine fit_goodness
!       ========================================================================

!       ========================================================================
        subroutine discard_component_module(goto1)

!       NOTE:
!       get rid of components which are:
!       - uncertain, i.e. with relative errors in N and / or b above a given
!       threshold
!       - weak, to keep the number of components to a minimum
!       - irrelevant, i.e. components with parameter values at the boundaries of
!       the respective range

!       but only if doing so does not increase chi^2 above chi2bad_reduced

        use set_precision
        use ifit_variables

        implicit none
        
        integer(kind=singleI) :: indx
        integer(kind=singleI) :: discard_component
        
        logical :: do_discard_component
        logical, intent(inout) :: goto1   !signal to restart fit: goto1 = .true.

        character(len=64) :: routinemsg_warn
        
!       ------------------------------------------------------------------------
!       warning message (yellow)
        routinemsg_warn=trim(colour_warnmsg_prefix)//'discard not accepted:'//
     &  trim(colour_msg_suffix)

!       ------------------------------------------------------------------------
!       skip during first iteration (per absorption profiles)
        if ((fit_attempt.eq.1).and.(fwhm_kms.eq.0.0d0).and.
     &  (chi2_current/dble(degrees_of_freedom)).gt.chi2bad_reduced) return
        
!       skip if currently a component has been added; these are handled
!       separately
        if (adding_component) return

!       skip if doing fit
        if (do_fit) return

!       skip during multi-transition fit
        if (sweep.eq.3) return

!       if a component was discarded, accept only if chi^2 value decreases
!       The following condition is true for first call while fitting a given
!       absorption profiles

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        if
     &  (do_discard_component(chi2_current,chi2_discard_save,degrees_of_freedom,
     &   degrees_of_freedom_save,num_param))
     &  then
!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!       ------------------------------------------------------------------------
!       if taking instrumental LSF into account, tag ALL components in current
!       absorption profiles as `working/discardable' after a successful discard
!       NOTE: in this unit component_discarded = goto1

        if ((component_discarded).and.(sweep.eq.2)) then

          if (.not.quite) write(6,*) trim(colour_warnmsg_prefix)//
     &    'successful discard; tagging all components as "discardable"'//
     &    trim(colour_msg_suffix)

!       set status of all components as `working/discardable'
          do indx=1,num_param,3
           IONS(true_ion(component_id(indx)))%component(component_id(indx))%status = 1
          end do

         end if

!       ------------------------------------------------------------------------
!       load all current (accepted) component-parameter values 'trial_param'
!       (and corresponding errors) into 'vel_c, col_dens, bvalue'
         call
     &   save_fit_parameters(trial_param,trial_param_err,num_param,component_id)

         discarded_component = discard_component(num_param)

         if (discarded_component.gt.0) then ! there is a discardable component

!       tag component as discarded
          IONS(true_ion(discarded_component))%component(discarded_component)%status = 0

!       increase number of discarded components (book-keeping)
          discarded_components = discarded_components + 1

!       load parameters 'vel_c, col_dens, bvalue' (and corresponding errors) of
!       *valid* components (i.e., excluding the component just discarded) 
!       present in current absorption profiles into array `trial_param';
!       determine new number of parameters `num_param'
          call
     &    load_fit_parameters(trial_param,trial_param_err,
     &    component_id,param_to_min,num_param,num_param_var)

!       store chi^2 value before discarding component
          chi2_discard_save = chi2_current
          best_chi2_add = huge(real_single)

!       output
          call verbose_output(debug,trial_param,trial_param_err,component_id)

!       ...and re-start fit:
          goto1 = .true.
          return

         else

          goto1 = .false.

         end if !discarded_component > 0

!       ------------------------------------------------------------------------

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        else ! do_discard_component = false
!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!       do_discard_component = false means that chi^2 increased with component
!       discarded; add component back to list and tag as non-discardable 

!       since discarding a component increases degrees of freedom by 3, correct
!       for this when displaying chi^2-value previous to discard

         write(6,*)
         write(6,'(a)') trim(output_dashed_line)
         write(6,'(27x,a)') 'ID  chi^2 (   current   |   previous   )'
         write(6,'(1x,a,1x,i5,6x,2(f14.3))') trim(routinemsg_warn),
     &   discarded_component, (chi2_current/dble(degrees_of_freedom)),
     &   (chi2_discard_save/dble(degrees_of_freedom_save))
         write(6,*)

!       decrease number of discarded components
         discarded_components = discarded_components - 1

!       tag previously discarded component as non-discardable
         IONS(true_ion(discarded_component))%component(discarded_component)%status = -1

!       load parameters 'vel_c, col_dens, bvalue' (and corresponding errors) of
!       *valid* components (i.e., including the component previously discarded) 
!       present in current absorption profiles into array `trial_param';
!       determine new number of parameters `num_param'

         call load_fit_parameters(trial_param,trial_param_err,
     &   component_id,param_to_min,num_param,num_param_var)

!       if the previously attempted, discarded component is the weakest, tag ALL
!       components in absorption profiles as non-discardable, since it does not
!       make sense to discard stronger components; but keep discarding when
!       taking LSF into account 

         if ((discarded_component.eq.weak_component_id).and.(sweep.eq.1))
     &   then

          if (.not.quite) write(6,*) trim(colour_warnmsg_prefix)//
     &    'will not discard further components!'//
     &    trim(colour_msg_suffix)

          do indx=1,num_param,3
           IONS(true_ion(component_id(indx)))%component(component_id(indx))%status = -1
          end do

         end if ! weakest component discarded?

!       ------------------------------------------------------------------------
!       visualisation (on-the-fly)

         call
     &   visualise(visual,trial_param,velocity_min,velocity_max,abs_profile_id,
     &   num_abs_profiles,num_param,
     &   (chi2_discard_save/dble(degrees_of_freedom_save)),
     &   'discard_component_module')

!       ------------------------------------------------------------------------

!       re-assign chi^2 value previous to discarding component
         chi2_current = chi2_discard_save

!       output
         call verbose_output(debug,trial_param,trial_param_err,component_id)

!       ------------------------------------------------------------------------
!       output chi^2 values to visualise fit behaviour for each absorption
!       profiles

         call
     &   write_chi2_evolution(3,visual,
     &   chi2_discard_save/dble(degrees_of_freedom_save))

!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       try discarding a different component

         discarded_component = discard_component(num_param)

         if (discarded_component.gt.0) then ! there is a discardable component

!       tag discarded component
          IONS(true_ion(discarded_component))%component(discarded_component)%status = 0

!       Reduce number of working components
          discarded_components = discarded_components + 1

!       load parameters 'vel_c, col_dens, bvalue' (and corresponding errors) of
!       *valid* components (i.e., ignoring the component just discarded) 
!       present in current absorption profiles into array `trial_param';
!       determine new number of parameters `num_param'

          call load_fit_parameters(trial_param,trial_param_err,
     &    component_id,param_to_min,num_param,num_param_var)

!       store chi^2 value before discarding component
          chi2_discard_save = chi2_current *
     &    (dble(degrees_of_freedom) / dble(degrees_of_freedom_save))

          best_chi2_add = huge(real_single)

!       output
          call verbose_output(debug,trial_param,trial_param_err,component_id)

!       ...and re-start fit:
          goto1 = .true.
         
         else ! discarding = 0

          write(6,'(1x,a,i5,a)') trim(colour_statmsg_prefix)//
     &    'component set in absorption profile restored'//
     &    trim(colour_msg_suffix)
         
          goto1 = .false.

         end if ! no further component to be discarded

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        end if !do_discard_component false or true
!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!       ------------------------------------------------------------------------
        return

        end subroutine discard_component_module
!       ========================================================================

!       ========================================================================
        function discard_component(np)

!       NOTE:
!       
!       identifies components that are either:
!       -at the boundaries of parameter space (N, b, vel)
!       -uncertain (relative errors in N or b larger than 50%)
!       -weak, to keep the number of components to a minimum

!       each components gets 'points' for each satisfied criterium;
!       the component with more points is tagged as 'discardable'; if two or
!       more components have the same (highest) number of points, the first one
!       on the list (see use of FORTRAN instrinsic function MAXLOC)

!       discarded component is moved to the end of the component list and the
!       number of components reduced by one; the corresponding parameter values
!       are kept in case discarding the component increases chi^2 above allowed
!       value 

        use set_precision
        use ifit_variables

        implicit none

        integer(kind=singleI) :: discard_component

        integer(kind=singleI) :: i, np

        integer(kind=singleI) :: comp_id

        integer(kind=singleI) :: discard_component_id(1)
!       size of array is total (current) number of components
        integer(kind=singleI) :: discard_component_list(num_components)
        
        real(kind=doubleR) :: weakest
!       maximum tolerated relative error in b and N
        real(kind=doubleR), parameter :: b_error_max = 0.25,
     &  N_error_max = 0.5
        real(kind=doubleR) :: b_error, N_error

        character(len=64) :: routinemsg_stat
        
        logical :: b_uncertain_component, N_uncertain_component
        logical :: unphysical_component, weak_component

!       ------------------------------------------------------------------------
!       initialise
        discard_component = 0

!       ------------------------------------------------------------------------
!       status message (green)
        routinemsg_stat=trim(colour_statmsg_prefix)//'discard_component:'//
     &  trim(colour_msg_suffix)

!       output
        if (verbose) write(6,'(a)') trim(output_dashed_line)

!       ------------------------------------------------------------------------
!       initialise logical flags

        b_uncertain_component = .false.
        N_uncertain_component = .false.
        unphysical_component = .false.
        weak_component = .false.

!       initialise function
        discard_component = 0

!       initialise number and list of discardable components
        discard_component_list(:) = 0

!       ------------------------------------------------------------------------
!       CASE 1: component has a large relative error in b and / or N

!       NOTE:
!       such components are quite often only fitting VERY
!       broad and extremely shallow components which should disappear
!       if spectrum is first continuum-normalised -> TO DO!

        do i=1,np,3

!       component ID
         comp_id = component_id(i)         

         N_error = 
     &   IONS(true_ion(comp_id))%component(comp_id)%col_dens_err/
     &   IONS(true_ion(comp_id))%component(comp_id)%col_dens

         b_error =
     &   IONS(true_ion(comp_id))%component(comp_id)%b_value_err/
     &   IONS(true_ion(comp_id))%component(comp_id)%b_value

!       exclude non-discardable components (component status=-1)

         if (IONS(true_ion(comp_id))%component(comp_id)%status.ne.-1) then
         
          if (b_error.ge.b_error_max) then

!       ------------------------
           if (verbose)
     &      write(6,'(1x,a,i2,a,i5,1x,f6.2,1x,2pe9.2,1x,1pe9.2,1x,2pe9.2)')
     &      trim(colour_warnmsg_prefix)//
     &      'component (b-error >', int(b_error_max*100),'%)'//
     &      trim(colour_msg_suffix), 
     &      comp_id,
     &      IONS(true_ion(comp_id))%component(comp_id)%b_value, b_error*100,
     &      IONS(true_ion(comp_id))%component(comp_id)%col_dens, N_error*100

!       increase number of 'discard points' for this component

           discard_component_list(comp_id) =
     &     discard_component_list(comp_id) + 1

!       update flag to record that there is at least one uncertain component
           b_uncertain_component = .true.
          
          end if ! error in b

          if (N_error.ge.N_error_max) then
         
!       ------------------------
           if (verbose)
     &      write(6,'(1x,a,i2,a,i5,1x,f6.2,1x,2pe9.2,1x,1pe9.2,1x,2pe9.2)')
     &      trim(colour_warnmsg_prefix)//
     &      'component (N-error >',int(N_error_max*100),'%)'//
     &      trim(colour_msg_suffix),
     &      comp_id,
     &      IONS(true_ion(comp_id))%component(comp_id)%col_dens, N_error*100

!       increase number of 'discard points' for this component

           discard_component_list(comp_id) =
     &     discard_component_list(comp_id) + 1

!       update flag to record that there is at least one uncertain component
           N_uncertain_component = .true.

          end if ! error in N
         
         end if ! component added or non-discardable?
        
        end do

!       ------------------------------------------------------------------------
!       CASE 2: component parameters (col_dens, bvalue) at boundary of
!       respective range

        do i=1,np,3
          
!       component ID
         comp_id = component_id(i)         

!       exclude non-discardable components (component status=-1)

         if (IONS(true_ion(comp_id))%component(comp_id)%status.ne.-1) then

           if ((IONS(true_ion(comp_id))%component(comp_id)%b_value.le.
     &     b_value_min(true_ion(comp_id))).or.
     &     (IONS(true_ion(comp_id))%component(comp_id)%b_value.ge.
     &     b_value_max(true_ion(comp_id))).or.
     &     (IONS(true_ion(comp_id))%component(comp_id)%col_dens.le.
     &     col_dens_min(true_ion(comp_id))).or.
     &     (IONS(true_ion(comp_id))%component(comp_id)%col_dens.ge.
     &     col_dens_max(true_ion(comp_id))).or.
     &     (IONS(true_ion(comp_id))%component(comp_id)%vel_c.le.
     &     velocity_min).or.
     &     (IONS(true_ion(comp_id))%component(comp_id)%vel_c.ge.
     &     velocity_max)) then

!       b_value_min is set to the b-value related to FWHM of the instrument via:
!       b_value_min = 0.5*FWHM/SQRT(ln2)
!       The idea is to avoid fitting components which are below the resolution
!       limit, and thus are un-physical

            if (verbose)
     &       write(6,'(1x,a,i5)') trim(colour_warnmsg_prefix)//
     &       'unphysical component'//
     &       trim(colour_msg_suffix), comp_id

!       increase number of 'discard points' for this component

            discard_component_list(comp_id) =
     &      discard_component_list(comp_id) + 2

!       update flag to record that there is at least one uncertain component
            unphysical_component = .true.
          
           end if ! unphysical component

          end if ! component added or non-discardable?

         end do

!       ------------------------------------------------------------------------
!       CASE 3: weakest component, where strength is measured by (N/b),
!       which is proportional to the central optical depth
!       tag it only if there is more than one component in absorption profiles

!       initialise component index and component strength to maximum value

         weakest = col_dens_max(1)
         weak_component_id = 0

!       find weakest component
         do i=1,np,3

!       component ID
         comp_id = component_id(i)         

          if ((IONS(true_ion(comp_id))%component(comp_id)%col_dens.lt.weakest)
     &    .and.(IONS(true_ion(comp_id))%component(comp_id)%status.ne.-1)) then
        
           weakest = IONS(true_ion(comp_id))%component(comp_id)%col_dens
           weak_component_id = comp_id

          end if

         end do

         if (weak_component_id.gt.0) then

!       exclude non-discardable components (component status=-1)

          if (IONS(true_ion(weak_component_id))%component(weak_component_id)%status.ne.-1) then

            if (verbose)
     &       write(6,'(1x,a,i5,1pe12.4)') trim(colour_warnmsg_prefix)//
     &       'weakest component [index | col.dens.]:'//
     &       trim(colour_msg_suffix),
     &       weak_component_id, weakest

!       increase number of 'discard points' for this component

            discard_component_list(weak_component_id) =
     &      discard_component_list(weak_component_id) + 1

!       update flag to record that there is at least one weak component
            weak_component = .true.
         
          end if ! component added or non-discardable?
         
         end if !weak_component_id > 0

!       ------------------------------------------------------------------------
!       identify component with most 'discard points' and tag

!       NOTE:
!       a possible approach: given each component a weight based on its
!       uncertainty and weakness; the component with the highest weight is
!       discarded 

!       another approach: determine which components affects less chi^2 when
!       discarded

!       discard component:

        if ((b_uncertain_component).or.(N_uncertain_component).or.
     &  (unphysical_component).or.(weak_component)) then
        
         write(6,'(a)') trim(output_dashed_line)
         write(6,'(23x,a)')
     &   'ID  col.dens   err(%)    b-value   err(%)    vel_c'

         discard_component_id =
     &   maxloc(discard_component_list,discard_component_list.gt.0)
         discard_component = discard_component_id(1)

         write(6,'(1x,a,i5,6(1pe10.2))') trim(routinemsg_stat),
     &   discard_component,
     &   IONS(true_ion(discard_component))%component(discard_component)%col_dens,
     &   (IONS(true_ion(discard_component))%component(discard_component)%col_dens_err/
     &   IONS(true_ion(discard_component))%component(discard_component)%col_dens)*100,
     &   IONS(true_ion(discard_component))%component(discard_component)%b_value,
     &   (IONS(true_ion(discard_component))%component(discard_component)%b_value_err/
     &   IONS(true_ion(discard_component))%component(discard_component)%b_value)*100,
     &   IONS(true_ion(discard_component))%component(discard_component)%vel_c

         write(6,*)

        end if

!       ------------------------------------------------------------------------
        return

!       ------------------------------------------------------------------------
        end function discard_component
!       ========================================================================

!       ========================================================================
        subroutine irrelevant_component_module(goto1)

!       NOTE:
!       get rid of components which are:
!       - irrelevant, i.e. components with parameter values at the boundaries of
!       the respective range

!       but only if doing so does not increase chi^2 above chi2bad_reduced

        use set_precision
        use ifit_variables

        implicit none
        
        integer(kind=singleI) :: irrelevant_component
        
        logical, intent(out) :: goto1

!       ------------------------------------------------------------------------
!       initialise return value; signal to restart fit: goto1 = .true.
        goto1 = .false.

!       skip if no components in absorption profile
        if (num_param.eq.0) return

!       skip if currently a component has been added but only if ignoring
!       instrumental convolution; when taking convolution into account, some of
!       the first guess components might become irrelevant and should hence be
!       discarded 

        if (adding_component.and.(fwhm_kms.eq.0.0d0)) return

!       skip if doing fit
        if (do_fit) return

!       skip if in the process of discarding a component
        if (component_discarded) return

!       skip if chi2 still too large
        if ((chi2_current/dble(degrees_of_freedom)).gt.
     &  (1.0d0/average_noise_global)) return

!       load all current component-parameter values 'trial_param'
!       (and corresponding errors) into 'vel_c, col_dens, bvalue'
        call
     &  save_fit_parameters(trial_param,trial_param_err,num_param,component_id)

        discarded_component = irrelevant_component(num_param)

        if (discarded_component.gt.0) then ! true if there is a discardable
                                           ! component

!       tag discarded component
         IONS(true_ion(discarded_component))%component(discarded_component)%status = 0

!       load parameters 'vel_c, col_dens, bvalue' (and corresponding errors) of
!       *valid* components (i.e., EXCLUDING the component just discarded) 
!       present in current absorption profiles into array `trial_param';
!       determine new number of parameters `num_param'
         call
     &   load_fit_parameters(trial_param,trial_param_err,
     &   component_id,param_to_min,num_param,num_param_var)

!       increase number of discarded components
         discarded_components = discarded_components + 1

!       ------------------------------------------------------------------------
         call verbose_output(debug,trial_param,trial_param_err,component_id)
!       ------------------------------------------------------------------------

!       ...and re-start fit:
         goto1 = .true.
         return

        end if !discarded_component > 0

!       ------------------------------------------------------------------------
        return

        end subroutine irrelevant_component_module
!       ========================================================================

!       ========================================================================
        function irrelevant_component(np)

!       NOTE:
!       
!       identifies components that are either:
!       -at the lower boundaries of parameter space (N,b) or at the boundary
!       of the fitting absorption profiles

        use set_precision
        use ifit_variables

        implicit none

        integer(kind=singleI) :: irrelevant_component

        integer(kind=singleI) :: i, np

        integer(kind=singleI) :: comp_id

        integer(kind=singleI) :: irrelevant_component_index(1)
!       size of array is total (current) number of components
        integer(kind=singleI) :: irrelevant_component_list(1:num_components)

        character(len=64) :: routinemsg='irrelevant_component: '
        
        logical :: any_irrelevant_component

!       ------------------------------------------------------------------------
!       initialise
        irrelevant_component = 0

!       skip if the *total* number of components is 1
        if (num_components.eq.1) return

!       ------------------------------------------------------------------------
!       initialise logical flags

        any_irrelevant_component = .false.

!       initialise function
        irrelevant_component = 0

!       initialise number and list of discardable components
        irrelevant_component_list(1:num_components) = 0

!       ------------------------------------------------------------------------
!       component parameters (N,b) at boundary of respective range;
!       allow for a given tolerance; the adopted value should be tied to preci-
!       sion of derivatives (see param_derivatives); now set to 0%

        do i=1,np,3

!       component ID
         comp_id = component_id(i)

!       apply this condition only if convolution is not being taken into account

         if ((fwhm_kms.eq.0.0d0).and.
     &   (IONS(true_ion(comp_id))%component(comp_id)%b_value.le.
     &   b_value_min(true_ion(comp_id))))
     &    irrelevant_component_list(comp_id) =
     &    irrelevant_component_list(comp_id) + 1
         
         if (IONS(true_ion(comp_id))%component(comp_id)%col_dens.le.
     &   col_dens_min(true_ion(comp_id)))
     &    irrelevant_component_list(comp_id) =
     &    irrelevant_component_list(comp_id) + 1
         
         if (IONS(true_ion(comp_id))%component(comp_id)%vel_c.le.velocity_min)
     &    irrelevant_component_list(comp_id) =
     &    irrelevant_component_list(comp_id) + 1

         if (IONS(true_ion(comp_id))%component(comp_id)%vel_c.ge.velocity_max)
     &    irrelevant_component_list(comp_id) =
     &    irrelevant_component_list(comp_id) + 1

!       update flag to record that there is at least one irrelevant component
         if (any(irrelevant_component_list(1:num_components).gt.0))
     &    any_irrelevant_component = .true.

         end do ! over parameters

!       ------------------------------------------------------------------------
!       identify component with most 'irrelevance points' and discard:
        
        if (any_irrelevant_component) then
        
         write(6,'(a)') trim(output_dashed_line)
         write(6,'(25x,a)')
     &   'ID    col.dens  err(%)   bvalue   err(%)   vel_c'

         irrelevant_component_index =
     &   maxloc(irrelevant_component_list,irrelevant_component_list.gt.0)
         
         irrelevant_component = irrelevant_component_index(1)

         write(6,'(1x,a,i5,5(1pe10.2))') trim(routinemsg),
     &   irrelevant_component,
     &   IONS(true_ion(irrelevant_component))%component(irrelevant_component)%col_dens,
     &   (IONS(true_ion(irrelevant_component))%component(irrelevant_component)%col_dens_err/
     &   IONS(true_ion(irrelevant_component))%component(irrelevant_component)%col_dens)*1.0d2,
     &   IONS(true_ion(irrelevant_component))%component(irrelevant_component)%b_value,
     &   (IONS(true_ion(irrelevant_component))%component(irrelevant_component)%b_value_err/
     &   IONS(true_ion(irrelevant_component))%component(irrelevant_component)%b_value)*1.0d2,
     &   IONS(true_ion(irrelevant_component))%component(irrelevant_component)%vel_c

         write(6,*)

        end if

!       ------------------------------------------------------------------------
        return

!       ------------------------------------------------------------------------
        end function irrelevant_component
!       ========================================================================

!       ========================================================================
        logical function do_discard_component(chi2_new,chi2_save,dof_new,
     &  dof_save,np)

!       NOTE:
!       this is one of the MOST important functions, since it it a decision-
!       maker and completely determines the progress of the fit;
!       it decides whether a fit is good or not (in the chi^2 sense)

!       returns true (i.e. accept discard) if:
!
!       1) first call (chi2_discard_save = 0),

!       2) or reduced new chi^2 value decreases, or

!       3) or reduced new chi^2 < chi^2_bad

!       3) no component and reduced (new chi^2) < 2 * chi^2_good

!       since discarding a component increases degrees of freedom by 3, correct
!       for this when comparing chi^2-value before and after discard

        use set_precision
        use ifit_variables, only: chi2bad_reduced, chi2good_reduced

        implicit none

        integer(kind=singleI) :: dof_new, dof_save, np

        real(kind=doubleR) :: chi2_new, chi2_save

        do_discard_component = .false.

        if (
     &  (chi2_save.eq.0.0d0)
     &  .or.((chi2_new/dble(dof_new)).le.(chi2_save/dble(dof_save)))
     &  .or.(((chi2_new/dble(dof_new)).le.chi2bad_reduced))
     &  .or.((np.eq.0).and.((chi2_new/dble(dof_new)).le.2.0d0*chi2good_reduced))
     &   )
     &   do_discard_component = .true.

!       NOTE:
!       CONTINUOUSLY CHECK THE ABOVE CRITERIA FOR CONSISTENCY

!       ------------------------------------------------------------------------
        return

        end function do_discard_component
!       ========================================================================

!       ========================================================================
        subroutine insert_component_module(goto1)

!       NOTE:
!       if fit not good (i.e. chi2 > chi2_bad) try to add a new component
!       this is only a desperate measure to improve fit
!       this should be the LAST solution to be tried, since the idea is
!       to keep the number of components to a minimum
!       however, sometimes throwing in a random component (and thus extending
!       parameter space) helps MRQMIN finding a better solution (i.e.
!       fit), for which some components are irrelevant and thus discarded

        use set_precision
        use ifit_variables

        implicit none
        
        integer(kind=singleI) :: j
        logical :: insert_component                 ! function
        logical :: residual_absorption              ! function
        logical, intent(out) :: goto1

!       ------------------------------------------------------------------------
        goto1 = .false.

!       skip if doing fit
        if (do_fit) return

!       skip if discarded irrelevant component
        if (component_irrelevant) return

!       skip if discarded any component
        if (component_discarded) return

!       skip if taking LSF convolution into account
        if (fwhm_kms.gt.0.0d0) return

!       skip during multi-transition fit
        if (sweep.eq.3) return

!       if reduced chi^2 is above maximum tolerable value AND there is any sig-
!       nificant residual absorption (with respect to the local noise), try
!       adding a new component;
!       if any of these conditions is false, stop adding components and
!       set add_more = .false.

!       ------------------------------------------------------------------------
        IF (((chi2_current/dble(degrees_of_freedom)).gt.chi2bad_reduced).and.
     &  residual_absorption(
     &  abs_profile_id,xdata_in,ydata_in,trial_param,num_param).and.
     &  (add_more)) THEN
!       ------------------------------------------------------------------------

!       if adding a component increased chi^2 value, stop adding components,
!       when taking instrumental convolution into account; 
!       otherwise try adding another component:

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         if ((fwhm_kms.gt.0.0d0).and.
     &   (adding_component.and.((chi2_current/dble(degrees_of_freedom)).ge.
     &   (best_chi2_add/dble(degrees_of_freedom_save))))) then
!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          
!       no improvement after adding component; hence:

          write(6,'(a,i5)') trim(warnmsg)//
     &   'Increased chi^2-value! Will stop adding components'
          write(6,*)

!       signal that no further component has been added
          adding_component = .false.

!       signal to redo fit
          do_fit = .true.

!       signal to avoid adding more components
         add_more = .false.

!       ...and re-start fit
          goto1 = .true.

          return

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         else
!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         
          chi2_add_save = chi2_current
          best_chi2_add = chi2_add_save

!       save current parameter values
          do j=1,num_param
           trial_param_save(j) = trial_param(j)
          end do
          num_param_save = num_param

!       create trial component
!       insert_component returns a logical value; .true. = component has been
!       added

          adding_component =
     &    insert_component(abs_profile_id,xdata_in,ydata_in,trial_param,
     &    num_param)

!       re-load all current component-parameter values (what for?)
!       since num_param is not changed, this won't affect anything...
          call
     &    save_fit_parameters(trial_param,trial_param_err,
     &    num_param,component_id)

!       ...and re-start fit (if required, i.e., if adding_component = true)
          goto1 = adding_component
          return

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         end if !adding a component decreased chi2 value?
!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!       ------------------------------------------------------------------------
!       if no new component required or degrees of freedom at minimum

        ELSE IF (add_more) THEN
!       ------------------------------------------------------------------------
        
!       load current (accepted) component-parameter values 'trial_param' (and
!       corresponding errors) into 'vel_c, col_dens, bvalue'
         call
     &   save_fit_parameters(trial_param,trial_param_err,num_param,component_id)

!       signal that no further components are being added
         adding_component = .false.

!       signal to avoid adding more components
         add_more = .false.

!       re-start fit and try to get rid of irrelevant / uncertain components:
         goto1 = .true.
         return

!       ------------------------------------------------------------------------
        END IF !add a component?
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        return

        end subroutine insert_component_module
!       ========================================================================

!       ========================================================================
        logical function insert_component(absprofile_id,veloc,fluxx,par_val,np)

!       generates a new component at the pixel with the highest difference
!       between model and data; note that using the difference (rather than the
!       quotient) is inconsistent with the approach otherwise used throughout
!       the code; but it turns out to be the most efficient approach to identi-
!       fy the pixels where a new component significantly improves the fit

        use set_precision
        use ifit_variables

        implicit none

        integer(kind=singleI) :: np, absprofile_id, px_min, px_max, inter_size
        integer(kind=singleI) :: j
        integer(kind=singleI) :: pixel_add
        integer(kind=singleI) :: insert_px(1)

        real(kind=doubleR) :: par_val(1:np)
        real(kind=doubleR) :: veloc(*), fluxx(*)
        real(kind=doubleR), allocatable :: smooth_dat(:), dat(:)
        real(kind=doubleR) :: tau_th, min_flux_level
        real(kind=doubleR) :: model_at_px                             !function

!       IMPORTANT:
!       the following variable is not yet used; it was originally intended to
!       solve the following problem: when two narrow, but saturared features are
!       present, the code tends to use one single broad, saturated component to
!       fit the profile, since this yields a `small' chi2-value too
!       DO SOMETHING ABOUT IT!

        real(kind=doubleR) :: model_abs_xs

        character(len=64) :: routinemsg='insert new component: '
        
!       ------------------------------------------------------------------------
!       initialise
        insert_component = .false.
        insert_px(1) = 0
        tau_th = 0.0d0

        px_min = abs_profile_bounds(absprofile_id,1)
        px_max = abs_profile_bounds(absprofile_id,2)
        inter_size = px_max - px_min + 1

        if (allocated(dat)) deallocate(dat)
        if (allocated(smooth_dat)) deallocate(smooth_dat)
        allocate(dat(1:inter_size))
        allocate(smooth_dat(1:inter_size))

!       compute component central pixel where difference between model and data
!       is largest (and significant)

!       compute difference between model and data;
!       impose a limit (epsilon) to avoid loss of precision (i.e. 1 - x = 1
!       when x < epsilon[x]) and a code crash when computing dlog(dat) below
!       NOTE: epsilon(x) gives the smallest positive number (of kind x) such
!       that 1 + epsilon(x) > 1

        do j=1,inter_size
         dat(j) =
     &   max(fluxx(j),epsilon(real_double)) -
     &   model_at_px(par_val,np,veloc(j),absorption_type)
        end do

!       proceed differently depending on absorption type:
!       non-saturated | saturated

        if (trim(absorption_type).eq.'nonsat') then
        
!       threshold value
         min_flux_level = average_noise_local_aod

!       smooth residual optical depth
         call
     &   smooth_sg(smooth_dat,dat,1,inter_size,4,0,2*resol_elem_px,
     &   min_flux_level,debug)

!       keep smoothed optical depth bound by original value to remove glitches
!       that may result from smoothing
         where(smooth_dat(1:inter_size).gt.dat(1:inter_size))
     &   smooth_dat(1:inter_size) = dat(1:inter_size)

        else ! saturated absorption
        
!       compute model absorption excess
!        model_abs_xs =0.0d0
!        model_abs_xs = sum(dat(:))

!       compute residual absorption; transform into effective optical depth
         dat(:) = 1.0d0 + dat(:)
         dat(:) = -1.0d0 * dlog(dat(:))

!       threshold value; minimum of intrinisc or smoothed noise
         min_flux_level = min(average_noise_local_smooth,average_noise_local)

!       smooth residual (effective) optical depth
         call
     &   smooth_sg(smooth_dat,dat,1,inter_size,1,0,resol_elem_px,min_flux_level,
     &   debug)

        end if ! saturated absorption?

!       determine pixel with largest data/model difference
        
        insert_px =
     &  maxloc(smooth_dat(1:inter_size),
     &  smooth_dat(1:inter_size).gt.(min_flux_level))
        
!       if no significant difference (wrt local noise), return
        if (insert_px(1).eq.0) then
         write(6,'(1x,a)') trim(warnmsg)//
     &   'No significant residual absorption left;'//
     &   'no new component is being added!'
         insert_component = .false.
         return
        end if

!       set absorption pixel
        pixel_add = px_min + insert_px(1) - 1

!       set function value
        insert_component = .true.

!       check that maximum number of components is not superseeded; abort if so
        if (num_components+1.gt.max_components) then
         write(6,'(a,i5)') trim(errmsg)//
     &   'Maximum number of allowed components has been superseeded: ',
     &   max_components
         write(6,'(20x,a)')
     &   'Increase value of paremeter max_components_abs in module '//
     &   'ifit_variables'
         stop 1
        end if

!       otherwise continue:

!       increase total number of components and number of added components

        num_components = num_components+1

        inserted_components = inserted_components + 1

!       IMPORTANT: note that ION_INDEX is used here (rather than the vector
!       true_ion), since new components are added only during sweep 1, i.e.,
!       during the brute-force, single-ion (transition) fit, whose index is
!       ION_INDEX=1

!       set component status

        IONS(ION_INDEX)%component(num_components)%status = 1

!       set component velocity centroid

        IONS(ION_INDEX)%component(num_components)%vel_c = velocity(pixel_add)

!       set component b-value
!       make it dependent on pixel size and resolution window which is set
!       to min_component_res_px; see absorption_profiles

        IONS(ION_INDEX)%component(num_components)%b_value =
     &  min_component_res_px * pixel_size_kms

!       set central optical depth (needed only locally)

        tau_th = smooth_dat(insert_px(1))

!       set component column density using the central optical depth and b-value
!       ASSUME it is the strongest transition of the current ion ("brute-force")

        IONS(ION_INDEX)%component(num_components)%col_dens =
     &  (tau_th/IONS(ION_INDEX)%transition(strongest(ION_INDEX,1))%tau_0) *
     &  IONS(ION_INDEX)%component(num_components)%b_value

        write(6,*)
        write(6,'(a)') trim(output_dashed_line)
        write(6,'(27x,a)') 'ID   pixel     vel_c    log10(N)    b [km/s]'
        write(6,'(1x,a,i6,2x,i6,f10.2,f10.2,4x,f10.2)')
     &  trim(colour_statmsg_prefix)//trim(routinemsg)//trim(colour_msg_suffix),
     &  num_components, pixel_add, IONS(ION_INDEX)%component(num_components)%vel_c,
     &  dlog10(IONS(ION_INDEX)%component(num_components)%col_dens),
     &  IONS(ION_INDEX)%component(num_components)%b_value

!       ------------------------------------------------------------------------
        return

        end function insert_component
!       ========================================================================

!       ========================================================================
        function residual_absorption(absprofile_id,veloc,fluxx,par_val,np)

!       determines if there is any significant residual absorption
!       returns the pixel with the highest difference between
!       model and data
!       same approach as routine insert_component()

        use set_precision
        use ifit_variables

        implicit none

        integer(kind=singleI) :: np, absprofile_id, px_min, px_max, inter_size
        integer(kind=singleI) :: j
        integer(kind=singleI) :: insert_px(1)

        real(kind=doubleR) :: par_val(1:np)
        real(kind=doubleR) :: veloc(*), fluxx(*)
        real(kind=doubleR), allocatable :: smooth_dat(:), dat(:)
        real(kind=doubleR) :: min_flux_level, model_abs_xs
        real(kind=doubleR) :: N_min, b_min, tau_th
        real(kind=doubleR) :: model_at_px                       !function

        character(len=64) :: routinemsg='residual absorption:'

        logical :: residual_absorption
        
!       ------------------------------------------------------------------------
!       initialise
        residual_absorption = .false.
        tau_th = 0.0d0
        insert_px(1) = 0

        px_min = abs_profile_bounds(absprofile_id,1)
        px_max = abs_profile_bounds(absprofile_id,2)
        inter_size = px_max - px_min + 1

!       check for minimum number of degrees of freedom (taking into account a
!       potential new component)
        
        if ((inter_size - (np + min_dof)).le.min_dof) then
         write(6,*) trim(routinemsg)//
     &   ' degrees of freedom have reached its minimum value:', min_dof
         return
        end if

!       otherwise continue ...
        if (allocated(dat)) deallocate(dat)
        if (allocated(smooth_dat)) deallocate(smooth_dat)
        allocate(dat(1:inter_size))
        allocate(smooth_dat(1:inter_size))

!       compute component central pixel where difference between model and data
!       is largest (and significant)

!       compute difference between model and data;
!       impose a limit (epsilon) to avoid loss of precision (i.e. 1 - x = 1
!       when x < epsilon[x]) and a code crash when computing dlog(dat) below
!       NOTE: epsilon(x) gives the smallest positive number (of kind x) such
!       that 1 + epsilon(x) > 1

        do j=1,inter_size
         dat(j) =
     &   max(fluxx(j),epsilon(real_double)) -
     &   model_at_px(par_val,np,veloc(j),absorption_type)
        end do

!       proceed differently depending on absorption type:
!       non-saturated | saturated

        if (trim(absorption_type).eq.'nonsat') then

!       threshold value
         min_flux_level = average_noise_local_aod

!       smooth residual optical depth
         call
     &   smooth_sg(smooth_dat,dat,1,inter_size,4,0,2*resol_elem_px,
     &   min_flux_level,debug)
        
!       keep smoothed optical depth bound by original value to remove glitches
!       that may result from smoothing
         where(smooth_dat(1:inter_size).gt.dat(1:inter_size))
     &   smooth_dat(1:inter_size) = dat(1:inter_size)

        else ! saturated absorption
        
!       compute model absorption excess
!        model_abs_xs =0.0d0
!        model_abs_xs = sum(dat(:))

!       compute residual absorption; transform into effective optical depth
         dat(:) = 1.0d0 + dat(:)
         dat(:) = -1.0d0 * dlog(dat(:))

!       threshold value; minimum of intrinisc or smoothed noise
         min_flux_level = min(average_noise_local_smooth,average_noise_local)

!       smooth residual (effective) optical depth
         call
     &   smooth_sg(smooth_dat,dat,1,inter_size,1,0,resol_elem_px,min_flux_level,
     &   debug)
        
        end if ! saturated absorption?

!       determine pixel with larges data/model difference
        
        insert_px =
     &  maxloc(smooth_dat(1:inter_size),
     &  smooth_dat(1:inter_size).gt.(min_flux_level))

!       return if no significant absorption found (wrt local noise)
        if (insert_px(1).eq.0) then         

         write(6,*)
         write(6,'(1x,a)') trim(statmsg)//
     &   'No significant (residual) absorption found!'
         write(6,*)
         
         return
        
        end if

!       estimate required column density
!       ASSUME it is the strongest transition of the current ion ("brute-force")

        b_min = min_component_res_px * pixel_size_kms
        tau_th = smooth_dat(insert_px(1))

        N_min = (tau_th /
     &  IONS(ION_INDEX)%transition(strongest(ION_INDEX,1))%tau_0) * b_min

!       notify if no significant residual absorption present with respect to
!       minimum input column density (of first listed ion)
        if (N_min.lt.col_dens_min(ION_INDEX)) then

         write(6,*)
         write(6,'(1x,a)') trim(warnmsg)
         write(6,'(1x,a)')
     &   'Column density of residual absorption smaller than minimum '//
     &   'input column density'
         write(6,*)

         residual_absorption = .false.

        else

         residual_absorption = .true.

        end if

!       ------------------------------------------------------------------------
        return

        end function residual_absorption
!       ========================================================================

!       ========================================================================
        subroutine levenberg_marquardt_minimisation()

!       encapsulates the LM-minimisation process

        use set_precision
        use ifit_variables
        use new_ops

!       ------------------------------------------------------------------------
!       set lambda_mrqmin < 0 to initialise mrqmin
           lambda_mrqmin = -1.0d0

!       initialise LM success iteration counter;
!       avoid iterating if no components in absorption profiles (i.e.,
!       num_param=0)
           
           if (num_param.eq.0) then
            success_mrqmin_iteration = iter_max
            call
     &      write_chi2_evolution(1,visual,chi2_current/dble(degrees_of_freedom))

           else
            mrqmin_iteration = 0
            success_mrqmin_iteration = 0

           end if

!       ------------------------------------------------------------------------
!       signal to begin minimisation
           chi2_previous = 0.0d0

!      start minimisation;
!      note: operator `.converged.' and corresponding function `converged' are
!      defined in module `new_ops'

           do while (.not.(.converged.chi2_current))

!       update iteration counter
            mrqmin_iteration = mrqmin_iteration + 1

!       save chi^2 value to compare with new value
            chi2_previous = chi2_current

!       compute new parameter values and new chi^2 value
!       NOTE: if chi^2 does not improve, mrqmin returns the chi^2 input value

            call mrqmin(xdata_in,ydata_in,yerror_in,absprofile_size_px,
     &      trial_param,num_param,param_to_min,COVAR,ALPHA,max_num_param,
     &      chi2_current,lambda_mrqmin)

!       store chi^2 values to visualise fit behaviour
            call
     &      write_chi2_evolution(1,visual,chi2_current/dble(degrees_of_freedom))

!       compute [and print] goodness-of-fit parameters
            call fit_goodness(1,degrees_of_freedom,sweep)

!       output info (if so required)
            call verbose_output(debug,trial_param,trial_param_err,component_id)

           end do !while
!       ------------------------------------------------------------------------


!       ------------------------------------------------------------------------
        return

        end subroutine levenberg_marquardt_minimisation
!       ========================================================================

!       ========================================================================
        subroutine chi2_outliers(dof)

!       NOTE:
!       very often, single pixels with noise 10x (or more) below the
!       average local noise (usually in saturated absorption profiless) lead to
!       a substantial --but meaningless-- increase of the local chi^2 value and
!       thus to an overfitting, using more components than actually needed

!       this routine simply identifies these pixels; can be useful when investi-
!       gating the cause of a poor fit

        use set_precision
        use ifit_variables

        implicit none
        
        integer(kind=singleI), intent(in) :: dof
        integer(kind=singleI) :: counter
        integer(kind=singleI) :: outlier_pixel_min(2), outlier_pixel_max(2)
        integer(kind=singleI) :: chi2_terms_index(1:dof)

        real(kind=doubleR) :: partial_chi2

        character(len=64) :: routinemsg='chi2_outliers:'

!       ------------------------------------------------------------------------
!       print routine name for information
        write(6,*)
        write(6,'(a)') trim(colour_warnmsg_prefix)//trim(routinemsg)//
     &  trim(colour_msg_suffix)

!       ------------------------------------------------------------------------
!       NOTE:
!       individual terms of chi^2 function are stored in the array 'chi2_terms'
!       and are computed by function `chi2_function'

!       index and sort array in ascending order
        call indexx(dof,chi2_terms,chi2_terms_index)
        
        outlier_pixel_min(1) = chi2_terms_index(1)
        outlier_pixel_min(2) = chi2_terms_index(2)
        outlier_pixel_max(1) = chi2_terms_index(dof-1)
        outlier_pixel_max(2) = chi2_terms_index(dof)

!       determine maximum number of terms which lead to chi^2 =< chi2bad_reduced
        partial_chi2 = 0.0d0
        counter = 0
        
        do while ((counter.lt.dof).and.
     &  (partial_chi2.le.chi2bad_reduced*counter))
         counter = counter + 1
         partial_chi2 = partial_chi2 + chi2_terms(chi2_terms_index(counter))
        end do
        
        if (.not.quite) then

         write(6,'(a,17x,i7)')
     &   ' chi^2 values above chi^2_good:',
     &   count(chi2_terms(1:dof).gt.chi2good_reduced)
        
         write(6,'(a,17x,i7)')
     &   ' chi^2 values below chi^2_good:',
     &   count(chi2_terms(1:dof).le.chi2good_reduced)
        
         write(6,'(a,17x,i7)')
     &   ' total number of true outliers:',
     &   dof - counter

         write(6,'(a,16x,2(1pe12.4))')
     &   ' lowest chi^2 values (outliers):',
     &   chi2_terms(outlier_pixel_min(1)),
     &   chi2_terms(outlier_pixel_min(2))
        
         write(6,'(a,15x,2(1pe12.4))')
     &   ' highest chi^2 values (outliers):',
     &   chi2_terms(outlier_pixel_max(1)),
     &   chi2_terms(outlier_pixel_max(2))
        
         write(6,'(a,2x,23x,2(i8))') ' corresponding pixel:',
     &   outlier_pixel_min(1) + pixel_min - 1,
     &   outlier_pixel_min(2) + pixel_min - 1

         write(6,'(22x,24x,2(i8))')
     &   outlier_pixel_max(1) + pixel_min - 1,
     &   outlier_pixel_max(2) + pixel_min - 1

         write(6,'(a,2x,19x,2(f12.2))') ' corresponding velocity:',
     &   xdata_in(outlier_pixel_min(1)),
     &   xdata_in(outlier_pixel_min(2))

         write(6,'(25x,20x,2(f12.2))')
     &   xdata_in(outlier_pixel_max(1)),
     &   xdata_in(outlier_pixel_max(2))

         write(6,*)

        end if ! quite?

!       ------------------------------------------------------------------------
        return

        end subroutine chi2_outliers
!       ========================================================================

!       ========================================================================
        subroutine setup_abs_profile()

!       determines absorption profiles pixel/velociy range and size
!       initialises several relevant parameters

        use set_precision
        use ifit_variables
        use constants, only: c_kms

        implicit none
        
        integer(kind=singleI) :: pick_abs_profile

        logical :: split_abs_profile       !to split large absorption profiles

!       define output formats
 10     format(1x,i5,6x,i5,3f13.2,2x,i5)

!       ------------------------------------------------------------------------
!       initialise parameters for each absorption profile

         chi2_start = 0.0d0                     !initial chi^2 value
         chi2_current = 0.0d0
         chi2_previous = 0.0d0          
         best_reduced_chi2 = 0.0d0

         chi2_add_save = huge(real_single)      !chi^2 before adding a component
         best_chi2_add = huge(real_single)
         chi2_discard_save = huge(real_single)
                                            !chi^2 before discarding a component

         adding_component = .false.          !signal that components are added
         add_more = .true.                   !signal to (stop) adding components
         discarded_component = 0             !index of a discarded component
         component_discarded = .false.       !signal to discard components
         outlier_pixels = 0

!       the following are used to determine the convergence of the
!       LM-minimisation process:

         chi2_decrease_rate_diff = 0.0d0
         chi2_decrease_rate_average_array(:) = 0.0d0
         log_lambda_mrqmin_oscillation_diff = 1.0d10
         log_lambda_mrqmin_oscillation(:) = 0.0d0
         chi2_decrease_rate_average = 0.0d0
         array_aux(:) = 0.0d0

!       total iterations needed for current absorption profile
         tot_iter_per_abs_profile = 0

!       initialise counts of fitting attempts
          fit_attempt = 0

!       ------------------------------------------------------------------------
!       pick up absorption profile with minimum average flux

         abs_profile_id = pick_abs_profile(num_abs_profiles)

!       see if can split absorption profile to speed-up fit;
!       start fitting absorption profile with lowest average flux;
!       new absorption profile added at end of list 
         do while(split_abs_profile(abs_profile_id,num_abs_profiles))
          abs_profile_id = pick_abs_profile(num_abs_profiles)
         end do

!       flag absorption profile as fitted
         abs_profile_tag(abs_profile_id) = 0

!       pixel range
         pixel_min = abs_profile_bounds(abs_profile_id,1)
         pixel_max = abs_profile_bounds(abs_profile_id,2)

!       velocity range
         velocity_min = velocity(abs_profile_bounds(abs_profile_id,1))
         velocity_max = velocity(abs_profile_bounds(abs_profile_id,2))

!       determine size of absorption profile (in pixel)
         absprofile_size_px = pixel_max - pixel_min + 1

!       ------------------------------------------------------------------------
!       set local velocity scale
         if (allocated(velocity_local)) deallocate(velocity_local)
         if (allocated(velocity_aux)) deallocate(velocity_aux)
         allocate(velocity_local(1:absprofile_size_px))
         allocate(velocity_aux(1:absprofile_size_px))

         velocity_local(1:absprofile_size_px) =
     &   c_kms * dlog(wlength(pixel_min:pixel_max)/wlength(pixel_min))

         velocity_aux = eoshift(velocity_local, shift = 1)

         pixel_size_kms_local =
     &   sum(velocity_aux(1:absprofile_size_px-1) -
     &   velocity_local(1:absprofile_size_px-1))/dble(absprofile_size_px - 1)

!       ------------------------------------------------------------------------
!       set effective size of absorption profile (may differ from initial value
!       since some pixels [chi^2_outliers] are discarded during fitting
!       process (see call to routine `chi2_outliers' below)

         degrees_of_freedom = absprofile_size_px
         degrees_of_freedom_save = absprofile_size_px

!       update counter
         processed_abs_profiles = processed_abs_profiles+1

!       ------------------------------------------------------------------------
!       info output
         write(6,*)
         write(6,'(a)') trim(output_dashed_line)
         write(6,'(1x,a)') trim(output_abs_profile_info_short)
         write(6,10) abs_profile_id, num_abs_profiles-processed_abs_profiles,
     &   average_flux_local, velocity_min, velocity_max, sweep
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       write absorption profile id to string
         write(abs_profile_id_str,'(i3)') abs_profile_id

!       define file name for file required by `visualise' containing:
!       iteration | reduced current chi^2 | No. of components

         file_chi2_evolution = 'chi2_abs_profile_'//
     &   trim(adjustl(abs_profile_id_str))//'_'//trim(IONS(ION_INDEX)%name)//
     &   '.vis'

!       initialise plotting absorption profile counter
!       (used only if visual = true)
         visual_counter = 0
         first_call_visual = .true.

!       ------------------------------------------------------------------------
        return

        end subroutine setup_abs_profile
!       ========================================================================

!       ========================================================================
        subroutine set_spectral_data()

!       initialises several quantities previous to the fitting process

        use set_precision
        use ifit_variables

!       ------------------------------------------------------------------------

!       initialise spectral vectors for absorption profile
!       recall:
!       modelflux is the model;
!       flux_residum is the residual flux; 
!       modelflux_convolved is the model [convolved with Gaussian PSF whose
!       width is fwhm_kms (input parameter)]

         flux_residum(:) = 0.0d0
         flux_residum_error(:) = 1.0d0
         aod_residum(:) = 0.0d0
         aod_residum_error(:) = 0.0d0
         xdata_in(:) = 0.0d0
         ydata_in(:) = 0.0d0
         yerror_in(:) = 0.0d0
         chi2_terms(:) = 0.0d0

!       ------------------------------------------------------------------------
!       set residual flux

!       IMPORTANT: since model is normalised by construction, i.e., bound to
!       values <= 1, limit data to this range for a fair computation of chi^2
!       value 

!       IMPORTANT: this may affect fitting at absorption profile boundaries,
!       since pixels with flux >=1 will be ignored in the computation of the
!       chi-square value 


!		  CHECK THIS AND THE USE OF SMOOTHED FLUX RATHER THAN THE INPUT FLUX

		  if (use_smooth_flux) then

!			 avoid negative interpolated values
 			 where (flux_smooth.le.0.0d0) flux_smooth = noise

          flux_residum(1:absprofile_size_px) =
     &    min(1.0d0,(flux_smooth(pixel_min:pixel_max) /
     &    modelflux_convolved(pixel_min:pixel_max)))
         
        else

          flux_residum(1:absprofile_size_px) =
     &    min(1.0d0,(flux(pixel_min:pixel_max) /
     &    modelflux_convolved(pixel_min:pixel_max)))
        
        end if

!       set flux error vector

         flux_residum_error(1:absprofile_size_px) = noise(pixel_min:pixel_max)

!       transform flux into optical depth; put a lower bound to avoid underflow
          
         aod_residum(1:absprofile_size_px) =
     &   max(dble(tiny(real_single)),
     &   -1.0d0*dlog(flux_residum(1:absprofile_size_px)))

!       optical depth error (assuming Gaussian error propagation):
!
!       if tau(f) = -ln(f), then sigma(tau) = abs(1/f) sigma(f)

!       IMPORTANT: vanishingly small values of flux are checked for (and edited
!       if needed) by read_spectrum
          
         aod_residum_error(1:absprofile_size_px) =
     &   abs(flux_residum_error(1:absprofile_size_px)/
     &   flux_residum(1:absprofile_size_px))

!       ------------------------------------------------------------------------
!       compute average noise in absorption profile
!       take only unabsorbed pixels into account; these exclude also pixels with
!       error = 1 (which had originally error < 0)

         average_noise_local =
     &   sum(noise(pixel_min:pixel_max) *
     &   unabsorbed_pixel(pixel_min:pixel_max))/
     &   dble(count(unabsorbed_pixel(pixel_min:pixel_max).eq.1))

!       compute *ideal* average noise in absorption profile (using difference
!       between actual and smoothed flux)
         
         average_noise_local_smooth =
     &   dsqrt(
     &   sum((flux(pixel_min:pixel_max) -
     &   flux_smooth(pixel_min:pixel_max))**2.0d0)/
     &   dble(absprofile_size_px-1))

!       compute average noise in absorption profile (over full pixel range)
!       ignore pixels with error = 1 (which had originally error < 0)

         average_noise_local_aod =
     &   sum(
     &   aod_residum_error(1:absprofile_size_px) *
     &   unabsorbed_pixel(pixel_min:pixel_max))/
     &   dble(count(unabsorbed_pixel(pixel_min:pixel_max).eq.1)
     &   )

!       compute average signal-to-noise ratio in absorption profile
!       take only unabsorbed pixels into account; these exclude also pixels with
!       error = 1 (which had originally error < 0)

         if (count(unabsorbed_pixel(pixel_min:pixel_max).eq.1).gt.0) then
          average_sn_local =
     &    sum((flux_residum(1:absprofile_size_px) * 
     &    unabsorbed_pixel(pixel_min:pixel_max))/
     &    noise(pixel_min:pixel_max))/
     &    dble(count(unabsorbed_pixel(pixel_min:pixel_max).eq.1))

         else if (average_noise_local.gt.0.0d0) then
          average_sn_local = 1.0d0/average_noise_local

         end if

!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        return

        end subroutine set_spectral_data
!       ========================================================================

!       ========================================================================
        subroutine load_spectral_data()

!       determines type of absorption (saturated/non-saturated) and
!       correspondingly sets the input data (flux/apparent optical depth)

        use set_precision
        use ifit_variables

!       ------------------------------------------------------------------------
!       define type of absorption in current absorption profile:
!       non-saturated | saturated
!       define input data correspondingly

!       IMPORTANT: Do this at every iteration to distinguish between components
!       instead of absorption profiles

!       IMPORTANT: include the parameter given below as input parameter, since
!       the adopted value is somehow arbitrary (i.e., not well defined)

        tau_sat = 4.0

        if (count(-dlog(flux_residum(1:absprofile_size_px)).gt.tau_sat)
     &  .ge.resol_elem_px) then
         absorption_type = 'sat'
         absorption_type_str = 'SATURATED'
         fit_type_str = 'FLUX'
         smoothing_str = 'YES'
        else
         absorption_type = 'nonsat'
         absorption_type_str = 'NON-SATURATED'
         fit_type_str = 'APPARENT OPTICAL DEPTH'
         smoothing_str = 'NO'
        end if

!       ------------------------------------------------------------------------
!       set input data (x,y) and associated error (sigma_y only)

         xdata_in(1:absprofile_size_px) = velocity(pixel_min:pixel_max)
           
         if (trim(absorption_type).eq.'nonsat') then ! fit on optical depth
         
          ydata_in(1:absprofile_size_px) =
     &    aod_residum(1:absprofile_size_px)

          yerror_in(1:absprofile_size_px) =
     &    aod_residum_error(1:absprofile_size_px)
         
         else if (trim(absorption_type).eq.'sat') then ! fit on flux

          ydata_in(1:absprofile_size_px) =
     &    flux_residum(1:absprofile_size_px)

          yerror_in(1:absprofile_size_px) =
     &    flux_residum_error(1:absprofile_size_px)
         
         end if
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        return

        end subroutine load_spectral_data
!       ========================================================================

!       ========================================================================
        subroutine save_fit_parameters(par_val,par_err,np,component_index)

!       component status = 1 -> working/discardable component
!       component status = 0 -> discarded component
!       component status = -1 -> non-discardable component
!       component status = 3 -> first-guess component (when including inst.conv)

        use set_precision
        use ifit_variables, only: IONS, true_ion, trial_param_cov

        implicit none

        integer(kind=singleI), intent(in) :: np, component_index(*)
        real(kind=doubleR), intent(in) :: par_val(*), par_err(*)

        integer(kind=singleI) :: j, comp_id

        do j=1,np,3

!       auxiliary index
         comp_id = component_index(j)

         IONS(true_ion(comp_id))%component(comp_id)%vel_c = par_val(j)
         IONS(true_ion(comp_id))%component(comp_id)%col_dens = par_val(j+1)
         IONS(true_ion(comp_id))%component(comp_id)%b_value = par_val(j+2)

!       parameter error

         IONS(true_ion(comp_id))%component(comp_id)%vel_c_err =
     &   par_err(j)
         IONS(true_ion(comp_id))%component(comp_id)%col_dens_err =
     &   par_err(j+1)
         IONS(true_ion(comp_id))%component(comp_id)%b_value_err =
     &   par_err(j+2)

!       parameter covariance

!       velocity -- column density
         IONS(true_ion(comp_id))%component(comp_id)%v_coldens_cov =
     &   trial_param_cov(j,j+1)

!       velocity -- b-value
         IONS(true_ion(comp_id))%component(comp_id)%v_b_cov =
     &   trial_param_cov(j,j+2)

!       column density -- b-value
         IONS(true_ion(comp_id))%component(comp_id)%coldens_b_cov =
     &   trial_param_cov(j+1,j+2)

        end do
!       ------------------------------------------------------------------------
        return

        end subroutine save_fit_parameters
!       ========================================================================

!       ========================================================================
        subroutine load_fit_parameters(par_val,par_err,component_index,
     &  varying_params,tot_param,var_param)

!       finds all working components contained in absorption profile within
!       [velocity_min,velocity_max].
!       loads parameters for those components into par_val(tot_param), which are
!       the parameters varied by the mrqmin minimization routine;
!       `varying_params' contains indices of var_param parameters to be varied;
!       for now, vary all parameters (var_param=tot_param).

!       component_index is each component's id

!       component status contains a status tag for each component according to
!       the following:

!       component status = 1 -> working/discardable component
!       component status = 0 -> discarded component
!       component status = -1 -> non-discardable component
!       component status = 3 -> first-guess component (when including inst.conv)

!       NOTE:
!       all components are set as 'working' during first call
!       note that parameters j (vel_c), j+1 (col_dens), j+2 (b_value) are
!       associated to component j

        use set_precision
        use ifit_variables, only: velocity_min, velocity_max, IONS, true_ion,
     +  num_components, trial_param_cov

        implicit none

        integer(kind=singleI) :: j
        integer(kind=singleI), intent(out) :: tot_param, var_param
        integer(kind=singleI) :: component_index(*), varying_params(*)

        real(kind=doubleR) :: par_val(*), par_err(*)

!       initialise relevant variables
        tot_param = 0

!       loop over ALL components; should improve this to speed-up code...

        do j=1,num_components

!       find all components contained in absorption profile within
!       [velocity_min,velocity_max] 


         if (((IONS(true_ion(j))%component(j)%vel_c.ge.floor(velocity_min)).and.
     &   (IONS(true_ion(j))%component(j)%vel_c.le.ceiling(velocity_max)))
     &   .and.(IONS(true_ion(j))%component(j)%status.ne.0)) then

!       increase number of parameters; store component's velocity centroid;
!       store component ID (index)
          tot_param = tot_param + 1
          par_val(tot_param) = IONS(true_ion(j))%component(j)%vel_c
          par_err(tot_param) = IONS(true_ion(j))%component(j)%vel_c_err
          component_index(tot_param) = j

!       increase number of parameters; store component's column density
          tot_param = tot_param + 1
          par_val(tot_param) = IONS(true_ion(j))%component(j)%col_dens
          par_err(tot_param) = IONS(true_ion(j))%component(j)%col_dens_err
         
!       increase number of parameters; store component's b-value
          tot_param = tot_param + 1
          par_val(tot_param) = IONS(true_ion(j))%component(j)%b_value
          par_err(tot_param) = IONS(true_ion(j))%component(j)%b_value_err

!       parameter covariance for component j:

!       velocity -- column density
         trial_param_cov(tot_param-2,tot_param-1) =
     &   IONS(true_ion(j))%component(j)%v_coldens_cov

!       velocity -- b-value
         trial_param_cov(tot_param-2,tot_param) =
     &   IONS(true_ion(j))%component(j)%v_b_cov

!       column density -- b-value
         trial_param_cov(tot_param-1,tot_param) =
     &   IONS(true_ion(j))%component(j)%coldens_b_cov

!       change status of first-guess components (3) in current absorption
!       profile to 1, i.e. 'valid / working'

          if (IONS(true_ion(j))%component(j)%status.eq.3)
     &    IONS(true_ion(j))%component(j)%status = 1

         end if ! valid component within current absorption profile
        
        end do

!       tag parameters over which to minimise (default: all)
!       if varying_params = 0, parameter value is kept constant during MRQMIN
!       minimisation; otherwise it is varied so as to minimise chi2

        do j=1,tot_param
         varying_params(j) = 1
        end do

!       Define number of varying parameters (default: all)
        var_param = tot_param

!       ------------------------------------------------------------------------
        return

        end subroutine load_fit_parameters
!       ========================================================================

!       ========================================================================
        subroutine funcs(veloc,par_val,mod_flux,dfdp,np)

!       NOTE:
!       find value of flux f and derivatives dfdp at veloc
!       given the presence of np/3 Voigt profile components
!       for component i
!       par_val(3(i-1)+1) => velocity centre
!       par_val(3(i-1)+2) => column density
!       par_val(3i) => b-value

!       Call chain:
!       ifitnot -> mrqmin -> mrqcof -> funcs

        use set_precision
        use ifit_variables
        use constants

        implicit none

        integer(kind=singleI) :: np
        integer(kind=singleI) :: i
        real(kind=doubleR) :: veloc, mod_flux, dfdp(np), par_val(np)
        real(kind=doubleR) :: model_at_px, param_derivatives        ! functions

!       check parameters for out of bounds
!       NOTE: NEED TO LOOK FOR AN ALTERNATIVE SINCE REPEATED CALLS TO THIS
!       ROUTINE SLOW DOWN THE CODE DRAMATICALLY WHEN ABSORPTION PROFILE SIZE IS
!       LARGER THAN 1000 pixel
        call parameter_boundaries(par_val,np)

!       Q: Why should parameters be out of bounds anyway???
!       A: Because mrqmin does not know anything about physical bounds, it just
!       looks for the parameter values that minimise the chi^2 value
        
!       compute model flux at veloc (output)
!       if convolution with instrumental LSF is to be taken into account, this
!       has to be done at this step in function model_at_px

        mod_flux = model_at_px(par_val,np,veloc,absorption_type)

!       compute derivatives at veloc wrt each parameter

        do i=1,np
         dfdp(i) = param_derivatives(par_val,np,veloc,i)
        end do

!       NOTE:
!       If all derivates are 0 (to double precision), the code will
!       crash at one or another point, so avoid this AND find out why
!       this happens -> probably because for large column densities,
!       the flux goes to zero, or for very small column densities it is one
!       everywhere...

!       NOTE: COMMENTED OUT TO SPEED UP CODE; SHOULD LOOK FOR AN ALTERNATIVE...
!       if (all(abs(dfdp).le.tiny(real_double))) then
!        if (debug) then
!         write(6,*)
!         write(6,*) trim(warnmsg)//'funcs: all derivatives vanish!'
!         write(6,*) 'funcs:', mod_flux, ceiling(dble(np)/3.0d0)
!         write(6,*) 'funcs:', (dlog10(par_val(i)),i=2,np,3)
!        end if
!       end if

!       NOTE:
!       here, call subroutine that computes analytical
!       derivatives of flux f with respect to (vel_c,N,b) at v

!       ------------------------------------------------------------------------
        return

        end subroutine funcs
!       ========================================================================

!       ========================================================================
        function param_derivatives(par_val,np,veloc,ip)

!       NOTE:
!       computes derivative of model at veloc wrt parameter ip from list
!       par_val.
!       compute derivative numerically on successively smaller intervals until
!       convergence is achieved

!       note that for component i
!       par_val(3(i-1)+1) => velocity centre
!       par_val(3(i-1)+2) => column density
!       par_val(3i) => b-value

!       NOTE:
!       TO DO: Use Savitzky-Golay smoothing (see smooth_sg) prior to
!       compute the first derivatives

!       OR: use routine dfridr(func,x,h,err) (Num.Rec. Chap. 5.7)

        use set_precision
        use ifit_variables
        use constants

        implicit none

        real(kind=doubleR) :: param_derivatives

        integer(kind=singleI) :: np, ip

        real(kind=doubleR) :: veloc, par_val(np)
        real(kind=doubleR) :: der_increment, par_val_save
        real(kind=doubleR) :: modelhi, modellow, model_at_px

!       initialise
        param_derivatives = 0.0d0
        der_increment = 0.0d0

!       save original value of parameter
        par_val_save = par_val(ip)

!       compute parameter variation; velocity is tied to a fixed value;
!       velocity / b-value variation on the order of 0.1 m/s

        if (mod(ip,3).eq.1)
     &  der_increment = der_minimum*pixel_size_kms*1.0d2   ! vel_c

        if (mod(ip,3).eq.2)
     &  der_increment = der_minimum*par_val(ip)            ! col.dens.

        if (mod(ip,3).eq.0)
     &  der_increment = der_minimum*par_val(ip)*1.0d2      ! bvalue

        if (der_increment.lt.tiny(real_double)) der_increment = der_minimum

!       compute first guess at derivative

        par_val(ip) = par_val_save+der_increment
        modelhi = model_at_px(par_val,np,veloc,absorption_type)
        
        par_val(ip) = par_val_save-der_increment
        modellow = model_at_px(par_val,np,veloc,absorption_type)

!       compute first derivative
        
        if (der_increment.ne.0.0d0)
     &  param_derivatives =
     &  0.5d0*(modelhi/der_increment - modellow/der_increment)
        
!       NOTE:
!       avoid zero values in derivatives; set to minimum single real value
        if (abs(param_derivatives).lt.der_minimum**5) then
         param_derivatives = sign(der_minimum**5,param_derivatives)
                                            ! preserve sign; sign(x,y) transfers
                                            ! sign from y to x
        end if

!       reset original value of par_val
        par_val(ip) = par_val_save

!       ------------------------------------------------------------------------
        return

        end function param_derivatives
!       ========================================================================

!       ========================================================================
        subroutine funcs_new(veloc,par_val,f,dfdp,np)

!       NOT YET USED!

        use set_precision
        use ifit_variables
        use constants

        implicit none

        integer(kind=singleI) :: np
        integer(kind=singleI) :: i
        real(kind=doubleR) :: veloc,f,dfdp(np),par_val(np)
        real(kind=doubleR) :: model_derivatives

!       check parameters for out of bounds

        call parameter_boundaries(par_val,np)

!       At velocity v, compute flux f, optical depth tau, and distance
!       to component centre in Doppler units x = dv

        f = 0.0d0
!       f = model_at_px(par_val,np,veloc,absorption_type)
!       tau = model_spectrum(par_val,np,veloc,tau)
!       dv = model_spectrum(par_val,np,veloc,dv)

!       At velocity v, compute derivate of flux f with respect to
!       component centroid vel_c, column density col_dens, and bvalue


!       My changes: here, call subroutine that computes analytical
!       derivates of flux f with respect to (vel_c,N,b) at v

        do i=1,np
         dfdp(i) = model_derivatives(par_val,np,veloc,i)
        end do

        return
        end subroutine funcs_new
!       ========================================================================

!       ========================================================================
        function model_derivatives(param,num_par,veloc,par_id)

!       NOT YET USED!

!       Returns the (analytically computed) derivative of:
!       of flux f with respect to component centroid vel_c, column density,
!       and bvalue

!       USES flux_functions

        use set_precision
        use ifit_variables
        use constants
        implicit none

        integer(kind=singleI) :: num_par, par_id
        real(kind=doubleR) :: veloc, param(num_par)
        real(kind=doubleR) :: model_derivatives
        
        model_derivatives = 0.0d0
        par_id = 0
        param = 0.0d0
        veloc = 0.0d0

!       ------------------------------------------------------------------------
        return

        end function model_derivatives
!       ========================================================================

!       ========================================================================
        subroutine parameter_uncertainty(params)

!       computes the parameter errors using the diagonal elements of the cova-
!       riance matrix returned by mrqmin when setting lambda=0
        
        use set_precision
        use ifit_variables

        implicit none

        integer(kind=singleI) :: i, params
        
        character(len=22) :: routinemsg = 'parameter_uncertainty:'

!       ------------------------------------------------------------------------
!       print routine name for information
        write(6,*)
        write(6,*)
        write(6,'(1x,a)') trim(colour_statmsg_prefix)//
     &  trim(routinemsg)//trim(colour_msg_suffix)//
     &   'computing covariance (error) matrix'

!       ------------------------------------------------------------------------
!       set lambda_mrqmin=0 to signal mrqmin to compute covariance matrix

        lambda_mrqmin = 0.0d0

!       final call to mrqmin
        call mrqmin(xdata_in,ydata_in,yerror_in,absprofile_size_px,trial_param,
     &  num_param,param_to_min,COVAR,ALPHA,max_num_param,chi2_current,
     &  lambda_mrqmin)

!       approximate (formal) parameter errors by square root of diagonal
!       elements of covariance matrix
        do i=1,params,3
         trial_param_err(i) = dsqrt(dabs(COVAR(i,i)))       ! velocity error
         trial_param_err(i+1) = dsqrt(dabs(COVAR(i+1,i+1))) ! col.dens. error
         trial_param_err(i+2) = dsqrt(dabs(COVAR(i+2,i+2))) ! b-value error
        end do

!       parameter covariance; need only the upper half of matrix COVAR
        do i=1,params,3
         trial_param_cov(i,i+1) = COVAR(i,i+1)        ! vel-col.dens. cov.
         trial_param_cov(i,i+2) = COVAR(i,i+2)        ! vel-b cov.
         trial_param_cov(i+1,i+2) = COVAR(i+1,i+2)    ! col.dens.-b cov.
        end do

!       TO DO: COMPUTE AND STORE COVARIANCE OF PARAMETERS, e.g., cov(N,b)

!       ------------------------------------------------------------------------
        return

        end subroutine parameter_uncertainty
!       ========================================================================

!       ========================================================================
        subroutine compute_rest_ew_width(cycl)
        
!       compute *deconvolved*, rest-frame (wlength(j)/lambda_0) equivalent width
!       of each component *in Angstroem*;
!       this is fine since EqW should be conserved by convolution;
!       could compute both and check!
        
        use set_precision
        use ifit_variables, only: IONS, ION_INDEX, true_ion, num_ions, wlength,
     +  modelflux, spectrum_size_px, true_transition, num_components,
     +  colour_statmsg_prefix, colour_msg_suffix

        implicit none
        
        integer(kind=singleI), intent(in) :: cycl

        integer(kind=singleI) :: j, k
        integer(kind=singleI) :: ionindex

        character(len=22) :: routinemsg = 'compute_rest_ew_width:'

!       ------------------------------------------------------------------------
!       print routine name for information
        write(6,'(1x,a)',advance='no') trim(colour_statmsg_prefix)//
     &  trim(routinemsg)//trim(colour_msg_suffix)

!       ------------------------------------------------------------------------
!       initialise data elements
        if (cycl.le.2) then ! during sweep=1,2

         IONS(ION_INDEX)%component(:)%eq_width = 0.0d0

        else ! during sweep=3

         do ionindex=1,num_ions
          IONS(ionindex)%component(:)%eq_width = 0.0d0
         end do

        end if

        do k=1,num_components

!       select valid components
         if (abs(IONS(true_ion(k))%component(k)%status).eq.1) then

          modelflux(1:spectrum_size_px) = 1.0d0

          call model_spectrum(k,k,1,spectrum_size_px) 

          do j=2,spectrum_size_px

           IONS(true_ion(k))%component(k)%eq_width =
     &     IONS(true_ion(k))%component(k)%eq_width + 
     &     (wlength(j)-wlength(j-1))*(1.0d0-modelflux(j)) /
     &     (wlength(j) /
     &     IONS(true_ion(k))%transition(true_transition(k))%lambda_0)

          end do

         end if ! valid components
        
        end do !k-loop; over components

!       ------------------------------------------------------------------------
!       output info
        write(6,*) 'done.'

!       ------------------------------------------------------------------------
        return

        end subroutine compute_rest_ew_width
!       ========================================================================

!       ========================================================================
        subroutine compute_model(ionindex)

        use set_precision
        use ifit_variables
        use constants, only: c_kms

        implicit none

        integer(kind=singleI), intent(in) :: ionindex
        integer(kind=singleI) :: comp_id, pixel

        real(kind=doubleR) :: pixel_size_voc = 1.0d0
        integer(kind=singleI) :: spectrum_size_voc

        real(kind=doubleR), allocatable :: vel_binned(:)
        real(kind=doubleR), allocatable :: flux_binned(:), flux_binned_conv(:)
        real(kind=doubleR), allocatable :: d2flux_dw2(:)

        character(len=14) :: routinemsg = 'compute_model:'

!       ------------------------------------------------------------------------
!       print routine name for information
        write(6,'(1x,a)',advance='no') trim(colour_statmsg_prefix)//
     &  trim(routinemsg)//trim(colour_msg_suffix)

!       ------------------------------------------------------------------------
!       Compute final synthetic spectrum using line parameters

!       Initializing flux
        modelflux(:)=1.0d0

!       recall: num_components is the TOTAL (also discarded) number of lines
        do comp_id=1,num_components

!       select valid component; compute absorption component
         if (abs(IONS(true_ion(comp_id))%component(comp_id)%status).eq.1)
     &    call model_spectrum(comp_id,comp_id,1,spectrum_size_px) 
        
        end do

!       ------------------------------------------------------------------------
        IF (fwhm_kms.gt.0.0d0) THEN
!       ------------------------------------------------------------------------

!       IMPORTANT: previous to computing the convolution, resample the spectrum
!       onto pixel of constant size (in v/c space); then convolve, and then
!       remap back to original resolution

!       pixel_size_voc is set to 1.0d0; but choose average pixel size if smaller

         pixel_size_voc = min(pixel_size_voc,pixel_size_kms)

!       new spectrum size

         spectrum_size_voc =
     &   ceiling((velocity(spectrum_size_px)-velocity(1))/pixel_size_voc)

!       allocate data elements
         allocate(flux_binned(1:spectrum_size_voc))
         allocate(flux_binned_conv(1:spectrum_size_voc))
         allocate(vel_binned(1:spectrum_size_voc))
         flux_binned(:) = 0.0d0
         flux_binned_conv(:) = 0.0d0
         vel_binned(:) = 0.0d0

         if (allocated(d2flux_dw2)) deallocate(d2flux_dw2)
         allocate(d2flux_dw2(1:spectrum_size_px))
         d2flux_dw2(:) = 0.0d0

!       output info
          write(6,*)
          write(6,'(a,1pe12.4)')
     &    're-binning spectrum to pixel size [v/c]: ', pixel_size_voc

!       ------------------------------------------------------------------------
!       map model spectrum onto new v/c baseline, point by point

!       compute second-derivative of model spectrum at each velocity point
         call
     &   spline(velocity,modelflux,spectrum_size_px,1.0d31,1.0d31,d2flux_dw2)

         do pixel=1,spectrum_size_voc

!       compute new velocity point
          vel_binned(pixel) = dble(pixel-1) * pixel_size_voc

!       interpolate flux at new wavelength
          call
     &    splint(velocity,modelflux,d2flux_dw2,spectrum_size_px,
     &    vel_binned(pixel),flux_binned(pixel))

         end do ! over v/c pixels

!       ------------------------------------------------------------------------
!       convolve model spectrum with instrumental LSF (if fwhm_kms > 0)

!       output info
         write(6,'(a,1pe12.4)')
     &   'convolving spectrum with instrumental profile [km/s]: ', fwhm_kms

         flux_binned_conv(:) = flux_binned(:)

         call
     &   gauss_conv(flux_binned_conv,1,spectrum_size_voc,pixel_size_voc,
     &   fwhm_kms)

!       ------------------------------------------------------------------------
!       map *convolved* spectrum back onto old v/c baseline, point by point

!       output info
         write(6,'(a,1pe12.4)')
     &   're-binning spectrum back to pixel size [v/c]: ', pixel_size_kms

         if (allocated(d2flux_dw2)) deallocate(d2flux_dw2)
         allocate(d2flux_dw2(1:spectrum_size_voc))
         d2flux_dw2(:) = 0.0d0

!       compute second-derivative of model flux at each velocity point
         call
     &   spline(vel_binned,flux_binned_conv,spectrum_size_voc,
     &   1.0d31,1.0d31,d2flux_dw2)

         do pixel=1,spectrum_size_px

          call
     &    splint(vel_binned,flux_binned_conv,d2flux_dw2,spectrum_size_voc,
     &    velocity(pixel),modelflux_convolved(pixel))

         end do ! over old v/c pixels


!       ------------------------------------------------------------------------
        ELSE ! no convolution
!       ------------------------------------------------------------------------

         modelflux_convolved(:) = modelflux(:)

!       ------------------------------------------------------------------------
        END IF ! interpolate and convolve spectrum?
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       if spectrum with periodic boundary conditions was shifted, transform
!       spectrum back to original reference frame

        if (spectrum_shifted) then

         flux = cshift(flux,-spectrum_shift_px)

         sigma = cshift(sigma,-spectrum_shift_px)

         modelflux_convolved = cshift(modelflux_convolved,-spectrum_shift_px)

         modelflux = cshift(modelflux,-spectrum_shift_px)

        end if
!       ------------------------------------------------------------------------
!       Create ascii file with wavelength, flux (fit), noise

        open(40,file=trim(ifit_file_prefix(ionindex))//'_model.fit')
        
         do pixel=1,spectrum_size_px  !loop over wl range
          write(40,*) wlength(pixel), modelflux_convolved(pixel), sigma(pixel),
     &    flux(pixel), velocity(pixel), modelflux(pixel)
         end do

        close(40)

!       ------------------------------------------------------------------------
!       output info
        write(6,*) 'done.'

!       ------------------------------------------------------------------------
        return

        end subroutine compute_model
!       ========================================================================

!       ========================================================================
        subroutine generate_linelist(ionindex,cycl)

        use set_precision
        use ifit_variables
        use constants, only: c_kms

        implicit none

        integer(kind=singleI), intent(in) :: ionindex, cycl

        integer(kind=singleI) :: num_comp
        integer(kind=singleI) :: i

!       to circularly shift spectra
        integer(kind=singleI) :: velocity_shift_px
        integer(kind=singleI), dimension(1) :: arr_pos_min
        real(kind=doubleR) :: dv_shift = 0.0d0

        real(kind=doubleR) :: zmin

        character(len=18) :: routinemsg = 'generate_linelist:'

!       ------------------------------------------------------------------------
!       print routine name for information
        write(6,'(1x,a)',advance='no') trim(colour_statmsg_prefix)//
     &  trim(routinemsg)//trim(colour_msg_suffix)

!       ------------------------------------------------------------------------
!       write file with line parameters

        if (allocated(wl_comp)) deallocate(wl_comp)
        if (allocated(z_abs_comp)) deallocate(z_abs_comp)

        allocate(wl_comp(1:num_components))
        allocate(z_abs_comp(1:num_components))
        wl_comp(1:num_components) = 0.0d0
        z_abs_comp(1:num_components) = 0.0d0

!       set file name
        ifit_lines(ionindex)=trim(ifit_file_prefix(ionindex))//
     &  '_lines.fit'

!       compute number of valid lines (of all ions)

        if (cycl.le.2) then ! during sweep=1,2

         num_comp = count(abs(IONS(ION_INDEX)%component(:)%status).eq.1)

        else ! during sweep=3

         num_comp = 0
         do i=1,num_ions
          num_comp = num_comp +
     &    count(abs(IONS(i)%component(:)%status).eq.1)
         end do

        end if

!       ------------------------------------------------------------------------
        open(50,file=trim(ifit_lines(ionindex)))

!       NOTE:
!       supress output irrelevant for testing        
        IF (.not.test_run) THEN

!       write file content info
         write(50,'(a)') '# file contains (in that order):'
         write(50,'(a)') '# lines  <S/N>  FWHM [km/s] line-profile'
         write(50,'(a)') '# bad-fit(T/F)   <chi2-value> '
         write(50,'(a)') '# spectrum-shifted(T/F)   shift [pixel]'
         write(50,'(a)') '# lim.equivalent width [mA] lim.col.dens (log10)'
         write(50,'(a)') '# ion | wl_0 | z_abs | wl_abs | log10(N) | b-value |'
     &   //' EqW [mA] | v_c |  err(N) | err(b) | err(v_c) | shifted wl_abs | '//
     &   'shifted v_c'

!       NOTE: Equivalent width in mA
!       Column densities given in log10, associated errors in linear scale!

!       number of fitted components, minimum redshift, overal S/N,
!       instrumental broadening FWHM [km/s], line profile

         write(50,'(1x,i5,2(2x,f14.6),2x,a)')
     &   num_comp, average_sn_global, fwhm_kms, profile_str

!       fit quality
         write(50,*)  trim(check_fit_str), mean_chi2

!       spectrum shift
         write(50,*)  trim(spectrum_shifted_str), spectrum_shift_px

!       formal completeness limit
         write(50,*)  equiv_width_lim_mA, dlog10(col_dens_lim)
         
        ELSE
!       useful for testing code performace

         write(6,'(1x,a)') trim(warnmsg)
         write(6,'(1x,a)') 'test_run modus on! Can lead to'//
     &   ' EXTERNAL code crash!'
         write(6,*)

        END IF !test_run output
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        SELECT CASE(spectrum_shifted)

!       ------------------------------------------------------------------------
        CASE(.false.)
!       if spectrum has not been cyclically shifted:
!       Note that component velocity/wavelength centroid are written twice;
!       it's redundant, but it doesn't harm...

         do i=1,num_components

!       select valid component
          if (abs(IONS(true_ion(i))%component(i)%status).eq.1) then

!       transition's least readshift
          zmin = z_min(true_ion(i),true_transition(i))

!       central absorption wavelength of i-th component

          wl_comp(i) =
     &    IONS(true_ion(i))%transition(true_transition(i))%lambda_0
     &    * (1.0d0+zmin) *
     &    dexp(IONS(true_ion(i))%component(i)%vel_c/c_kms)

!       central absorption redshift of i-th component

          z_abs_comp(i) = ((1.0d0+zmin) *
     &    dexp(IONS(true_ion(i))%component(i)%vel_c/c_kms)) - 1.0d0

!       NOTE: Equivalent width in mA
!       Column densities given in log10, associated errors in linear scale!

           write(50,*)  IONS(true_ion(i))%name,
     &     IONS(true_ion(i))%transition(true_transition(i))%lambda_0,
     &     z_abs_comp(i), wl_comp(i),
     &     dlog10(IONS(true_ion(i))%component(i)%col_dens),
     &     IONS(true_ion(i))%component(i)%b_value,
     &     1.0d3*IONS(true_ion(i))%component(i)%eq_width, ! in mili-Angst.
     &     IONS(true_ion(i))%component(i)%vel_c,
     &     IONS(true_ion(i))%component(i)%col_dens_err,
     &     IONS(true_ion(i))%component(i)%b_value_err,
     &     IONS(true_ion(i))%component(i)%vel_c_err,
     &     IONS(true_ion(i))%transition(true_transition(i))%lambda_0 *
     &     (1.0d0+zmin) * dexp(IONS(true_ion(i))%component(i)%vel_c/c_kms),
     &     IONS(true_ion(i))%component(i)%vel_c

          end if ! valid component

         end do ! over components

!       ------------------------------------------------------------------------
        CASE(.true.)
!       if spectrum has been cyclically shifted:

         do i=1,num_components

!       select valid component
          if (abs(IONS(true_ion(i))%component(i)%status).eq.1) then

!       locate pixel corresponding to fitted component centroid with respect
!       to UN-SHIFTED velocity array:
          
           arr_pos_min =
     &     maxloc(velocity,(velocity.le.IONS(true_ion(i))%component(i)%vel_c))

!       NOTE:
!       shift component centroid to original position, i.e. before shifting
!       spectrum; compute the off-set 'dv_shift' between fitted
!       component centroid and velocity bin.
!       Note that dv_shift >= 0 by definition

           velocity_shift_px =
     &     mod(arr_pos_min(1)+spectrum_shift_px,spectrum_size_px)

           dv_shift =
     &     IONS(true_ion(i))%component(i)%vel_c - velocity(arr_pos_min(1))

!       NOTE:
!       Correct for the case when vel_shift happens to be 0, implying that
!       component centroid is at upper spectrum boundary; redefine dv_shift
!       to be negative!

           if (velocity_shift_px.eq.0) then
            velocity_shift_px = spectrum_size_px
            dv_shift = -1.0d0*dv_shift
           end if

!       transition's least readshift
         zmin = z_min(true_ion(i),true_transition(i))

!       true central absorption wavelength of i-th component

           wl_comp(i) =
     &     IONS(true_ion(i))%transition(true_transition(i))%lambda_0 *
     &     (1.0d0+zmin) * dexp((velocity(velocity_shift_px)+dv_shift)/c_kms)

!       true central absorption redshift of i-th component

           z_abs_comp(i) = ((1.0d0+zmin) *
     &     dexp((velocity(velocity_shift_px)+dv_shift)/c_kms)) - 1.0d0

!       NOTE: Equivalent width in mA
!       Column densities given in log10, associated errors in linear scale!

           write(50,*)
           write(50,*)  IONS(true_ion(i))%name,
     &     IONS(true_ion(i))%transition(true_transition(i))%lambda_0,
     &     z_abs_comp(i),                              ! true abs. redshift
     &     wl_comp(i),                                 ! true abs. wavelength
     &     dlog10(IONS(true_ion(i))%component(i)%col_dens),
     &     IONS(true_ion(i))%component(i)%b_value,
     &     1.0d3*IONS(true_ion(i))%component(i)%eq_width, ! in mili-Angst.
     &     velocity(velocity_shift_px)+dv_shift,       ! true velocity centre
     &     IONS(true_ion(i))%component(i)%col_dens_err,
     &     IONS(true_ion(i))%component(i)%b_value_err,
     &     IONS(true_ion(i))%component(i)%vel_c_err,

!         shifted wavelength centre
     &     IONS(true_ion(i))%transition(true_transition(i))%lambda_0 *
     &     (1.0d0+zmin) * dexp(IONS(true_ion(i))%component(i)%vel_c/c_kms),

!         shifted velocity centre
     &     IONS(true_ion(i))%component(i)%vel_c

          end if ! valid component

         end do ! over components

        END SELECT ! spectrum shifted?
!       ------------------------------------------------------------------------

        close(50)

!       ------------------------------------------------------------------------
!       output info
        write(6,*) 'done.'
        write(6,*)

!       ------------------------------------------------------------------------
!       free memory
        deallocate(wl_comp)
        deallocate(z_abs_comp)

!       ------------------------------------------------------------------------
        return

        end subroutine generate_linelist
!       ========================================================================

!       ========================================================================
        subroutine search_multiplets()

!       identifies members of a multiplet and links these to the strongest (in
!       terms of tau_0) member present in the spectrum
!       assigns the true (i.e., correct) transition parameters to each component

!       lead(i)%component and lead(i)%transition give the index
!       of the component and the index of the transition to which component i is
!       associated  
!
!       true_transition(i) gives the index of the i component's actual
!       transition 
!
!       true_ion(i) gives the index of the i component's actual ion

!       IMPORTANT: STILL NEED TO TAKE CARE OF LINES WITH LARGE ERRORS IN
!       N,b,vel_c THAT MAY LEAD TO MISIDENTIFICATIONS

!       NOTE:
!
!       components corresponding to multiplets of a given ion have to meet the
!       following conditions:
!
!       1) they must have the same redshift (within a given accuracy)
!       2) their absorption strength relative to e.g., the strongest transition,
!         has a precise value
!       3) their velocity difference must be significant (within a given
!         accuracy) 

!
!       While condition 1 can be easily tested (with the accuracy given by the
!       wavelength / velocity calibration of the input spectrum, condition 2 can
!       be difficoult to test due to contamination (blending) with lines other
!       than those corresponding to the considered transition. Here, however,
!       this is not an issue since the fit yields "deblended" components.
!
!       IDEA: when fitting taking into account all ions (and their
!       transition[s]), do this on a component-by-component basis, thus keeping
!       the fitted wavelength range to a minimum (assuming the transitions of a
!       given ion are not far apart from each other in wavelenght space)

        use set_precision
        use ifit_variables, only: ion, IONS, ION_INDEX, strongest,
     +  lead, num_ions, true_transition, true_ion, z_min,
     +  num_components, colour_statmsg_prefix, colour_errmsg_prefix,
     +  colour_msg_suffix, debug, test_run, pixel_size_kms,
     +  unidentified_components

        use constants, only: c_kms

        implicit none

        type(ion), allocatable :: IONS_AUX(:) ! main working auxiliary structure

        integer(kind=singleI) :: ionindex, indx
        integer(kind=singleI), allocatable :: num_comp_ion(:)
        integer(kind=singleI), allocatable :: comp_id_ion(:,:)
        integer(kind=singleI), allocatable :: true_trans_aux(:)
        integer(kind=singleI), allocatable :: lead_component(:)

        integer(kind=singleI) :: strong, weak, strong_id(1)
        integer(kind=singleI) :: trans_strong, trans_weak

        real(kind=doubleR) :: wl_rest_strong, wl_rest_weak
        real(kind=doubleR) :: z_strong, z_weak
        real(kind=doubleR) :: z_std_dev, z_strong_err, z_weak_err

        real(kind=doubleR) :: N_strong, N_weak
        real(kind=doubleR) :: N_strong_err, N_weak_err

        real(kind=doubleR) :: b_strong, b_weak
        real(kind=doubleR) :: b_strong_err, b_weak_err

        real(kind=doubleR) :: Nb_cov_strong, Nb_cov_weak

        real(kind=doubleR) :: vel_strong, vel_weak
        real(kind=doubleR) :: vel_std_dev, vel_strong_err, vel_weak_err

        real(kind=doubleR) :: tau_0_strong, tau_0_weak
        real(kind=doubleR) :: tau_c_strong, tau_c_weak
        real(kind=doubleR) :: tau_c_strong_variance, tau_c_weak_variance
        real(kind=doubleR) :: tau_c_ratio_theor, tau_c_ratio
        real(kind=doubleR) :: tau_c_ratio_std_dev

!       the following tolerance factors are used to increase the uncertainty of
!       the corresponding quantity (tau_c_ratio, z), since the latter are
!       usually too low thus leading to no positive cross-identification;
!       note that, while a factor 10 may seem large, the difference in z and
!       tau_c_ratio of members of different multiplets is usually at least a few
!       orders of magnitude apart from the expected value
!      
        real(kind=doubleR), parameter :: z_tolerance=1.0d1
        real(kind=doubleR), parameter :: tau_c_ratio_tolerance=1.0d1

        real(kind=doubleR) :: zmin

        character(len=21) :: routinemsg = 'search_multiplets: '

!       ------------------------------------------------------------------------
!       skip when testing code (for now)
        if (test_run) return

!       ------------------------------------------------------------------------
!       IMPORTANT: NOTE THAT, SINCE ALL COMPONENTS ARE INITIALLY FITTED ASSUMING
!       THEY CORRESPOND TO THE STRONGEST TRANSITION OF THE FIRST ION IN INPUT
!       LIST, WHENEVER REFERRING TO THESE COMPONENTS THE (CONSTANT-VALUE=1)
!       'ION_INDEX' IS USED INSTEAD OF THE (VARIABLE) 'ionindex'
!
!       ------------------------------------------------------------------------
!       allocate and initialise structure to tag components belonging to a
!       multiplet and their corresponding transtion

        if (allocated(lead)) deallocate(lead)
        if (allocated(true_transition)) deallocate(true_transition)
        if (allocated(true_trans_aux)) deallocate(true_trans_aux)
        if (allocated(true_ion)) deallocate(true_ion)

        allocate(lead(1:num_components))
        allocate(true_transition(1:num_components))
        allocate(true_trans_aux(1:num_components))
        allocate(true_ion(1:num_components))
        allocate(lead_component(1:num_components))

        lead(1:num_components)%component = 0
        lead(1:num_components)%transition = 0
!       auxiliary / working structure
        lead_component(1:num_components) = 0

!       default value: strongest transition of first ion on input list
        true_transition(1:num_components) = strongest(ION_INDEX,1)
!       auxiliary / working structure
        true_trans_aux(1:num_components) = 0

!       default value: first ion on input list
        true_ion(1:num_components) = ION_INDEX

        if (allocated(num_comp_ion)) deallocate(num_comp_ion)
        if (allocated(comp_id_ion)) deallocate(comp_id_ion)
        allocate(num_comp_ion(1:num_ions))
        allocate(comp_id_ion(1:num_ions,1:num_components))

!       auxiliary / working structure
        allocate(IONS_AUX(1:num_ions))

!       number of components per ion
        num_comp_ion(1:num_ions) = 0

!       components index per ion
        comp_id_ion(1:num_ions,1:num_components) = 0

!       ------------------------------------------------------------------------
!       loop over all ions
        DO ionindex=1,num_ions
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       print routine name for information
         write(6,'(1x,a)') trim(colour_statmsg_prefix)//
     &   trim(routinemsg)//trim(colour_msg_suffix)//' processing ion '//
     &   trim(IONS(ionindex)%name)
         write(6,*) 'valid lines ',
     &   count(abs(IONS(ION_INDEX)%component(1:num_components)%status).eq.1)


!       ------------------------------------------------------------------------
!       loop over all transitions of given ion, in order of decresing strength;
!       the latter is ensured by the use of the indexed array strongest(i,j),
!       which stores the index (as given in input list) of the j-th strongest
!       transition of the i-th ion

        DO trans_strong=1,IONS(ionindex)%num_transitions
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       loop over assumed *strongest* member component of a multiplet
        do strong=1,num_components
!       ------------------------------------------------------------------------

!       select a valid component, i.e., not yet associated to another component
!       and yet not-discarded

         if ((lead(strong)%component.eq.0).and.
     &   (.not.any(lead(:)%component.eq.strong)).and.
     &   (abs(IONS(ION_INDEX)%component(strong)%status).eq.1)) then

!       centre of stronger transition and uncertainty (velocity scale);
!       the uncertainty includes half the size of a pixel [km/s]

          vel_strong =
     &    IONS(ION_INDEX)%component(strong)%vel_c

          vel_strong_err = dsqrt(
     &    IONS(ION_INDEX)%component(strong)%vel_c_err *
     &    IONS(ION_INDEX)%component(strong)%vel_c_err +
     &       0.25 * pixel_size_kms * pixel_size_kms )

!       restframe wavelength of strongest transition
          wl_rest_strong =
     &    IONS(ionindex)%transition(strongest(ionindex,trans_strong))%lambda_0

!       transition's least redshift
          zmin = z_min(ionindex,strongest(ionindex,trans_strong))

!       redshift of strongest transition and corresponding standard deviation;
!       assumed Gaussian error propagation
          z_strong = ((1.0d0+zmin) *
     &    dexp(vel_strong/c_kms)) - 1.0d0

          z_strong_err = (1.0d0+zmin) *
     &    dexp(vel_strong_err/c_kms) * (vel_strong_err / c_kms)

!       column density and b-value and uncertainty of strongest transition
          N_strong = IONS(ION_INDEX)%component(strong)%col_dens
          N_strong_err = IONS(ION_INDEX)%component(strong)%col_dens_err
          b_strong = IONS(ION_INDEX)%component(strong)%b_value
          b_strong_err = IONS(ION_INDEX)%component(strong)%b_value_err
          Nb_cov_strong = IONS(ION_INDEX)%component(strong)%coldens_b_cov

!       line strength (up to constant factors) of strongest transition
!       and variance;
!       assumed Gaussian error propagation;
!       note that under this assumption the vel-contribution to the error
!       vanishes at the line centre
 
!       IMPORTANT:
!       all line-strengths (`tau_c') here have dimensions (cm^2 / km/s)
!       all `tau_0') here have dimensions (cm^2 / km/s)^-1
!       hence, the product (tau_0 * tau_c) is *dimensionless*

          tau_0_strong =
     &    IONS(ionindex)%transition(strongest(ionindex,trans_strong))%tau_0

          tau_c_strong = N_strong / b_strong

          tau_c_strong_variance = (tau_c_strong)**2.0d0 * (
     &    (b_strong_err / b_strong)**2.0d0 +
     &    (N_strong_err / N_strong)**2.0d0 -
     &    2.0d0 * (Nb_cov_strong / N_strong / b_strong) )

!       ------------------------------------------------------------------------
!       loop over transitions of decreasing strength;
!       (SKIP strongest currently used as reference)
          do trans_weak=trans_strong+1,IONS(ionindex)%num_transitions
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       loop over assumed `weaker' member component(s) of a multiplet
           do weak=1,num_components
!       ------------------------------------------------------------------------

!       select a valid component, i.e., other than the strongest component,
!       not yet associated to another component, and yet not-discarded

!       ------------------------------------------------------------------------
           if ((strong.ne.weak).and. ! takes care of the fact that the *absolut*
                                     ! strongest transition of an ion might be
                                     ! missing in the spectra

     &     (lead(weak)%component.eq.0).and.     ! avoids linking a transition
                                                ! with the immediate stronger
                                                ! thereby ignoring the strongest
                                                ! member of the multiplet
                                                ! *present* in the spectrum

     &     (abs(IONS(ION_INDEX)%component(weak)%status).eq.1)) then
!       ------------------------------------------------------------------------


!       centre of weaker transition and uncertainty (velocity scale);
!       the uncertainty includes half the size of a pixel [km/s]

             vel_weak =
     &       IONS(ION_INDEX)%component(weak)%vel_c
             vel_weak_err = dsqrt(
     &       IONS(ION_INDEX)%component(weak)%vel_c_err *
     &       IONS(ION_INDEX)%component(weak)%vel_c_err +
     &       0.25 * pixel_size_kms * pixel_size_kms)

!       restframe wavelength of weaker transition
             wl_rest_weak =
     &       IONS(ionindex)%transition(strongest(ionindex,trans_weak))%lambda_0

!       redshift of weaker transition and corresponding standard deviation;
!       assumed Gaussian error propagation
             z_weak = ((1.0d0+zmin) * (wl_rest_strong / wl_rest_weak) *
     &      (dexp(vel_weak/c_kms))) - 1.0d0      

             z_weak_err = (1.0d0+zmin) * (wl_rest_strong / wl_rest_weak) *
     &       dexp(vel_weak_err/c_kms) * (vel_weak_err / c_kms)

!       column density and b-value and uncertainty of weaker transition
             N_weak = IONS(ION_INDEX)%component(weak)%col_dens
             N_weak_err = IONS(ION_INDEX)%component(weak)%col_dens_err
             b_weak = IONS(ION_INDEX)%component(weak)%b_value
             b_weak_err = IONS(ION_INDEX)%component(weak)%b_value_err
             Nb_cov_weak = IONS(ION_INDEX)%component(weak)%coldens_b_cov

!       line strength (up to constant factors) of weaker transition
!       and variance;
!       assumed Gaussian error propagation;
!       note that under this assumption the vel-contribution to the error
!       vanishes at the line centre

             tau_0_weak =
     &       IONS(ionindex)%transition(strongest(ionindex,trans_weak))%tau_0

             tau_c_weak = N_weak / b_weak

             tau_c_weak_variance = (tau_c_weak)**2.0d0 * (
     &       (N_weak_err / N_weak)**2.0d0 +
     &       (b_weak_err / b_weak)**2.0d0 -
     &       2.0d0 * (Nb_cov_weak / N_weak / b_weak) )

!       ------------------------------------------------------------------------
!       CONDITION 1: matching redshift

!       joint redshift standard deviation;
!       assumed Gaussian error propagation;
!       need a fudge factor to overestimate uncertainty, which is usually too
!       low (LM might be highly underestimating error parameters...)

             z_std_dev = z_tolerance *
     &       dsqrt(z_strong_err * z_strong_err + z_weak_err * z_weak_err)

!       ------------------------------------------------------------------------
!       CONDITION 2: appropriate line-strenght ratio (weaker-to-strongest)

!       theoretical line-strenght ratio (weaker-to-strongest);
!       independent of (N/b), which should be the same for all transitions of
!       a given ion (in a single absorption element)

             tau_c_ratio_theor = tau_0_weak / tau_0_strong

!       measured ratio
             tau_c_ratio = tau_c_weak / tau_c_strong

!       joint line-strength ratio standard deviation;
!       assumed Gaussian error propagation;
!       need a fudge factor to overestimate uncertainty, which is usually too
!       low (LM might be highly underestimating error parameters...)

             tau_c_ratio_std_dev = tau_c_ratio_tolerance * (tau_c_ratio) *
     &       dsqrt(
     &       tau_c_strong_variance /(tau_c_strong)**2.0d0 +
     &       tau_c_weak_variance / (tau_c_weak)**2.0d0 )


!       ------------------------------------------------------------------------
!       CONDITION 3: "anti-blending"

!       joint velocity standard deviation; assumed Gaussian error propagation

             vel_std_dev = dsqrt(
     &       vel_strong_err * vel_strong_err +
     &       vel_weak_err * vel_weak_err )

!       ------------------------------------------------------------------------
!       apply conditions 1 (matching redshift) and 2 (appropriate line-strenght
!       ratio) to identify multiplet members;
!       link index of strongest component to weaker component;
!       save their true transiton

             if (
     &       (abs(z_strong - z_weak).le.z_std_dev)                ! condition 1
     &       .and.
     &       (abs(tau_c_ratio_theor - tau_c_ratio).le.tau_c_ratio_std_dev)  ! 2
     &       .and.
     &       (abs(vel_strong - vel_weak).gt.vel_std_dev)                    ! 3
     &       ) then

              lead(weak)%component = strong

              lead(weak)%transition = strongest(ionindex,trans_strong)

              true_transition(weak) = strongest(ionindex,trans_weak)

              num_comp_ion(ionindex) = num_comp_ion(ionindex) + 1

              comp_id_ion(ionindex,num_comp_ion(ionindex)) = weak

!             end if
!        print*, strong, weak, trans_strong, trans_weak
!        print*, vel_strong_err, vel_weak_err
!        print*, abs(z_strong - z_weak), z_std_dev
!        print*, abs(tau_c_ratio_theor - tau_c_ratio), tau_c_ratio_std_dev
!        print*, tau_c_ratio,  tau_c_ratio_theor

             end if

!       ------------------------------------------------------------------------
            end if ! valid 'weaker' components
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
           end do ! loop over 'weaker' components
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
          end do ! loop over `weaker' transitions
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
         end if ! valid stronger components
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        end do ! loop over stronger components
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        END DO ! loop over stronger transitions
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       check for no positive cross identifications;
!       abort if this is the case (?)

        if ( all(lead(:)%component.eq.0).and.
     &  all(lead(:)%transition.eq.0) ) then

         write(6,'(1x,a)') trim(colour_errmsg_prefix)//
     &  trim(routinemsg)//trim(colour_msg_suffix)
         write(6,'(1x,a)') 'Non cross identifications!'
         write(6,'(1x,a)') 'possible causes:'
         write(6,'(1x,a)') '1) ion '//trim(IONS(ionindex)%name)//
     &   ' was not found, i.e., is not present in spectrum (likely)'
         write(6,'(1x,a)') '2) all components correspond to strongest '//
     &   'transition of ion '//trim(IONS(ionindex)%name)//' (unlikely)'
         write(6,'(1x,a)') '3) the adopted FWHM (fwhm_kms) is not appropriate, or'
         write(6,'(1x,a)') '4) the instrumental broadening is not Gaussian-like'
         write(6,*)
         write(6,'(1x,a)') 'NOTE: set parameter single_ion_fit_str=TRUE'
         write(6,'(1x,a)') '      to perform a single ion (transition) fit'
         write(6,*)
         stop 1
         return

        end if

!       ------------------------------------------------------------------------
!       the strongest transition superordinated to other(s) will (obviously)
!       not have any component nor the appropriate transiton associated to it,
!       so fix it:

         do strong=1,num_components

          if ((lead(strong)%component.eq.0).and.
     &    (lead(strong)%transition.eq.0)) then

!       use any subordinate
           strong_id =
     &     maxloc(lead(:)%component,lead(:)%component.eq.strong)

           if (strong_id(1).gt.0) then ! exclude lines with no subordinates yet
                                       ! IMPORTANT: at the end, these should be
                                       ! tagged as *UNIDENTIFIED*

            lead(strong)%component =
     &      lead(strong_id(1))%component

            lead(strong)%transition =
     &      lead(strong_id(1))%transition

            true_transition(strong) =
     &      lead(strong_id(1))%transition
 
            num_comp_ion(ionindex) = num_comp_ion(ionindex) + 1

            comp_id_ion(ionindex,num_comp_ion(ionindex)) = strong

           else if (abs(IONS(ION_INDEX)%component(strong)%status).eq.1) then
                                                       ! unidentified components
            write(6,*) 'unidentified (ID):', strong

           end if

          end if

         end do

!       ------------------------------------------------------------------------
!       rescale column density to the approximate true column density;
!       assumes fitted b-values are comparable among members of a multiplet

         do indx=1,num_comp_ion(ionindex)

          if
     &    (lead(comp_id_ion(ionindex,indx))%transition.gt.0)
     &    then                                                 ! exclude
                                                               ! unidentified
                                                               ! components

           IONS(ION_INDEX)%component(comp_id_ion(ionindex,indx))%col_dens = 

!       parameters of strongest transition of first listed ion
     &     IONS(ION_INDEX)%component(comp_id_ion(ionindex,indx))%col_dens *
     &     (IONS(ION_INDEX)%transition(strongest(ION_INDEX,1))%f_osc *
     &     IONS(ION_INDEX)%transition(strongest(ION_INDEX,1))%lambda_0) /

!       parameters of actual transition of current ion
     &     (IONS(ionindex)%transition(true_transition(
     &                                    comp_id_ion(ionindex,indx)))%f_osc *
     &     IONS(ionindex)%transition(true_transition(
     &                                    comp_id_ion(ionindex,indx)))%lambda_0)

          end if ! exclude unidentified components

         end do

!       ------------------------------------------------------------------------
!       here: allocate and store the correct parameter values for components of
!       current ion;
!       note that the *total* number of components is conserved at this stage
!       (sweep=3), since components will neither be added nor discarded

!       allocate using total number of components;
!       this is memory demanding but greatly simplifies the code;
!       besides, this auxiliary structure is deallocated at the end of routine

         allocate(IONS_AUX(ionindex)%component(1:num_components))

!       store component parameters; all at once;
!       note the use of the array comp_id_ion which maps the correct component
!       indices of current ion into the auxiliary array

         IONS_AUX(ionindex)%component(1:num_comp_ion(ionindex))%vel_c =
     &   IONS(ION_INDEX)%component(comp_id_ion(ionindex,
     &   1:num_comp_ion(ionindex)))%vel_c

         IONS_AUX(ionindex)%component(1:num_comp_ion(ionindex))%col_dens =
     &   IONS(ION_INDEX)%component(comp_id_ion(ionindex,
     &   1:num_comp_ion(ionindex)))%col_dens

         IONS_AUX(ionindex)%component(1:num_comp_ion(ionindex))%b_value =
     &   IONS(ION_INDEX)%component(comp_id_ion(ionindex,
     &   1:num_comp_ion(ionindex)))%b_value

         IONS_AUX(ionindex)%component(1:num_comp_ion(ionindex))%status =
     &   IONS(ION_INDEX)%component(comp_id_ion(ionindex,
     &   1:num_comp_ion(ionindex)))%status

!       ------------------------------------------------------------------------
        END DO ! over all ions
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       determine number of UNIDENTIFIED components;
!       these are characterised by abs(status)=1 and not being super- or
!       subordinated to any other component,
!       i.e. lead()%transition = 0;
!       they are regarded as corresponding to the first listed ion

         do indx=1,num_components

          if ((abs(IONS(ION_INDEX)%component(indx)%status).eq.1).and.    ! valid
     &    (lead(indx)%transition.eq.0)) then            ! unidentified

!       count these as belonging to the strongest transition of first listed ion

            num_comp_ion(ION_INDEX) = num_comp_ion(ION_INDEX) + 1

            comp_id_ion(ION_INDEX,num_comp_ion(ION_INDEX)) = indx

            unidentified_components = unidentified_components + 1

!       save component parameters into auxiliary structure

            IONS_AUX(ION_INDEX)%component(num_comp_ion(ION_INDEX))%vel_c =
     &      IONS(ION_INDEX)%component(indx)%vel_c

            IONS_AUX(ION_INDEX)%component(num_comp_ion(ION_INDEX))%col_dens =
     &      IONS(ION_INDEX)%component(indx)%col_dens

            IONS_AUX(ION_INDEX)%component(num_comp_ion(ION_INDEX))%b_value =
     &      IONS(ION_INDEX)%component(indx)%b_value

            IONS_AUX(ION_INDEX)%component(num_comp_ion(ION_INDEX))%status =
     &      IONS(ION_INDEX)%component(indx)%status

          end if ! unidentified components

         end do
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       restore component parameters omitting not-valid lines;
!       the idea is to have the components consecutively numbered, which need
!       not be the case if any has been discarded

!       save subordination information
        lead_component(1:num_components) =
     &  lead(1:num_components)%component

        lead(1:num_components)%component = 0

!       save transitions
        true_trans_aux(1:num_components) = true_transition(1:num_components)

!       reset number of valid components
        num_components = 0

        do ionindex=1,num_ions

!       de/re-allocate number of components, with increasing index;
!       this is as memory efficient as possible without (unnecesarily) making
!       the code much more complex...
!       beware of crashes when attempting to access a non-allocated data element
!       note also that this does not affec the rest of the IONS structure, such
!       as its name, mass, transitions, etc.

         if (allocated(IONS(ionindex)%component))
     &   deallocate(IONS(ionindex)%component)
         allocate(
     &   IONS(ionindex)%component(
     &   num_components+1:num_components+num_comp_ion(ionindex)))

         do indx=1,num_comp_ion(ionindex)

!       increase component count
          num_components = num_components + 1

!       store component parameters, one by one

!       velocity centre
          IONS(ionindex)%component(num_components)%vel_c =
     &    IONS_AUX(ionindex)%component(indx)%vel_c

!       column density
          IONS(ionindex)%component(num_components)%col_dens =
     &    IONS_AUX(ionindex)%component(indx)%col_dens

!       b-value
          IONS(ionindex)%component(num_components)%b_value =
     &    IONS_AUX(ionindex)%component(indx)%b_value

!       status (1=valid component)
          IONS(ionindex)%component(num_components)%status =
     &    IONS_AUX(ionindex)%component(indx)%status

!       corresponding ion (as indexed in input list)
          true_ion(num_components) = ionindex

!       corresponding transition (as indexed in input list)
          true_transition(num_components) =
     &    true_trans_aux(comp_id_ion(ionindex,indx))

!       index of superordinated component as given in new orderded list
          strong_id(1) = 0

          strong_id =
     &    maxloc(comp_id_ion(ionindex,:),
     &    (comp_id_ion(ionindex,:).eq.
     &    lead_component(comp_id_ion(ionindex,indx))).and.
     &    lead_component(comp_id_ion(ionindex,indx)).gt.0)

          if (strong_id(1).gt.0)
     &     lead(num_components)%component =
     &     strong_id(1) + sum(num_comp_ion(1:ionindex-1))

         end do

        end do

!       ------------------------------------------------------------------------
!       output info

        write(6,*)
     &  'COMPONENT  LOG10(N) ION  TRANSITION (RANK)   WAVELENGTH [A]   LEADER'
     &  //'   z'

        do indx=1,num_components
         write(6,'(i6,4x,f8.2,3x,a5,2i6,6x,f10.2,i12,4x,f5.3)') indx,
     &   dlog10(IONS(true_ion(indx))%component(indx)%col_dens),
     &   IONS(true_ion(indx))%name, true_transition(indx),
     &   maxloc(strongest(true_ion(indx),:),
     &   strongest(true_ion(indx),:).eq.true_transition(indx)),
     &   IONS(true_ion(indx))%transition(true_transition(indx))%lambda_0,
     &   lead(indx)%component,
     &   (1.0d0+z_min(true_ion(indx),true_transition(indx))) *
     &   dexp(IONS(true_ion(indx))%component(indx)%vel_c/c_kms) - 1.0d0
         end do ! over components

!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       output info and checks

        write(6,*) 'total components ', num_components
        write(6,*) 'valid components of ion:'
        do ionindex=1,num_ions
         write(6,*) trim(IONS(ionindex)%name),
     &   count(abs(IONS(ionindex)%component(:)%status).eq.1)
        end do
        write(6,*) 'identified components ',
     &  count(lead(1:num_components)%component.gt.0)
        write(6,*) 'possible unidentified components ', unidentified_components

!       check sum: total number of components

        if (num_components.ne.sum(num_comp_ion(1:num_ions))) then
         write(6,'(a)') trim(colour_errmsg_prefix)//trim(routinemsg)//
     &   trim(colour_msg_suffix)
         write(6,'(a)') 'Non-matching number of total and recovered components:'
         write(6,*) num_components, sum(num_comp_ion(1:num_ions))
         stop 1
        end if ! check sum

!       ------------------------------------------------------------------------
!       free memory

        if (allocated(IONS_AUX)) deallocate(IONS_AUX)
        if (allocated(true_trans_aux)) deallocate(true_trans_aux)
        if (allocated(lead_component)) deallocate(lead_component)
        if (allocated(num_comp_ion)) deallocate(num_comp_ion)
        if (allocated(comp_id_ion)) deallocate(comp_id_ion)

!       ------------------------------------------------------------------------
        return

        end subroutine search_multiplets
!       ========================================================================

!       ========================================================================
        subroutine write_debug_file(arr,nd,fileprefix,overwrite)

!       writes a file named filename.dbg with a list of the values stored in
!       arr(1:nd)
!       useful for debugging
        
        use set_precision

        implicit none

        integer(kind=singleI) :: i, nd
        real(kind=doubleR) :: arr(*)
        character(len=*) :: fileprefix
        logical :: overwrite
        
        open(300,file=trim(fileprefix)//'.dbg')

        if (.not.overwrite) then

!       read until end of file
         do
          read(300,*,end=310)
         end do
 310     continue
         backspace(300) ! avoid end-of-file (EOF) marker
         write(300,*)
         write(300,*)
        end if

        do i=1,nd
         write(300,*) i, arr(i)
        end do
         
        close(300)

!       ------------------------------------------------------------------------
        return

        end subroutine write_debug_file
!       ========================================================================

!       ========================================================================
        subroutine chi2_stats

!       Prints useful information to the screen

        use set_precision

        use ifit_variables, only: IONS, num_ions,
     +  num_abs_profiles, processed_abs_profiles,
     +  abs_profile_id, degrees_of_freedom, fitted_components,
     +  chi2_current, mean_chi2, min_chi2, max_chi2, check_fit_str,
     +  check_fit, chi2bad_reduced, colour_msg_suffix, colour_warnmsg_prefix,
     +  num_param, velocity_min, velocity_max, output_dashed_line

        implicit none

        integer(kind=singleI) :: ionindex

        logical :: compute_mean

!       Compute average, minimum, and maximum chi^2
          
          if (fitted_components.gt.0)
     &     mean_chi2 = mean_chi2 + (chi2_current/dble(degrees_of_freedom))
          
          if ((chi2_current/dble(degrees_of_freedom)).lt.min_chi2(1)) then
           min_chi2(1) = chi2_current/dble(degrees_of_freedom)
           min_chi2(2) = dble(abs_profile_id)
          end if

          if ((chi2_current/dble(degrees_of_freedom)).gt.max_chi2(1)) then
           max_chi2(1) = chi2_current/dble(degrees_of_freedom)
           max_chi2(2) = dble(abs_profile_id)
          end if

!       compute final average chi^2

          if (processed_abs_profiles.eq.num_abs_profiles) then

           compute_mean = .false.
           do ionindex=1,num_ions
            compute_mean = any(abs(IONS(ionindex)%component(:)%status).eq.1)
            if (compute_mean) exit
           end do

!          if (processed_abs_profiles.eq.num_abs_profiles) then
           if ((num_abs_profiles.gt.0).and.compute_mean) then
            mean_chi2 = dble(mean_chi2)/dble(num_abs_profiles)
           else
            mean_chi2 = 0.0d0
           end if

          end if ! all abs.profiles processed?

!       ------------------------------------------------------------------------
!       Compare final chi^2 (for current absorption profile) to largest
!       acceptable value; give a warning at the end if chi^2 in ANY absorption
!       profile is larger than highest acceptable value chi2bad_reduced 

          if ((chi2_current/dble(degrees_of_freedom)).gt.chi2bad_reduced) then

           check_fit = .true.
           check_fit_str = 'TRUE'

           write(6,*)
           write(6,*)
           write(6,'(a)') trim(output_dashed_line)
           write(6,'(11x,a)')
     &     'RED.CHI^2    DoF   ABS.PROF.ID    VEL.RANGE [km/s]  COMPONENTS'
           write(6,'(1x,a,f8.3,i8,6x,i3,5x,2f10.2,4x,i5)') 
     &     trim(colour_warnmsg_prefix)//'CHECK FIT'//trim(colour_msg_suffix),
     &     (chi2_current/dble(degrees_of_freedom)), degrees_of_freedom,
     &     abs_profile_id, velocity_min, velocity_max, ceiling(dble(num_param) /
     &     3.0d0)
           write(6,'(a)') trim(output_dashed_line)
           write(6,*)
           write(6,*)

          end if

!       ------------------------------------------------------------------------
        return

        end subroutine chi2_stats
!       ========================================================================

!       ========================================================================
        function size_of_file(filename)

!       Returns the number of records in file 'filename'

        use set_precision
        use ifit_variables
        
        implicit none

        integer(kind=singleI) :: size_of_file, records
        character(len=*) :: filename

!       ------------------------------------------------------------------------
!       Open file to get dimension
         
         size_of_file = -1
         records = 0
         open(10,file=trim(filename), status='old')
          do
           read(10, *, end=10)
           records = records + 1
           end do
  10     close(10)

         size_of_file = records
         
         if (size_of_file .le. 0) then
          write(6,*) trim(warnmsg)//'Empty file: ', trim(filename)
         end if

!       ------------------------------------------------------------------------
        return
        
        end function size_of_file
!       ========================================================================

!       ========================================================================
        subroutine alloc_component_param(ionindex,num_comps)

!       NOTE:
!       INVOKE ONLY VARIABLES ACTUALLY BEING USED

        use set_precision
        use ifit_variables, only: IONS,
     +  colour_statmsg_prefix, colour_msg_suffix, quite

        implicit none
        
        integer(kind=singleI), intent(in) :: ionindex, num_comps

        character(len=64) :: routinemsg = 'alloc_component_param: '

!       output info
        if (.not.quite) then
         write(6,*)
         write(6,'(1x,a)') trim(colour_statmsg_prefix)//
     &   trim(routinemsg)//trim(colour_msg_suffix)
         write(6,'(a,i8)')
     &   'allocated size of component parameter arrays: ', num_comps
        end if

!       make sure data are de-allocated
        if (allocated(IONS(ionindex)%component))
     &  deallocate(IONS(ionindex)%component)

!       (re-)allocate
        allocate(IONS(ionindex)%component(1:num_comps))

!       Initialise data elements

        IONS(ionindex)%component(:)%col_dens = 0.0d0
        IONS(ionindex)%component(:)%col_dens_err = 0.0d0
        IONS(ionindex)%component(:)%vel_c = -1.0d0
        IONS(ionindex)%component(:)%vel_c_err = 0.0d0
        IONS(ionindex)%component(:)%b_value = 0.0d0
        IONS(ionindex)%component(:)%b_value_err = 0.0d0
        IONS(ionindex)%component(:)%status = 0

!       parameter covariance
        IONS(ionindex)%component(:)%v_coldens_cov = 0.0d0
        IONS(ionindex)%component(:)%v_b_cov = 0.0d0
        IONS(ionindex)%component(:)%coldens_b_cov = 0.0d0

        end subroutine alloc_component_param
!       ========================================================================

!       ========================================================================
        subroutine alloc_fitting_params(pars)

!       NOTE:
!       INVOKE ONLY VARIABLES ACTUALLY BEING USED

        use set_precision
        use ifit_variables, only: param_to_min, component_id, trial_param,
     +  trial_param_err, trial_param_save, COVAR, ALPHA, colour_statmsg_prefix,
     +  colour_msg_suffix, quite, trial_param_cov

        implicit none
        
        integer(kind=singleI) :: pars

        character(len=64) :: routinemsg = 'alloc_fitting_params: '

!       output info
        if (.not.quite) then
         write(6,*)
         write(6,'(1x,a)') trim(colour_statmsg_prefix)//
     &   trim(routinemsg)//trim(colour_msg_suffix)
         write(6,'(a,i8)')
     &   'allocated size of fit parameter arrays: ', pars
        end if

!       make sure arrays are de-allocated
        if (allocated(param_to_min))  deallocate(param_to_min)
        if (allocated(component_id))  deallocate(component_id)
        if (allocated(trial_param))  deallocate(trial_param)
        if (allocated(trial_param_err))  deallocate(trial_param_err)
        if (allocated(trial_param_cov))  deallocate(trial_param_cov)
        if (allocated(trial_param_save))  deallocate(trial_param_save)
        if (allocated(COVAR))  deallocate(COVAR)
        if (allocated(ALPHA))  deallocate(ALPHA)

!       (re-)allocate
        allocate(param_to_min(1:pars))
        allocate(component_id(1:pars))
        allocate(trial_param(1:pars))
        allocate(trial_param_err(1:pars))
        allocate(trial_param_cov(1:pars,1:pars))
        allocate(trial_param_save(1:pars))
        allocate(COVAR(1:pars,1:pars))
        allocate(ALPHA(1:pars,1:pars))

!       initialise
        param_to_min(:) = 0
        component_id(:) = 0
        trial_param(:) = 0.0d0
        trial_param_err(:) = 0.0d0
        trial_param_cov(:,:) = 0.0d0
        trial_param_save(:) = 0.0d0
        COVAR(:,:) = 0.0d0
        ALPHA(:,:) = 0.0d0

        end subroutine alloc_fitting_params
!       ========================================================================

!       ========================================================================
        subroutine alloc_spec(size)

!       NOTE:
!       INVOKE ONLY VARIABLES ACTUALLY BEING USED

        use set_precision, only: singleI
        
        use ifit_variables, only: flux, aod_data, velocity, modelflux,
     +  modelflux_convolved, flux_smooth, noise, wlength,
     +  modelflux_local, modelflux_local_aux, sigma, unabsorbed_pixel,
     +  dfdv, d2fdv2, linear_chi2_spec, velocity_aux, wlength_aux

        implicit none
        
        integer(kind=singleI) :: size

!       make sure arrays are de-allocated

        if (allocated(flux)) deallocate(flux)
        if (allocated(aod_data)) deallocate(aod_data)
        if (allocated(velocity)) deallocate(velocity)
        if (allocated(velocity_aux)) deallocate(velocity_aux)
        if (allocated(modelflux)) deallocate(modelflux)
        if (allocated(modelflux_convolved)) deallocate(modelflux_convolved)
        if (allocated(flux_smooth)) deallocate(flux_smooth)
        if (allocated(noise)) deallocate(noise)
        if (allocated(wlength)) deallocate(wlength)
        if (allocated(wlength_aux)) deallocate(wlength_aux)
        if (allocated(modelflux_local)) deallocate(modelflux_local)
        if (allocated(modelflux_local_aux)) deallocate(modelflux_local_aux)
        if (allocated(sigma)) deallocate(sigma)
        if (allocated(unabsorbed_pixel)) deallocate(unabsorbed_pixel)
        if (allocated(dfdv)) deallocate(dfdv)
        if (allocated(d2fdv2)) deallocate(d2fdv2)
        if (allocated(linear_chi2_spec)) deallocate(linear_chi2_spec)

!       re-allocate arrays

        allocate(flux(1:size))
        allocate(aod_data(1:size))
        allocate(velocity(1:size))
        allocate(velocity_aux(1:size))
        allocate(modelflux(1:size))
        allocate(modelflux_convolved(1:size))
        allocate(flux_smooth(1:size))
        allocate(noise(1:size))
        allocate(wlength(1:size))
        allocate(wlength_aux(1:size))
        allocate(modelflux_local(1:size))
        allocate(modelflux_local_aux(1:size))
        allocate(sigma(1:size))
        allocate(unabsorbed_pixel(1:size))
        allocate(dfdv(1:size))
        allocate(d2fdv2(1:size))
        allocate(linear_chi2_spec(1:size))

        end subroutine alloc_spec
!       ========================================================================

!       ========================================================================
        subroutine alloc_fit(size)
        
        use set_precision, only: singleI

        use ifit_variables, only: flux_residum, aod_residum, xdata_in, 
     +  flux_residum_error, aod_residum_error, chi2_terms, ydata_in, yerror_in 


        implicit none
        
        integer(kind=singleI) :: size

!       make sure arrays are de-allocated

        if (allocated(flux_residum)) deallocate(flux_residum)
        if (allocated(flux_residum_error)) deallocate(flux_residum_error)
        if (allocated(aod_residum)) deallocate(aod_residum)
        if (allocated(aod_residum_error)) deallocate(aod_residum_error)
        if (allocated(xdata_in)) deallocate(xdata_in)
        if (allocated(ydata_in)) deallocate(ydata_in)
        if (allocated(yerror_in)) deallocate(yerror_in)
        if (allocated(chi2_terms)) deallocate(chi2_terms)

!       re-allocate arrays

        allocate(flux_residum(1:size))
        allocate(flux_residum_error(1:size))
        allocate(aod_residum(1:size))
        allocate(aod_residum_error(1:size))
        allocate(xdata_in(1:size))
        allocate(ydata_in(1:size))
        allocate(yerror_in(1:size))
        allocate(chi2_terms(1:size))

!      initialise
        flux_residum(:) = 0.0d0
        flux_residum_error(:) = 0.0d0
        aod_residum(:) = 0.0d0
        aod_residum_error(:) = 0.0d0
        xdata_in(:) = 0.0d0
        ydata_in(:) = 0.0d0
        yerror_in(:) = 0.0d0
        chi2_terms(:) = 0.0d0

        end subroutine alloc_fit
!       ========================================================================

!       ========================================================================
        subroutine init_components(cycl)

!       NOTE:
!       INVOKE ONLY VARIABLES ACTUALLY BEING USED

        use set_precision
        use ifit_variables, only: IONS, ION_INDEX, num_components, true_ion,
     +  max_components, max_components_abs, valid_components_save,
     +   max_num_param, true_transition, true_ion, strongest,
     +  spectrum_size_px_effective,min_absprofile_size_px, 
     +  colour_statmsg_prefix, colour_errmsg_prefix, colour_msg_suffix

        implicit none

        integer(kind=singleI) :: j
        integer(kind=singleI), intent(in) :: cycl
        character(len=64) :: routinemsg = 'init_components: '

!       ------------------------------------------------------------------------
        select case(cycl)

!       ------------------------------------------------------------------------
        case(1) ! sweep=1; no lines yet

!       inital number of components
         num_components = 0

!       define maximum number of components, assuming one per smallest allowed
!       absorption profile;
!       spectrum_size_px_effective is computed in read_spectrum()
!       min_absprofile_size_px is defined in module ifit_variables

         max_components =
     &   min(max_components_abs,
     &   ceiling(dble(spectrum_size_px_effective)/dble(min_absprofile_size_px)))

!       define maximum number of components and parameters
         max_num_param = 3 * max_components

!       output info
        if (max_components.eq.0) then
         write(6,*)
         write(6,'(1x,a)') trim(colour_errmsg_prefix)//
     &   trim(routinemsg)//trim(colour_msg_suffix)
         write(6,'(1x,a)')
     &   'zero initial number of components'
         write(6,'(1x,a)') 
     &   'check input spectrum and '//"'''parameter min_absprofile_size_px'''"
         write(6,*)
         stop 1
        end if

!       allocate and initialise arrays
        call alloc_component_param(ION_INDEX,max_components)
        call alloc_fitting_params(max_num_param)

!       ------------------------------------------------------------------------
!       IMPORTANT:
!       assume that all potentially identified components correspond to the
!       strongest transtion (1) of the first listed ion ION_INDEX=1, e.g.,
!      "brute-force" fitting;
!       this will be fixed later by routine `search_multiplets'

!       make sure data are de-allocated
        if (allocated(true_transition)) deallocate(true_transition)
        if (allocated(true_ion)) deallocate(true_ion)

!       (re-)allocate
        allocate(true_transition(1:max_components))
        allocate(true_ion(1:max_components))

!       initialise data elements
        true_ion(:) = ION_INDEX
        true_transition(:) = strongest(ION_INDEX,1)


!       ------------------------------------------------------------------------
        case (2)     ! sweep=2; use single-ion (transition) first-guess lines to
                     ! fit taking instrumental broadening into account

!       save number of valid components (for book-keeping)
         valid_components_save =
     &   count(abs(IONS(ION_INDEX)%component(:)%status).eq.1)

!       read in component parameters
!       initialise component status with default value

         do j=1,num_components

          if (abs(IONS(ION_INDEX)%component(j)%status).eq.1) then

           IONS(ION_INDEX)%component(j)%status = 3 ! first-guess components

          else

           IONS(ION_INDEX)%component(j)%col_dens = 0.0d0
           IONS(ION_INDEX)%component(j)%vel_c = -1.0d0
           IONS(ION_INDEX)%component(j)%b_value = 0.0d0
           IONS(ION_INDEX)%component(j)%status = 0

          end if

         end do

!       output info
         write(6,*)
         write(6,'(1x,a)') trim(colour_statmsg_prefix)//
     &   trim(routinemsg)//trim(colour_msg_suffix)
         write(6,'(1x,a,2i6)')
     &   'initial number of components (total | valid):',
     &   num_components, valid_components_save


!       ------------------------------------------------------------------------
        case (3) ! sweep=3; use multi-transition first-guess lines to refine fit

!       TO DO: SAVE PARAMETER VALUES OF VALID COMPONENTS IN AUXILIARY ION
!       STRUCTURE, REALLOCATE IONS STRUCTURE, AND STORE VALUES

!       read in component parameters
!       initialise component status with default value

         do j=1,num_components

          if (abs(IONS(true_ion(j))%component(j)%status).eq.1) then

           IONS(true_ion(j))%component(j)%status = 3 ! first-guess components

          else

           IONS(true_ion(j))%component(j)%col_dens = 0.0d0
           IONS(true_ion(j))%component(j)%vel_c = -1.0d0
           IONS(true_ion(j))%component(j)%b_value = 0.0d0
           IONS(true_ion(j))%component(j)%status = 0

          end if

         end do

!       output info
         write(6,*)
         write(6,'(1x,a)') trim(colour_statmsg_prefix)//
     &   trim(routinemsg)//trim(colour_msg_suffix)
         write(6,'(1x,a,2i6)')
     &   'initial number of components:', num_components

        end select

!       ------------------------------------------------------------------------
        return

        end subroutine init_components
!       ========================================================================

!       ========================================================================
        subroutine visualise(run,par_val,vel_min,vel_max,absprofile_id,
     &  tot_inter,numpar,chi2_current_reduced,called_from_str)

!       NOTE:
!       'numpar' corresponds to the number of parameters, i.e. the actual number
!       of lines x3 in the current absorption profile
!     
        use set_precision
        use ifit_variables

        use constants

        implicit none

!       LOCAL variables
        integer(kind=singleI) :: i, j
!       Fitting absorption profile ID; toal number of absorption profiles
        integer(kind=singleI) :: absprofile_id, tot_inter
!       actual number of components in absorption profile equals numpar/3
        integer(kind=singleI) :: numpar, component_index

!       horizontal terminal window size; will be x2 as large vertically
!       (not sure about the units used by gnuplot...)
        
        integer(kind=singleI) :: wsizeh=1100

!       panel margins
        real(kind=singleR), parameter :: lmargin_left = 0.12,
     +  rmargin_left=0.52
        real(kind=singleR), parameter :: lmargin_right = 0.62,
     +  rmargin_right=0.99
        real(kind=singleR), parameter :: tmargin_top = 0.90,
     +  bmargin_top = tmargin_top-0.36
        real(kind=singleR), parameter :: tmargin_mid = bmargin_top-0.01,
     +  bmargin_mid = tmargin_mid-0.38  
        real(kind=singleR), parameter :: tmargin_bot=bmargin_mid-0.01,
     +  bmargin_bot = tmargin_bot - 0.08

!       size of absorption profile (in velocity); velocity width to improve
!       plotting 
        real(kind=doubleR) :: vel_min, vel_max
        real(kind=singleR), parameter :: vel_shift = 5.0d1
!       current reduced chi-square value for fit
        real(kind=doubleR) :: chi2_current_reduced

        real(kind=doubleR) :: par_val(*)

        real(kind=doubleR), allocatable :: model_individual(:,:)

        character(len=3) :: absprofile_id_str
        character(len=256) :: visual_file_data, visual_file_plot
        character(len=*) :: called_from_str
        
        logical, intent(in) :: run

!       ------------------------------------------------------------------------
!       do only if required
        if ((.not.run).or.(numpar.eq.0)) return

        if ((trim(called_from_str).eq.'discard_component_module').and.
     &  (numpar.eq.3))            ! meaning that discarding single component in
     &  return                    ! abs.profile was not accepted

!       ------------------------------------------------------------------------
!       give feedback when debugging
        if (debug)
     &  write(6,*) 'CALLING VISUAL FROM '//trim(called_from_str), fit_attempt

!       ------------------------------------------------------------------------
!       define data file names

        write(absprofile_id_str,'(i3)') absprofile_id

!       file with: vel | model | spectrum | individual component profiles

        if (fwhm_kms.gt.0.0d0) then
         visual_file_data = 'visual_'//trim(adjustl(absprofile_id_str))//'_'//
     &   trim(IONS(ION_INDEX)%name)//'_conv.vis'
        else
         visual_file_data = 'visual_'//trim(adjustl(absprofile_id_str))//'_'//
     &   trim(IONS(ION_INDEX)%name)//'_noconv.vis'        
        end if

!       ------------------------------------------------------------------------
!       remap chi-square spectrum into array for visualisation

         chi2_terms(1:spectrum_size_px) =
     &   cshift(chi2_terms(1:spectrum_size_px),-pixel_min+1)

!       remap arrays aod_residum and aod_residum_error to match input data

         aod_residum(1:spectrum_size_px) =
     &   cshift(aod_residum(1:spectrum_size_px),-pixel_min+1)

         aod_residum_error(1:spectrum_size_px) =
     &   cshift(aod_residum_error(1:spectrum_size_px),-pixel_min+1)

!       ------------------------------------------------------------------------
        if (test_run) then
         
!       allocate memory for array storing individual component profiles
         if (allocated(model_individual)) deallocate(model_individual)
         allocate(model_individual(ceiling(numpar/3.),1:spectrum_size_px))
         model_individual(:,:) = 1.0
        
        end if ! test_run

!       load model with fitted components in all absorption profiles processed
!       so far:

        modelflux_local(1:spectrum_size_px) = modelflux(1:spectrum_size_px)

!       include new components (one by one, in current absorption profile);
!       result is stored in modelflux_local_aux, which is set to 1.0 when
!       calling model_local 

        do j=1,numpar,3

         call model_local(par_val,j,1,spectrum_size_px)

!       compute model flux with all components (iteratively)
         modelflux_local(1:spectrum_size_px) =
     &   modelflux_local(1:spectrum_size_px) *
     &   modelflux_local_aux(1:spectrum_size_px)

!       convolve model spectrum including individual components only when
!       testing to visualise the contribution of each component
         
         if (test_run) then
         
          call gauss_conv(modelflux_local_aux,1,spectrum_size_px,
     &    pixel_size_kms_local,fwhm_kms)
          
          component_index = ceiling(dble(j)/3.0d0)
          model_individual(component_index,:) = modelflux_local_aux(:)
         
         end if ! test_run

        end do ! loop over lines

!       convolve model spectrum with all components
        
        call
     &  gauss_conv(modelflux_local,1,spectrum_size_px,
     &  pixel_size_kms_local,fwhm_kms)
         
!       ------------------------------------------------------------------------
!       Write (append) model (fit) and data (spectrum) to (existing) datafile

        open(100,file=trim(visual_file_data))

!       read until end of file
         do
          read(100,*,end=110)
         end do
 110     continue
         backspace(100) ! avoid end-of-file (EOF) marker

         write(100,*) '# Region:', absprofile_id, ' attempt ', fit_attempt

         if (test_run) then ! include individual component profiles
         
          do j=1,spectrum_size_px
           write(100,*) velocity(j), modelflux_local(j), flux(j),
     &     chi2_terms(j), noise(j), aod_residum(j), aod_residum_error(j),
     &     (model_individual(i,j), i=1,ceiling(dble(numpar)/3.0d0))
          end do !j-loop over pixel
         
         else
         
          do j=1,spectrum_size_px
           write(100,*) velocity(j), modelflux_local(j), flux(j), chi2_terms(j),
     &     noise(j), aod_residum(j), aod_residum_error(j)
          end do !j-loop over pixel

         end if

         write(100,*)               
         write(100,*)               

         write(100,*) '# Line centroid'
         do j=1,numpar,3
          write(100,*) par_val(j)
         end do !j-loop over pixel
         write(100,*)               
         write(100,*)               

        close(100)
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       restore vectors chi2_terms, aod_residum and aod_residum_error

         chi2_terms(1:spectrum_size_px) =
     &   cshift(chi2_terms(1:spectrum_size_px),pixel_min-1)

         aod_residum(1:spectrum_size_px) =
     &   cshift(aod_residum(1:spectrum_size_px),pixel_min-1)

         aod_residum_error(1:spectrum_size_px) =
     &   cshift(aod_residum_error(1:spectrum_size_px),pixel_min-1)

!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       output chi^2 values to visualise fit behaviour for each absorption
!       profile 
          if (first_call_visual)
     &    call
     &    write_chi2_evolution(2,visual,chi2_current/dble(degrees_of_freedom))

!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       Create gnuplot file

        if (fwhm_kms.gt.0.0d0) then
         visual_file_plot = 'visual_'//trim(adjustl(absprofile_id_str))//'_'//
     &   trim(IONS(ION_INDEX)%name)//'_conv.plot'
        else
         visual_file_plot = 'visual_'//trim(adjustl(absprofile_id_str))//'_'//
     &   trim(IONS(ION_INDEX)%name)//'_noconv.plot'        
        end if

        open(110,file=trim(visual_file_plot))
         
!       read until end of file
         do
          read(110,*,end=10)
         end do
 10      continue
         backspace(110) ! avoid end-of-file (EOF) marker

!       Set terminal type (only once!)
!         if (first_call_visual) then
          write(110,*)
          write(110,*)
          write(110,*) 'reset'
          write(110,*)
          write(110,*)
          write(110,*) 'set border lc rgb "white"'
          write(110,*) 'set key top right tc rgb "white"'
          write(110,'(a,i5)') 'wsizeh=',wsizeh
          write(110,*)
     &    'set term x11 0 enhanced size wsizeh,wsizeh/sqrt(2) position 1,1'
!         write(110,*)
!     &    'set term x11 0 enhanced position 1,1'
          write(110,*)

!       define functions

!       to highligh actual fitted range
           write(110,*) '# user defined functions'
           write(110,*)
           write(110,*) '# fitted range high-lightling'
           write(110,*) 'fit_range(x) = x >= ', vel_min ,' && x <= ', vel_max,
     &     ' ? 1 : 1/0' 
           write(110,*)
           write(110,*) '# max / min functions'
           write(110,*) 'min(x,y) = (x < y) ? x : y'
           write(110,*) 'max(x,y) = (x > y) ? x : y'
           write(110,*)

!       boxes style
           write(110,*) 'set style fill solid 1.0'

!         end if

!       Multiplot environment
         write(110,*) 'set multiplot'
         write(110,*)

!       background colour
         write(110,*) 'set object 1 rectangle from screen 0,0 to '//
     &   'screen 1,1 fillcolor rgb "black" behind'

!       ------------------------------------------------------------------------
         write(110,'(a,f5.2)') '# left top panel'

         write(110,'(a,f5.2)') 'set lmargin screen ', lmargin_left
         write(110,'(a,f5.2)') 'set rmargin screen ', rmargin_left
         write(110,'(a,f5.2)') 'set bmargin screen ', bmargin_top
         write(110,'(a,f5.2)') 'set tmargin screen ', tmargin_top
         write(110,*)

!       Remove x/y tics and labels
         write(110,*) 'unset xtics'
         write(110,*) 'unset ytics'
         write(110,*) 'unset x2tics'
         write(110,*) 'unset y2tics'
         write(110,*) 'set xtics format " "'
         write(110,*) 'set ytics format "%5.2f"'
         write(110,*) 'set x2tics format " "'
         write(110,*) 'set y2tics format " "'
 
!       plotting ranges
         write(110,*) 'unset logscale'
         write(110,*) 'set xrange[',max(velocity(1),vel_min-vel_shift),':',
     &   min(velocity(spectrum_size_px),vel_max+vel_shift),']'
         write(110,*) 'set x2range[',max(velocity(1),vel_min-vel_shift),':',
     &   min(velocity(spectrum_size_px),vel_max+vel_shift),']'
         write(110,*) 'set yrange[-0.1:1.2]'
         write(110,*) 'set y2range[-0.1:1.2]'
         write(110,*)

!       Display current chi-square value; first remove previous label
         write(110,*) 'unset label'
         write(110,*) 'set label sprintf("{/=14 abs.prof.: %3i/%3i}", ',
     &   absprofile_id, ',', tot_inter,') at screen 0.10,',
     &   tmargin_top+0.05, 'tc rgb "white"'
         write(110,*) 'set label sprintf("{/=14 attempt: %4.0f}", ',
     &   fit_attempt, ') at screen 0.20,',
     &   tmargin_top+0.05, 'tc rgb "white"'
         write(110,*) 'set label sprintf("{/=14 components: %4.0f}", ', 
     &   ceiling(dble(numpar)/3.0d0), ') at screen 0.285,',
     &   tmargin_top+0.05, 'tc rgb "white"'
         write(110,*)
     &   'set label sprintf("{/=14 {/Symbol c}^2_{red}(dof)=%8.3f (%5.0f)}", ',
     &   chi2_current_reduced, ',', degrees_of_freedom,
     &    ') at screen 0.40,',
     &   tmargin_top+0.05, 'tc rgb "white"'
         write(110,*)

         write(110,*) 'unset xlabel'
         write(110,*) 'unset ylabel'
         write(110,*) 'unset x2label'
         write(110,*) 'unset y2label'
         write(110,*) 'set ylabel "{/= 12 transmission [a.u.]}"'//
     &   ' tc rgb "white" offset 0,0,0'
         write(110,*)

!       plot ideal continuum, spectrum +/- sigma and fit
         write(110,*) 'plot 1 w l lt 4 lw 1 lc rgb "white" notitle \'
         write(110,*) ', 0 w l lt 4 lw 1 lc rgb "white" notitle \'
         write(110,*) ', "'//trim(visual_file_data)//'" index ',
     &   2*visual_counter,
     &   ' u 1:($3+$5) w boxes lt 1 lw 1 lc rgb "gray" notitle\'
         write(110,*) ', "" index ',
     &   2*visual_counter,
     &   ' u 1:($3-$5) w boxes lt 1 lw 1 lc rgb "black" notitle \'
         write(110,*) ', "" index ',
     &   2*visual_counter,
     &   ' u 1:3 w histeps lt 1 lw 2 lc rgb "yellow" title "data"\'
         write(110,*) ', ""  index ', 2*visual_counter,
     &   'u 1:2 w l lt 1 lw 3 lc rgb "red" title "model"\'
         
         if ((test_run).and.(ceiling(dble(numpar)/3.0d0).gt.1)) then
!       plot individual component profiles
          do i=1,ceiling(dble(numpar)/3.0d0)
           write(110,*) ', "" index ', 2*visual_counter,
     &     ' u 1:($', i+7,') w l lt 1 lw 2 lc rgb "white" notitle \'
          end do
         end if
!       include component centroid(s)
         write(110,*) ', "" index ', 2*visual_counter+1,' using '
     &   //'($1):(1.1):("|") w labels tc rgb "green" notitle \'
!       highlight fitting range
         write(110,*) ', fit_range(x) w l lt 1 lw 2 lc rgb "magenta" notitle \'
         write(110,*)

!       ------------------------------------------------------------------------
         write(110,'(a,f5.2)') '# right top panel'

         write(110,'(a,f5.2)') 'set lmargin screen ', lmargin_right
         write(110,'(a,f5.2)') 'set rmargin screen ', rmargin_right
         write(110,'(a,f5.2)') 'set bmargin screen ', bmargin_top
         write(110,'(a,f5.2)') 'set tmargin screen ', tmargin_top
         write(110,*)

!       Remove background colour to avoid overlaying on previous panel
         write(110,*) 'unset object'
         write(110,*)

!       Remove previous label
         write(110,*) 'unset label'
         write(110,*)

!       Remove x/y tics and labels
         write(110,*) 'unset xtics'
         write(110,*) 'unset ytics'
         write(110,*) 'unset x2tics'
         write(110,*) 'unset y2tics'
         write(110,*) 'set xtics format " "'
         write(110,*) 'set ytics format " %g"'
         write(110,*) 'set x2tics format " %g "'
         write(110,*) 'set y2tics format " "'
         write(110,*)
 
!       plotting ranges
         write(110,*) 'set xrange [*:*]'
         write(110,*) 'set yrange [*:*]'
         write(110,*) 'unset logscale' ! important to do before call `stats'!
         write(110,*)

!       get maximum and minimum value using gnuplot built-in command 'stats'
!       these are stored in the gnuplot internal variables SIGMA_min_x, etc.
         write(110,*) 'stats "'//trim(visual_file_data)//'" index ',
     &   2*visual_counter,' u 1:5 name "SIGMA" nooutput'
         write(110,*)

         write(110,*) 'set xrange[',max(velocity(1),vel_min-vel_shift),':',
     &   min(velocity(spectrum_size_px),vel_max+vel_shift),']'
         write(110,*) 'set x2range[',max(velocity(1),vel_min-vel_shift),':',
     &   min(velocity(spectrum_size_px),vel_max+vel_shift),']'
         write(110,*) 'set yrange[1-',
     &   Nsigma*chi2bad_reduced,'*SIGMA_max_y:1+',
     &   Nsigma*chi2bad_reduced,'*SIGMA_max_y]'
         write(110,*) 'set y2range[1-',
     &   Nsigma*chi2bad_reduced,'*SIGMA_max_y:1+',
     &   Nsigma*chi2bad_reduced,'*SIGMA_max_y]'
         write(110,*)

         write(110,*) 'unset xlabel'
         write(110,*) 'unset ylabel'
         write(110,*) 'unset x2label'
         write(110,*) 'unset y2label'
         write(110,*) 'set x2label "velocity [kms^{-1}]" tc rgb "white"'
         write(110,*) 'set ylabel "{/= 12 residuum}" '//
     &   'tc rgb "white" offset 0,0,0'
         write(110,*)

!       Plot spectrum and fit
         write(110,*) 'plot "'//trim(visual_file_data)//'" index ',
     &   2*visual_counter,
     &   ' u 1:(abs($2/$3)) w histeps lt 1 lw 1 lc rgb "yellow" notitle \'
         write(110,*) ', ""  index ', 2*visual_counter,
     &   'u 1:(1+$5) w histeps lt 1 lw 2 lc rgb "orange" '//
     &   'title "{/=14 {/Symbol s}}" \'
         write(110,*) ', ""  index ', 2*visual_counter,
     &   'u 1:(1-$5) w histeps lt 1 lw 2 lc rgb "orange" notitle \'

!       include component centroid(s)
         write(110,*) ', "" index ', 2*visual_counter+1,' using '//
     &   '($1):(1-0.5*',Nsigma*chi2bad_reduced,'*SIGMA_max_y):("|") w labels '//
     &   'tc rgb "green" notitle \'
         write(110,*) ', 1 w l lt 4 lw 1 lc rgb "white" notitle \'
         write(110,*) ', fit_range(x) w l lt 1 lw 2 lc rgb "magenta" notitle \'
         write(110,*)

!       ------------------------------------------------------------------------
         write(110,'(a,f5.2)') '# left middle panel'

         write(110,'(a,f5.2)') 'set lmargin screen ', lmargin_left
         write(110,'(a,f5.2)') 'set rmargin screen ', rmargin_left
         write(110,'(a,f5.2)') 'set bmargin screen ', bmargin_mid
         write(110,'(a,f5.2)') 'set tmargin screen ', tmargin_mid
         write(110,*)

!       Remove background colour to avoid overlaying on previous panel
         write(110,*) 'unset object'
         write(110,*)

!       Remove previous label
         write(110,*) 'unset label'
         write(110,*)

!       Remove x/y tics and labels
         write(110,*) 'unset xtics'
         write(110,*) 'unset ytics'
         write(110,*) 'unset x2tics'
         write(110,*) 'unset y2tics'
         write(110,*) 'set ytics format " %6.3f"'
         write(110,*) 'set x2tics format " "'
         write(110,*) 'set y2tics format " "'
         write(110,*)
 
!       plotting ranges
         write(110,*) 'set xrange [*:*]'
         write(110,*) 'set yrange [*:*]'
         write(110,*) 'unset logscale' ! important to do before call `stats'!
         write(110,*)

!       get maximum and minimum value using gnuplot built-in command 'stats'
!       these are stored in the gnuplot internal variables TAU_min, etc.
         write(110,*) 'stats "'//trim(visual_file_data)//'" index ',
     &   2*visual_counter,' u 6 name "TAU" nooutput'
         write(110,*)

         write(110,*) 'set xrange[',max(velocity(1),vel_min-vel_shift),':',
     &   min(velocity(spectrum_size_px),vel_max+vel_shift),']'
         write(110,*) 'set x2range[',max(velocity(1),vel_min-vel_shift),':',
     &   min(velocity(spectrum_size_px),vel_max+vel_shift),']'
         write(110,*) 'set yrange[-0.5*min(TAU_max,',
     &   -log(average_noise_local),'):2*min(TAU_max,',
     &   -log(average_noise_local),')]'
         write(110,*) 'set y2range[-0.5*min(TAU_max,',
     &   -log(average_noise_local),'):2*min(TAU_max,',
     &   -log(average_noise_local),')]'
         write(110,*)

         write(110,*) 'unset xlabel'
         write(110,*) 'unset ylabel'
         write(110,*) 'unset x2label'
         write(110,*) 'unset y2label'
         write(110,*) 'set ylabel "{/= 12 {/Symbol t}(v) [a.u.]}" '//
     &   'tc rgb "white" offset 0,0,0'
         write(110,*)

!       plot tau(spectrum) +/- sigma, and tau(fit)
         write(110,*) 'plot "'//trim(visual_file_data)//'" index ',
     &   2*visual_counter,
     &   ' u 1:($6+$7) w boxes lt 1 lw 1 lc rgb "gray" notitle \'
         write(110,*) ', "" index ', 2*visual_counter,
     &   ' u 1:($6-$7) w boxes lt 1 lw 1 lc rgb "black" notitle \'
         write(110,*) ', ""  index ', 2*visual_counter,
     &   ' u 1:($6) w histeps lt 1 lw 2 lc rgb "yellow" notitle \'
         write(110,*) ', ""  index ', 2*visual_counter,
     &   'u 1:(-log($2)) w l lt 1 lw 3 lc rgb "red" notitle \'

!       include component centroid(s)
         write(110,*) ', "" index ', 2*visual_counter+1,' using '//
     &   '($1):(-0.5):("|") w labels '//
     &   'tc rgb "green" notitle \'

!       include optical depth corresponding to saturation due to noise;
!       compare to input value
         write(110,*) ',', -log(average_noise_local),
     &   'w l lt 4 lw 1 lc rgb "white" notitle \'
         write(110,*) ',', tau_sat, 'w l lt 4 lw 1 lc rgb "yellow" notitle \'
         write(110,*) ', 0 w l lt 4 lw 1 lc rgb "white" notitle \'
         write(110,*)

!       ------------------------------------------------------------------------
         write(110,'(a,f5.2)') '# right middle panel'

         write(110,'(a,f5.2)') 'set lmargin screen ', lmargin_right
         write(110,'(a,f5.2)') 'set rmargin screen ', rmargin_right
         write(110,'(a,f5.2)') 'set bmargin screen ', bmargin_mid
         write(110,'(a,f5.2)') 'set tmargin screen ', tmargin_mid
         write(110,*)

!       Remove background colour to avoid overlaying on previous panel
         write(110,*) 'unset object'
         write(110,*)

!       Remove previous label
         write(110,*) 'unset label'
         write(110,*)

!       Remove x/y tics and labels
         write(110,*) 'unset xtics'
         write(110,*) 'unset ytics'
         write(110,*) 'unset x2tics'
         write(110,*) 'unset y2tics'
         write(110,*) 'set xtics format " "'
         write(110,*) 'set ytics format " %g"'
         write(110,*) 'set x2tics format " "'
         write(110,*) 'set y2tics format " "'
         write(110,*)
 
!       plotting ranges
         write(110,*) 'set xrange [*:*]'
         write(110,*) 'set yrange [*:*]'
         write(110,*) 'set x2range [*:*]'
         write(110,*) 'set y2range [*:*]'
         write(110,*) 'unset logscale' ! important to do before call `stats'!
         write(110,*)

!       get maximum and minimum value using gnuplot built-in command 'stats'
!       these are stored in the gnuplot internal variables CHI2evol_min_x, etc.
         write(110,*) 'stats "'//trim(file_chi2_evolution)//'" '//
     &   ' u 1:2 name "CHI2evol" nooutput'
         write(110,*)

         write(110,*) 'set xrange[-1:max(50,CHI2evol_max_x+2)]'
         write(110,*) 'set x2range[-1:max(50,CHI2evol_max_x+2)]'
         write(110,*) 'set logscale yy2'
         write(110,*) 'set yrange[0.1:1.5*CHI2evol_max_y]'
         write(110,*) 'set y2range[0.1:1.5*CHI2evol_max_y]'
         write(110,*)

         write(110,*) 'unset xlabel'
         write(110,*) 'unset ylabel'
         write(110,*) 'unset x2label'
         write(110,*) 'unset y2label'
         write(110,*) 'set ylabel "{/= 12 {/Symbol c}^2}"'//
     &   ' tc rgb "white" offset 0,0,0'
         write(110,*)

!       plot chi2 evolution
!        write(110,*) 'plot "'//trim(file_chi2_evolution)//'" \'!//
!        if (visual_counter.gt.0)
!     &   write(110,*) ' index ', visual_counter-1, ':', visual_counter, '\'
!        if (visual_counter.eq.0)
!     &   write(110,*) ' index ', visual_counter, '\'
!        write(110,*) ' u 1:2 w ste lt 1 lw 3 lc rgb "cyan" '//
!     &   'title "total" \'
!        write(110,*) ', ""  index ', visual_counter,
!     &     ' u 1:2 w p pt 7 ps 0.8 lc rgb "cyan" notitle \'
         write(110,*) 'plot "'//trim(file_chi2_evolution)//'" \'
         write(110,*) ' index ', fit_attempt-1, ':', fit_attempt, '\'
         write(110,*) ' u 1:2 w ste lt 1 lw 3 lc rgb "cyan" '//
     &   'title "total" \'
         write(110,*) ', ""  index ', fit_attempt,
     &     ' u 1:2 w p pt 7 ps 0.8 lc rgb "cyan" notitle \'

!       chi^2 threshold values
         write(110,*) ', ', chi2bad_reduced,
     &   'w l lt 4 lw 2 lc rgb "orange" notitle \'
         write(110,*) ', ', chi2good_reduced,
     &   ' w l lt 4 lw 2 lc rgb "green" notitle \'
         write(110,*)       
         write(110,*)

!       ------------------------------------------------------------------------
         write(110,'(a,f5.2)') '# left bottom panel '

         write(110,'(a,f5.2)') 'set lmargin screen ', lmargin_left
         write(110,'(a,f5.2)') 'set rmargin screen ', rmargin_left
         write(110,'(a,f5.2)') 'set bmargin screen ', bmargin_bot
         write(110,'(a,f5.2)') 'set tmargin screen ', tmargin_bot
         write(110,*)

!       Remove background colour to avoid overlaying on previous panel
         write(110,*) 'unset object'
         write(110,*)

!       Remove previous label
         write(110,*) 'unset label'
         write(110,*)

!       Remove x/y tics and labels
         write(110,*) 'unset xtics'
         write(110,*) 'unset ytics'
         write(110,*) 'unset x2tics'
         write(110,*) 'unset y2tics'
         write(110,*) 'set xtics format " %g"'
         write(110,*) 'set ytics format " %L.00"'
         write(110,*)

         write(110,*) 'unset xlabel'
         write(110,*) 'unset ylabel'
         write(110,*) 'unset x2label'
         write(110,*) 'unset y2label'
         write(110,*) 'set xlabel "{/= 12 velocity [kms^{-1}]}" tc rgb "white"'
         write(110,*) 'set ylabel "{/= 12 log_{10} {/Symbol c}^2(v)}"'//
     &   ' tc rgb "white" offset 0,-2,0'
         write(110,*)

!       plotting ranges
         write(110,*) 'set xrange [*:*]'
         write(110,*) 'set yrange [*:*]'
         write(110,*) 'set x2range [*:*]'
         write(110,*) 'set y2range [*:*]'
         write(110,*) 'unset logscale' ! important to do before call `stats'!
         write(110,*)

!       get maximum and minimum value using gnuplot built-in command 'stats'
!       these are stored in the gnuplot internal variables CHI2_min_x, etc.
         write(110,*) 'stats "'//trim(visual_file_data)//'" index ',
     &   2*visual_counter,' u 1:4 name "CHI2" nooutput'
         write(110,*)

         write(110,*) 'set logscale yy2'
         write(110,*) 'set xrange[',max(velocity(1),vel_min-vel_shift),':',
     &   min(velocity(spectrum_size_px),vel_max+vel_shift),']'
         write(110,*) 'set x2range[',max(velocity(1),vel_min-vel_shift),':',
     &   min(velocity(spectrum_size_px),vel_max+vel_shift),']'
         write(110,*) 'set yrange[',0.1*chi2good_reduced,':10*CHI2_max_y]'
         write(110,*) 'set y2range[',0.1*chi2good_reduced,':10*CHI2_max_y]'
         write(110,*)

!       plot chi-square spectrum
          write(110,*) 'plot "'//trim(visual_file_data)//'" index ',
     &    2*visual_counter,
     &    ' u 1:4 w impulses lt 1 lw 1 lc rgb "yellow" notitle \'

         write(110,*) ', ', chi2bad_reduced,
     &   'w l lt 4 lw 2 lc rgb "orange" notitle \'
         write(110,*) ', ', chi2_current_reduced,
     &   ' w l lt 4 lw 2 lc rgb "red" notitle \'
         write(110,*)

!       ------------------------------------------------------------------------
         write(110,'(a,f5.2)') '# right bottom panel '
         write(110,*)

         write(110,'(a,f5.2)') 'set lmargin screen ', lmargin_right
         write(110,'(a,f5.2)') 'set rmargin screen ', rmargin_right
         write(110,'(a,f5.2)') 'set bmargin screen ', bmargin_bot
         write(110,'(a,f5.2)') 'set tmargin screen ', tmargin_bot
         write(110,*)

!       Remove background colour to avoid overlaying on previous panel
         write(110,*) 'unset object'

!       Remove previous label
         write(110,*) 'unset label'

!       Remove x/y tics and labels
         write(110,*) 'unset xtics'
         write(110,*) 'unset ytics'
         write(110,*) 'unset x2tics'
         write(110,*) 'unset y2tics'
         write(110,*) 'set xtics format " %g"'
         write(110,*) 'set ytics format " %g"'
         write(110,*)

         write(110,*) 'unset xlabel '
         write(110,*) 'unset ylabel '
         write(110,*) 'unset x2label '
         write(110,*) 'unset y2label '
         write(110,*) 'set xlabel "{/= 12 iteration}" tc rgb "white"'
         write(110,*) 'set ylabel "{/= 12 No. of components}"'//
     &   ' tc rgb "white" offset 0,-2,0'
         write(110,*)

!       plotting ranges
         write(110,*) 'set xrange [*:*]'
         write(110,*) 'set yrange [*:*]'
         write(110,*) 'set x2range [*:*]'
         write(110,*) 'set y2range [*:*]'
         write(110,*) 'unset logscale' ! important to do before call `stats'!
         write(110,*)

!       get maximum and minimum value using gnuplot built-in command 'stats'
!       these are stored in the gnuplot internal variables LINE_min_x, etc.
         write(110,*) 'stats "'//trim(file_chi2_evolution)//'" '//
     &   ' u 3 name "LINE" nooutput'
         write(110,*)

         write(110,*) 'set xrange[-1:max(50,CHI2evol_max_x+2)]'
         write(110,*) 'set x2range[-1:max(50,CHI2evol_max_x+2)]'
         write(110,*) 'set yrange [-0.5:LINE_max+2]'
         write(110,*) 'set y2range [-0.5:LINE_max+2]'
         write(110,*)

!       plot component-number evolution
         
         write(110,*) 'plot "'//trim(file_chi2_evolution)//'" '//
     &   ' u 1:3 w l lt 1 lw 1 lc rgb "magenta" notitle \'
         write(110,*) ', 0 w l lt 0 lw 1 lc rgb "white" notitle \'
         write(110,*)

!       ------------------------------------------------------------------------
!       exit multiplot environment

         write(110,*) 'unset multiplot'
         write(110,*)

         if (test_run) then
          write(110,*) 'pause 1'
         else
          write(110,*) 'pause 1'
         end if
         write(110,*)

        close(110)
!       ------------------------------------------------------------------------
        
!       Update counter
        visual_counter = visual_counter + 1

        if (first_call_visual) first_call_visual = .false.

!       ------------------------------------------------------------------------
        return

        end subroutine visualise
!       ========================================================================

!       ========================================================================
        subroutine verbose_output(output,par_val,parval_err,component_index)

!       Prints useful information to the screen

        use set_precision
        use ifit_variables, only: IONS, true_ion, output_dashed_line, num_param

        implicit none
        
        integer(kind=singleI) :: j, jmax
        integer(kind=singleI) :: component_index(*)

        real(kind=doubleR) :: par_val(*), parval_err(*)
        
        logical :: output
        
        if (.not.output) return

        write(6,*)
        write(6,'(a)') trim(output_dashed_line)
        write(6,'(1x,a,2x,a,2x,a,6x,a,6x,a,5x,a,6x,a,3x,a)') 'component ID',
     &  'log(N)', 'error(%)', 'b', 'error(%)', 'vel_c', 'dvel_c', 'status'
        write(6,*)

!       total number working of components
        jmax = num_param

        do j=1,jmax,3
         write(6,'(i5,3x,3(2pe9.2,1x,2pe9.2,2x),i3)') component_index(j),
     &   dlog10(par_val(j+1)), 1.0d2*parval_err(j+1)/par_val(j+1),
     &   par_val(j+2), 1.0d2*parval_err(j+2)/par_val(j+2),
     &   par_val(j), parval_err(j),
     &   IONS(true_ion(component_index(j)))%component(component_index(j))%status
        end do
        write(6,*)

!       ------------------------------------------------------------------------
        return

        end subroutine verbose_output
!       ========================================================================

!       ========================================================================
        subroutine write_chi2_evolution(flag,run,chi2_reduced)

!       writes out chi^2 spectrum for visualisation
        
        use set_precision
        use ifit_variables, only: file_chi2_evolution, chi2_evolution,
     +  component_number_evolution, num_param, tot_iter_per_abs_profile,
     +  fit_attempt

        implicit none
        
        integer(kind=singleI), intent(in) :: flag
        
        integer(kind=singleI) :: loop

        real(kind=doubleR) :: chi2_reduced

        logical, intent(in) :: run
        
        
!       do only of required
        if (.not.run) return

!       define loop limits
        
        tot_iter_per_abs_profile = tot_iter_per_abs_profile + 1
        
        chi2_evolution(tot_iter_per_abs_profile) = chi2_reduced

        component_number_evolution(tot_iter_per_abs_profile) =
     &  ceiling(dble(num_param)/3.0d0)
            
        select case (flag)
        
!       ------------------------------------------------------------------------
        case(1)
!       do nothing

!       ------------------------------------------------------------------------
        case (2)

!       open file with chi^2 to visualise fit behaviour for each absorption
!       profile
         open(200,file=trim(file_chi2_evolution))
       
!       read until end of file
          do
           read(200,*,end=90)
          end do
 90       continue
          backspace(200) ! avoid end-of-file (EOF) marker
 
!       output chi^2 values to visualise fit behaviour for each absorption
!       profile
          write(200,*) '#', fit_attempt
          do loop = 1,tot_iter_per_abs_profile
           write(200,*)
     &     loop, chi2_evolution(loop), component_number_evolution(loop)
          end do

!       insert double space to separate blocks in file chi2_abs_profile_*.vis
!       (for easy visualisation with gnuplot)
           write(200,*) 
           write(200,*) 

!       close file with chi^2 to visualise fit behaviour for each absorption
!       profile
         close(200)

!       ------------------------------------------------------------------------
        case (3)

!       open file with chi^2 to visualise fit behaviour for each absorption
!       profile
         open(200,file=trim(file_chi2_evolution))
       
!       read until end of file
          do
           read(200,*,end=100)
          end do
 100      continue
          backspace(200) ! avoid end-of-file (EOF) marker
 
!       rewind one block
          do loop = 1,tot_iter_per_abs_profile+2
           backspace(200) ! avoid end-of-file (EOF) marker
          end do
 
!       output chi^2 values to visualise fit behaviour for each absorption
!       profile
          write(200,*) '#', fit_attempt
          do loop = 1,tot_iter_per_abs_profile
           write(200,*)
     &     loop, chi2_evolution(loop), component_number_evolution(loop)
          end do

!       insert double space to separate blocks in file chi2_abs_profile_*.vis
!       (for easy visualisation with gnuplot)
           write(200,*) 
           write(200,*) 

!       close file with chi^2 to visualise fit behaviour for each absorption
!       profile
         close(200)

        end select
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        return

        end subroutine write_chi2_evolution
!       ========================================================================

!       ========================================================================
        subroutine component_statistics(attempt,cycl)

!       Prints useful information to the screen

        use set_precision
        use ifit_variables

        implicit none
        
        integer(kind=singleI), intent(in) :: attempt, cycl
        
        integer(kind=singleI) :: i
        integer(kind=singleI) :: num_comps, current_comps, discardable_comps
        character(len=64) :: routinemsg = 'component_statistics: '
        

!       ------------------------------------------------------------------------
!       consistency check

        if (cycl.le.2) then ! during sweep=1,2

         num_comps = count(abs(IONS(ION_INDEX)%component(:)%status).eq.1)

        else ! during sweep=3

         num_comps = 0
         do i=1,num_ions
          num_comps =
     &    num_comps + count(abs(IONS(i)%component(:)%status).eq.1)
         end do

        end if 

        if ((num_comps - fitted_components)
     &  .ne.(ceiling(dble(num_param)/3.0d0))) then
         write(6,*) trim(errmsg)//trim(routinemsg)//'Non-matching number'//
     &   ' of working components and parameters:',
     &   num_comps - fitted_components, ceiling(dble(num_param)/3.0d0)
         stop 1
        end if

!       ------------------------------------------------------------------------
!       determine number of components
        current_comps = ceiling(dble(num_param)/3.0d0)

        discardable_comps = 0
        do i=1,num_param,3
         if (
     &   IONS(true_ion(component_id(i)))%component(component_id(i))%status.eq.1)
     &    discardable_comps = discardable_comps + 1
        end do
        
!       do only once per absorption profile
        if (attempt.gt.1) then
         write(6,'(a)') trim(output_dashed_line)
         write(6,'(a,3(i4))')
     &   ' components in current absorption profile (working | discardable):',
     &   current_comps, discardable_comps
         if (.not.quite) write(6,*)
         return
        end if

!       ------------------------------------------------------------------------
!       supress further output if so required
        if (quite) return
!       ------------------------------------------------------------------------

        write(6,'(a)') trim(output_dashed_line)
        write(6,'(a,33x,a)') ' ABSORPTION TYPE:', trim(absorption_type_str)
        write(6,'(a,39x,a)') ' FIT USING:', trim(fit_type_str)
        write(6,'(a,37x,a)') ' SMOOTH FLUX:', trim(smoothing_str)
        write(6,'(a,32x,i5)') ' total components:', num_components
        write(6,'(a,26x,i5)') ' total valid components:', num_comps
        write(6,'(a,27x,i5)') ' valid components here:',
     &  (ceiling(dble(num_param)/3.0d0))

        if (verbose) then
         write(6,'(a,28x,i5)') ' total fitted components:', fitted_components
         write(6,'(a,25x,i5)') ' total discarded components:',
     &   discarded_components
         write(6,'(a,29x,i5)') ' total added components:',
     &   inserted_components
        end if

        write(6,'(a,23x,i8,1pe12.5)') ' maximum pixel / velocity:',
     &  spectrum_size_px, velocity(spectrum_size_px)
        write(6,'(a,25x,i5)') ' spectrum shift (pixel):',
     &  spectrum_shift_px
        write(6,'(a,36x,2(i8))') ' pixel range:',
     &  max(1,mod(pixel_min+spectrum_shift_px,spectrum_size_px+1)),
     &  max(1,mod(pixel_max+spectrum_shift_px,spectrum_size_px+1))
        write(6,'(a,28x,2(i8))') ' shifted pixel range:', pixel_min, pixel_max
        write(6,'(a,25x,2(1pe12.5))') ' velocity range [km/s]:', 
     &  velocity(max(1,mod(pixel_min+spectrum_shift_px,spectrum_size_px+1))),
     &  velocity(max(1,mod(pixel_max+spectrum_shift_px,spectrum_size_px+1)))
        write(6,'(a,24x,2(1pe12.5))') ' shifted velocity range:',
     &  velocity_min, velocity_max 
        write(6,'(a,24x,2(1pe12.5))') ' wavelength range [Ang]:', 
     &  wlength(max(1,mod(pixel_min+spectrum_shift_px,spectrum_size_px+1))),
     &  wlength(max(1,mod(pixel_max+spectrum_shift_px,spectrum_size_px+1)))
        write(6,'(a,23x,2(1pe12.5))') ' shifted wavelength range:',
     &  wlength(pixel_min), wlength(pixel_max)
        write(6,*)
        write(6,'(a,27x,1pe12.4)') ' local pixel size [km/s]:',
     &  pixel_size_kms_local
        write(6,*)
        write(6,'(a,11x,f8.2)') ' local average signal-to-noise ratio:',
     &  average_sn_local
        write(6,'(a,27x,1pe12.4)') ' local average noise:',
     &  average_noise_local
        write(6,'(a,18x,1pe12.4)') ' local average noise (smooth):',
     &  average_noise_local_smooth
        write(6,'(a,23x,2(1pe12.4))') ' minimum / maximum noise:',
     &  minval(flux_residum_error(1:absprofile_size_px)),
     &  maxval(flux_residum_error(1:absprofile_size_px),
     &  flux_residum_error(1:absprofile_size_px).lt.huge(1.0)) 
        write(6,*)
        
!       ------------------------------------------------------------------------
!       verbose output; useful for debbuging
!       effective if debug_str = TRUE in input parameter file
        call verbose_output(debug,trial_param,trial_param_err,component_id)

!       ------------------------------------------------------------------------
        return

        end subroutine component_statistics
!       ========================================================================


!       ========================================================================
        subroutine abs_profile_visual_final(run)

!       induces visualisation of fit for current absorption profile
!       this won't be executed unless viusal = true at input

        use ifit_variables, only: statmsg, visual_counter, IONS,
     +  ION_INDEX, fwhm_kms, test_run,
     +  abs_profile_id, abs_profile_id_str, num_param, fit_attempt

        implicit none
        
        character(len=512) :: command
        logical, intent(in) :: run
        
        if (.not.run) return

          if (run.and.(num_param.ge.3)) then

           write(6,*)
           write(6,'(1x,a,i5)') trim(statmsg)//
     &     'Visualising fitted absorption profile:', abs_profile_id
           write(6,'(19x,a,2i5)') 'fitting attempts:',
     &     visual_counter, fit_attempt

           if (fwhm_kms.gt.0.0d0) then
            command = 'gnuplot visual_'//trim(adjustl(abs_profile_id_str))//
     &      '_'//trim(IONS(ION_INDEX)%name)//'_conv.plot'
           else
            command = 'gnuplot visual_'//trim(adjustl(abs_profile_id_str))//
     &      '_'//trim(IONS(ION_INDEX)%name)//'_noconv.plot'
           end if
           call system(trim(command))

!       Remove previous files
           if (.not.test_run) then
            write(6,'(19x,a)') 'Removing files used for visualisation...'
            call system('rm visual_*_'//trim(IONS(ION_INDEX)%name)//'*.vis')
            call system('rm visual_*_'//trim(IONS(ION_INDEX)%name)//'*.plot')
            call
     &      system('rm chi2_abs_profile_*_'//trim(IONS(ION_INDEX)%name)//'.vis')
           end if

          end if !visual = true

!       ------------------------------------------------------------------------
        return

        end subroutine abs_profile_visual_final
!       ========================================================================

!       ========================================================================
        subroutine fit_summary_std_out

!       prints fit summary to standard output
!       IMPORTANT: invoke only necessary variables

        use set_precision
        use ifit_variables

        implicit none

        integer(kind=singleI) :: ionindex

        write(6,*)
        write(6,'(a)') trim(output_dashed_line)
        write(6,'(a)') trim(output_dashed_line)
        write(6,'(1x,a)') 'FIT SUMMARY:'
        write(6,*)

!       book-keeping

        write(6,'(18x,a,17x,a)') 'reference ion: ', trim(IONS(ION_INDEX)%name)
        do ionindex=1,num_ions
         write(6,'(18x,a,17x,a)') 'fitted ion(s): ', trim(IONS(ionindex)%name)
        end do
        write(6,*)
        write(6,'(18x,a,20x,a)') 'profile: ', trim(profile_str)
        write(6,*)

        if (spectrum_shifted) then
         write(6,'(18x,a,13x,i6)') 'shift [pixel]: ', spectrum_shift_px
         write(6,*)
        end if

        write(6,'(18x,a,10x,i10)')
     &  'spectrum size [pixel]:', spectrum_size_px
        write(6,'(18x,a,9x,f9.4)')
     &  'average pixel size [Ang]:', pixel_size_ang
        write(6,'(18x,a,8x,f9.4)')
     &  'average pixel size [km/s]:', pixel_size_kms
        write(6,*)

        if (neg_flux) then
         write(6,'(18x,a,i5)') 'Negative/zero input flux values:',
     &   neg_flux_counts
         write(6,*)
        end if

        if (neg_noise) then
         write(6,'(18x,a,i4)') 'Negative/zero input noise values:',
     &   neg_noise_counts
         write(6,*)
        end if

        if (bad_pixel) then
         write(6,'(18x,a,16x,i4)') 'bad pixel(s):', bad_pixel_counts
         write(6,*)
        end if

        write(6,'(18x,a,16x,1p,e8.2)')
     &  'average S/N:', average_sn_global
        write(6,'(18x,a,14x,1p,e8.2)')
     &  'average noise:', average_noise_global
        write(6,*)

        if (fwhm_kms.gt.0.0d0) then
         write(6,'(18x,a,f6.2)') 'Gaussian LSF (FWHM [km/s]): ',
     &   fwhm_kms
         write(6,*)
        end if
        
        write(6,'(18x,a)') 'total number of absorption profiles'
        write(6,'(18x,a,9x,i4)') 'initially detected:', num_abs_profiles_save
        write(6,'(18x,a,19x,i4)') 'splitted:', splitted_abs_profiles
        write(6,'(18x,a,18x,i4)') 'processed:', processed_abs_profiles
        write(6,'(18x,a,22x,i4)') 'empty:', empty_abs_profiles
        write(6,*)
        write(6,'(18x,a)') 'total number of components'
        write(6,'(18x,a,23x,i4)') 'input:', num_components_save
        if (fwhm_kms.gt.0.0d0) then
         write(6,'(18x,a,23x,i4)') 'valid:', valid_components_save
        else
         write(6,'(18x,a,20x,i4)') 'inserted:', inserted_components
        end if
        write(6,'(18x,a,19x,i4)') 'discarded:', discarded_components
        write(6,'(18x,a,22x,i4)') 'fitted:', fitted_components
        write(6,*)
        write(6,'(18x,a,16x,f8.3)') 'average reduced chi^2: ',
     &  mean_chi2 

!       reset value if no components were found
        if (min_chi2(1).eq.huge(real_double)) min_chi2(1) = 0.0d0

        write(6,'(18x,a,2x,f8.3,i5)') 'minimum reduced chi^2 | abs.prof.ID: ',
     &  min_chi2(1), int(min_chi2(2))
        write(6,'(18x,a,2x,f8.3,i5)') 'maximum reduced chi^2 | abs.prof.ID: ',
     &  max_chi2(1), int(max_chi2(2))
        write(6,'(a)') trim(output_dashed_line)
        write(6,*)

!       ------------------------------------------------------------------------
        return

        end subroutine fit_summary_std_out
!       ========================================================================

!       ========================================================================
        subroutine comp_time

!       prints out computation time

        use ifit_variables, only: time_start, time_end, itime_start, itime_end,
     +  output_dashed_line

        implicit none

        itime_end = time8()
        time_end = ctime(itime_end)

        write(6,'(18x,a,16x,a)') 'computation start:',
     &  trim(time_start)
        write(6,'(30x,a,18x,a)') 'end:', trim(time_end)
        print
     &  '(30x,"duration (hh:mm:ss:cs):",12x,i2.2,":",i2.2,":",i2.2,":",i2.2)',
     &  int((itime_end-itime_start)/3600.),
     &  int(dble(mod(itime_end-itime_start,3600))/6.0d1),
     &  int(mod(mod(itime_end-itime_start,3600),60)),
     &  int(1.0d2*((dble(mod(itime_end-itime_start,3600))/6.0d1) -
     &  int(dble(mod(itime_end-itime_start,3600))/6.0d1)))
        write(6,*)
        write(6,'(a)') trim(output_dashed_line)
        write(6,*)


!       ------------------------------------------------------------------------
        return

        end subroutine comp_time
!       ========================================================================

!       ========================================================================
        integer(kind=singleI) function count_lines(absprofile_id)

!       NOTE:
!       determines number of lines in region defined by [vmin,vmax]

        use set_precision
        use ifit_variables, only: IONS, ION_INDEX, num_ions, abs_profile_bounds,
     +  velocity, quite, debug, colour_statmsg_prefix, colour_msg_suffix, sweep
        
        implicit none

        integer(kind=singleI), intent(in) :: absprofile_id

        integer(kind=singleI) :: ionindex

        real(kind=doubleR) :: v_min, v_max
        
        character(len=13) :: routinemsg='count_lines: '

!       ------------------------------------------------------------------------
!       initialise
        
        count_lines = 0

!       define region boundaries
        
        v_min = velocity(abs_profile_bounds(absprofile_id,1))
        v_max = velocity(abs_profile_bounds(absprofile_id,2))

!       determine number of lines in region; exclude processed, discarded lines
!       that might have been shifted into region while fitting another region;
!       this would not happen if regions are not expanded, or combined after
!       expanding (see detect_region)


        if (sweep.le.2) then ! during sweep=1,2

         count_lines =
     &   count((IONS(ION_INDEX)%component(:)%vel_c.ge.floor(v_min)).and.
     &   (IONS(ION_INDEX)%component(:)%vel_c.le.ceiling(v_max)).and.
     &   IONS(ION_INDEX)%component(:)%status.ne.0)

        else ! during sweep=3


         do ionindex=1,num_ions
          count_lines = count_lines + 
     &    count((IONS(ionindex)%component(:)%vel_c.ge.floor(v_min)).and.
     &    (IONS(ionindex)%component(:)%vel_c.le.ceiling(v_max)).and.
     &    IONS(ionindex)%component(:)%status.ne.0)
         end do

         end if

!       ------------------------------------------------------------------------
!       print routine name for information
        if (debug)
!        if (.not.quite)
     &  write(6,'(a,2i6)') trim(colour_statmsg_prefix)//trim(routinemsg)//
     &  trim(colour_msg_suffix)//' region | lines:', absprofile_id, count_lines

!       ------------------------------------------------------------------------
        return

        end function count_lines
!       ========================================================================

!       ========================================================================
        subroutine gplot(ionindex)       
        
        use set_precision
        use ifit_variables, only: IONS, abs_profile_bounds, spectrum_size_px,
     +  velocity, spectrum_shifted, spectrum_shift_px, num_abs_profiles,
     +  z_min, true_transition, num_components, ifit_file_prefix,
     +  colour_statmsg_prefix, colour_msg_suffix, true_ion

        use gplot_variables
        use constants, only: c_kms

        implicit none

        integer(kind=singleI), intent(in) :: ionindex

        integer(kind=doubleI) :: i
        integer(kind=singleI) :: pixel
        integer(kind=doubleI) :: seed

        real(kind=doubleR) :: gasdev ! function

        character(len=256) :: psfile_dat, dummy_str, prefix, num_absprofiles_str

        logical :: file_exists

        character(len=7) :: routinemsg='gplot: '

!       ------------------------------------------------------------------------
!       print routine name for information
        write(6,'(a)') trim(colour_statmsg_prefix)//trim(routinemsg)//
     &  trim(colour_msg_suffix)

!       ------------------------------------------------------------------------
!       skip if no lins have been identifieds
        if (num_components.eq.0) then
         write(6,'(a)') 'no significant absorption detected'
         return
        end if
!       ------------------------------------------------------------------------
!       generate Gaussian random numbers to perform a statistical comparison of
!       residuals to intrinsic noise

        if (allocated(gauss_dev)) deallocate(gauss_dev)
        allocate(gauss_dev(1:spectrum_size_px))

!       seed to initialise Gaussian random number generator;
!       use a random seed
        call system('echo $RANDOM > seed')
        open(10,file='seed')
        read(10,*) seed
        close(10)

        seed= -1*seed

        gauss_dev(:) = 0.0d0

        do pixel=1,spectrum_size_px
         gauss_dev(pixel) = gasdev(seed)
        end do

!       ------------------------------------------------------------------------
!       compute *true* absorption profile boundaries (in case
!       spectrum has been shifted)

!       store total number of absorption profiles
        num_abspro_gplot = num_abs_profiles

        if (allocated(abspro_bounds_true)) deallocate(abspro_bounds_true)
        if (allocated(abspro_bounds_aux)) deallocate(abspro_bounds_aux)
        allocate(abspro_bounds_true(1:num_abspro_gplot,2))
        allocate(abspro_bounds_aux(1:num_abspro_gplot,2))
        abspro_bounds_true(:,:) = 0
        abspro_bounds_aux(:,:) = 0

!       compute and store *true* absorption redshift / velocity centre (in case
!       spectrum has been shifted)

        if (allocated(z_abs_true)) deallocate(z_abs_true)
        if (allocated(vel_c_true)) deallocate(vel_c_true)
        allocate(z_abs_true(1:num_components))
        allocate(vel_c_true(1:num_components))
        z_abs_true(1:num_components) = 0.0d0
        vel_c_true(1:num_components) = 0.0d0

!       ----------------------------------------------------------------
        SELECT CASE(spectrum_shifted)

!       ----------------------------------------------------------------
        CASE(.false.)

!       absorption profile boundaries

         abspro_bounds_true(1:num_abspro_gplot,1:2) =
     &   abs_profile_bounds(1:num_abspro_gplot,1:2)


!       line parameters
         do i=1,num_components

!       select valid component
          if (abs(IONS(true_ion(i))%component(i)%status).eq.1) then

!       transition's least readshift
           zmin = z_min(true_ion(i),true_transition(i))

!       central absorption redshift / velocity centre of i-th component

           vel_c_true(i) = IONS(true_ion(i))%component(i)%vel_c

           z_abs_true(i) = ((1.0d0+zmin) * dexp(vel_c_true(i)/c_kms)) - 1.0d0

          end if ! valid component

         end do !i-loop over components

!       ----------------------------------------------------------------
        CASE(.true.)

!       absorption profile boundaries

!       initialise counter
         i=1

         do while (i.le.num_abspro_gplot)

!       define lower and upper index bounds

          abspro_bounds_true(i,1) =
     &    mod(abs_profile_bounds(i,1)+spectrum_shift_px,spectrum_size_px)

          abspro_bounds_true(i,2) =
     &    mod(abs_profile_bounds(i,2)+spectrum_shift_px,spectrum_size_px)

!       If any of the above happens to be zero (an infortunate consequence
!       of the use of mod()), re-define:

          if (abspro_bounds_true(i,1).eq.0)
     &     abspro_bounds_true(i,1) = spectrum_size_px

          if (abspro_bounds_true(i,2).eq.0)
     &     abspro_bounds_true(i,2) = spectrum_size_px
        
!       Check that absorption profiles are not splitted across the spectrum
!       boundary; if so, define two new absorption profiles;
!       IMPORTANT: increase number of identified absorption profiles by one
         
          if (abspro_bounds_true(i,1).gt.abspro_bounds_true(i,2)) then

!       save values stored so far
           abspro_bounds_aux(1:i,1:2) = abspro_bounds_true(1:i,1:2)

!       de/re-allocate array increasing size by one along dim=1
           deallocate(abspro_bounds_true)
           allocate(abspro_bounds_true(1:size(abspro_bounds_aux,dim=1)+1,2))

!       store values saved and new values
           abspro_bounds_true(1:i,1) = abspro_bounds_aux(1:i,1)
           abspro_bounds_true(1:i,2) = abspro_bounds_aux(1:i,2)

           abspro_bounds_true(i,1) = abspro_bounds_aux(i,1)
           abspro_bounds_true(i,2) = spectrum_size_px
           abspro_bounds_true(i+1,1) = 1
           abspro_bounds_true(i+1,2) = abspro_bounds_aux(i,2)

!       increase counter by one to avoid overwriting
           i=i+1

!       increase effective number of absorption profiles
           num_abspro_gplot = num_abspro_gplot + 1

          end if

!       increase couter by one to process all absorption profiles
           i=i+1
         
        end do ! while i <= total absorption profiles


!       line parameters
         do i=1,num_components

!       select valid component
          if (abs(IONS(true_ion(i))%component(i)%status).eq.1) then

!       locate pixel corresponding to fitted component centroid with respect
!       to UN-SHIFTED velocity array:
          
           arr_pos_min =
     &     maxloc(velocity,(velocity.le.IONS(true_ion(i))%component(i)%vel_c))

!       NOTE:
!       shift component centroid to original position, i.e. before shifting
!       spectrum; compute the off-set 'dv_shift' between fitted
!       component centroid and velocity bin.
!       Note that dv_shift >= 0 by definition

           velocity_shift_px =
     &     mod(arr_pos_min(1)+spectrum_shift_px,spectrum_size_px)

           dv_shift =
     &     IONS(true_ion(i))%component(i)%vel_c - velocity(arr_pos_min(1))

!       NOTE:
!       Correct for the case when vel_shift happens to be 0, implying that
!       component centroid is at upper spectrum boundary; redefine dv_shift
!       to be negative!

           if (velocity_shift_px.eq.0) then
            velocity_shift_px = spectrum_size_px
            dv_shift = -1.0d0*dv_shift
           end if

!       transition's least readshift
           zmin = z_min(true_ion(i),true_transition(i))

!       central absorption redshift / velocity centre of i-th component

           vel_c_true(i) = velocity(velocity_shift_px)+dv_shift

           z_abs_true(i) = ((1.0d0+zmin) * dexp(vel_c_true(i)/c_kms)) - 1.0d0
           
          end if ! valid component

         end do !i-loop over components


        END SELECT ! spectrum shifted
!       ----------------------------------------------------------------

!       ------------------------------------------------------------------------
!       create data file with different blocks blocks

!       common data file
        
        psfile_dat=trim(ifit_file_prefix(ionindex))//'.gpf'

!       Remove data file if existent
        
        inquire(file=trim(psfile_dat),exist=file_exists)
        if (file_exists) call system('rm -f '//trim(psfile_dat))

!       Block 1: (wavel.,flux,fit,[v_Hubble],flux_nonoise,noise(random),sigma)
        write(6,'(a)',advance='no') ' creating model spectrum file...'
        call create_datafile_spec(psfile_dat)
        write(6,'(a)',advance='no') 'done.'
        write(6,*)

!       Block 2+3: line parameters: (in wavelength/v_Hubble, in redshift);
!       skip if no graphical output desired
        write(6,'(a)',advance='no') ' creating line-parameter file...'
        call create_datafile_lineparams(psfile_dat,num_components)
        write(6,'(a)',advance='no') 'done.'
        write(6,*)

!       N.B.: Output for blocks 2 is slightly different for 'term' and
!       'ps'. In the former, plus-minus sign is simply replaced by
!       brackets; also, column densities (and corresponding errors) are
!       given in logarithmic scale

!       Block 4: (residuals ,noise)
!       only for pixels within detected regions
        write(6,'(a)',advance='no') ' creating residuals file...'
        call create_datafile_residuals(psfile_dat)
        write(6,'(a)',advance='no') 'done.'
        write(6,*)

!       ------------------------------------------------------------------------
!       PostScript output

!       Remove previous plot-/ps-file if existent

        prefix=trim(ifit_file_prefix(ionindex))

        inquire(file=trim(prefix)//'.plot',exist=file_exists)
        if (file_exists) call system('rm -f '//trim(prefix)//'.plot')

        inquire(file=trim(prefix)//'.ps',exist=file_exists)
        if (file_exists) call system('rm -f '//trim(prefix)//'.ps')

!       Write ONE gnuplot file per detected region; this loop starts
!       from i=0, where region = 0 means the full spectrum, and i=-1
!       plots the residuals, i.e. (fit - spectrum) and noise

        write(num_absprofiles_str,'(i3)') num_abspro_gplot

!       For outtype=ps, create only ONE gnuplot file with different
!       blocks, in order to plot spectra in a single document with
!       several pages (one page per detected absorption profile)

        call create_plotfile_ps_wl(ionindex,psfile_dat,prefix,spectrum_size_px)

!       Compile gnuplot file and open corresponding ps-file
         
        dummy_str='gnuplot '//trim(prefix)//'.plot'
        write(6,*)
        write(6,'(a)') trim(dummy_str)
        call system(trim(dummy_str))

        dummy_str='gv -orientation=landscape '//trim(prefix)//'.ps &'
        write(6,*)
        write(6,'(a)') trim(dummy_str)
        call system(trim(dummy_str))

!       Write temporal ps-file leaving out the first page (-p2), which
!       contains the residuals histogram

        dummy_str='psselect -e -o -p2- '//trim(prefix)//'.ps  temp.ps'
        write(6,*)
        write(6,'(a)') trim(dummy_str)
        call system(trim(dummy_str))

!       Merge all pages (as panels) into a one-single page file and open
!       corresponding ps-file
!       A maximum number of 9 panels per pages seems to be a nice layout,
!       hence, handle following cases:
!       NEW: include S/N in file name

!       Case: regions < 9
        
        if (num_abspro_gplot.le.9) then

!       Case: regions = 5 -> needs special handling since psnup fails at
!       finding an acceptable layout for this
         if (num_abspro_gplot.eq.5) then

          dummy_str='psnup -l -m25 -n 6 temp.ps '//trim(prefix)//'_thumbs.ps'

!       Case: regions = 7 -> IDEM
         else if (num_abspro_gplot.eq.7) then

          dummy_str='psnup -l -m25 -n 8 temp.ps '//trim(prefix)//'_thumbs.ps'

         else
        
           dummy_str='psnup -l -m25 -n '//trim(adjustl(num_absprofiles_str))//
     &     ' temp.ps '//trim(prefix)//'_thumbs.ps'
          
         end if

        else !regions > 9

           dummy_str='psnup -l -m25 -n 9 temp.ps '//trim(prefix)//'_thumbs.ps'

        end if

        write(6,*)
        write(6,'(a)') trim(dummy_str)
        call system(trim(dummy_str))

        dummy_str='gv '//trim(prefix)//'_thumbs.ps &'
        write(6,*)
        write(6,'(a)') trim(dummy_str)
        call system(trim(dummy_str))

!       remove temporal ps-file
        dummy_str='rm -f temp.ps'
        write(6,*)
        write(6,'(a)') trim(dummy_str)
        call system(trim(dummy_str))
         
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        return

        end subroutine gplot
!       ========================================================================

!       ========================================================================
        subroutine create_datafile_spec(outfile)

!       Creates a file 'outfile' containing three [four] columns:
!       wavelength | flux | fit | [Hubble velocity]
!       where [] indicates optional for short spectra

        use set_precision
        use ifit_variables, only: wlength, flux, modelflux_convolved, velocity,
     +  modelflux, sigma, spectrum_size_px
        use gplot_variables, only: gauss_dev, abspro_bounds_true, px_offset,
     +  num_abspro_gplot

        implicit none
        
        integer(kind=doubleI) :: abspro_id, pixel, px_min, px_max
        
        character(len=*), intent(in) :: outfile

!       ------------------------------------------------------------------------
!       Create file

        open(60,file=trim(outfile))

!       loop over individual absorption profiles

         do abspro_id=1,num_abspro_gplot

          px_min =
     &    max(1,abspro_bounds_true(abspro_id,1)-px_offset)
          px_max =
     &    min(abspro_bounds_true(abspro_id,2)+px_offset,spectrum_size_px)

          write(60,*) '# region ', abspro_id
          write(60,*) '# obs. wavelength, flux, fit [conv], Hubble velocity, '//
     &    'fit [no conv.], noise'
          write(60,*)

          do pixel=px_min,px_max
           write(60,*) wlength(pixel), flux(pixel), modelflux_convolved(pixel),
     &     velocity(pixel), modelflux(pixel), sigma(pixel)*gauss_dev(pixel),
     &     sigma(pixel)
          end do

          write(60,*)
          write(60,*)

         end do ! over individual absorption profiles
         
        close(60)
        
        end subroutine create_datafile_spec
!       ========================================================================

!       ========================================================================
        subroutine create_datafile_lineparams(outfile,lines)

!       Creates a file 'outfile' containing three [four] columns:
!       wavelength | flux | fit | [Hubble velocity]
!       where [] indicates optional for short spectra

        use set_precision
        use ifit_variables, only: IONS, true_transition, true_ion, lead, sweep
        use gplot_variables, only: z_abs_true, vel_c_true

        implicit none
        
        integer(kind=singleI), intent(in) :: lines
        character(len=*), intent(in) :: outfile

        integer(kind=doubleI) :: i
        real(kind=singleR) :: pos(0:2)
        
        character(len=256) :: bvalue_str, zabs_str, eqw_str, coldens_str,
     &  bvalue_str_err, coldens_str_err
        character(len=7) :: comp_id_status

!       ------------------------------------------------------------------------
!       Define labels positons (later for plot with gnuplot)
!       Alternate between values (2 for now), to plot labels at different
!       levels and avoid overlapping
!       Since LOADS of things depend on the number of parameters plotted
!       (4 for now), include a parameter that accounts for this!

         pos(0) = 1.16
         pos(1) = pos(0) + 0.04
         pos(2) = pos(1) + 0.14

!       ------------------------------------------------------------------------
!       Create file

        open(60,file=trim(outfile))
         
         do                ! read until end of file
          read(60,*,end=70)
         end do
  70     continue
         backspace(60) ! avoid end-of-file (EOF) marker

!       Block separation required by gnuplot multiple data
        
         write(60,*)                 
         write(60,*)                     
         write(60,*) '# Line center: in v_Hubble, in Redshift'
         write(60,*)
         
!       These are the line parameters which should in turn be included
!       in the spectra plots; the last columns are used to set the po-
!       sitions of the labels in the plot

         do i=1,lines

!       select valid component
          if (abs(IONS(true_ion(i))%component(i)%status).eq.1) then

!       write line parameters into strings to facilitate label positioning

           write(zabs_str,'(f8.4)') z_abs_true(i)
           write(bvalue_str,'(f6.2)') IONS(true_ion(i))%component(i)%b_value
           write(bvalue_str_err,'(f6.2)')
     &     IONS(true_ion(i))%component(i)%b_value_err
           write(eqw_str,'(f6.2)')
     &     1.0d3*IONS(true_ion(i))%component(i)%eq_width! in mili-Angst.
           write(coldens_str,'(f6.2)')
     &     dlog10(IONS(true_ion(i))%component(i)%col_dens)
           write(coldens_str_err,'(f6.2)')
     &     dlog10(IONS(true_ion(i))%component(i)%col_dens_err)

!       The entries below are needed to include in the plot the line
!       parameters +/- errors at the right positions. The encoding
!       {/CMSY10 \\006} for +/- only works if label is enclosed in quotation
!       marks, and the gnuplot files loads the required font cmsy10.pfb
!       (see create_plotfile_ps below)
!       double backslash required since the first is escaped: \\ -> \
!       The line ID is now included as last (10) entry as an alternative way
!       of labelling lines, since for crowded regions this is not working
!       properly due to extreme overlapping. The idea is to
!       include the line center '|', redshift and line ID as usual, and
!       include a legend with the line ID's and corresponding parameters

           write(60,'(f12.4,4(1x,a),4(1x,f5.2),1x,i2)')
     &     vel_c_true(i),
     &     'z='//trim(adjustl(zabs_str)),
     &     '"b='//trim(adjustl(bvalue_str))//'{/CMSY10 \\006}'//
     &     trim(adjustl(bvalue_str_err))//'"',
     &     'EW='//trim(adjustl(eqw_str)),
     &     '"N=10^{'//trim(adjustl(coldens_str))//'}{/CMSY10 \\006}10^{'
     &     //trim(adjustl(coldens_str_err))//'}"',
     &     pos(mod(i,2)), pos(mod(i,2))+0.04, pos(mod(i,2))+0.08,
     &     pos(mod(i,2))+0.12,
     &     i
           
          end if ! valid component

         end do !i-loop over components

         write(60,*)                     
         write(60,*)                     
         write(60,*) '# line center: velocity | redshift | line ID | ion | wl_0'
         write(60,*)
         
!       These block includes the line parameters used when alternative
!       labelling is switched on (default; see create_plotfile_ps_wl below)

         do i=1,lines

!       select valid component
         if (abs(IONS(true_ion(i))%component(i)%status).eq.1) then

!       write line parameters into strings to facilitate label positioning
          write(zabs_str,'(f8.4)') z_abs_true(i)

!       flag component ID status (identified / unidentified)
          comp_id_status = '{\305} '
          if (sweep.eq.3) then
           if (lead(i)%component.eq.0)
     &     comp_id_status = '{\305}?'
	  end if

          write(60,'(f12.4,1x,a,2(1x,f5.2),1x,i4,1x,a6,1x,f10.4,a7,1x,f5.2)')
     &    vel_c_true(i),
     &    trim(adjustl(zabs_str)), pos(0), pos(1), i,
     &    IONS(true_ion(i))%name,
     &    IONS(true_ion(i))%transition(true_transition(i))%lambda_0,
     &    comp_id_status,
     &    pos(2)

         end if ! valid component

         end do

        close(60)

!       ------------------------------------------------------------------------
        return

        end subroutine create_datafile_lineparams
!       ========================================================================

!       ========================================================================
        subroutine create_datafile_residuals(outfile)

!       Creates a file 'outfile' containing two columns:
!       resiudals | noise 

        use set_precision
        use ifit_variables, only: flux, modelflux_convolved, sigma
        use gplot_variables, only: gauss_dev, abspro_bounds_true,
     +  num_abspro_gplot

        implicit none
        
        integer(kind=singleI) :: abspro_id, pixel, px_min, px_max
        
        character(len=*), intent(in) :: outfile

        integer(kind=singleI) :: count_lines ! function

!       ------------------------------------------------------------------------
!       Create file

        open(60,file=trim(outfile))

         do        !reading until end of file
          read(60,*,end=70)
         end do
  70     continue
         backspace(60) ! avoid end-of-file (EOF) marker

         write(60,*)                 

!       Block separation required by gnuplot multiple data

         write(60,*)                 
         write(60,*) '# residuals, noise'
         write(60,*)
          
         do abspro_id=1,num_abspro_gplot
          if (count_lines(abspro_id).gt.0) then
           px_min = abspro_bounds_true(abspro_id,1)
           px_max = abspro_bounds_true(abspro_id,2)
           do pixel=px_min,px_max
            write(60,*) flux(pixel)-modelflux_convolved(pixel),
     &      sigma(pixel)*gauss_dev(pixel)
           end do
          end if
         end do
         

        close(60)
        
        end subroutine create_datafile_residuals
!       ========================================================================

!       ========================================================================
        subroutine create_plotfile_ps_wl(ionindex,datafile,gpfile_prefix,maxval)

!       Creates a file to plot a spectrum as a function of wavelength (long)
!       or Hubble velocity (short). 
!       Datafiles contain two blocks: (wavelength,flux,fit[,Hubble velocity])
!       and (line center in wavelength/redshift), respectively.
!       Loaded it within gnuplot. Output is to term.
        
        use set_precision
        use ifit_variables, only: IONS, true_ion, wlength, velocity, 
     +  ifit_file_prefix, num_components, average_sn_global, empty_abs_profiles,
     +  spectrum_size_px, spectrum_shift_px

        use gplot_variables
        use constants, only: c_kms

        implicit none
        
        integer(kind=singleI), intent(in) :: ionindex, maxval
        character(len=*), intent(in) :: datafile, gpfile_prefix

        integer(kind=singleI) :: i
        integer(kind=singleI) :: abspro_id
        integer(kind=singleI) :: absprof_indxx, full_spec_indxx,
     +  comp_param_inset_indxx, comp_param_above_indxx, residuals_indxx

        integer(kind=doubleI) :: low, high, bound

        integer(kind=singleI) :: num_comps_local

        real(kind=singleR) :: posit

        real(kind=doubleR) :: vel_low, vel_hi, wl_low, wl_hi

        character(len=256) :: title, abspro_id_str

        character(len=6) :: num_comps_str, num_comps_local_str,
     +  num_absprofiles_str

        character(len=32) :: bvalue_str, zabs_str, eqw_str, col_dens_str,
     +  bvalue_str_err, col_dens_str_err, average_sn_global_str,
     +  spectrum_shift_px_str

        integer(kind=singleI) :: count_lines ! function

        logical :: alternative_labelling = .true.

!       ------------------------------------------------------------------------
!       write relevant parameter values into string

          write(num_absprofiles_str,'(i6)')
     &    num_abspro_gplot - empty_abs_profiles
          write(num_comps_str,'(i6)') num_components
          write(average_sn_global_str,'(f6.2)') average_sn_global
          write(spectrum_shift_px_str,'(i6)') spectrum_shift_px

!       create gnuplot script file
        
        open(50,file=trim(gpfile_prefix)//'.plot')

!       HEADER

           write(50,*) '# HEADER'
           write(50,*)

           write(50,*) '# clean start'
           write(50,*) 'reset'
           write(50,*)

!       set fontpath for using fancy symbols (see below)

          write(50,*) '# Set font path'
          write(50,*)
     &    "set fontpath '/usr/share/texmf/fonts/type1/public/amsfonts/cm/'"

!       Set term x11 and font cmsy10.pfb for special symbols (CMSY10 font) in
!       OMS encoding (see table ~/ps_fontfile_doc.ps)

          write(50,*) '# Set term x11 and font cmsy10.pfb for special symbols '
     &    //'(CMSY10 font) in OMS encoding (see table ~/ps_fontfile_doc.ps)'
          write(50,*) 'set term post enhanced color landscape fontfile '
     &    //'"cmsy10.pfb" font 20 dashlength 4'

!       set output file and plot legend

          write(50,*) 'set output "'//trim(gpfile_prefix)//'.ps"'
          write(50,*) 'set key bottom right samplen 2'
          write(50,*)

!       include fancy encoding (e.g. for "Angstroem" (\305) symbol)

          write(50,*) '# include fancy symbol fonts'
          write(50,*) 'set encoding iso_8859_1'
          write(50,*)

!       set plot margins

          write(50,*) '# Fixed position using screen coordinates where:'
          write(50,*) '# (0,0)=bottom-left corner of PAGE'
          write(50,*) '# (1,1)=top-right corner of PAGE'
          write(50,*) '# The following setting are such that the printing'
          write(50,*) '# area is maximed, reducing the margin around the plot'
          write(50,*) '# to a bearable minimum (for this type of plot)'
          write(50,*) 'set lmargin screen 0.12'
          write(50,*) 'set rmargin screen 0.99'
          write(50,*) 'set bmargin screen 0.08'
          write(50,*) 'set tmargin screen 0.90'
          write(50,*)

          write(50,*) '# Set alternative x-axis'
          write(50,*) 'set xtics nomirror'
          write(50,*) 'set mxtics'
          write(50,*) 'set x2tics'
          write(50,*) 'set mx2tics'
          write(50,*)

          write(50,*) '# Set y-axis format'
          write(50,*) 'set ytics format "%5.2f"'
          
!       TITLE

!       replace underscore(s) from spectrum file name by '-';
!       this is required for the title since gnuplot places the character
!       following an underscore as an index

         do i=1,len_trim(ifit_file_prefix(ionindex))
          if (ifit_file_prefix(ionindex)(i:i).eq.'_')
     &     ifit_file_prefix(ionindex)(i:i)='-'
         end do


!       ------------------------------------------------------------------------
!        PLOT PANELS

!       set appropriate plotting indices for full spectrum, residuals, and
!       component parameters; the corresponding parameter for individual
!       absorption profiles is set in corresponding panel below

         full_spec_indxx = num_abspro_gplot-1
         comp_param_inset_indxx = num_abspro_gplot
         comp_param_above_indxx = num_abspro_gplot+1
         residuals_indxx = num_abspro_gplot+2

!       ------------------------------------------------------------------------
!        RESIDUALS HISTOGRAM

!       NOTE: title is common to following three panels

           write(50,*) '# Set title'
           write(50,*)

           title=trim(ifit_file_prefix(ionindex))//
     &     ' S/N= '//trim(adjustl(average_sn_global_str))//
     &     ' regions: '//trim(adjustl(num_absprofiles_str))//
     &     ' lines: '//trim(adjustl(num_comps_str))//
     &     ' SHIFT = '//trim(adjustl(spectrum_shift_px_str))

           write(50,*) 'set title "{/=10 '//trim(title)//'}"'

           write(50,*) '# RESIDUALS HISTOGRAM'
           write(50,*)

           write(50,*) 'set xrange[*:*]'
           write(50,*) 'set yrange[*:*]'
           write(50,*) 'set ytics format "%5.2f"'
           write(50,*) 'set ylabel "PDF"'
           write(50,*) 'set xlabel "residuals"'
           write(50,*)

           write(50,*) '# Box width'
           write(50,*) 'bw = ', 2.0d-1/average_sn_global
           write(50,*) 'set boxwidth bw'

           write(50,*) '# Maximum value (to normalise histogram)'
           write(50,*) 'maxval = ', maxval

           write(50,*) '# Binning function'
           write(50,*) 'binc(x,s) = s*(ceil(x/s))'
           write(50,*) 'binr(x,s) = s*(ceil(x/s)+0.5)'

           write(50,*) '# Plot histogram '
           write(50,*) 'unset style'
!       residuals: (data - model) vs. wavelength
           write(50,*) 'plot '''//trim(datafile)//''' index', residuals_indxx, 'using '
     &     //'(binc(($1),bw)):(1./(maxval)) smooth frequency w boxes '
     &     //'lt 1 lc rgb "black" title "residuals" \'
           write(50,*) ', "" index', residuals_indxx, 'using '
     &     //'(binr(($2),bw)):(1./(maxval)) smooth frequency w boxes '
     &     //'lt 1 lw 1.5 lc rgb "red" title "noise" \'

           write(50,*)

!       ------------------------------------------------------------------------
!        FULL SPECTRUM (FLUX VS. VEL) ! full spectrum

           write(50,*) '# FULL SPECTRUM (FLUX VS. VEL)'
           write(50,*) '# NOW OMITTED TO SPEED-UP PLOTTING'
           write(50,*)

!       set plotting range
           bound = spectrum_size_px
          
           write(50,*)

           write(50,*) 'unset boxwidth'
           write(50,*)
           write(50,*) 'set xrange[',velocity(1),':',velocity(bound),']'
           write(50,*) 'set x2range[',wlength(1),':',wlength(bound),']'
           write(50,*) 'set yrange[-0.05:1.5]'
           write(50,*) 'set ytics format "%5.2f"'

           write(50,*) 'set xlabel "v_{Hubble} [km/s]"'
           write(50,*) 'set x2label "wavelength [\305]"'
           write(50,*) 'set ylabel "normalised flux"'
           write(50,*)

!       data | model vs. velocity
           write(50,*) 'set style fill solid 1.0'
           write(50,*)
           write(50,*) '# plot '''//trim(datafile)//''' index 0:',
     &     full_spec_indxx, ' using '
     &     //'($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "data" \'
!       model
           write(50,*) ', "" index 0:',
     &     full_spec_indxx, ' using '
     &     //'($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "model (ifit!)" \'
!       pseudo-continuum
           write(50,*) ', 1 w l lt 4 lw 1.5 lc rgb "black" notitle \'

!       Include line parameters in plot
!       line centre
           write(50,*) ', '''//trim(datafile)//''' index ',
     &     comp_param_above_indxx, ' using '
     &     //'($1):(1.1):("|") w labels notitle'
        
           write(50,*)

!       ------------------------------------------------------------------------
!        INDIVIDUAL REGIONS WITH AT LEAST ONE COMPONENT


!       ------------------------------------------------------------------------
!        LOOP OVER NUMBER OF DETECTED ABSORPTION PROFILES 

         DO abspro_id=1,num_abspro_gplot

!        write abs.prof. ID into string
          write(abspro_id_str,'(i6)') abspro_id

!        write local number of components into string
          num_comps_local = count_lines(abspro_id)
          write(num_comps_local_str,'(i6)') num_comps_local

!       set plotting index
          absprof_indxx = abspro_id - 1

          IF (num_comps_local.eq.0) THEN

           write(50,*) '# EMPTY REGION ', abspro_id
           write(50,*)

          ELSE

           write(50,*) '# REGION ', abspro_id
           write(50,*)

           write(50,*) '# Set title'

           write(50,*)
            title=trim(ifit_file_prefix(ionindex))//
     &     ' S/N= '//trim(adjustl(average_sn_global_str))//
     &     ' region: '//trim(adjustl(abspro_id_str))//
     &     ' lines: '//trim(adjustl(num_comps_local_str))//' / '//
     &     trim(adjustl(num_comps_str))

           write(50,*) 'set title "{/=10 '//trim(title)//'}"'

!       set multiplot enviornment
           write(50,*) 'set multiplot'

!       ----------------------------------------------------------------
!       MAIN PANEL

           write(50,*) '# MAIN PANEL'

!       set plot margins
           write(50,*) 'set tmargin screen 0.90'
           write(50,*) 'set bmargin screen 0.18'

!       ----------------------------------------------------------------
!       include list of components and corresponding parameters,
!       vertically aligned at bottom-left of panel

           if (alternative_labelling) then

!       These are the line parameters which should in turn be included
!       in the spectra plots; the last columns are used to set the po-
!       sitions of the labels in the plot

           posit = 0.1

           do i=1,num_components

!       Write line parameters into strings to facilitate label positioning
!       Only for lines in current region

!       write line parameters into strings to facilitate label positioning
            if (
     &      (vel_c_true(i).ge.velocity(abspro_bounds_true(abspro_id,1))).and.
     &      (vel_c_true(i).le.velocity(abspro_bounds_true(abspro_id,2)))
     &      ) then
            
!       select valid component
             if (abs(IONS(true_ion(i))%component(i)%status).eq.1) then

              write(zabs_str,'(f8.4)') z_abs_true(i)
              write(bvalue_str,'(f6.2)') IONS(true_ion(i))%component(i)%b_value
              write(bvalue_str_err,'(f6.2)')
     &        IONS(true_ion(i))%component(i)%b_value_err
              write(eqw_str,'(f6.2)')
     &        1.0d3*IONS(true_ion(i))%component(i)%eq_width ! in mili-Angst.
              write(col_dens_str,'(f6.2)')
     &        dlog10(IONS(true_ion(i))%component(i)%col_dens)
              write(col_dens_str_err,'(f6.2)')
     &        dlog10(IONS(true_ion(i))%component(i)%col_dens_err)

!       The entries below are needed to include in the plot the line
!       parameters +/- errors at the right positions. The encoding
!       {/CMSY10 \\006} for +/- only works if label is enclosed in quotation
!       marks, and the gnuplot files loads the required font cmsy10.pfb
!       (see create_plotfile_ps below)

              write(50,*) 'set label front "{/=9 ',i,'}" at graph 0.02, ',
     &         posit, 'tc rgb "black"'
              write(50,*) 'set label front "{/=8 N_{'//
     &        trim(IONS(true_ion(i))%name)
     &        //'}=10^{'//trim(adjustl(col_dens_str))//'}{/CMSY10 \\006}10^{'//
     &        trim(adjustl(col_dens_str_err))//'}" at graph 0.05, ', posit,
     &        'tc rgb "royalblue"'
              write(50,*) 'set label front "{/=8 b='//trim(adjustl(bvalue_str))
     &        //'{/CMSY10 \\006}'//trim(adjustl(bvalue_str_err))//
     &        '}" at graph 0.20, ', posit, 'tc rgb "royalblue"'
              write(50,*) 'set label front "{/=8 EW'
     &        //'='//trim(adjustl(eqw_str))//'}" at graph 0.29, ', posit,
     &        'tc rgb "royalblue"'
              write(50,*) 'set label front "{/=8 z'
     &        //'='//trim(adjustl(zabs_str))//'}" at graph 0.37, ', posit,
     &        'tc rgb "royalblue"'
          
!       Increment positioning
              posit = posit + 0.04

             end if ! valid component

            end if !line in region

           end do ! over components

          end if !alternative labelling

!       ------------------------------------------------------------------------
!       set plotting ranges:

!       pixel
           low = abspro_bounds_true(abspro_id,1)-px_offset
           high = abspro_bounds_true(abspro_id,2)+px_offset

!       velocity
           vel_low = velocity(max(1,low))
           vel_hi = velocity(min(high,bound))

!       wavelength
           wl_low = wlength(max(1,low))
           wl_hi = wlength(min(high,bound))
         

           write(50,*) 'set xrange[',vel_low,':',vel_hi,']'
           write(50,*) 'set x2range[',wl_low,':',wl_hi,']'
           write(50,*) 'set yrange[-0.05:1.5]'
           write(50,*) 'set xtics format " "'
           write(50,*) 'set x2tics format "% g"'
           write(50,*) 'set ytics format "%5.2f"'

           write(50,*) 'unset xlabel'
           write(50,*) 'set x2label "wavelength [\305]"'
           write(50,*) 'set ylabel "normalised flux"'
           write(50,*)

!       data | model vs. velocity
           write(50,*) 'plot '''//trim(datafile)//''' index',
     &     max(0,absprof_indxx-1), ':', min(num_abspro_gplot,absprof_indxx+1),
     &     'using '
     &     //'($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \'
           write(50,*) ', "" index',
     &     max(0,absprof_indxx-1), ':', min(num_abspro_gplot,absprof_indxx+1),
     &     'using '
     &     //'($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \'
           write(50,*) ', "" index',
     &     max(0,absprof_indxx-1), ':', min(num_abspro_gplot,absprof_indxx+1),
     &     'using '
     &     //'($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "data" \'
!       model
           write(50,*) ', "" index',
     &     max(0,absprof_indxx-1), ':', min(num_abspro_gplot,absprof_indxx+1),
     &     'using '
     &     //'($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "model (ifit!)" \'
!       pseudo-continuum
           write(50,*) ', 1 w l lt 4 lw 1.5 lc rgb "black" notitle \'
          
!       Include line parameters above spectrum

          if (alternative_labelling) then

!       line centre
           write(50,*) ', '''//trim(datafile)//''' index ',
     &     comp_param_above_indxx, ' using '
     &     //'($1):(1.1):("|") w labels notitle \'
!       line ID
           write(50,*) ', '''//trim(datafile)//''' index ',
     &     comp_param_above_indxx, ' using '
     &     //'($1):($3):($5) w labels font "Helvetica,9" tc lt 3 notitle \'
!       ion name
           write(50,*) ', '''//trim(datafile)//''' index ',
     &     comp_param_above_indxx, ' using '
     &     //'($1):($4):6 w labels font "Helvetica,9" tc lt 3 notitle \'
!       ion laboratory wavelength (vertically written)
           write(50,*) ', '''//trim(datafile)//''' index ',
     &     comp_param_above_indxx, ' using '//
     &     '($1):($8):($7) w labels rotate font "Helvetica,9" tc lt 3 notitle \'
!       line redshift (disabled)
           write(50,*) '#, '''//trim(datafile)//''' index ',
     &     comp_param_above_indxx, ' using '
     &     //'($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \'
       
          else ! not alternative labelling
        
!       line centre
           write(50,*) ', '''//trim(datafile)//''' index ',
     &     comp_param_above_indxx, ' using '
     &     //'($1):(1.1):("|") w labels notitle \'
!       line redshift
           write(50,*) ', '''//trim(datafile)//''' index ',
     &     comp_param_above_indxx, ' using '
     &     //'($1):($6):($2) w labels font "Helvetica,9" tc lt 3 notitle \'
!       b-value
           write(50,*) ', '''//trim(datafile)//''' index ',
     &     comp_param_above_indxx, ' using '
     &     //'($1):($7):($3) w labels font "Helvetica,9" tc lt 3 notitle \'
!       equivalent width
           write(50,*) ', '''//trim(datafile)//''' index ',
     &     comp_param_above_indxx, ' using '
     &     //'($1):($8):($4) w labels font "Helvetica,9" tc lt 3 notitle \'
!       column density
           write(50,*) ', '''//trim(datafile)//''' index ',
     &     comp_param_above_indxx, ' using '
     &     //'($1):($9):($5) w labels font "Helvetica,9" tc lt 3 notitle'

          end if !alternative labelling

          write(50,*)

!       ----------------------------------------------------------------
!       INSET

           write(50,*) '# INSET: RESIDUALS: (DATA - MODEL) VS. VEL'

!       unset title and labels
           write(50,*) 'unset title'
           write(50,*) 'unset label'

!       set plot margins
           write(50,*) 'set tmargin screen 0.18'
           write(50,*) 'set bmargin screen 0.08'

           write(50,*) 'set xrange[',vel_low,':',vel_hi,']'
           write(50,*) 'set x2range[',vel_low,':',vel_hi,']'
           write(50,*) 'set yrange[*:*]'
           write(50,*) 'set xtics format "% g"'
           write(50,*) 'set x2tics format " "'
           write(50,*) 'set ytics format " "'

           write(50,*) 'unset x2label'
           write(50,*) 'set xlabel "v_{Hubble} [km/s]"'
           write(50,*) 'set ylabel "{/=12 residuals}" offset 0.5,0'
           write(50,*)

!       residuals: (data - model) vs. wavelength
           write(50,*) ' plot '''//trim(datafile)//''' index',
     &     max(0,absprof_indxx-1), ':', min(num_abspro_gplot,absprof_indxx+1),
     &     'using '
     &     //'($4):(($2)-($3)) w histeps lt 1 lw 1 lc rgb "red" title '//
     &     '"{/=10 (model - data)}" \'
!       input noise
           write(50,*) ', '''//trim(datafile)//''' index',
     &     max(0,absprof_indxx-1), ':', min(num_abspro_gplot,absprof_indxx+1),
     &     'using '
     &     //'($4):($6) w histeps lt 1 lw 1 lc rgb "black" title '//
     &     '"{/=10 noise}" \'
           write(50,*) ', 0 w l lt 3 lw 1 lc rgb "royalblue" notitle \'
           write(50,*)

!       unset multiplot enviornment
           write(50,*) 'unset multiplot'



         END IF ! empty region?

         END DO ! LOOP OVER NUMBER OF DETECTED ABSORPTION PROFILES 
!       ------------------------------------------------------------------------

        close(50)

        end subroutine create_plotfile_ps_wl
!       ========================================================================


!       ------------------------------------------------------------------------
!       Numerical Recipes
!       ------------------------------------------------------------------------

!       The following routines are taken from:

!       Numerical Recipes FORTRAN 77, 2001, Volume 1 (software version 2.10)

!       ========================================================================
        FUNCTION ran3(idum)
        
        use set_precision

        implicit none

        integer(kind=singleI) :: idum
        integer(kind=singleI), parameter :: MBIG=1000000000, MSEED=161803398,
     &  MZ=0
C        REAL MBIG,MSEED,MZ
        real(kind=doubleR) :: ran3
        real(kind=doubleR), parameter :: FAC=1.d0/MBIG
!       PARAMETER (MBIG=1000000000,MSEED=161803398,MZ=0,FAC=1./MBIG)
C        PARAMETER (MBIG=4000000.,MSEED=1618033.,MZ=0.,FAC=1./MBIG)
        integer(kind=singleI) :: i,iff,ii,inext,inextp,k
        integer(kind=singleI) :: mj,mk,ma(55)
C        REAL mj,mk,ma(55)
        SAVE iff,inext,inextp,ma
        DATA iff /0/
        if (idum.lt.0.or.iff.eq.0)then
        iff=1
        mj=MSEED-iabs(idum)
        mj=mod(mj,MBIG)
        ma(55)=mj
        mk=1
        do 11 i=1,54
         ii=mod(21*i,55)
         ma(ii)=mk
         mk=mj-mk
         if (mk.lt.MZ)mk=mk+MBIG
         mj=ma(ii)
11        continue
        do 13 k=1,4
         do 12 i=1,55
          ma(i)=ma(i)-ma(1+mod(i+30,55))
          if (ma(i).lt.MZ)ma(i)=ma(i)+MBIG
12        continue
13        continue
        inext=0
        inextp=31
        idum=1
        endif
        inext=inext+1
        if (inext.eq.56)inext=1
        inextp=inextp+1
        if (inextp.eq.56)inextp=1
        mj=ma(inext)-ma(inextp)
        if (mj.lt.MZ)mj=mj+MBIG
        ma(inext)=mj
        ran3=mj*FAC
        return
        END FUNCTION ran3

!       ========================================================================

!       ========================================================================
        SUBROUTINE mrqmin(x,y,sig,ndata,a,ma,ia,covar,alpha,nca,chisq,alamda)

!       NOTE: MMAX has been changed from 300 to 500

        use set_precision

        implicit none

        integer(kind=singleI) :: ma,nca,ndata,ia(ma)
        integer(kind=singleI), parameter :: MMAX=500
        real(kind=doubleR) :: alamda,chisq,a(ma),alpha(nca,nca),covar(nca,nca),
     +  sig(ndata),x(ndata),y(ndata)
!       PARAMETER (MMAX=500)
CU      USES covsrt,gaussj,mrqcof
        integer(kind=singleI) :: j,k,l,m,mfit
        real(kind=doubleR) :: ochisq,atry(MMAX),beta(MMAX),da(MMAX)
        SAVE ochisq,atry,beta,da,mfit

        ochisq=0.0d0

        if (alamda.lt.0.0d0)then
          mfit=0
          do 11 j=1,ma
           if (ia(j).ne.0) mfit=mfit+1
11        continue
          alamda=0.001

          call mrqcof(x,y,sig,ndata,a,ia,ma,alpha,beta,nca,chisq)

          ochisq=chisq
          do 12 j=1,ma
           atry(j)=a(j)
12        continue
        endif !lambda < 0

        ochisq=chisq

        j=0
        do 14 l=1,ma
         if (ia(l).ne.0) then
          j=j+1
          k=0
          do 13 m=1,ma
           if (ia(m).ne.0) then
            k=k+1
            covar(j,k)=alpha(j,k)
           endif
13        continue
          covar(j,j)=alpha(j,j)*(1.+alamda)
          da(j)=beta(j)
         endif
14       continue

!       NOTE:
!       Need to check that covar-matrix diagonal does not contain zeros
!       to avoid crash when inverting using gaussj

        call gaussj(covar,mfit,nca,da,1,1)

        if (alamda.eq.0.0d0)then
         call covsrt(covar,nca,ma,ia,mfit)
         return
        endif
        
        j=0
        do 15 l=1,ma
          if (ia(l).ne.0) then
           j=j+1
           atry(l)=a(l)+da(j)
          endif
15      continue

        call mrqcof(x,y,sig,ndata,atry,ia,ma,covar,da,nca,chisq)

        if (chisq.lt.ochisq)then
          alamda=0.1*alamda
          ochisq=chisq
          j=0
          do 17 l=1,ma
         if (ia(l).ne.0) then
          j=j+1
          k=0
          do 16 m=1,ma
            if (ia(m).ne.0) then
              k=k+1
              alpha(j,k)=covar(j,k)
            endif
16         continue
          beta(j)=da(j)
          a(l)=atry(l)
         endif
17        continue
        else ! pseudo-adaptive lambda step to speed up code
          if (alamda.lt.1.0d-5) alamda=100.*alamda
          if (alamda.ge.1.0d-5) alamda=10.*alamda
!         alamda=10.*alamda
          chisq=ochisq
        endif

        return

        END SUBROUTINE mrqmin
!       ========================================================================

!       ========================================================================
        SUBROUTINE mrqcof(x,y,sig,ndata,a,ia,ma,alpha,beta,nalp,chisq)
        
!       NOTE: MMAX has been changed from 300 to 500
        
        use set_precision

        implicit none

        integer(kind=singleI) :: ma,nalp,ndata,ia(ma)
        integer(kind=singleI), parameter :: MMAX=500
        real(kind=doubleR) :: chisq,a(ma),alpha(nalp,nalp),beta(ma),
     +  sig(ndata),x(ndata),y(ndata)
        EXTERNAL funcs
!       PARAMETER (MMAX=500)
        integer(kind=singleI) :: mfit,i,j,k,l,m
        real(kind=doubleR) :: dy,sig2i,wt,ymod,dyda(MMAX)

        mfit=0
        do 11 j=1,ma
          if (ia(j).ne.0) mfit=mfit+1
11      continue
        do 13 j=1,mfit
          do 12 k=1,j
           alpha(j,k)=0.0d0
12        continue
          beta(j)=0.0d0
13      continue

        chisq=0.0d0
        
        do 16 i=1,ndata

!       NOTE:
!       if model is taking convolution into account, it is done in the next call
!       the function 'funcs' returns the model value Y(i) and its derivatives,
!       dydx, with respect to each of the ma parameters at x(i)
          
         call funcs(x(i),a,ymod,dyda,ma)

!       NOTE: COMMENTED OUT TO SPEED UP CODE; SHOULD LOOK FOR AN ALTERNATIVE...
!       re-define value to avoid code crash
!         if (sig(i).lt.tiny(real_single)) then
!          write(6,*) 'mrqcof: vanishing sigma value:', sig(i)
!          sig(i) = tiny(real_single)
!         end if

          sig2i=1.0d0/sig(i)/sig(i)
          dy=y(i)-ymod
          j=0
          do 15 l=1,ma
           if (ia(l).ne.0) then
            j=j+1
            wt=dyda(l)*sig2i
            k=0
            do 14 m=1,l
             if (ia(m).ne.0) then
              k=k+1
              alpha(j,k)=alpha(j,k)+wt*dyda(m)
             endif
14          continue
            beta(j)=beta(j)+dy*wt
           endif
15        continue
          
          chisq=chisq+dy*dy*sig2i

16       continue
         do 18 j=2,mfit
          do 17 k=1,j-1
           alpha(k,j)=alpha(j,k)
17        continue
18       continue

        return

        END SUBROUTINE mrqcof
!       ========================================================================

!       ========================================================================
        SUBROUTINE gaussj(a,n,np,b,m,mp)
        
!       NOTE: NMAX has been changed from 300 to 500

        use set_precision

        implicit none

        integer(kind=singleI) :: m,mp,n,np
        integer(kind=singleI), parameter :: NMAX=500
        real(kind=doubleR) :: a(np,np),b(np,mp)

        integer(kind=singleI) :: i,icol,irow,j,k,l,ll,indxc(NMAX),indxr(NMAX),
     +  ipiv(NMAX)
        real(kind=doubleR) :: big,dum,pivinv
        
        irow = 0
        icol = 0

        do 11 j=1,n
         ipiv(j)=0
11      continue
        do 22 i=1,n
          big=0.0d0
          do 13 j=1,n
           if (ipiv(j).ne.1)then
            do 12 k=1,n
             if (ipiv(k).eq.0) then
              if (abs(a(j,k)).ge.big)then
               big=abs(a(j,k))
               irow=j
               icol=k
              endif
             else if (ipiv(k).gt.1) then
              write(6,'(a)',advance='no') 'gaussj: singular matrix (1)'
             endif
12          continue
           endif
13        continue
          ipiv(icol)=ipiv(icol)+1
          if (irow.ne.icol) then
          do 14 l=1,n
           dum=a(irow,l)
           a(irow,l)=a(icol,l)
           a(icol,l)=dum
14        continue
          do 15 l=1,m
           dum=b(irow,l)
           b(irow,l)=b(icol,l)
           b(icol,l)=dum
15        continue
          endif
          indxr(i)=irow
          indxc(i)=icol

!       NOTE:
!       Check that matrix diagonal does not contain 'zeros'
          if (abs(a(icol,icol)).lt.tiny(real_single)) then
!         write(6,*)
!         write(6,'(a)',advance='no') 'gaussj: *singular* matrix (2)'
!         write(6,*)
!       re-define value to avoid code crash
           a(icol,icol) = sign(dble(tiny(real_single)),a(icol,icol))
          end if

          pivinv=1.0d0/a(icol,icol)
          a(icol,icol)=1.0d0
          do 16 l=1,n
           a(icol,l)=a(icol,l)*pivinv
16        continue
          do 17 l=1,m
           b(icol,l)=b(icol,l)*pivinv
17        continue
          do 21 ll=1,n
           if (ll.ne.icol)then
            dum=a(ll,icol)
            a(ll,icol)=0.0d0
            do 18 l=1,n
             a(ll,l)=a(ll,l)-a(icol,l)*dum
18          continue
            do 19 l=1,m
             b(ll,l)=b(ll,l)-b(icol,l)*dum
19          continue
           endif
21        continue
22      continue
        do 24 l=n,1,-1
          if (indxr(l).ne.indxc(l))then
           do 23 k=1,n
            dum=a(k,indxr(l))
            a(k,indxr(l))=a(k,indxc(l))
            a(k,indxc(l))=dum
23          continue
           endif
24      continue

        return

        END SUBROUTINE gaussj
!       ========================================================================

!       ========================================================================
        SUBROUTINE covsrt(covar,npc,ma,ia,mfit)
        
        use set_precision

        implicit none

        integer(kind=singleI) :: ma,mfit,npc,ia(ma)
        real(kind=doubleR) :: covar(npc,npc)
        integer(kind=singleI) :: i,j,k
        real(kind=doubleR) :: swap

        do 12 i=mfit+1,ma
         do 11 j=1,i
          covar(i,j)=0.0d0
          covar(j,i)=0.0d0
11       continue
12      continue
        k=mfit
        do 15 j=ma,1,-1
         if (ia(j).ne.0)then
          do 13 i=1,ma
           swap=covar(i,k)
           covar(i,k)=covar(i,j)
           covar(i,j)=swap
13        continue
          do 14 i=1,ma
           swap=covar(k,i)
           covar(k,i)=covar(j,i)
           covar(j,i)=swap
14        continue
          k=k-1
         endif
15       continue

        return

        END SUBROUTINE covsrt
!       ========================================================================

!       ========================================================================
        SUBROUTINE savgol(c,np,nl,nr,ld,m)

!       USES lubksb,ludcmp

        use set_precision

        implicit none

        integer(kind=singleI) :: ld,m,nl,np,nr
        integer(kind=singleI), parameter :: MMAX=6
        real(kind=doubleR) :: c(np)

        integer(kind=singleI) :: imj,ipj,j,k,kk,mm,indx(MMAX+1)
        real(kind=doubleR) :: d,fac,sum,a(MMAX+1,MMAX+1),b(MMAX+1)
        if (np.lt.nl+nr+
     *  1.or.nl.lt.0.or.nr.lt.0.or.ld.gt.m.or.m.gt.MMAX.or.nl+nr.lt.m) 
     *  write(6,*) 'bad args in savgol'
        do 14 ipj=0,2*m
          sum=0.
          if (ipj.eq.0)sum=1.
          do 11 k=1,nr
         sum=sum+float(k)**ipj
11        continue
          do 12 k=1,nl
         sum=sum+float(-k)**ipj
12        continue
          mm=min(ipj,2*m-ipj)
          do 13 imj=-mm,mm,2
         a(1+(ipj+imj)/2,1+(ipj-imj)/2)=sum
13        continue
14        continue
        call ludcmp(a,m+1,MMAX+1,indx,d)
        do 15 j=1,m+1
          b(j)=0.
15        continue
        b(ld+1)=1.
        call lubksb(a,m+1,MMAX+1,indx,b)
        do 16 kk=1,np
          c(kk)=0.
16        continue
        do 18 k=-nl,nr
          sum=b(1)
          fac=1.
          do 17 mm=1,m
         fac=fac*k
         sum=sum+b(mm+1)*fac
17        continue
          kk=mod(np-k,np)+1
          c(kk)=sum
18        continue
        return
        END

!       ========================================================================

!       ========================================================================
        SUBROUTINE lubksb(a,n,np,indx,b)

        use set_precision

        implicit none

        integer(kind=singleI) :: n,np,indx(n)
        real(kind=doubleR) :: a(np,np),b(n)
        integer(kind=singleI) :: i,ii,j,ll
        real(kind=doubleR) :: sum
        ii=0
        do 12 i=1,n
          ll=indx(i)
          sum=b(ll)
          b(ll)=b(i)
          if (ii.ne.0)then
         do 11 j=ii,i-1
          sum=sum-a(i,j)*b(j)
11          continue
          else if (sum.ne.0.) then
         ii=i
          endif
          b(i)=sum
12        continue
        do 14 i=n,1,-1
          sum=b(i)
          do 13 j=i+1,n
         sum=sum-a(i,j)*b(j)
13        continue
          b(i)=sum/a(i,i)
14        continue
        return
        END

!       ========================================================================

!       ========================================================================
        SUBROUTINE ludcmp(a,n,np,indx,d)

        use set_precision

        implicit none

        integer(kind=singleI) :: n,np,indx(n)
        integer(kind=singleI), parameter :: NMAX=500
        real(kind=doubleR) :: d,a(np,np)
        real(kind=doubleR), parameter :: TINY=1.0e-20
!       PARAMETER (NMAX=500,TINY=1.0e-20)
        integer(kind=singleI) :: i,imax,j,k
        real(kind=doubleR) :: aamax,dum,sum,vv(NMAX)
        d=1.
        imax = 0
        do 12 i=1,n
          aamax=0.
          do 11 j=1,n
         if (abs(a(i,j)).gt.aamax) aamax=abs(a(i,j))
11        continue
          if (aamax.eq.0.) write(6,*) 'singular matrix in ludcmp'
          vv(i)=1./aamax
12        continue
        do 19 j=1,n
          do 14 i=1,j-1
         sum=a(i,j)
         do 13 k=1,i-1
          sum=sum-a(i,k)*a(k,j)
13          continue
         a(i,j)=sum
14        continue
          aamax=0.
          do 16 i=j,n
         sum=a(i,j)
         do 15 k=1,j-1
          sum=sum-a(i,k)*a(k,j)
15          continue
         a(i,j)=sum
         dum=vv(i)*abs(sum)
         if (dum.ge.aamax) then
          imax=i
          aamax=dum
         endif
16        continue
          if (j.ne.imax)then
         do 17 k=1,n
          dum=a(imax,k)
          a(imax,k)=a(j,k)
          a(j,k)=dum
17          continue
         d=-d
         vv(imax)=vv(j)
          endif
          indx(j)=imax
          if (a(j,j).eq.0.)a(j,j)=TINY
          if (j.ne.n)then
         dum=1./a(j,j)
         do 18 i=j+1,n
          a(i,j)=a(i,j)*dum
18          continue
          endif
19        continue
        return
        END

!       ========================================================================

!       ========================================================================
!       Taken (!) from SpecWizard

        subroutine gauss_conv(flux_aux,px_min,px_max,px_size,fwhm)

!       USES convlv

!       flux_aux = signal
!       num_px = number of pixels
!       px_size = size of pixel in km/s
!       fwhm_kms = FWHM of Gaussian response in km/s

        use set_precision

        implicit none

        real(kind=doubleR) :: sigmakms, b, norm, fwhm, px_size
        integer(kind=singleI) :: i,j,off, nvpix, num_px
        integer(kind=singleI) :: px_min, px_max
        real(kind=doubleR), allocatable, save :: gauss(:)
        real(kind=doubleR), intent(inout) :: flux_aux(*)
        real(kind=doubleR), allocatable  :: convl_flux(:),
     &  convl_flux_convolved(:)

!       convolve only if required
        if (fwhm.le.0.0d0) return

!       write(*,*) 'Convolving with Gaussian LSF using fwhm = ', fwhm
!       write(*,*)

!       determine number of pixels
        num_px = px_max - px_min + 1
        nvpix = num_px

!       compute Gaussian width of convolution kernel (in km/s)
        sigmakms = fwhm / (2.0d0 * dsqrt( 2.0d0 * dlog(2.0d0)))

!       express Gaussian width (in pixels)
        b = sigmakms / px_size
        
!       For convolution with instrumental LSF we need to Fourier 
!       transform, we thus need to increase the array so that it is a
!       power of 2.
        nvpix = int(2.0d0**(aint(dlog(dble(nvpix))/dlog(2.0d0)) + 1.0d0))
        
!       Create normalized Gaussian in wrap-around order (required by
!       convlv)
        
        if (allocated(gauss)) deallocate(gauss)
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

!       normalise Gaussian kernel
        gauss  = gauss / sum(gauss)

        allocate(convl_flux(nvpix),convl_flux_convolved(2*nvpix))
        convl_flux(:) = 0.0d0
        convl_flux(1:num_px) = flux_aux(1:num_px)

!       make periodic copies of the flux signal into the zero buffer
!       to avoid aliasing (or end) effects
        do i=num_px+1,nvpix
        off = i-num_px
        if (off .lt. (nvpix-num_px)/2.0d0) then
        convl_flux(i) = convl_flux(i-num_px)
        else
        convl_flux(i) = convl_flux(i-(nvpix-num_px))
        endif
        enddo

        convl_flux_convolved(:) = 0.0d0

        call convlv(convl_flux,nvpix,gauss,nvpix,1,convl_flux_convolved)

        flux_aux(1:num_px) = convl_flux_convolved(1:num_px)

        deallocate(convl_flux,convl_flux_convolved)

        return
        
        end subroutine gauss_conv
!       ========================================================================

!       ========================================================================
!       Taken (!) from SpecWizard

!       NOTE:
!       `ans' is complex here, but it is real when routine is called by
!       gauss_conv; this causes the compiler (gfortran) to print a warning or
!       to abort altogether when using the flag -pedantic
        
        subroutine convlv(data,n,respns,m,isign,ans)

!       Convolves or deconvolves a real data set data(1:n) (including any
!       user-supplied zero padding) with a response function respns, stored in
!       wrap-around order in a real array of length m <=n. (m should be an odd
!       integer.) Wrap-around order means that the first half of the array
!       respns contains the impulse response function at positive times, while
!       the second half of the array contains the impulse response function at
!       negative times, counting down from the highest element respns(m). On
!       input isign is +1 for convolution, -1 for deconvolution. The answer is
!       returned in the first n components of ans. However, ans must be supplied
!       in the calling program with length at least 2*n, for consistency with
!       twofft. n MUST be an integer power of two.

!       USES twofft, realft

        use fourier ! declares complex array fft
        use set_precision

        implicit none 

        integer(kind=singleI), intent(in) :: n, m, isign
        real(kind=doubleR), intent(in)    :: data(n)
        real(kind=doubleR), intent(inout) :: respns(n)
!       double complex, intent(out):: ans(2*n)
        complex(kind=doubleR), intent(out):: ans(2*n)

        integer(kind=singleI) :: i,no2
        integer(kind=singleI), save :: nfft=-1

        if (nfft .ne. n)then
         if (allocated(fft)) deallocate(fft)
         allocate(fft(n))
         nfft = n
        endif

        do  i=1,(m-1)/2
         respns(n+1-i)=respns(m+1-i)
        enddo
        do  i=(m+3)/2,n-(m-1)/2
         respns(i)=0.0
        enddo

        call twofft(data,respns,fft,ans,n) ! discrete fourier transforms:
                                           ! data -> fft
                                           ! respns -> ans
        no2=n/2
        do i=1,no2+1
         if (isign.eq.1) then
          ans(i)=fft(i)*ans(i)/no2
         else if (isign.eq.-1) then
          if (abs(ans(i)).eq.0.0) then
           write(*,*) 'deconvolving at response zero in convlv'
           stop 1
          end if
          ans(i)=fft(i)/ans(i)/no2
         else
          stop 'no meaning for isign in convlv'
         endif
        enddo
        ans(1)=dcmplx(dble(ans(1)),dble(ans(no2+1)))
        call realft(ans,n,-1)              ! inverse fourier transforms ans

        return
        end subroutine convlv
!       ========================================================================

!       ========================================================================
!       Taken (!) from SpecWizard

        subroutine twofft(data1,data2,fft1,fft2,n)

!       Given two real input arrays data1(1:n) and data2(1:n), this routine
!       calls four1 and returns two complex output arrays, fft1(1:n) and
!       fft2(1:n), each of complex length n (i.e., real length 2*n), which
!       contain the discrete Fourier transforms of the respective data arrays.
!       n MUST be an integer power of 2.

!       USES four1

        use set_precision

        implicit none 

        integer(kind=singleI), intent(in) :: n
        real(kind=doubleR), intent(in) :: data1(n), data2(n)
!       double complex :: fft1(n), fft2(n)
        complex(kind=doubleR) :: fft1(n), fft2(n)

        ! local variables
        integer(kind=singleI) :: j,n2
!       double complex ::  h1,h2,c1,c2
        complex(kind=doubleR) ::  h1,h2,c1,c2

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
!       ========================================================================

!       ========================================================================
!       Taken (!) from SpecWizard
!       USES four1

        subroutine realft(data,n,isign)

!       Calculates the Fourier transform of a set of n real-valued data points.
!       Replaces this data (which is stored in array data(1:n)) by the positive
!       frequency half of its complex Fourier transform. The real-valued first
!       and last components of the complex transform are returned as elements
!       data(1) and data(2), respectively. n must be a power of 2. This routine
!       also calculates the inverse transform of a complex data array if it is
!       the transform of real data. (Result in this case must be multiplied by
!       2/n.)

        use set_precision

        implicit none

        integer(kind=singleI), intent(in) :: n, isign
        real(kind=doubleR), intent(inout) :: data(n)
        ! local variables
        integer(kind=singleI) :: i,i1,i2,i3,i4,n2p3
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
!       ========================================================================

!       ========================================================================
!       Taken (!) from SpecWizard

        subroutine four1(data,nn,isign)

!       Replaces data(1:2*nn) by its discrete Fourier transform, if isign is
!       input as 1; or replaces data(1:2*nn) by nn times its inverse discrete
!       Fourier transform, if isign is input as  -1. data is a complex array of
!       length nn or, equivalently, a real array of length 2*nn. nn MUST be an
!       integer power of 2 (this is not checked for!). 

!       NOTE:
!       data is real here, but it is complex when routine is called by twofft
!       gfortran-mo-4.7 compiler does not accept this (in contrast to previous
!       compiler versions)

        use set_precision

        implicit none

        integer(kind=singleI), intent(in) :: nn, isign
        real(kind=doubleR), intent(inout) :: data(2*nn)
        ! local variables  
        integer(kind=singleI) :: i,istep,j,m,mmax,n
        real(kind=doubleR) :: tempi,tempr
        real(kind=doubleR) ::  theta,wi,wpi,wpr,wr,wtemp

        n=2*nn
        j=1
        do  i=1,n,2
        if (j.gt.i)then
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
!       ========================================================================



!       ========================================================================

!       ----------------------------------------------------------------
!       Quicksort of an array
!       ----------------------------------------------------------------

        subroutine sort(n,arr)

        use set_precision

        implicit none

        integer(kind=singleI) :: n
        real(kind=doubleR) :: arr(n)
        integer(kind=singleI), parameter :: M=7, NSTACK=50
        integer(kind=singleI) :: i,ir,j,jstack,k,l,istack(NSTACK)
        real(kind=doubleR) :: a,temp
        jstack=0
        l=1
        ir=n
1       if (ir-l.lt.M)then
         do 12 j=l+1,ir
          a=arr(j)
          do 11 i=j-1,1,-1
            if (arr(i).le.a)goto 2
            arr(i+1)=arr(i)
11          continue
          i=0
2          arr(i+1)=a
12         continue
          if (jstack.eq.0)return
          ir=istack(jstack)
          l=istack(jstack-1)
          jstack=jstack-2
               else
          k=(l+ir)/2
          temp=arr(k)
          arr(k)=arr(l+1)
          arr(l+1)=temp
          if (arr(l+1).gt.arr(ir))then
           temp=arr(l+1)
           arr(l+1)=arr(ir)
           arr(ir)=temp
          endif
          if (arr(l).gt.arr(ir))then
           temp=arr(l)
           arr(l)=arr(ir)
           arr(ir)=temp
          endif
          if (arr(l+1).gt.arr(l))then
           temp=arr(l+1)
           arr(l+1)=arr(l)
           arr(l)=temp
          endif
          i=l+1
          j=ir
          a=arr(l)
3          continue
           i=i+1
          if (arr(i).lt.a)goto 3
4          continue
           j=j-1
          if (arr(j).gt.a)goto 4
          if (j.lt.i)goto 5
          temp=arr(i)
          arr(i)=arr(j)
          arr(j)=temp
          goto 3
5          arr(l)=arr(j)
          arr(j)=a
          jstack=jstack+2
          if (jstack.gt.NSTACK) write(6,*) 'NSTACK too small in sort'
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
         end subroutine sort
C  (C) Copr. 1986-92 Numerical Recipes Software 5.W7.
!       ========================================================================



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
        function reduced_chi2(dof,conf)
        
!       returns the reduced (chi^2 / n) value for a chi^2 distribution with
!       n degrees of freedom that has a probability of (1-'conf') of being
!       exceeded by chance; i.e. the value that has a cumulative probability of
!       'conf' 

!       the cumulative chi^2 distribution of n degrees of freedom F(n,chi2) is
!       given by the incomplete gamma function P(n/2,chi2/2) (function gammp)

!       this programs computes the root of the function
!       
!       red_gamma(x) = P(n/2,x/2) - p = 0
!
!       using the bisection method within an accuracy x_acc, assuming
!       the root to lie between x=0.0d0 and x=max(500,1.5n); the last value
!       corresponds to 1.5 times the median (n) of the chi^2 distribution;
!       note that its variance (standard deviation squared) is 2n


!       input parameters:
!       dof = degrees of freedom
!       conf = cumulative probability

        use set_precision
        use cum_chi2_dist
        use ifit_variables, only: colour_errmsg_prefix, colour_msg_suffix

        implicit none

        integer(kind=singleI), intent(in) :: dof
        
        real(kind=doubleR), intent(in) :: conf
        real(kind=doubleR) :: gammq, gammp
        
        real(kind=doubleR) :: chi2
        real(kind=doubleR) :: red_gamma
        real(kind=doubleR) :: x_lo, x_hi
        real(kind=doubleR) :: rtbis
        
        real(kind=doubleR) :: reduced_chi2

        real(kind=doubleR), parameter :: x_acc = 1.0d-10
        
        external :: red_gamma

!       initialise
        reduced_chi2 = 0.0d0
        deg_of_free = dof
        confidence = conf

!       define range to look for root
        x_lo = 0.0d0
        x_hi = max(5.0d2,1.5d0*dble(deg_of_free))

!       check for valid number of DoF
        if ((5.0d-1*dble(deg_of_free)).le.0) then
         write(6,*) trim(colour_errmsg_prefix)//
     &   ' reduced_chi2: zero degrees of freedom! stop'//
     &   trim(colour_msg_suffix)
         stop 1
        end if
!       check that complementary incomplete gamma functions give the expected
!       answer
        
        if (
     &  abs(gammq(5.0d-1*dble(deg_of_free),5.0d-1*x_lo) +
     &  gammp(5.0d-1*dble(deg_of_free),5.0d-1*x_lo) - 1.0d0)
     &  .gt.x_acc)
     &  write(6,*) 'reduced_chi2: possible loss of accuracy!'

        if (
     &  abs(gammq(5.0d-1*dble(deg_of_free),5.0d-1*x_hi) +
     &  gammp(5.0d-1*dble(deg_of_free),5.0d-1*x_hi) - 1.0d0)
     &  .gt.x_acc)
     &  write(6,*) 'reduced_chi2: possible loss of accuracy!'
        
!       compute chi2-value for corresponding probability       
        chi2 = rtbis(red_gamma,x_lo,x_hi,x_acc)

!       return value
        reduced_chi2 = chi2/dble(deg_of_free)
        
        return

        end function reduced_chi2
!       ========================================================================

!       ========================================================================
        function red_gamma(x)
        
        use set_precision
        use cum_chi2_dist
        
        implicit none
         
        real(kind=doubleR) :: red_gamma, x
        real(kind=doubleR) :: gammp
        
!       define function
        red_gamma = 0.0d0

        red_gamma = gammp(5.0d-1*dble(deg_of_free),5.0d-1*x) - confidence

        return
        
        end function red_gamma
!       ========================================================================

!       ========================================================================
        function gammp(a,x)

        use set_precision

        implicit none
         
CU      USES gcf,gser
        real(kind=doubleR) :: a,gammp,x
        real(kind=doubleR) :: gammcf,gamser,gln
        if (x.lt.0.0d0.or.a.le.0.0d0) then
         write(6,*) 'bad arguments in gammp'
         stop 1
        end if
        if (x.lt.a+1.0d0) then
         call gser(gamser,a,x,gln)
         gammp=gamser
        else
         call gcf(gammcf,a,x,gln)
         gammp=1.0d0-gammcf
        endif
        return

        end function gammp
!       ========================================================================

!       ========================================================================
        function gammq(a,x)

        use set_precision

        implicit none
         
CU      USES gcf,gser
        real(kind=doubleR) :: a,gammq,x
        real(kind=doubleR) :: gammcf,gamser,gln

        if (x.lt.0..or.a.le.0.) then
         write(6,*) 'bad arguments in gammq'
         stop 1
        end if
        if (x.lt.a+1.0d0)then
         call gser(gamser,a,x,gln)
         gammq=1.0d0-gamser
        else
         call gcf(gammcf,a,x,gln)
         gammq=gammcf
        endif
        return

        end function gammq
!       ========================================================================

!       ========================================================================
        SUBROUTINE gser(gamser,a,x,gln)

        use set_precision

        implicit none
         
CU      USES gammln
        real(kind=doubleR) :: a,gamser,gln,x
        real(kind=doubleR), parameter :: EPS=1.0d-20
        integer(kind=singleI), parameter :: ITMAX=1000
        integer(kind=singleI):: n
        real(kind=doubleR) :: ap,del,sum,gammln
        gln=gammln(a)
        if (x.le.0.0d0)then
         if (x.lt.0.0d0) then
          write(6,*) 'x < 0 in gser'
          stop 1
         end if
         gamser=0.0d0
         return
        endif
        ap=a
        sum=1.0d0/a
        del=sum
        do 11 n=1,ITMAX
         ap=ap+1.0d0
         del=del*x/ap
         sum=sum+del
         if (abs(del).lt.abs(sum)*EPS)goto 1
11      continue
        write(6,*) 'gser: a too large, ITMAX too small!'
        stop 1
1       continue
!       avoid underflow of gamser
        if ((dlog(sum)-x+a*dlog(x)-gln).lt.dlog(tiny(real_double))) then
         write(6,*)
         write(6,*) 'gser: too small gamser! Will set gamser to 0.0d0'
         write(6,*)
         gamser=0.0d0
         return
!         stop 1
        end if
!       avoid overflow of gamser
        if ((dlog(sum)-x+a*dlog(x)-gln).gt.dlog(huge(real_double))) then
         write(6,*) 'gser: too big gamser!'
         stop 1
        end if
        
        gamser=sum*dexp(-x+a*dlog(x)-gln)
        return

        END SUBROUTINE gser
!       ========================================================================

!       ========================================================================
        SUBROUTINE gcf(gammcf,a,x,gln)

        use set_precision

        implicit none
         
CU      USES gammln
        real(kind=doubleR) :: a,gammcf,gln,x
        real(kind=doubleR), parameter :: EPS=1.0d-20,
     &  FPMIN=dble(tiny(real_single))
        integer(kind=singleI), parameter :: ITMAX=1000
        integer(kind=singleI):: i
        real(kind=doubleR) :: an,b,c,d,del,h,gammln
        gln=gammln(a)
        b=x+1.0d0-a
        c=1.0d0/FPMIN
        d=1.0d0/b
        h=d
        do 11 i=1,ITMAX
         an=-i*(i-a)
         b=b+2.0d0
         d=an*d+b
         if (abs(d).lt.FPMIN)d=FPMIN
         c=b+an/c
         if (abs(c).lt.FPMIN)c=FPMIN
         d=1.0d0/d
         del=d*c
         h=h*del
         if (abs(del-1.0d0).lt.EPS)goto 1
11      continue
        write(6,*) 'gcf: a too large, ITMAX too small!'
        stop 1
1       continue
!       avoid underflow of gammcf (edited by TTG)
        if ((dlog(h)-x+a*dlog(x)-gln).lt.dlog(tiny(real_double))) then
         write(6,*)
         write(6,*) 'gcf: too small argument in dexp!'
         write(6,*)
         stop 1
        end if
!       avoid overflow of gammcf (edited by TTG)
        if ((dlog(h)-x+a*dlog(x)-gln).gt.dlog(huge(real_double))) then
         write(6,*) 'gcf: too big argument in dexp!'
         stop 1
        end if

        gammcf=dexp(-x+a*dlog(x)-gln)*h
        return

        END SUBROUTINE gcf
!       ========================================================================

!       ========================================================================
        FUNCTION gammln(xx)

        use set_precision

        implicit none
         
        real(kind=doubleR) :: gammln,xx
        integer(kind=singleI):: j
        real(kind=doubleR) :: ser,stp,tmp,x,y,cof(6)
        SAVE cof,stp
        DATA cof,stp/76.18009172947146d0,-86.50532032941677d0,
     &  24.01409824083091d0,-1.231739572450155d0,0.1208650973866179d-2,
     &  -0.5395239384953d-5,2.5066282746310005d0/
        x=xx
        y=x
        tmp=x+5.5d0
        tmp=(x+5.0d-1)*dlog(tmp)-tmp
        ser=1.000000000190015d0
        do 11 j=1,6
         y=y+1.0d0
         ser=ser+cof(j)/y
11      continue
        gammln=tmp+dlog(stp*ser/x)
        return

        END FUNCTION gammln
!       ========================================================================

!       ========================================================================
        function rtbis(func,x1,x2,xacc)

        use set_precision

        implicit none
         
        integer(kind=singleI), parameter :: JMAX=100
        real(kind=doubleR) :: rtbis,x1,x2,xacc,func
        external func
        integer(kind=singleI):: j
        real(kind=doubleR) :: dx,f,fmid,xmid
        fmid=func(x2)
        f=func(x1)
        if (f*fmid.ge.0.0d0) then
         write(6,*) 'root must be bracketed in rtbis'
         stop 1
        end if
        if (f.lt.0.0d0) then
         rtbis=x1
         dx=x2-x1
        else
         rtbis=x2
         dx=x1-x2
        endif
        do 11 j=1,JMAX
         dx=dx*5.0d-1
         xmid=rtbis+dx
         fmid=func(xmid)
         if (fmid.le.0.)rtbis=xmid
         if (abs(dx).lt.xacc .or. fmid.eq.0.) return
11      continue
        
        write(6,*) 'too many bisections in rtbis'
        
        end function rtbis
!       ========================================================================

!       ========================================================================

!       ------------------------------------------------------------------------
!       RANDOM-Numbers (uniformily distributed 1)
!       ------------------------------------------------------------------------
!       Minimal random number generator of Park and Miller with Bays-Durham
!       shuffle and added safeguards. Returns a uniform random deviate between
!       0.0 and 1.0 (exclusive of the endpoint values). Call with idum a
!       negative integer to initialize; thereafter, do not alter idum between
!       successive deviates in a sequence. RNMX should approximate the largest
!       floating value that is less than 1. 
  
        function ran1(idum)  

        use set_precision  

        implicit none

        integer(kind=doubleI) :: idum
        integer(kind=doubleI), parameter :: IA=16807, IM=2147483647, IQ=127773,
     +  IR=2836, NTAB=32, NDIV=1+(IM-1)/NTAB  
        real(kind=doubleR), parameter :: AM=1.0d0/dble(IM), EPS=1.2d-7,
     +  RNMX=1.0d0-EPS 
        real(kind=doubleR) :: ran1
        integer(kind=doubleI) :: j,k,iv(NTAB),iy

        save iv,iy
        data iv /NTAB*0/, iy /0/
        if (idum.le.0.or.iy.eq.0) then
         idum=max(-idum,1)
         do 11 j=NTAB+8,1,-1
          k=idum/IQ
          idum=IA*(idum-k*IQ)-IR*k
          if (idum.lt.0) idum=idum+IM
          if (j.le.NTAB) iv(j)=idum
11         continue
         iy=iv(1)
        endif
        k=idum/IQ
        idum=IA*(idum-k*IQ)-IR*k
        if (idum.lt.0) idum=idum+IM
        j=1+iy/NDIV
        iy=iv(j)
        iv(j)=idum
        ran1=min(AM*iy,RNMX)
        return

        end function ran1  

C  (C) Copr. 1986-92 Numerical Recipes Software 5.W7.
!       ========================================================================


!       ========================================================================

!       ------------------------------------------------------------------------
!       RANDOM-Numbers (normal distributed)
!       ------------------------------------------------------------------------
!       Returns a normally distributed deviate with zero mean and unit variance, using
!       ran1(idum) as the source of uniform deviates.

        function gasdev(idum)

        use set_precision

        implicit none

        integer(kind=doubleI) ::idum
        real(kind=doubleR) :: gasdev
CU      USES ran1
        integer(kind=doubleI) ::iset
        real(kind=doubleR) :: fac,gset,rsq,v1,v2,ran1
        save iset,gset
        data iset/0/
        if (iset.eq.0) then
1         v1=2.0d0*ran1(idum)-1.0d0
         v2=2.0d0*ran1(idum)-1.0d0
         rsq=v1**2+v2**2
         if(rsq.ge.1.0d0.or.rsq.eq.0.0d0) goto 1
         fac=sqrt((-2.0d0)*log(rsq)/rsq)
         gset=v1*fac
         gasdev=v2*fac
         iset=1
        else
         gasdev=gset
         iset=0
        endif
        return
        end function gasdev
C  (C) Copr. 1986-92 Numerical Recipes Software 5.W7.
!       ========================================================================

!       ========================================================================
        subroutine spline(x,y,n,yp1,ypn,y2)

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

        end subroutine spline
!       ========================================================================

!       ========================================================================
        subroutine splint(xa,ya,y2a,n,x,y)

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

        end subroutine splint
!       ========================================================================
