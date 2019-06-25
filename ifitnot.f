!       ========================================================================
        program ifitnot

!       IMPORTANT:
!       TO DO:
!
!       1) output a log file
!
!       2) Check using system time which routine takes longest and why
!          --> it is mrqmin -> mrqcof -> funcs
!          which computes the derivatives of the flux for EACH parameter at
!          EACH point along the spectrum; computational time increases dramati-
!          cally with number of absorption components
!
!       3) Reduce output by separating into classes, controled by input flags:
!          verbose; debug -> DONE; but can there is room for improvement
!
!       4) VERY IMPORTANT: resolve conflict with complex/real inout variables
!          in routine `convlv' (and associated routines);
!          Solution perhaps with the use of generic functions declared through
!          interfaces?
!
!       5) IMPORTANT: Declare types (e.g., absorption components) and its
!          associated parameters e.g., (v,N,b), errors, component_status
!
!       6) VERY IMPORTANT: in my opinion, the way in which chi^2 is applied is
!           wrong! Fluxes should not be subtracted from each other but divided
!           The reason is that a spectrum consists of the product of the
!           individual component (absorption) fluxes. f(v) = PI_i [f_i(v)]
!           In consequence, optical depths are added and this should be the
!           quantity to which the chi^2 reduction method should be applied to!
!           -> FIXED
!           NOW CODE DISTINGUISHES BETWEEN NON-SATURATED AND SATURATED
!           ABSORPTION PROFILES;
!           THE FORMER ARE FITTED USING THE APARENT OPTICAL DEPTH; THE LATTER
!           USING THE FLUX
!
!       7) DEALLOCATE all arrays which are not needed where appropriate
!
!       8) estimate the average noise from smoothed flux -> DONE; Now, compare
!          with noise vector; may use this information to estimate noise if
!          this is not given in the input spectrum file -> allow for this
!          option through an additional input parameter (e.g., noise_vector=F)
!           
!       10) IMPORTANT: save partial fits (on a absorption profile by absorption
!           profile basis) to avoid fitting a spectrum from the beginning if
!           code crashes or any absorption profile is troublesome; implement
!           some sort of RESUME option to continue fitting a spectrum 
!
!       11) IMPORTANT: avoid adding potential components with N or (N/b) below
!           formal detection limit in N or (N/b)
!
!       12) TO NOTE: when taking instrumental broadening into account, it is
!           important to choose sensible values for b_value_min and fwhm_kms; a
!           poor (chi^2 > chi2bad_reduced) fit and/or large relative errors in
!           (N,b) may indicate that:
!
!           a) b_value_min and/or fwhm_kms is/are too large
!           b) LSF is NOT a Gaussian
!           c) fwhm_kms is NOT constant over the given wavelenght range
!
!       13) IMPORTANT: include the parameter tau_sat as input parameter, since
!           the adopted value is somehow arbitrary (i.e., not well defined)
!
!       14) IMPORTANT: scale the parameter uncertainties by a factor
!           corresponding to final reduced chi2-value in each absorption profile
!           expressed in `sigma' units
!
!       15) IMPORTANT: accept a fit with no components if reduced chi^2 is below
!           a the value corresponding to a cumulative probability XXX; set XXX 
!
!       16) IMPORTANT: redefine dof -> dof - 1 (a further constraint comes from
!           the fact that chi^2 has a minimum at the `optimal' parameter vector)
!           Is this true? Yes, according to
!
!           http://ned.ipac.caltech.edu/level5/Wall2/Wal3_4.html
!
!           but the Num.Rec (Ch. 15.6, Theorem A) state otherwise, i.e.,
!           dof = N - M, where N = data points, M =  num. parameters.
!
!           Will hardly make a difference when (N - M) >> 1, but it is important
!           to understand the concept!
!
!           Fisher (1924) is apparently the first to have recognised that
!           dof = N - 1 in general, and less M when M parameters are considered
!
!       17) IMPORTANT: MAKE ATOMIC DATA INPUT UNIFORM AMONG ALL INVOLVE CODES:
!           ifitnot, abs_line_profiles, eqw, ...
!
!       19) IMPORTANT: COMPUTE MODEL DERIVATIVES USING *ANALYTIC* EXPRESSIONS TO
!           SPEED UP CODE BY AVOIDING DERIVATIVE-CONVERGENCE TESTS (see funcs())
!
!       20) IMPORTANT: Design test to test wavelenght/velocity accuracy
!
!       21) Perhaps related to previous point: check origin of P-cygnyi-type
!           profiles in residum (flux - model) at high resolution and S/N=1000;
!           is due to velocity zero-point displacement as a result of
!           v/c = ln(lambda) transformation
!
!       22) Make flow charts, a diagram showing the structure IONS and its
!           dependencies graphically (main body and nodes to components, etc.)
!
!       23) SUPER SUPER IMPORTANT: MAKE SURE THAT THE SIZE OF AN INDIVIDUAL
!           ABSORPTION PROFILE IS *AT LEAST* TWICE AS WIDE AS THE SIZE OF THE
!           CONVOLUTION WINDOW (see gauss_conv; parameter b and windo 10*b)
!           WHEN INCLUDING INSTRUMENTAL BROADENING, i.e. when fwhm_kms > 0
!           IDEA: during sweep=2, make sure that each abs.profile has a size
!           such that the range around each of its components with a radius of
!           10*b pixel is completely contained within the profile 
!
!       26) flag unidentified lines and decide what to do with these;
!           solve issue related to tolerance used to identify multiplets (see
!           search_multiplet);
!           in this routine, improve the calculation of quantities and their
!           uncertainties by defining a new variable type which encloses them
!           all and calling a routine to compute them
!
!       27) IMPORTANT: construct a correct atomic data base for commonon ions;
!           the more the better! In particular, note that the values of HI
!           stored in atom_data.h1 are WRONG!
!
!       ------------------------------------------------------------------------
!       GLOBAL variables

        use set_precision
        use ifit_variables
        use constants
        use cum_chi2_dist

        implicit none

!       ------------------------------------------------------------------------
!       local variables

!       functions and subroutines
        real(kind=doubleR) :: chi2_function
        logical :: do_sweep
		  integer :: compid
        character(len=12) :: routinemsg = 'ifitnot:'

!       ------------------------------------------------------------------------
!       program start
        call start_program(routinemsg)

!       ------------------------------------------------------------------------
!       get values of input parameters set by user
        call input_parameters()

!       ------------------------------------------------------------------------
!       load atomic physical paramaters for all ions
        call load_ions(num_ions)

!       """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        DO WHILE (do_sweep(sweep))
!       """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
   
!       ------------------------------------------------------------------------
!       fit sweeps
!
!       sweep=1: single ion (transition) fit *ignoring* instrumental broadening
!
!       [sweep=2: single ion (transition) fit including instrumental broadening]
!
!       [sweep=3: multiple transition fit including instrumental broadening]


!       NOTES:
!       - sweep=2 is not performed whenever the input parameter fwhm_kms=0
!
!       - sweep=3 is not performed whenever the input parameter
!         single_ion_fit_str=FALSE
!
!       - instrumental broadening is assumed to be well described by the
!         convolution of the spectrum with a Gaussian line-spread function
!         (LSF), with full width at half maximum FWHM=fwhm_kms
!
!       - sweep=3 uses the `raw' fit (sweep=1,2) as input for a full fit to
!         speed up the computation;
!         during this finale sweep, no additional components will be inserted,
!         regardless of the fit quality (i.e., chi^2-value) 

!       """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
!       jump to end if ignoring instrumental broadening during second sweep
        IF (.not.((sweep.eq.2).and.(fwhm_kms.eq.0.0d0))) THEN
!       """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

!       @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!        DO ION_INDEX = 1, num_ions
!       @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

         ION_INDEX = 1

         call set_filename(sweep)

!       ------------------------------------------------------------------------
!       read spectrum, spectrum size in pixel (spectrum_size_px), allocate and
!       initialise spectrum arrays, and shift spectrum circularly (if desired
!       when using, e.g. synthetic spectra obtained from simulations with
!       periodic boundary conditions);
!       initialises modelflux = 1 and modelflux_convolved = 1

         call read_spectrum(sweep)

!       ------------------------------------------------------------------------
!       defines the maximum number of components and component parameters;
!       allocate and initialise component arrays, and get first-guess values for
!       component parameters vel_c, col_dens, bvalue 

         call init_components(sweep)
        
!       store number of input components (for book-keeping only; 0 for sweep 1)
         num_components_save = num_components

!       IMPORTANT: PERHAPS MOVE OUT OF SWEEPS LOOP?
!       ------------------------------------------------------------------------
!       allocate fit arrays
         call alloc_fit(spectrum_size_px)
      
!       IMPORTANT: PERHAPS MOVE OUT OF SWEEPS LOOP?
!       ------------------------------------------------------------------------
!       remove previous files used for visualisation and chi^2 behaviour
         if (visual) then
          call system('rm visual_*_'//trim(IONS(ION_INDEX)%name)//'*.vis')
          call system('rm visual_*_'//trim(IONS(ION_INDEX)%name)//'*.plot')
          call
     &    system('rm chi2_abs_profile_*_'//trim(IONS(ION_INDEX)%name)//'.vis')
         end if

!       ------------------------------------------------------------------------
!       identify (and get number of) significant absorption profiles
         call get_absorption_profiles()

!       ------------------------------------------------------------------------
!       compute spectrum characteristics: average S/N, average noise, etc.
         call spectrum_characteristics(sweep)

!       ========================================================================
         DO WHILE (any(abs_profile_tag.eq.1)) ! any unprocessed abs.prof. left?
!       ========================================================================

!       ------------------------------------------------------------------------
          call setup_abs_profile()

!       ------------------------------------------------------------------------
          call set_spectral_data()

!       ------------------------------------------------------------------------
!       IMPORTANT: should do this after every fit attempt (i.e., within the
!       do_fit loop) to take into account the change from saturated to
!       non-saturated residual absorption

          call load_spectral_data()  ! define abs.type, set input data and error

!       ------------------------------------------------------------------------
!       signal to start fit
          do_fit = .true.

!       ------------------------------------------------------------------------
!       (RE-)START FITTING LOOP FOR EACH DETECTED ABSORPTION PROFILE
!       ------------------------------------------------------------------------
          DO WHILE (do_fit)
!       ------------------------------------------------------------------------

!       (re)set arrays to compute average decrease rate of chi^2 value
!       initialise with large value
           chi2_decrease_rate_average_array(:) = convergence_criterium*1.0d2
          
           chi2_decrease_rate_average = convergence_criterium*1.0d2
          
           log_lambda_mrqmin_oscillation(chi2_decrease_iteration-1) =
     &     -lambda_mrqmin_max
          
           log_lambda_mrqmin_oscillation(chi2_decrease_iteration) =
     &     lambda_mrqmin_max
          
           array_aux(:) = 0.0d0

!       ------------------------------------------------------------------------

!       load component parameters contained in current absorption profile into
!       array `trial_param';
!       determine number of parameters (3x number of components in abs.profile);

           call
     &     load_fit_parameters(trial_param,trial_param_err,
     &     component_id,param_to_min,num_param,num_param_var)

!       update degrees of freedom
           degrees_of_freedom = absprofile_size_px - num_param

!       ------------------------------------------------------------------------
!       update iteration attempt counter
           fit_attempt = fit_attempt + 1          

!       ------------------------------------------------------------------------
!       output useful info
           call component_statistics(fit_attempt,sweep)

!       ------------------------------------------------------------------------
!       determine absolut minimum, optiumu, and tolerable chi^2-values
           call set_chi2_range()

!       ------------------------------------------------------------------------
!       visualisation (on-the-fly)

!       compute starting chi^2 value in absorption profile

           chi2_current =
     &     chi2_function(xdata_in,ydata_in,yerror_in,1,absprofile_size_px,
     &     trial_param,num_param)

           call visualise(visual,trial_param,velocity_min,velocity_max,
     &     abs_profile_id,num_abs_profiles,num_param,
     &     chi2_current/dble(degrees_of_freedom),'ifitnot')

!       ------------------------------------------------------------------------
!       minimisation method: Levenberg-Marquardt
           call levenberg_marquardt_minimisation()

!       ------------------------------------------------------------------------
!       store chi2 values for each iteration; used to visualise fit evolution;
           call
     &     write_chi2_evolution(2,visual,chi2_current/dble(degrees_of_freedom))

!       ------------------------------------------------------------------------
!       estimate parameter errors from diagonal elements of covariance matrix
           call parameter_uncertainty(num_param)

!       ------------------------------------------------------------------------
!       output fit-relevant values
           call fit_goodness(2,degrees_of_freedom,sweep)

!       ------------------------------------------------------------------------
!       visualisation (on-the-fly)

!       compute starting chi^2 value in absorption profile

           chi2_current =
     &     chi2_function(xdata_in,ydata_in,yerror_in,1,absprofile_size_px,
     &     trial_param,num_param)

           call visualise(visual,trial_param,velocity_min,velocity_max,
     &     abs_profile_id,num_abs_profiles,num_param,
     &     chi2_current/dble(degrees_of_freedom),'ifitnot')

!       ------------------------------------------------------------------------

!       signal to stop fit
           do_fit = .false.

!       ========================================================================
!       REMOVE IRRELEVANT COMPONENTS OR ADD NEW COMPONENTS TO IMPROVE FIT 
!       (i.e. whenever chi2 > chi2bad_reduced)
!       ========================================================================

!       ------------------------------------------------------------------------
!       get rid of irrelevant components (if any) and re-fit 

           call irrelevant_component_module(component_irrelevant)
           if (component_irrelevant) do_fit = .true.

!       ------------------------------------------------------------------------
!       if fit not good, try adding a new component and re-fit

           call insert_component_module(component_added)
           if (component_added) do_fit = .true.

!       ------------------------------------------------------------------------
!       if fit is good, try getting rid of uncertain/weak components and
!       re-fit 

           call discard_component_module(component_discarded)
           if (component_discarded) do_fit = .true.

!       ========================================================================
!       store previous number of degrees of freedom
           degrees_of_freedom_save = degrees_of_freedom

!       ------------------------------------------------------------------------
          END DO
!       ------------------------------------------------------------------------
!       END OF FITTING LOOP FOR EACH ABS.PROFILE (do_fit = true)
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
!       update degrees of freedom (in case a component has been added/removed)
          degrees_of_freedom = absprofile_size_px - num_param

!       ------------------------------------------------------------------------
          call verbose_output(verbose,trial_param,trial_param_err,component_id)
C          call verbose_output(.true.,trial_param,trial_param_err,component_id)
          
          write(6,'(1x,a,i5)')
     &    'final total number of fitted components in absorption profile:',
     &    ceiling(dble(num_param)/3.0d0)

!       record if absorption profiles contains no lines
          if (ceiling(dble(num_param)/3.0d0).eq.0)
     &    empty_abs_profiles = empty_abs_profiles + 1

!       ------------------------------------------------------------------------
!       output chi^2 values to visualise fit behaviour for each absorption
!       profile 
          call
     &    write_chi2_evolution(3,visual,chi2_current/dble(degrees_of_freedom))

!       ------------------------------------------------------------------------
!       update number of fitted components
          fitted_components = fitted_components + ceiling(dble(num_param)/3.0d0)

!       ------------------------------------------------------------------------
!       computes model spectrum (modelflux) with new component(s) inserted;
!       required to fit next absorption profile

!       load all current (accepted) component-parameter values 'trial_param'
!       (and corresponding errors) into 'vel_c, col_dens, bvalue'
          call save_fit_parameters(trial_param,trial_param_err,
     &    num_param,component_id)

          if (num_param.gt.0) then

			  do compid = 1, num_param, 3
				  call
     &	     model_spectrum(component_id(compid),
     &   	  component_id(compid),1,spectrum_size_px) 
			  end do

			 end if

!       convolve model spectrum with instrumental LSF (if fwhm_kms > 0)

          modelflux_convolved(:) = modelflux(:)

          call gauss_conv(modelflux_convolved,1,spectrum_size_px,
     &    pixel_size_kms_local,fwhm_kms)
         
!       ------------------------------------------------------------------------
!       visualise fitting iterations (not real-time);
!       only if components present

          call abs_profile_visual_final(visual)

!       ------------------------------------------------------------------------
!       store chi^2-value to compute average, minimum, and maximum chi^2

          call chi2_stats()

!       ========================================================================
         END DO !over detected absorption profiles
!       ========================================================================

!       INFO: at this stage, all absorption profiles with significant absorption
!       have been processed; if convolution is required, write output/input file
!       and re-start fitting process

!       ------------------------------------------------------------------------
!       compute *deconvolved*, rest-frame equivalent width of each component

         call compute_rest_ew_width(sweep)

!       ------------------------------------------------------------------------
!       compute [final] model spectrum
         call compute_model(ION_INDEX)

!       ------------------------------------------------------------------------
!       generate line list (write [raw <- sweep=1] fit results to file)
         call generate_linelist(ION_INDEX,sweep)

!       @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!        END DO ! over ION_INDEX = 1, num_ions
!       @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

!       ------------------------------------------------------------------------
!       program graphical output

        if (psplot) call gplot(ION_INDEX)

!       """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        END IF ! (skip single_ion_fit with no instrumental broadening)
!       """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

!       ------------------------------------------------------------------------
!       search for component which are multiplet members of any ion in list
!       only if required and if yet ignoring instrumental broadening
        if ((.not.single_ion_fit).and.(sweep.eq.2)) then
         call search_multiplets()
        end if

!       ------------------------------------------------------------------------
!       IMPORTANT: WILL NEED AN ADDITIONAL ROUTINE TO IDENTIFY SYSTEMS

!       IMPORTANT: WILL NEED AN ADDITIONAL LOOP ONCE THE SYSTEMS HAVE BEEN
!       IDENTIFIED

!       """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        END DO ! WHILE do_sweep = .true.
!       """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

!       ------------------------------------------------------------------------
!       output fit summary
        call fit_summary_std_out()

!       ------------------------------------------------------------------------
!       program end

        call comp_time()

        stop

!       ========================================================================
        end program ifitnot
!       ========================================================================
