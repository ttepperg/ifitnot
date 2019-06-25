        
!       ========================================================================
        module set_precision

!       ------------------------------------------------------------------------
!       shamelessly stolen from SpecWizard (by Schaye, Booth, and Theuns)

!       set precision of single and double precision real and integers
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
        module ifit_variables

        use set_precision

!       ------------------------------------------------------------------------
        integer(kind=doubleI) :: itime_start, itime_end
        character(len=24) :: time_start, time_end


!       ------------------------------------------------------------------------
!       'max_abs_profiles' is used to dynamically allocate some arrays
!       and it is continously changed during run-time!
        integer(kind=singleI) :: max_abs_profiles

        integer(kind=singleI) :: num_abs_profiles, num_abs_profiles_save,
     &  processed_abs_profiles, splitted_abs_profiles, empty_abs_profiles
        integer(kind=singleI), allocatable :: abs_profile_bounds(:,:)
        integer(kind=singleI), allocatable :: abs_profile_tag(:)

!       maximum number or components = 
!       (spectrum size) / (minimum absorption profile size)

!       required to allocate arrays
        integer(kind=singleI) :: num_components,  num_components_save
        integer(kind=singleI) :: max_components
!       keep this number low to avoid a memory overload
        integer(kind=singleI), parameter :: max_components_abs = 500

        integer(kind=singleI) :: weak_component_id

!       ------------------------------------------------------------------------
!       resolution variables

!       size of resolution element [pixel]
        integer(kind=singleI) :: resol_elem_px

!       minimum allowed number of degrees of freedom in reduced chi^2
        integer(kind=singleI), parameter :: min_dof = 2

!       minimum number of pixel to consider a component resolved;
!       tied to number of component parameters (v,N,b)
        integer(kind=singleI), parameter :: min_component_res_px = 3

!       minimum allowed size of a detected absorption profile;
!       chosen to be an odd number
        integer(kind=singleI), parameter ::
     &  min_absprofile_size_px = min_component_res_px + min_dof

!       ------------------------------------------------------------------------
!       spectral data

        real(kind=doubleR), allocatable :: velocity(:), velocity_aux(:)
        real(kind=doubleR), allocatable :: velocity_local(:)
        real(kind=doubleR), allocatable :: wlength(:), wlength_aux(:)

        real(kind=doubleR), allocatable :: flux(:), flux_smooth(:), flux_conv(:)
        real(kind=doubleR), allocatable :: flux_residum(:)

        real(kind=doubleR), allocatable :: modelflux(:), modelflux_convolved(:),
     &  modelflux_local(:), modelflux_local_aux(:)

        real(kind=doubleR), allocatable :: aod_data(:)
        real(kind=doubleR), allocatable :: aod_residum(:)

        real(kind=doubleR), allocatable :: noise(:), sigma(:)
        real(kind=doubleR), allocatable :: flux_residum_error(:)
        real(kind=doubleR), allocatable :: aod_residum_error(:)

        real(kind=doubleR), allocatable :: xdata_in(:), ydata_in(:),
     &  yerror_in(:)

!       ------------------------------------------------------------------------
!       atomic variables
!       derived (user-defined) type

        type single_t
         sequence             ! store data elements next to each other in memory
         real(kind=doubleR) :: lambda_0, f_osc, big_gamma ! fundamental quant.
         real(kind=doubleR) :: tau_0, gamma_over_nu       ! derived quant.
        end type single_t

!       ------------------------------------------------------------------------
!       absorption component parameters;
!       derived (user-defined) type

        type absorption_line
         sequence
         integer(kind=singleI) :: status
         real(kind=doubleR) :: vel_c, col_dens, b_value
         real(kind=doubleR) :: vel_c_err, col_dens_err, b_value_err
         real(kind=doubleR) :: v_coldens_cov, v_b_cov, coldens_b_cov
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

        type link
         sequence
         integer(kind=singleI) :: component
         integer(kind=singleI) :: transition
        end type link

        integer(kind=singleI) :: num_ions
        integer(kind=singleI) :: ION_INDEX

        type(ion), allocatable :: IONS(:)               ! main working structure

        integer(kind=singleI), allocatable :: strongest(:,:)
        real(kind=singleR), allocatable :: z_min(:,:)

        integer(kind=singleI), allocatable :: true_ion(:)
        integer(kind=singleI), allocatable :: true_transition(:)
        type(link), allocatable :: lead(:)                   ! to tag multiplets


        real(kind=doubleR), allocatable :: wl_comp(:), z_abs_comp(:)

!       ------------------------------------------------------------------------
!       fitting parameters (these are esentially the component parameters,
!       stored in sequential order [v_c,N,b]_i, i=1, num_components
!       the corresponding data sets have thus size 3 * num_components

        real(kind=doubleR), allocatable :: trial_param(:), trial_param_err(:)
        real(kind=doubleR), allocatable :: trial_param_cov(:,:) ! parameter
                                                                ! covariance
        real(kind=doubleR), allocatable :: trial_param_save(:)
 
        integer(kind=singleI), allocatable :: param_to_min(:)
        integer(kind=singleI), allocatable :: component_id(:)


        integer(kind=singleI) :: max_num_param
        integer(kind=singleI) :: num_param, num_param_var, num_param_save

!       ------------------------------------------------------------------------
!       spectral characteristics

!       actual and effective (see read_spectrum) spectrum sizes [pixel]
        integer(kind=singleI) :: spectrum_size_px, spectrum_size_px_effective
!       the following only applies when constraining the wl-range to the range
!       where lines of the given ion are expected (given its restframe wlength)
        real(kind=doubleR) :: wlength_tolerance = 2 ! in Angstroem
        real(kind=doubleR) :: wl_min, wl_max

!       to flag absorbed (0) and unabsorbed (1) pixels 
        integer(kind=singleI), allocatable :: unabsorbed_pixel(:)

        real(kind=doubleR) :: velocity_min, velocity_max
        integer(kind=singleI) :: pixel_min, pixel_max

        real(kind=doubleR) :: average_sn_global, average_noise_global 
        real(kind=doubleR) :: average_noise_local_smooth
        real(kind=doubleR) :: average_noise_local, average_sn_local
        real(kind=doubleR) :: average_noise_local_aod
        real(kind=doubleR) :: average_flux_local
        real(kind=doubleR) :: equiv_width_lim_mA, col_dens_lim,
     &  absorption_strength_lim, tau_central_lim, b_instrumental

        real(kind=doubleR) :: pixel_size_kms, pixel_size_ang
        real(kind=doubleR) :: pixel_size_kms_local, pixel_size_ang_local

        
!       ------------------------------------------------------------------------
!       input parameters (and associated parameters)


!       these still to be included as input (user defined) parameters:
        real(kind=doubleR) :: tau_sat

!       IMPORTANT: these quantities are ion-dependent;
!       REQUIRE THE EXISTENCE OF A FILE ifit_parameters.<ion> TO BE LOADED

        real(kind=doubleR), allocatable :: col_dens_min(:), col_dens_max(:)
        real(kind=doubleR), allocatable :: b_value_min(:), b_value_max(:)

!       IMPORTANT: these quantities are general to all ions
!       REQUIRE THE EXISTENCE OF A FILE ifit_parameters.gen
        real(kind=doubleR) :: fwhm_kms, fwhm_kms_save
        real(kind=doubleR) :: scale_noise, Nsigma
        real(kind=doubleR) :: conf_hi, conf_lo, conf_min
        real(kind=doubleR) :: chi2bad_reduced, chi2good_reduced
        real(kind=doubleR) :: z_qso = -1.0d2

        character(len=5) :: profile_str

!       fit using the smoothed flux rather than the input flux; useful when S/N <~ 10,
!		  but refrain from using for high-resolution, high S/N data
!       Note: the default ist FALSE        
        character(len=5) :: use_smooth_flux_str
        logical :: use_smooth_flux

!       fit using the strongest transition of first input ion; other ions, if
!       given, are ignored
!       Note: the default ist TRUE        
        character(len=5) :: single_ion_fit_str
        logical :: single_ion_fit

!       set spectrum_shifted_str = TRUE to permute spectrum cyclically in order
!       to avoid absorption features at boundaries
!       meaningful only for simulated spectra from simulations with periodic
!       boundary conditions
        character(len=5) :: spectrum_shifted_str
        logical :: spectrum_shifted
        integer(kind=singleI) :: spectrum_shift_px

!       use to test code; output differs from default output
        character(len=5) :: test_run_str
        logical :: test_run

!       control output
        character(len=5) :: verbose_str, debug_str, quite_str
        logical :: verbose, debug, quite

!       use to visualise fit (on-the-fly)
        character(len=5) :: visual_str
        logical :: visual
        integer(kind=singleI) :: visual_counter

!       to produce graphical output (PS); requires gnuplot and gv!
        character(len=5) :: psplot_str
        logical :: psplot

!       convolution, tied to fwhm_kms
        logical :: convolve

!       number of times that spec is fit: 1,2 depending on the value of
!      `fwhm_kms' and `convolve'
        integer(kind=singleI) :: sweep

!       to flag `glitches' in input spectrum
        integer(kind=singleI) :: neg_flux_counts, neg_noise_counts,
     +  bad_pixel_counts
        real(kind=singleR), parameter :: sigma_bad_pixel = 1.0d1
        logical :: neg_flux, neg_noise, bad_pixel

        character(len=6) :: absorption_type
        character(len=24) :: absorption_type_str, fit_type_str, smoothing_str

!       ------------------------------------------------------------------------
!       numerical control parameters

        real(kind=doubleR), parameter :: convergence_criterium = 1.0d-2
        real(kind=doubleR), parameter ::
     &  strong_convergence_criterium = 1.0d-1 * convergence_criterium
        real(kind=doubleR), parameter :: lambda_mrqmin_max = 1.0d3

!       the following turns out to be a VERY important parameter, since the
!       results are highly sensitive to its value;
!       for instante, values =>1.0e-4 yield bad results, while <=1.0e-5 are more
!       suitable values; 
!       NOTE: the smaller its value, the more stable, but the slower is the code
!       since convergence requires a larger number of steps
!       (see function param_derivatives)
        real(kind=doubleR), parameter :: der_minimum = 1.0d-6

!       ------------------------------------------------------------------------
!       output variables

        character(len=128) :: output_dashed_line = 
     &  ' ----------------------------------------------------------------'//
     &  '----------'

        character(len=128) :: output_fitabs_profile_info_1 = 
     &  ' TYPE  ABS.PROFILE (TOT)  RANGE     PIXEL'//
     &      '    VEL   RESID   NOISE   log10(N)  b    GOODNESS'

        character(len=128) :: output_fitabs_profile_info_2 = 
     &  ' ABS.PROFILE  (REMAIN)  COMPONENTS    (MAX)    GOODNESS'

        character(len=256) :: sig_file
        character(len=256) :: specfile_in
        character(len=256), allocatable :: ifit_file_prefix(:), ifit_lines(:)

        character(len=256) :: statmsg, warnmsg, errmsg

!       coloured messages in red=31, yellow=33, green=32
        character(len=64), parameter :: colour_statmsg_prefix=CHAR(27)//'[1;32m'
        character(len=64), parameter :: colour_warnmsg_prefix=CHAR(27)//'[1;33m'
        character(len=64), parameter :: colour_errmsg_prefix=CHAR(27)//'[1;31m'
        character(len=64), parameter :: colour_msg_suffix=' '//CHAR(27)//'[0m'

        integer(kind=singleI) :: discarded_component
        integer(kind=singleI) :: outlier_pixels
        integer(kind=singleI) :: degrees_of_freedom, degrees_of_freedom_save
        integer(kind=singleI) :: abs_profile_id
        integer(kind=singleI) :: absprofile_size_px
        integer(kind=singleI) :: mrqmin_iteration, success_mrqmin_iteration,
     &  fit_attempt, tot_iter_per_abs_profile
        integer(kind=singleI) :: fitted_components, valid_components_save,
     &  discarded_components, inserted_components, unidentified_components

!       number of iterations used to average the decrease in chi^2 value and
!       decide whether it has converged
!       can be considered as a burn-out parameter
!       IMPORTANT:
!       low values (<10) speedup the code, but might not guarantee convergence

        integer(kind=singleI), parameter :: chi2_decrease_iteration = 10

!       used to identify significant absorption profiles
        real(kind=doubleR), allocatable :: linear_chi2_spec(:)

!       individual terms in chi^2 function
        real(kind=doubleR), allocatable :: chi2_terms(:)

!       chi^2 value after each mrqmin iteration
        real(kind=doubleR), allocatable :: chi2_evolution(:)
        real(kind=singleI), allocatable :: component_number_evolution(:)

!       Matrices needed by mrqmin
        real(kind=doubleR), allocatable :: COVAR(:,:), ALPHA(:,:)

!       model first and second derivaties
        real(kind=doubleR), allocatable :: dfdv(:), d2fdv2(:)

        integer(kind=singleI), parameter :: iter_max = 500

        real(kind=doubleR) :: lambda_mrqmin
        real(kind=doubleR) :: chi2_start, chi2_current, chi2_previous
        real(kind=doubleR) :: chi2_min_abs_local
        real(kind=doubleR) :: chi2_discard_save, chi2_add_save
        real(kind=doubleR) :: best_chi2_add, best_reduced_chi2
        real(kind=doubleR) :: chi2_decrease_rate_diff,
     &  log_lambda_mrqmin_oscillation_diff
        real(kind=doubleR) :: chi2_decrease_rate_average,
     &  chi2_decrease_rate_average_array(chi2_decrease_iteration),
     &  array_aux(chi2_decrease_iteration),
     &  log_lambda_mrqmin_oscillation(chi2_decrease_iteration)

        real(kind=doubleR) :: mean_chi2, min_chi2(2), max_chi2(2)

!       ------------------------------------------------------------------------
!       output variables

!       values of a sucessful MRQMIN iteration; needed only for output
!       purposes

        real(kind=doubleR) :: chi2_current_succ, best_reduced_chi2_succ

        character(len=3) :: abs_profile_id_str

        character(len=256) :: file_chi2_evolution

        character(len=128) :: output_abs_profile_info_debug = 
     &  ' ABS.PROF.ID (REMAIN)    <FLUX>       '//
     &  'VEL RANGE [km/s]      COMPONENTS  '//
     &  '(here | fitted | discarded | added | processed | total)'

        character(len=128) :: output_abs_profile_info_short = 
     &  ' ABS.PROF.ID (REMAIN)    <FLUX>       '//
     &  'VEL RANGE [km/s]      SWEEP'

        character(len=256) :: output_mrqmin_iteration_1_debug = 
     &  '  __iter__   '//
     &  '     ____chi^2 ____         __rel.decrease__ '//
     &  '   ___*lambda*___'

         character(len=256) :: output_mrqmin_iteration_2_debug=
     &  '  succ  tot'//
     &  '       best      now            diff    avg'

        character(len=256) :: output_mrqmin_iteration_1_short = 
     &  ' ___ iterations __    '//
     &  ' ________chi^2 ________     ___ relative decrease ___'

!        character(len=256) :: output_mrqmin_iteration_2_short=
!     &  ' successful  total'//
!     &  '       best        current      differential      average'

        character(len=256) :: output_mrqmin_iteration_2_short=
     &  ' successful  total'//
     &  '       best        current      average     log10(lambda)'

        character(len=128) :: output_fit_results_short =
     &  ' FIT ATTEMPT   chi^2/dof  '//
     &  ' ABS.PROF.ID (REMAIN)    VEL RANGE [km/s]  SWEEP'

        character(len=128) :: output_fit_results_debug =
     &  ' FIT ATTEMPT   chi^2/dof  '//
     &  ' ABS.PROF.ID (REMAIN)      VEL RANGE [km/s]   '//
     &  ' COMPONENTS (working | fitted | total)'

        character(len=5) :: check_fit_str = 'FALSE'
        logical :: check_fit = .false.        !to flag fits with high chi^2
        logical :: adding_component           !to add components
        logical :: add_more                   !to continue adding components
        logical :: component_discarded        !to discard components
        logical :: component_irrelevant       !to discard irrelevant components
        logical :: component_added = .false.  !to add components
        logical :: do_fit                     !signal to fit abs. profile
        logical :: first_call_visual          ! signal to write plotfile header


        end module ifit_variables
!       ========================================================================

!       ========================================================================
        module gplot_variables
        
        use set_precision

!       to circularly shift spectra
        integer(kind=singleI) :: velocity_shift_px
        integer(kind=singleI), dimension(1) :: arr_pos_min
        real(kind=doubleR) :: dv_shift = 0.0d0

!       offset in pixel to set plotting range
        integer(kind=doubleI), parameter :: px_offset = 75

        real(kind=doubleR) :: zmin
        real(kind=doubleR), allocatable :: z_abs_true(:), vel_c_true(:)

        integer(kind=singleI) :: num_abspro_gplot
        integer(kind=singleI), allocatable :: abspro_bounds_true(:,:)
        integer(kind=singleI), allocatable :: abspro_bounds_aux(:,:)

        real(kind=doubleR), allocatable :: gauss_dev(:)

        end module gplot_variables
!       ========================================================================


!       ========================================================================
        module constants
        
        use set_precision

!       mathematical and physical constants (in cgs,except e0 which is in ev)
!       math constants   
        real(kind=doubleR), parameter :: pi = 3.14159265

!       physical constants (cgs units; except c_kms)
        real(kind=doubleR), parameter :: charge_e = 4.80320682d-10,   
     &  c_cms       = 2.99792458d10, 
     &  c_kms    = c_cms * 1.0d-5,
     &  m_e      = 9.10938975d-28,   
     &  radius_e= (charge_e * charge_e) / (m_e * c_cms * c_cms)

        real(kind=doubleR), parameter :: atom_munit = 1.66053886e-24 ! g

        end module constants
!       ========================================================================

!       ========================================================================
        module fourier
        
        use set_precision, only: doubleR

        complex(kind=doubleR), allocatable :: fft(:)
        
        end module fourier
!       ========================================================================

!       ========================================================================
        module cum_chi2_dist
        
         use set_precision
         
         integer(kind=singleI) :: deg_of_free
         real(kind=doubleR) :: confidence, skewness
        
        end module cum_chi2_dist
!       ========================================================================
        
!       ========================================================================
        module new_ops
        
         use set_precision

         interface operator (.converged.)      ! the operator
          module procedure converged           ! the function that implements it
         end interface 

         contains

!       ------------------------------------------------------------------------
         logical function converged(chi2_new)

!       NOTE:
!       terminate Levenberg-Marquardt minimisation iteration if:
!       
!       -chi^2 improved, and convergence has been achieved,
!       or
!       -chi^2 improved and reduced chi^2 <= chi^2_good; no convergence required
!       or
!       -convergenced has been achieved, and chi^2 < chi^2_bad
!       or
!       -chi^2 improved, component has been added, and convergence
!       has been achieved
!       or
!       -if adding a component brings chi^2 below chi^2_bad; no convergence
!        required 

!       If none of these conditions is met, keep looking for a better
!       solution until maximum number of allowed iterations is reached

         use set_precision

         use ifit_variables, only: strong_convergence_criterium, chi2_previous,
     +   convergence_criterium, chi2good_reduced, chi2bad_reduced,
     +   chi2_decrease_rate_average, lambda_mrqmin, lambda_mrqmin_max,
     +   log_lambda_mrqmin_oscillation_diff, adding_component,
     +   success_mrqmin_iteration, iter_max,
     +   degrees_of_freedom, degrees_of_freedom_save

         implicit none
        
         integer(kind=singleI) :: dof_new, dof_save
         real(kind=doubleR) :: chi2_save
         real(kind=doubleR), intent(in) :: chi2_new

!       ------------------------------------------------------------------------
!       default value
         converged = .false.

!       set auxiliary values
         chi2_save = chi2_previous
         dof_new = degrees_of_freedom
         dof_save = degrees_of_freedom_save

!       ------------------------------------------------------------------------
!       convergence criteria

!       ------------------------------------------------------------------------
         if (((chi2_new/dble(dof_new)).lt.(chi2_save/dble(dof_save))).and.
     &   ((chi2_new/dble(dof_new)).le.chi2good_reduced)) then

          converged = .true.
          return
         end if
        
!       ------------------------------------------------------------------------
         if (((chi2_new/dble(dof_new)).lt.(chi2_save/dble(dof_save))).and.
     &   (chi2_decrease_rate_average.lt.convergence_criterium).and.
     &   ((chi2_new/dble(dof_new)).lt.chi2bad_reduced)) then

          converged = .true.
          return

         end if

!       ------------------------------------------------------------------------
         if (((chi2_new/dble(dof_new)).lt.(chi2_save/dble(dof_save)))
     &   .and.((chi2_new/dble(dof_new)).le.chi2bad_reduced).and.
     &   (adding_component)) then

          converged = .true.
          return

         end if
         
!       ------------------------------------------------------------------------
         if ((chi2_decrease_rate_average.lt.convergence_criterium).and.
     &   (log_lambda_mrqmin_oscillation_diff.le.1).and.
     &   (.not.adding_component)) then

          converged = .true.
          return
         end if

!       ------------------------------------------------------------------------
         if (chi2_decrease_rate_average.lt.strong_convergence_criterium) then

          converged = .true.
          return
         end if

!       ------------------------------------------------------------------------
         if (lambda_mrqmin.ge.lambda_mrqmin_max) then
          converged = .true.
          return
         end if
         
!       ------------------------------------------------------------------------
         if (success_mrqmin_iteration.ge.iter_max) then
          converged = .true.
          return
         end if
         
!       ------------------------------------------------------------------------
         return

         end function converged
!       ------------------------------------------------------------------------

        end module new_ops
!       ========================================================================
        
