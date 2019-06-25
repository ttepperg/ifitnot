        module common_strings

        character(len=256) :: infile, outfile, dummy, comstr, redshift_str,
     +  ion, simulation, specwizfile, fitfile_hdf5, spectype, spec_id, noise_added,
     +        errmsg, warnmsg, statmsg, lines_detected_str,
     +        ion_str, ion_name, ion_sw,
     +        homedir, outfile_avp, z_qso_avp 
        
!       character(len=256), parameter :: specdir='/SpecWizard_vdec2008/trials/'
!       character(len=256), parameter :: specdir='/SpecWizard_vapr2009/trials/'
!       character(len=256), parameter :: specdir='/SpecWizard_vjun2009/trials/'
        character(len=256), parameter :: specdir='/mySpecWizard/trials/'
        character(len=256), parameter :: ifit_dir='/ifitnot/'

        character(len=256) :: groupname, attributename, datasetname

        character(len=256) :: spectrum_fitted_str, spectrum_written_str,
     +        signaltonoise_str, spectrum_convolved_str, spectrum_shifted_str

        character(len=256) :: fitfile, linesfile, ewrfile

        character(len=10) :: date, clock, zone
        
        end module common_strings

!       ----------------------------------------------------------------

        module common_nums
        
        integer :: file_id, aux_value_int
        integer :: total_lines, total_los, total_regions
        integer :: specid_int
        integer :: nveloc
        integer :: spectrum_shift
        integer,dimension(1) :: datetime(8)

        double precision :: aux_value, z_box, z_min, z_max, z_qso,
     +  signaltonoise, pxsize_ang, pxsize_noconv, fwhm_kms,
     +        wl_central, f_osc, log_tot_coldens_ion

!        formal completeness limit
        double precision :: equiv_width_lim_mA, Nion_lim

        character(len=5) :: aux_value_str

        logical :: lines_detected, spectrum_fitted, spectrum_written,
     +        noisy, spectrum_convolved
             logical :: spectrum_shifted = .false.

        end module common_nums

!       ----------------------------------------------------------------

        module ifit_par
         
         integer :: nsplitmin, profile, maxiter
         
         double precision :: fsigma, Nionmin, Nionmaxns, Nionmaxsat, bparmax,
     +         bparmin, fwhm_avp, chisqbad,
     +         chisqgood, chisqtol, N_sigma
         
         character(len=10) :: spectrum_cycled_str
         character(len=5) :: profile_str

        end module ifit_par

!       ----------------------------------------------------------------

        module common_arrays_read
        
        double precision, allocatable :: flux(:), flux_noisy(:),
     +        wavelength(:), opticaldepth(:), noise(:), sigma(:), gaussdev(:),
     +        v_Hubble(:)
        
        end module common_arrays_read

!       ----------------------------------------------------------------

        module common_arrays_rw
        
        integer, allocatable :: spec_list(:), spec_list_aux(:)
        
        end module common_arrays_rw

!       ----------------------------------------------------------------

        module arrays_fit

        double precision, allocatable :: flux_fit(:), z_abs(:),
     +        log10_Nion(:), b_value(:), equiv_width_mA(:), dNion(:),
     +        db_value(:), v_line(:), dv_line(:), wl_line(:),
     +        wl_line_shift(:), !line centroid with respect to shifted spectrum
     +        v_line_shift(:) !line centroid with respect to shifted spectrum

        end module arrays_fit

!       ----------------------------------------------------------------

        module detected_regions
        
        integer, allocatable :: pixmin(:), pixmax(:), lines_in_region(:)
        double precision, allocatable :: wlmin(:), wlmax(:), velmin(:),
     +        velmax(:), significance(:)

        end module detected_regions

!       ----------------------------------------------------------------

        module fit_quality
        
        double precision :: chi_sq_reduced
        character(len=5) :: check_fit_str
        logical :: check_fit = .false.

        end module fit_quality

!       ----------------------------------------------------------------

        module numbers
!        set precision of single and double precision real and integers
        integer, parameter :: singleR = selected_real_kind(p=6,r=37)
        integer, parameter :: doubleR = selected_real_kind(p=15,r=307)
        integer, parameter :: singleI = selected_int_kind(9)
        integer, parameter :: doubleI = selected_int_kind(18)
        ! signal invalid values
        real(kind=doubleR), parameter         :: invalid_R = -1.d99
        integer(kind=singleI), parameter :: invalid_I = -1234567890
        character(len=20), parameter         :: invalid   = 'INVALID'

        end module numbers

!       ----------------------------------------------------------------

        module fourier
        
        double complex, allocatable :: fft(:)
        
        end module fourier

