!       plot_spec_hdf: modules

!       ========================================================================
        module common_strings

        character(len=256) :: prefix, infile, dummy_str, comstr,
     +  simulation, specfile,
     +  errmsg, warnmsg, statmsg,
     +  average_sn_str, specfile_dir, specfile_prefix,
     +  fitfile, linesfile, regionsfile, psfile, plotfile

        character(len=256) :: groupname, attributename, datasetname
        
        character(len=256) :: spectrum_fitted_str, spectrum_written_str,
     +  spectrum_shifted_str, lines_detected_str, profile_str
        
        character(len=32) :: ion, ion_in, spectype, spec_id, noise_added,
     +  identifier, out_type
        
        character(len=5) :: str_inset
        
        end module common_strings
!       ========================================================================

!       ========================================================================
        module common_nums
        
        integer, dimension(1) :: indx_aux, dummy_int, seed
        integer :: file_id
        integer :: total_lines, total_los, total_regions, indx
        integer :: specid_int
        integer :: spectrum_shift
        integer :: skip_regions

        double precision :: aux_value, z_max, z_qso,
     +  average_sn, pxsize_ang, pxsize_noconv, fwhm_kms,
     +  wl_central, f_osc, eqw_aux

        double precision, allocatable :: v_central_region(:),
     +  wl_central_region(:)
        double precision :: equiv_width_lim_mA, Nion_lim

        double precision, parameter :: CLIGHT=2.9979d+10

        logical :: lines_detected = .false.
        logical :: spectrum_shifted = .false.
        
        end module common_nums
!       ========================================================================

!       ========================================================================
        module spectrum
        
        
        integer :: nveloc
        integer, allocatable :: spec_list(:)
        
        double precision, allocatable :: flux(:), fit(:), wavelength(:),
     +  v_Hubble(:), sigma(:), gauss_random(:), flux_nonoise(:)
        
        end module spectrum
!       ========================================================================

!       ========================================================================
        module alloc

        integer :: i, j, iAs
        integer :: rank
        integer, dimension(3) :: dim

        end module alloc
!       ========================================================================

!       ========================================================================
        module arrays_fit

        double precision, allocatable :: flux_fit(:), z_abs(:),
     +  log10_Nion(:), b_value(:), equiv_width_mA(:), dNion(:),
     +  db_value(:), v_line(:), dv_line(:), wl_line(:)

        end module arrays_fit
!       ========================================================================

!       ========================================================================
        module detected_regions
        
        integer , allocatable :: pixmin(:), pixmax(:), lines_in_region(:)
        double precision, allocatable :: wlmin(:), wlmax(:), velmin(:),
     +  velmax(:), significance(:)

        end module detected_regions
!       ========================================================================

!       ========================================================================
        module fit_quality
        
        double precision :: chi_sq_reduced
        character(len=5) :: check_fit_str
        logical :: check_fit = .false.

        end module fit_quality
!       ========================================================================
