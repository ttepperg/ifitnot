!       (C) Thorsten Tepper Garcia 2006

!       CONTAINS THE VARIABLES AND PARAMETERS NEEDED TO RUN
!       profiles.f

!       ========================================================================
        module set_precision

!       ------------------------------------------------------------------------
!       shamelessly stolen from SpecWizard (by Schaye, Booth, and Theuns)

!       set precision of single and double precision real and integers
        integer, parameter :: singleR = selected_real_kind(p=6,r=37)
        integer, parameter :: doubleR = selected_real_kind(p=15,r=307)
        integer, parameter :: singleI = selected_int_kind(9)
!        integer, parameter :: doubleI = selected_int_kind(18)

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
        module common_nums
        
        integer :: nlines
        integer, allocatable :: z_indx(:)
        
        integer :: spectrum_shift

        double precision :: average_chi_sq, average_sn

        double precision, parameter :: fluxmin = 1.0d-99 !smallest
        ! flux value allowed

        double precision :: wl_central, wavelength, fvalue, gamma,
     +        wl_min, wl_max, zplus1_inv, dopp_width,damping, tau_central,
     +        tau, phi, sqrtPI, z_min, void, fwhm_kms,
     +  pixel_size_ang, pixel_size_kms

        double precision :: equiv_width_lim_mA, Nion_lim

             double precision, allocatable ::  b_value(:),
     +  Nion(:), dNion(:), dvel_line(:), db_value(:), wl_line(:),
     +        equiv_width_mA(:), vel_line(:), z_abs(:), vel_line_shift(:),
     +        wl_line_shift(:)
        
        double precision, allocatable ::  sigma(:), flux(:),
     +        fluxorig(:), waveln(:), wl_aux(:), vel(:), vel_aux(:)
        
        end module common_nums
!       ========================================================================

!       ========================================================================
        module common_strings
        
        character(len=256) :: ion_name, ion_str
        character(len=256) :: infile
        character(len=256) errmsg, warnmsg, statmsg
        character(len=256) :: signaltonoise_str
        character(len=5) :: check_fit_str !if true, chi^2 value
        !indicates bad fit
        character(len=5) :: spectrum_shifted_str !if true, spectrum has
        !been cycled by 'spectrum_shift' pixels (see common_nums)

        end module common_strings
!       ========================================================================

!       ========================================================================
        module physical
        
!       Physical parameters (cgs UNITS)

        double precision, parameter :: sigma0=6.3d-18
        double precision, parameter :: CLIGHT=2.9979d+10
        double precision, parameter :: ERADIUS=2.818d-13
        double precision, parameter :: PI=3.141593d0
        double precision, parameter :: ECHARGE=4.80d-10,EMASS=9.11d-28

        end module physical
!       ========================================================================

!       ========================================================================
        module fourier
        
        use set_precision, only: doubleR

        complex(kind=doubleR), allocatable :: fft(:)
        
        end module fourier
!       ========================================================================
