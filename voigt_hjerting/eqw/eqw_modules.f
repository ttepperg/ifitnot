!       (C) Thorsten Tepper Garcia 2004 (november)
!
!       CONTAINS THE VARIABLES AND PARAMETERS NEEDED TO RUN eqw.f
!       NOTE: All CONSTANTS IN CGS-UNITS!!!
!
!       IPOINTS   -> No. of IPOINTS after interpolation
!
!       =============================================================

        module variables

        integer, parameter :: IPOINTS=700000

!       --Physical parameters (cgs units)
!       
        double precision, parameter :: CLIGHT=2.9979d+10
        double precision, parameter :: ERADIUS=2.817d-13
        double precision, parameter :: PI=3.141593d0
        double precision, parameter :: ECHARGE=1.602d-19,EMASS=9.109d-31
        double precision, parameter :: fluxmin=1.0d-44
 
        double precision :: b_value, log_col_dens, b, N_ion, tau_central,
     +  sqrtPI, sqrtPIinv, lambda_0, fvalue, gamma

        double precision :: alambda(IPOINTS),flux(IPOINTS)
        
        character*128 :: homedir,infile,infile2
        character*10 :: ion_name, ion_str

        end module variables
