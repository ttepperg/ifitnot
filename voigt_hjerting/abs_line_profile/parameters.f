!        (C) Thorsten Tepper Garcia 2009
!
!        CONTAINS THE VARIABLES AND PARAMETERS NEEDED TO RUN
!        abs_line_profile.f
!

!        ========================================================================
!        Physical parameters (cgs UNITS)

        double precision, parameter :: sigma0=6.3d-18
        double precision, parameter :: CLIGHT=2.9979d+10
        double precision, parameter :: ERADIUS=2.817d-13
        double precision, parameter :: PI=3.141593d0
        double precision, parameter :: ECHARGE=1.602d-19
        double precision, parameter :: EMASS=9.109d-34

!        Atomic numbers
        integer, parameter :: A_H=1, A_He=4, A_O=16, A_Ne=20, A_C=12, A_Si=28

        double precision, parameter :: H_MASS = 1.673d-27 !kg
        double precision, parameter :: boltzmann=1.3806504d-23 !J/K
