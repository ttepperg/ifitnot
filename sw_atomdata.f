
!       ========================================================================
        module atomic_data

        use set_precision

!       ------------------------------------------------------------------------
!       data shamelessly stolen from SpecWizard (by Schaye, Booth, and Theuns)

!       ------------	
!       Atomic data:
!       ------------	
  
!       Atomic mass source: Atomic Weights of the Elements 2005, Pure Appl.
!       Chem, 78, 11 
!       Atomic mass unit source:
!       http://physics.nist.gov/cuu/Constants/index.html
!       Rest wavelength source: Morton 2003, ApJS, 149, 205
!       Oscillator strength sources:
!       Morton 2003, ApJS, 149, 205 (Most UV lines)
!       Verner et al 1994, AAPS, 108, 287

!       Multiplet transitions should be in order of decreasing osc strength!
!       ------------------------------------------------------------------------

        real(kind=doubleR), parameter :: atom_munit = 1.66053886e-24 ! g

!       ------------------------------------------------------------------------
!       parameters for hydrogen atom (H)

        real(kind=doubleR), parameter :: massH = 1.00794 * atom_munit ! g
        integer(kind=singleI), parameter ::  nlyman_all = 31

        real(kind=doubleR) ::  Lambda_H1(nlyman_all)
        data Lambda_H1 / 1215.6701, 1025.7223, 972.5368, 949.7431, &
        937.8035, 930.7483, 926.2257, 923.1504, &
        920.9631, 919.3514, 918.1294, 917.1806, &
        916.429, 915.824, 915.329, 914.919, &
        914.576, 914.286, 914.039, 913.826, &
        913.641, 913.480, 913.339, 913.215, &
        913.104, 913.006, 912.918, 912.839, &
        912.768, 912.703, 912.645 /

        real(kind=doubleR) ::  f_H1(nlyman_all)
        data f_H1 / 0.416400, 0.079120, 0.029000, 0.013940, &
        0.007799, 0.004814, 0.003183, 0.002216, &
        0.001605, 0.00120,  0.000921, 7.226e-4, &
        0.000577, 0.000469, 0.000386, 0.000321, &
        0.000270, 0.000230, 0.000197, 0.000170, &
        0.000148, 0.000129, 0.000114, 0.000101, &
        0.000089, 0.000080, 0.000071, 0.000064, &
        0.000058, 0.000053, 0.000048 /

!       ------------------------------------------------------------------------
!       He
        real(kind=doubleR), parameter :: massHe = 4.002602 * atom_munit
        real(kind=doubleR) ::  Lambda_He2(1), f_He2(1)
        data Lambda_He2 / 303.7822 /
        data f_He2 / 0.416 /

!       ------------------------------------------------------------------------
!       C
        real(kind=doubleR), parameter :: massC  = 12.0107 * atom_munit
        real(kind=doubleR) :: Lambda_C2(2), f_C2(2)
        data Lambda_C2 / 1334.5323, 1036.3367 /
        data f_C2 / 0.127800, 0.118000 /
        real(kind=doubleR) :: Lambda_C3(1), f_C3(1)
        data Lambda_C3 / 977.0201 /      
        data f_C3 / 0.7570 /
        real(kind=doubleR) :: Lambda_C4(2), f_C4(2)
        data Lambda_C4 / 1548.2041, 1550.7812 /      
        data f_C4 / 0.189900, 0.094750 /

!       ------------------------------------------------------------------------
!       N  
        real(kind=doubleR), parameter :: massN  = 14.0067 * atom_munit
        real(kind=doubleR)::  Lambda_N2(1), f_N2(1)
        data Lambda_N2 / 1083.9937 /      
        data f_N2 / 0.111 /
        real(kind=doubleR) :: Lambda_N3(1), f_N3(1)
        data Lambda_N3 / 989.799 /      
        data f_N3 / 0.12287 /
        real(kind=doubleR) :: Lambda_N4(1), f_N4(1)
        data Lambda_N4 / 765.148 /      
        data f_N4 / 0.632 /
        real(kind=doubleR) :: Lambda_N5(2), f_N5(2)
        data Lambda_N5 / 1238.821, 1242.804 /      
        data f_N5 / 0.156000, 0.0770 /

!       ------------------------------------------------------------------------
!       O  
        real(kind=doubleR), parameter :: massO  = 15.9994 * atom_munit
        real(kind=doubleR) :: Lambda_O1(3), f_O1(3)
        data Lambda_O1 / 1302.1685, 988.7734, 971.7382 /
        data f_O1 / 0.048000, 0.0465, 0.0116 /
        real(kind=doubleR) :: Lambda_O3(2), f_O3(2)
        data Lambda_O3 / 702.332, 832.927 /
        data f_O3 / 0.126, 0.0998 /
        real(kind=doubleR) :: Lambda_O4(1), f_O4(1)
        data Lambda_O4 / 787.711 /
        data f_O4 / 0.110 /
        real(kind=doubleR) :: Lambda_O5(1), f_O5(1)
        data Lambda_O5 / 629.730 /
        data f_O5 / 0.499 /
        real(kind=doubleR) :: Lambda_O6(2), f_O6(2)
        data Lambda_O6 / 1031.9261, 1037.6167 /      
        data f_O6 / 0.13250, 0.06580 /
        real(kind=doubleR) :: Lambda_O7(1), f_O7(1)
        data Lambda_O7 / 21.60169 /      
        data f_O7 / 0.696 /

!       ------------------------------------------------------------------------
!       Ne
        real(kind=doubleR), parameter :: massNe = 20.1797 * atom_munit
        real(kind=doubleR) :: Lambda_Ne8(2), f_Ne8(2)
        data Lambda_Ne8 / 770.409, 780.324 /
        data f_Ne8 / 0.103, 0.0505 /

!       ------------------------------------------------------------------------
!       Mg
        real(kind=doubleR), parameter :: massMg = 24.3050 * atom_munit
        real(kind=doubleR)::  Lambda_Mg2(2), f_Mg2(2)
        data Lambda_Mg2 / 2796.3543, 2803.5315 /
        data f_Mg2 / 0.6155, 0.3058 /

!       ------------------------------------------------------------------------
!       Al
        real(kind=doubleR), parameter :: massAl = 26.981538 * atom_munit
        real(kind=doubleR)::  Lambda_Al2(1), f_Al2(1)
        data Lambda_Al2 / 1670.79 /
        data f_Al2 / 1.73812 /
        real(kind=doubleR)::  Lambda_Al3(2), f_Al3(2)
        data Lambda_Al3 / 1854.72, 1862.79 /
        data f_Al3 / 0.559399, 0.277866 /

!       ------------------------------------------------------------------------
!       Si
        real(kind=doubleR), parameter :: massSi = 28.0855 * atom_munit
        real(kind=doubleR) :: Lambda_Si2(1), f_Si2(1)
        data Lambda_Si2 / 1260.420 /      
        data f_Si2 / 1.17621 /
        real(kind=doubleR) :: Lambda_Si3(1), f_Si3(1)
        data Lambda_Si3 / 1206.500 /      
        data f_Si3 / 1.63 /
        real(kind=doubleR) :: Lambda_Si4(2), f_Si4(2)
        data Lambda_Si4 / 1393.76018, 1402.77291 /      
        data f_Si4 / 0.513, 0.254 /

!       ------------------------------------------------------------------------
!       S
        real(kind=doubleR), parameter :: massS = 32.065 * atom_munit
        real(kind=doubleR) :: Lambda_S5(1), f_S5(1)
        data Lambda_S5 / 786.48 /      
        data f_S5 / 1.46 /

!       ------------------------------------------------------------------------
!       Fe  
        real(kind=doubleR), parameter :: massFe = 55.8452 * atom_munit
        real(kind=doubleR) ::  Lambda_Fe2(9), f_Fe2(9)
        data Lambda_Fe2 / 1144.9379, 1608.45085, 1063.1764, 1096.8769,  &
        1260.533, 1121.9748, 1081.8748, 1143.2260,  &
        1125.4477 /      
        data f_Fe2 / 0.083, 0.0577, 0.0547, 0.032700,  &
        0.024000, 0.0290, 0.012600, 0.0192, &
        0.0156 /
        real(kind=doubleR) ::  Lambda_Fe3(1), f_Fe3(1)
        data Lambda_Fe3 / 1122.52 /      
        data f_Fe3 / 0.0544257 /
  
  
        end module atomic_data
!       ========================================================================
