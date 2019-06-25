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
        module derived_types

        use set_precision

        type single_t
         sequence             ! store data elements next to each other in memory
         real(kind=doubleR) :: lambda_0, f_osc, big_gamma ! fundamental quant.
        end type single_t

        type ion
         sequence
         character(len=10) :: name
         integer(kind=singleI) :: num_transitions
         type(single_t), allocatable :: transition(:)
        end type ion

        type atom
         sequence
         character(len=10) :: name
         double precision :: mass
         integer(kind=singleI) :: num_ions
         type(ion), allocatable :: ion(:)
        end type atom

        end module derived_types
!       ========================================================================

!       ========================================================================
        module atomdata

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
        integer(kind=singleI), parameter :: num_ions_H = 1
        character(len=4) :: ion_name_H(num_ions_H)
        data ion_name_H / 'H1' /

        integer(kind=singleI), parameter :: transitions_H1 = 31

        real(kind=doubleR) :: Lambda_H1(transitions_H1)
        data Lambda_H1 / 1215.6701, 1025.7223, 972.5368, 949.7431,
     +  937.8035, 930.7483, 926.2257, 923.1504,
     +  920.9631, 919.3514, 918.1294, 917.1806,
     +  916.429, 915.824, 915.329, 914.919,
     +  914.576, 914.286, 914.039, 913.826,
     +  913.641, 913.480, 913.339, 913.215,
     +  913.104, 913.006, 912.918, 912.839,
     +  912.768, 912.703, 912.645 /

        real(kind=doubleR) :: f_H1(transitions_H1)
        data f_H1 / 0.416400, 0.079120, 0.029000, 0.013940,
     +  0.007799, 0.004814, 0.003183, 0.002216,
     +  0.001605, 0.00120,  0.000921, 7.226e-4,
     +  0.000577, 0.000469, 0.000386, 0.000321,
     +  0.000270, 0.000230, 0.000197, 0.000170,
     +  0.000148, 0.000129, 0.000114, 0.000101,
     +  0.000089, 0.000080, 0.000071, 0.000064,
     +  0.000058, 0.000053, 0.000048 /

!    the values from the 5th on are probably WRONG!
        real(kind=doubleR) :: Gamma_H1(transitions_H1)
        data Gamma_H1 / 6.26500e+08, 1.89700e+08,
     +  8.12700e+07, 4.20400e+07, 2.45000e+07, 2.45000e+07, 2.45000e+07,
     +  2.45000e+07, 2.45000e+07, 2.45000e+07, 2.45000e+07, 2.45000e+07,
     +  2.45000e+07, 2.45000e+07, 2.45000e+07, 2.45000e+07, 2.45000e+07,
     +  2.45000e+07, 2.45000e+07, 2.45000e+07, 2.45000e+07, 2.45000e+07,
     +  2.45000e+07, 2.45000e+07, 2.45000e+07, 2.45000e+07, 2.45000e+07,
     +  2.45000e+07, 2.45000e+07, 2.45000e+07, 2.45000e+07 /

!       ------------------------------------------------------------------------
!       He
        real(kind=doubleR), parameter :: massHe = 4.002602 * atom_munit
        integer(kind=singleI), parameter :: num_ions_He = 1
        character(len=4) :: ion_name_He(num_ions_He)
        data ion_name_He / 'He2' /

        integer(kind=singleI), parameter :: transitions_He2 = 1
        real(kind=doubleR) :: Lambda_He2(transitions_He2),
     +  f_He2(transitions_He2)
        data Lambda_He2 / 303.7822 /
        data f_He2 / 0.416 /
!    the values from the 5th on are probably WRONG!
        real(kind=doubleR) :: Gamma_He2(transitions_He2)
        data Gamma_He2 / 1.0026d10 /

!       ------------------------------------------------------------------------
!       C
        real(kind=doubleR), parameter :: massC  = 12.0107 * atom_munit
        integer(kind=singleI), parameter :: num_ions_C = 3
        character(len=4) :: ion_name_C(num_ions_C)
        data ion_name_C / 'C2', 'C3', 'C4' /

        integer(kind=singleI), parameter :: transitions_C2 = 2
        real(kind=doubleR) :: Lambda_C2(transitions_C2), f_C2(transitions_C2)
        data Lambda_C2 / 1334.5323, 1036.3367 /
        data f_C2 / 0.127800, 0.118000 /
        real(kind=doubleR) ::Gamma_C2(transitions_C2)
        data Gamma_C2 / 2.90000e+08, 2.20000e+09 /

        integer(kind=singleI), parameter :: transitions_C3 = 1
        real(kind=doubleR) :: Lambda_C3(transitions_C3), f_C3(transitions_C3)
        data Lambda_C3 / 977.0201 /      
        data f_C3 / 0.7570 /
        real(kind=doubleR) ::Gamma_C3(transitions_C3)
        data Gamma_C3 / 1.80000e+09 / 

        integer(kind=singleI), parameter :: transitions_C4 = 2
        real(kind=doubleR) :: Lambda_C4(transitions_C4), f_C4(transitions_C4)
        data Lambda_C4 / 1548.2041, 1550.7812 /      
        data f_C4 / 0.189900, 0.094750 /
        real(kind=doubleR) ::Gamma_C4(transitions_C4)
        data Gamma_C4 / 2.60000e+08,2.60000e+08 /

!       ------------------------------------------------------------------------
!       N  
        real(kind=doubleR), parameter :: massN  = 14.0067 * atom_munit
        integer(kind=singleI), parameter :: num_ions_N = 4
        character(len=4) :: ion_name_N(num_ions_N)
        data ion_name_N / 'N2', 'N3', 'N4', 'N5' /

        integer(kind=singleI), parameter :: transitions_N2 = 1
        real(kind=doubleR) :: Lambda_N2(transitions_N2), f_N2(transitions_N2)
        data Lambda_N2 / 1083.9937 /      
        data f_N2 / 0.111 /
        real(kind=doubleR) ::Gamma_N2(transitions_N2)
        data Gamma_N2 / 3.89000e+08 /

        integer(kind=singleI), parameter :: transitions_N3 = 1
        real(kind=doubleR) :: Lambda_N3(transitions_N3), f_N3(transitions_N3)
        data Lambda_N3 / 989.799 /      
        data f_N3 / 0.12287 /
        real(kind=doubleR) ::Gamma_N3(transitions_N3)
        data Gamma_N3 / 5.00000e+08 /

        integer(kind=singleI), parameter :: transitions_N4 = 1
        real(kind=doubleR) :: Lambda_N4(transitions_N4), f_N4(transitions_N4)
        data Lambda_N4 / 765.148 /      
        data f_N4 / 0.632 /
        real(kind=doubleR) ::Gamma_N4(transitions_N4)
        data Gamma_N4 / 5.00000e+08 /

        integer(kind=singleI), parameter :: transitions_N5 = 2
        real(kind=doubleR) :: Lambda_N5(transitions_N5), f_N5(transitions_N5)
        data Lambda_N5 / 1238.821, 1242.804 /      
        data f_N5 / 0.156000, 0.0770 /
        real(kind=doubleR) ::Gamma_N5(transitions_N5)
        data Gamma_N5 /3.40000e+08,3.40000e+08  /

!       ------------------------------------------------------------------------
!       O  
        real(kind=doubleR), parameter :: massO  = 15.9994 * atom_munit
        integer(kind=singleI), parameter :: num_ions_O = 6
        character(len=4) :: ion_name_O(num_ions_O)
        data ion_name_O / 'O1', 'O3', 'O4', 'O5', 'O6', 'O7'/

        integer(kind=singleI), parameter :: transitions_O1 = 3
        real(kind=doubleR) :: Lambda_O1(transitions_O1), f_O1(transitions_O1)
        data Lambda_O1 / 1302.1685, 988.7734, 971.7382 /
        data f_O1 / 0.048000, 0.0465, 0.0116 /
        real(kind=doubleR) ::Gamma_O1(transitions_O1)
        data Gamma_O1 / 6.11500e+08, 6.11500e+08, 6.11500e+08 /

        integer(kind=singleI), parameter :: transitions_O3 = 2
        real(kind=doubleR) :: Lambda_O3(transitions_O3), f_O3(transitions_O3)
        data Lambda_O3 / 702.332, 832.927 /
        data f_O3 / 0.126, 0.0998 /
        real(kind=doubleR) ::Gamma_O3(transitions_O3)
        data Gamma_O3 / 6.11500e+08, 6.11500e+08 /

        integer(kind=singleI), parameter :: transitions_O4 = 1
        real(kind=doubleR) :: Lambda_O4(transitions_O4), f_O4(transitions_O4)
        data Lambda_O4 / 787.711 /
        data f_O4 / 0.110 /
        real(kind=doubleR) ::Gamma_O4(transitions_O4)
        data Gamma_O4 / 6.11500e+08 /

        integer(kind=singleI), parameter :: transitions_O5 = 1
        real(kind=doubleR) :: Lambda_O5(transitions_O5), f_O5(transitions_O5)
        data Lambda_O5 / 629.730 /
        data f_O5 / 0.499 /
        real(kind=doubleR) ::Gamma_O5(transitions_O5)
        data Gamma_O5 / 6.11500e+08 /

        integer(kind=singleI), parameter :: transitions_O6 = 2
        real(kind=doubleR) :: Lambda_O6(transitions_O6), f_O6(transitions_O6)
        data Lambda_O6 / 1031.9261, 1037.6167 /      
        data f_O6 / 0.13250, 0.06580 /
        real(kind=doubleR) ::Gamma_O6(transitions_O6)
        data Gamma_O6 / 4.16000e+08, 4.09000e+08 /

        integer(kind=singleI), parameter :: transitions_O7 = 1
        real(kind=doubleR) :: Lambda_O7(transitions_O7), f_O7(transitions_O7)
        data Lambda_O7 / 21.60169 /      
        data f_O7 / 0.696 /
        real(kind=doubleR) ::Gamma_O7(transitions_O7)
        data Gamma_O7 / 6.11500e+08 /

!       ------------------------------------------------------------------------
!       Ne
        real(kind=doubleR), parameter :: massNe = 20.1797 * atom_munit
        integer(kind=singleI), parameter :: num_ions_Ne = 1
        character(len=4) :: ion_name_Ne(num_ions_Ne)
        data ion_name_Ne / 'Ne8' /

        integer(kind=singleI), parameter :: transitions_Ne8 = 2
        real(kind=doubleR) :: Lambda_Ne8(transitions_Ne8),
     +  f_Ne8(transitions_Ne8)
        data Lambda_Ne8 / 770.409, 780.324 /
        data f_Ne8 / 0.103, 0.0505 /
        real(kind=doubleR) ::Gamma_Ne8(transitions_Ne8)
        data Gamma_Ne8 / 1.00000e+08, 1.00000e+08 /

!       ------------------------------------------------------------------------
!       Mg
        real(kind=doubleR), parameter :: massMg = 24.3050 * atom_munit
        integer(kind=singleI), parameter :: num_ions_Mg = 2
        character(len=4) :: ion_name_Mg(num_ions_Mg)
        data ion_name_Mg / 'Mg2', 'MgX' /

        integer(kind=singleI), parameter :: transitions_Mg2 = 2
        real(kind=doubleR) :: Lambda_Mg2(transitions_Mg2),
     +  f_Mg2(transitions_Mg2)
        data Lambda_Mg2 / 2796.3543, 2803.5315 /
        data f_Mg2 / 0.6155, 0.3058 /
        real(kind=doubleR) :: Gamma_Mg2(transitions_Mg2)
        data Gamma_Mg2 / 2.60000e+08,2.60000e+08 /

        integer(kind=singleI), parameter :: transitions_MgX = 2
        real(kind=doubleR) :: Lambda_MgX(transitions_MgX),
     +  f_MgX(transitions_MgX)
        data Lambda_MgX / 6.09790e+02, 6.24950e+02  /
        data f_MgX / 8.42000e-02, 4.10000e-02 /
        real(kind=doubleR) :: Gamma_MgX(transitions_MgX)
        data Gamma_MgX / 2.00000e+08, 2.00000e+08 /

!       ------------------------------------------------------------------------
!       Al
        real(kind=doubleR), parameter :: massAl = 26.981538 * atom_munit
        integer(kind=singleI), parameter :: num_ions_Al = 2
        character(len=4) :: ion_name_Al(num_ions_Al)
        data ion_name_Al / 'Al2', 'Al3' /

        integer(kind=singleI), parameter :: transitions_Al2 = 1
        real(kind=doubleR) :: Lambda_Al2(transitions_Al2),
     +  f_Al2(transitions_Al2)
        data Lambda_Al2 / 1670.79 /
        data f_Al2 / 1.73812 /
        real(kind=doubleR) ::Gamma_Al2(transitions_Al2)
        data Gamma_Al2 / 1.40000e+09 /

        integer(kind=singleI), parameter :: transitions_Al3 = 2
        real(kind=doubleR) :: Lambda_Al3(transitions_Al3),
     +  f_Al3(transitions_Al3)
        data Lambda_Al3 / 1854.72, 1862.79 /
        data f_Al3 / 0.559399, 0.277866 /
        real(kind=doubleR) ::Gamma_Al3(transitions_Al3)
        data Gamma_Al3 / 5.40000e+08, 5.30000e+08 /

!       ------------------------------------------------------------------------
!       Si
        real(kind=doubleR), parameter :: massSi = 28.0855 * atom_munit
        integer(kind=singleI), parameter :: num_ions_Si = 3
        character(len=4) :: ion_name_Si(num_ions_Si)
        data ion_name_Si / 'Si2', 'Si3', 'Si4'/

        integer(kind=singleI), parameter :: transitions_Si2 = 1
        real(kind=doubleR) :: Lambda_Si2(transitions_Si2),
     +  f_Si2(transitions_Si2)
        data Lambda_Si2 / 1260.420 /      
        data f_Si2 / 1.17621 /
        real(kind=doubleR) ::Gamma_Si2(transitions_Si2)
        data Gamma_Si2 / 3.00000e+09 /

        integer(kind=singleI), parameter :: transitions_Si3 = 1
        real(kind=doubleR) :: Lambda_Si3(transitions_Si3),
     +  f_Si3(transitions_Si3)
        data Lambda_Si3 / 1206.500 /      
        data f_Si3 / 1.63 /
        real(kind=doubleR) ::Gamma_Si3(transitions_Si3)
        data Gamma_Si3 / 2.50000e+09 /

        integer(kind=singleI), parameter :: transitions_Si4 = 2
        real(kind=doubleR) :: Lambda_Si4(transitions_Si4),
     +  f_Si4(transitions_Si4) 
        data Lambda_Si4 / 1393.76018, 1402.77291 /      
        data f_Si4 / 0.513, 0.254 /
        real(kind=doubleR) ::Gamma_Si4(transitions_Si4)
        data Gamma_Si4 / 8.80000e+08, 8.60000e+08 /

!       ------------------------------------------------------------------------
!       S
        real(kind=doubleR), parameter :: massS = 32.065 * atom_munit
        integer(kind=singleI), parameter :: num_ions_S = 1
        character(len=4) :: ion_name_S(num_ions_S)
        data ion_name_S / 'S5'/

        integer(kind=singleI), parameter :: transitions_S5 = 1
        real(kind=doubleR) :: Lambda_S5(transitions_S5), f_S5(transitions_S5)
        data Lambda_S5 / 786.48 /      
        data f_S5 / 1.46 /
        real(kind=doubleR) ::Gamma_S5(transitions_S5)
        data Gamma_S5 / 1.70000e+09 /

!       ------------------------------------------------------------------------
!       Fe
!       Note added by TTG: Fe2 transitions are NOT in decreasing f_osc value!

        real(kind=doubleR), parameter :: massFe = 55.8452 * atom_munit
        integer(kind=singleI), parameter :: num_ions_Fe = 2
        character(len=4) :: ion_name_Fe(num_ions_Fe)
        data ion_name_Fe / 'Fe2', 'Fe3' /

        integer(kind=singleI), parameter :: transitions_Fe2 = 9
        real(kind=doubleR) :: Lambda_Fe2(transitions_Fe2),
     +  f_Fe2(transitions_Fe2)
        data Lambda_Fe2 / 1144.9379, 1608.45085, 1063.1764, 1096.8769, 
     +  1260.533, 1121.9748, 1081.8748, 1143.2260, 
     +  1125.4477 /      
        data f_Fe2 / 0.083, 0.0577, 0.0547, 0.032700, 
     +  0.024000, 0.0290, 0.012600, 0.0192,
     +  0.0156 /
        real(kind=doubleR) ::Gamma_Fe2(transitions_Fe2)
        data Gamma_Fe2 / 3.10000e+08, 2.30000e+08,
     &  3.10000e+08, 3.10000e+08, 3.10000e+08,  3.10000e+08, 3.10000e+08, 
     &  3.10000e+08, 3.10000e+08 /

        integer(kind=singleI), parameter :: transitions_Fe3 = 1
        real(kind=doubleR) :: Lambda_Fe3(transitions_Fe3),
     +  f_Fe3(transitions_Fe3)
        data Lambda_Fe3 / 1122.52 /      
        data f_Fe3 / 0.0544257 /
        real(kind=doubleR) ::Gamma_Fe3(transitions_Fe3)
        data Gamma_Fe3 / 3.10000e+08 /
  
  
        end module atomdata
!       ========================================================================

!       ========================================================================
        module subs_funcs

        use set_precision

!       ------------------------------------------------------------------------
        interface load_ion_matrix

         subroutine load_ion_matrix(atomname,numatoms,ionname,numions)

         use set_precision
         use derived_types
         use atomdata

         implicit none

         character(len=2) :: atomname(numatoms)

         integer(kind=singleI) :: i
         integer(kind=singleI), intent(in) :: numatoms
         integer(kind=singleI), intent(in) :: numions(numatoms)
         character(len=4), allocatable, intent(inout) :: ionname(:,:)
         character(len=4), allocatable :: dummy(:)

         end subroutine load_ion_matrix

        end interface load_ion_matrix
!       ------------------------------------------------------------------------

        end module subs_funcs
!       ========================================================================

!       ------------------------------------------------------------------------
        program atomic_data
!       ------------------------------------------------------------------------

        use set_precision
        use derived_types
        use atomdata
        use subs_funcs

        implicit none

        integer(kind=singleI) :: i, j
        integer(kind=singleI) :: count_atoms, count_ions

!       change all these when adding atoms (and or ions / transitions)

        integer(kind=singleI), parameter :: num_atoms=11
        character(len=2) :: atom_name(num_atoms)
        real(kind=doubleR) :: atom_mass(num_atoms)

        integer(kind=singleI) :: num_ions(num_atoms)
        character(len=4), allocatable :: ion_name(:,:)

!       OBSERVE the input order of the following vectors:

        data atom_name / 'H', 'He', 'C', 'N', 'O', 'Ne', 'Mg', 'Al', 'Si',
     +   'S', 'Fe' /
        data atom_mass / massH, massHe, massC, massN, massO, massNe, massMg,
     +  massAl, massSi, massS, massFe /

        data num_ions / num_ions_H, num_ions_He, num_ions_C, num_ions_N,
     +  num_ions_O, num_ions_Ne, num_ions_Mg, num_ions_Al, num_ions_Si,
     +  num_ions_S, num_ions_Fe /

        type(atom) :: atoms(num_atoms)

!       ------------------------------------------------------------------------
!       the following have to be edited by hand:
!       ------------------------------------------------------------------------
!       spread atoms and corresponding ions into matrix `ion_name'

        call load_ion_matrix(atom_name,num_atoms,ion_name,num_ions)

        DO i=1,num_atoms

         atoms(i)%name = atom_name(i)
         atoms(i)%mass = atom_mass(i)
         atoms(i)%num_ions = num_ions(i)
         allocate(atoms(i)%ion(atoms(i)%num_ions))

         DO j=1,num_ions(i)

          print*, ion_name(i,j+1)

         END DO ! over ions

        END DO ! over atoms

!       ------------------------------------------------------------------------
!       output

!       ------------------------------------------------------------------------
        end program atomic_data
!       ------------------------------------------------------------------------


!       ------------------------------------------------------------------------
!       Subroutines and functions
!       ------------------------------------------------------------------------

!       ========================================================================
        subroutine load_ion_matrix(atomname,numatoms,ionname,numions)

        use set_precision
        use derived_types
        use atomdata

        implicit none

        character(len=2) :: atomname(numatoms)

        integer(kind=singleI) :: i
        integer(kind=singleI), intent(in) :: numatoms
        integer(kind=singleI), intent(in) :: numions(numatoms)
        character(len=4), allocatable, intent(inout) :: ionname(:,:)
        character(len=4), allocatable :: dummy(:)

        allocate(ionname(numatoms,1+maxval(numions(1:numatoms))))
        allocate(dummy(maxval(numions(1:numatoms))))
        ionname(:,1) = atomname(:)

        DO i=1,numatoms

        select case(atomname(i))

        case ('H')

!        expand array ion_name_H with intrinsic PACK

         dummy = pack(ion_name_H, .true., dummy)
         ionname(i,2:1+maxval(numions(1:numatoms))) = dummy(:)
!        restore
         dummy = ''

        case ('He')

         dummy = pack(ion_name_He, .true., dummy)
         ionname(i,2:1+maxval(numions(1:numatoms))) = dummy(:)
         dummy = ''

        case ('C')

         dummy = pack(ion_name_C, .true., dummy)
         ionname(i,2:1+maxval(numions(1:numatoms))) = dummy(:)
         dummy = ''

        case ('N')

         dummy = pack(ion_name_N, .true., dummy)
         ionname(i,2:1+maxval(numions(1:numatoms))) = dummy(:)
         dummy = ''

        case ('O')

         dummy = pack(ion_name_O, .true., dummy)
         ionname(i,2:1+maxval(numions(1:numatoms))) = dummy(:)
         dummy = ''

        case ('Ne')

         dummy = pack(ion_name_Ne, .true., dummy)
         ionname(i,2:1+maxval(numions(1:numatoms))) = dummy(:)
         dummy = ''

        case ('Mg')

         dummy = pack(ion_name_Mg, .true., dummy)
         ionname(i,2:1+maxval(numions(1:numatoms))) = dummy(:)
         dummy = ''

        case ('Al')
         dummy = pack(ion_name_Al, .true., dummy)
         ionname(i,2:1+maxval(numions(1:numatoms))) = dummy(:)
         dummy = ''

        case ('Si')

         dummy = pack(ion_name_Si, .true., dummy)
         ionname(i,2:1+maxval(numions(1:numatoms))) = dummy(:)
         dummy = ''

        case ('S')

         dummy = pack(ion_name_S, .true., dummy)
         ionname(i,2:1+maxval(numions(1:numatoms))) = dummy(:)
         dummy = ''

        case ('Fe')

         dummy = pack(ion_name_Fe, .true., dummy)
         ionname(i,2:1+maxval(numions(1:numatoms))) = dummy(:)
         dummy = ''

        case default
         write(6,*) 'ERROR: uknown atom: '//trim(atomname(i))
         stop 1

        end select

        print*, ionname(i,:)

        END DO ! over atoms


        end subroutine load_ion_matrix
!       ========================================================================
