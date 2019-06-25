        program test_derived_type
        
        implicit none

        integer :: i, j
        integer, parameter :: counts=huge(1)

        type absline
         sequence
         integer :: line_id
         double precision :: col_dens, vel_c, b_value
        end type absline

        type single_t
         sequence
         double precision :: tau_0, lambda_0, f_osc, big_gamma, gamma_over_nu
        end type single_t

        type ion
         sequence
         character(len=4) :: name
         double precision :: mass
         integer :: num_transitions
         type(single_t), allocatable :: transition(:)
         type(absline), allocatable :: component(:)
        end type ion

        type(ion), allocatable :: IONS(:)

!        allocate(components(2))

!        components(:)%line_id = -1
!        components(1)%line_id = 1
!        components(2)%line_id = 2

!        print*, components(1)
!        print*, components(2)

         allocate(IONS(1))
         IONS(1)%name = 'o6'
         IONS(1)%num_transitions = 2
         allocate(IONS(1)%transition(IONS(1)%num_transitions))
         IONS(1)%transition(1)%lambda_0 = 1032
         IONS(1)%transition(2)%lambda_0 = 1038

         allocate(IONS(1)%component(4))

         do i=1,counts
          do j=1,counts
          end do
         end do

        end program test_derived_type
