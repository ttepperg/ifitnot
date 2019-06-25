        program test_numerics
        
        
        real*4 :: dummy_r4 = 1.234567890e-24
        real*8 :: dummy_r8 = 1.234567890e-28
        double precision :: dummy_d = 1.234567890d-100
	real (kind=8) :: dummy_rk8
        
        print*, 'smalles | largest representable number'
        print*, tiny(dummy_r4), huge(dummy_r4)
        print*, tiny(dummy_r8), huge(dummy_r8)
        print*, tiny(dummy_d), huge(dummy_d)
        print*, tiny(dummy_rk8), huge(dummy_rk8)
        
        print*, 'exponent'
        print*, exponent(dummy_r4)
        print*, exponent(dummy_r8)
        print*, exponent(dummy_d)
        
        print*, 'fraction'
        print*, fraction(dummy_r4)
        print*, fraction(dummy_r8)
        print*, fraction(dummy_d)

        print*
        print*, fraction(dummy_r4)*2.0d0**(exponent(dummy_r4))
        print*, fraction(dummy_r8)*2.0d0**(exponent(dummy_r8))
        print*, fraction(dummy_d)*2.0d0**(exponent(dummy_d))
        
!        print*, -log(tiny(1.0d0))
!        print*, -log(tiny(1.0e0))
        
        print*, 'decimal precision'
        print*, precision(dummy_r4)
        print*, precision(dummy_r8)
        print*, precision(dummy_d)
        print*, precision(dummy_rk8)

        end program test_numerics
