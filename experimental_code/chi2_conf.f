!       ========================================================================
        module cum_chi2_dist
        
         integer :: deg_of_free
         double precision :: input_chi2
        
        end module cum_chi2_dist
!       ========================================================================
        
!       ========================================================================
        program chi2_conf
        
!       returns the cumulative probability of a chi^2 distribution of n degrees
!       of freedom

!       the cumulative chi^2 distribution of n degrees of freedom F(n,x) is
!       given by the incomplete gamma function P(n/2,x/2) (function gammp)


!       input parameters:
!       n = degrees of freedom
!       chi2 = chi2-value

        use cum_chi2_dist

        implicit none
        
        integer :: argcount
        double precision :: gammp, gammq, qval, pval, reduced_std_dev, skewness
        character(len=10) :: args
        
!       ------------------------------------------------------------------------
!       input arguments

        argcount = IArgC()
        if (argcount.ne.2) then
         write(6,*) 'USAGE: chi2_conf <degrees of freedom> '//
     &   '<chi2>'        
         stop 1
        end if

        call getarg(1,args)
        read(args,'(i8)') deg_of_free
        call getarg(2,args)
        read(args,'(f14.12)') input_chi2

!       ------------------------------------------------------------------------

!       compute cumulative probability, complement, standard deviation and
!       skewness
!
!       note that std_dev = sqrt(2 * dof) and reduced_std_dev = std_dev / dof

        pval = gammp(5.0d-1*dble(deg_of_free),5.0d-1*input_chi2)
        qval = gammq(5.0d-1*dble(deg_of_free),5.0d-1*input_chi2)
        reduced_std_dev = dsqrt(2.0d0/ deg_of_free)
        skewness =  dsqrt(8.0d0 / deg_of_free)

!       consistency check
        if ((qval+pval).ne.1.0d0) then
         write(6,*) 'WARNING: Non-complementary Q- and P-values!'
        end if

        write(6,'(a)')
     &  'P-value   Q-value      DoF     CHI^2    REDUCED CHI^2'//
     &  '  RED.STD.DEV.    SKEW'
        write(6,'(2(f8.6,2x),i5,4(3x,f10.4))')
     &  pval, qval, deg_of_free, input_chi2, input_chi2/deg_of_free ,
     &  reduced_std_dev, skewness 

        end program chi2_conf
!       ========================================================================

!       ========================================================================
!       Numerical Recipes
!       ========================================================================

!       ========================================================================
        function gammp(a,x)

        implicit none
         
        double precision :: a,gammp,x
CU      USES gcf,gser
        double precision :: gammcf,gamser,gln
        if (x.lt.0.0d0.or.a.le.0.0d0) then
         write(6,*) 'bad arguments in gammp'
         stop 1
        end if
        if (x.lt.a+1.0d0) then
         call gser(gamser,a,x,gln)
         gammp=gamser
        else
         call gcf(gammcf,a,x,gln)
         gammp=1.0d0-gammcf
        endif
        return
        END
!       ========================================================================

!       ========================================================================
        function gammq(a,x)

        implicit none
         
        double precision :: a,gammq,x
CU      USES gcf,gser
        double precision :: gammcf,gamser,gln

        if(x.lt.0..or.a.le.0.) then
         write(6,*) 'bad arguments in gammq'
         stop 1
        end if
        if(x.lt.a+1.0d0)then
         call gser(gamser,a,x,gln)
         gammq=1.0d0-gamser
        else
         call gcf(gammcf,a,x,gln)
         gammq=gammcf
        endif
        return
        END
!       ========================================================================


!       ========================================================================
        SUBROUTINE gser(gamser,a,x,gln)

        implicit none
         
        double precision :: a,gamser,gln,x
        double precision, parameter :: EPS=1.0d-20
        integer, parameter :: ITMAX=1000
CU      USES gammln
        integer :: n
        double precision :: ap,del,sum,gammln
        gln=gammln(a)
        if(x.le.0.0d0)then
         if(x.lt.0.0d0) then
          write(6,*) 'x < 0 in gser'
          stop 1
         end if
         gamser=0.0d0
         return
        endif
        ap=a
        sum=1.0d0/a
        del=sum
        do 11 n=1,ITMAX
         ap=ap+1.0d0
         del=del*x/ap
         sum=sum+del
         if(abs(del).lt.abs(sum)*EPS)goto 1
11      continue
        write(6,*) 'gser: a too large, ITMAX too small!'
        stop 1
1       continue
!       avoid underflow of gamser
        if ((dlog(sum)-x+a*dlog(x)-gln).lt.dlog(tiny(1.0d0))) then
         write(6,*) 'gser: too small argument in dexp!'
         stop 1
        end if
!       avoid overflow of gamser
        if ((dlog(sum)-x+a*dlog(x)-gln).gt.dlog(huge(1.0d0))) then
         write(6,*) 'gser: too big argument in dexp!'
         stop 1
        end if
        
        gamser=sum*dexp(-x+a*dlog(x)-gln)
        return
        END
!       ========================================================================

!       ========================================================================
        SUBROUTINE gcf(gammcf,a,x,gln)

        implicit none
         
        double precision :: a,gammcf,gln,x
        double precision, parameter :: EPS=1.0d-20,
     &  FPMIN=dble(tiny(1.0e0))
        integer, parameter :: ITMAX=1000
CU      USES gammln
        integer :: i
        double precision :: an,b,c,d,del,h,gammln
        gln=gammln(a)
        b=x+1.0d0-a
        c=1.0d0/FPMIN
        d=1.0d0/b
        h=d
        do 11 i=1,ITMAX
         an=-i*(i-a)
         b=b+2.0d0
         d=an*d+b
         if(abs(d).lt.FPMIN)d=FPMIN
         c=b+an/c
         if(abs(c).lt.FPMIN)c=FPMIN
         d=1.0d0/d
         del=d*c
         h=h*del
         if(abs(del-1.0d0).lt.EPS)goto 1
11      continue
        write(6,*) 'gcf: a too large, ITMAX too small!'
        stop 1
1       continue
!       avoid underflow of gamser
        if ((dlog(h)-x+a*dlog(x)-gln).lt.dlog(tiny(1.0d0))) then
         write(6,*) 'gcf: too small argument in dexp!'
         stop 1
        end if
!       avoid overflow of gamser
        if ((dlog(h)-x+a*dlog(x)-gln).gt.dlog(huge(1.0d0))) then
         write(6,*) 'gcf: too big argument in dexp!'
         stop 1
        end if

        gammcf=dexp(-x+a*dlog(x)-gln)*h
        return
        END
!       ========================================================================

!       ========================================================================
        FUNCTION gammln(xx)

        implicit none
         
        double precision :: gammln,xx
        integer :: j
        double precision :: ser,stp,tmp,x,y,cof(6)
        SAVE cof,stp
        DATA cof,stp/76.18009172947146d0,-86.50532032941677d0,
     &  24.01409824083091d0,-1.231739572450155d0,0.1208650973866179d-2,
     &  -0.5395239384953d-5,2.5066282746310005d0/
        x=xx
        y=x
        tmp=x+5.5d0
        tmp=(x+5.0d-1)*dlog(tmp)-tmp
        ser=1.000000000190015d0
        do 11 j=1,6
         y=y+1.0d0
         ser=ser+cof(j)/y
11      continue
        gammln=tmp+dlog(stp*ser/x)
        return
        END
!       ========================================================================
