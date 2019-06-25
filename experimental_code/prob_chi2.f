!       ========================================================================
        module cum_chi2_dist
        
         integer :: deg_of_free
         double precision :: input_probability
        
        end module cum_chi2_dist
!       ========================================================================
        
!       ========================================================================
        program prob_chi2
        
!       returns the value of the cumulative chi^2 distribution of n degrees of
!       freedom above a given probability

!       the cumulative chi^2 distribution of n degrees of freedom F(n,x) is
!       given by the incomplete gamma function P(n/2,x/2) (function gammp)

!       this programs computes the root of the function
!       
!       red_gamma(x) = P(n/2,x/2) - p = 0
!
!       using the bisection method within an accuracy x_acc, assuming
!       the root to lie between x=0.0d0 and x=1.5n; the last value
!       corresponds to 1.5x the median (n) of the chi^2 distribution;
!       note that its variance (std deviation squared) is 2n


!       input parameters:
!       n = degrees of freedom
!       p = cumulative probability

        use cum_chi2_dist

        implicit none
        
        integer :: argcount
        double precision :: gammq, gammp
        character(len=10) :: args
        
        double precision :: reduced_std_dev, skewness
        double precision :: chi2_rtbis, chi2_zbrent
        double precision :: red_gamma
        double precision :: x_lo, x_hi
        double precision :: rtbis, zbrent

        double precision, parameter :: x_acc = 1.0d-10
        
        logical :: increase_hibound

        external :: red_gamma

!       ------------------------------------------------------------------------
!       input arguments

        argcount = IArgC()
        if (argcount.ne.2) then
         write(6,*) 'USAGE: prob_chi2 <degrees of freedom> '//
     &   '<confidence>'        
         stop 1
        end if

        call getarg(1,args)
        read(args,'(i8)') deg_of_free
        call getarg(2,args)
        read(args,'(f14.12)') input_probability

!       ------------------------------------------------------------------------
!       initalise flag to define look-up range; the reason is that the
!       (arbitrarily) chosen lower and upper bounds defining the look-up range
!       might not enclose the root; also, choosing a too high upper bound sig-
!       nificantly (and unnecesarily) increases the computation time; hence,
!       modify root-finding routines (rtbis, zbrent) to signal when the root is
!       not bracketed and increase the upper bound by an arbitrary factor (2
!       in this case)

        increase_hibound = .true.

!       define lower and upper bound of range to look for root
        x_lo = 0.0d0
        x_hi = 1.5d0*dble(deg_of_free)
        
        DO WHILE (increase_hibound)

        increase_hibound = .false.

!       check that complementary incomplete gamma functions give the expected
!       answer
        
        if (
     &  abs(gammq(5.0d-1*dble(deg_of_free),5.0d-1*x_lo) +
     &  gammp(5.0d-1*dble(deg_of_free),5.0d-1*x_lo) - 1.0d0)
     &  .gt.x_acc)
     &  write(6,*) 'possible loss of accuracy!'

        if (
     &  abs(gammq(5.0d-1*dble(deg_of_free),5.0d-1*x_hi) +
     &  gammp(5.0d-1*dble(deg_of_free),5.0d-1*x_hi) - 1.0d0)
     &  .gt.x_acc)
     &  write(6,*) 'possible loss of accuracy!'
        
!       compute chi2-value for corresponding probability       
        chi2_rtbis = rtbis(red_gamma,x_lo,x_hi,x_acc)

!       compute chi2-value for corresponding probability       
        chi2_zbrent = zbrent(red_gamma,x_lo,x_hi,x_acc)

        if ((chi2_rtbis.eq.-123.456).or.(chi2_zbrent.eq.-123.456)) then
!       reset flag
         increase_hibound = .true.
!       increase upper bound by a factor 2        
         x_hi = x_hi * 2.0d0
        end if

        END DO ! while

!       compute reduced standard deviation and skewness
        reduced_std_dev = dsqrt(2.0d0/deg_of_free)
        skewness = 2.0d0 * reduced_std_dev

        write(6,'(a)')
     &  'PROBABILITY        DoF     CHI^2    REDUCED CHI^2    RED.STD.DEV.    '
     &  //'SKEWNESS'
        write(6,'(f10.8,5x,i7,4(3x,f10.4))')
     &  input_probability, deg_of_free, chi2_rtbis, chi2_rtbis/deg_of_free,
     &  reduced_std_dev, skewness 

        write(6,'(f10.8,5x,i7,4(3x,f10.4))')
     &  input_probability, deg_of_free, chi2_zbrent, chi2_zbrent/deg_of_free ,
     &  reduced_std_dev, skewness 

        end program prob_chi2
!       ========================================================================

!       ========================================================================
        function red_gamma(x)
        
        use cum_chi2_dist
        
        implicit none
         
        double precision :: red_gamma, x
        double precision :: gammp
        
!       define function
        red_gamma = 0.0d0

        red_gamma = gammp(5.0d-1*dble(deg_of_free),5.0d-1*x) - input_probability

        return
        
        end function red_gamma
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

        end function gammp
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

        end function gammq
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
!       avoid underflow of gamser (edited by TTG)
        if ((dlog(sum)-x+a*dlog(x)-gln).lt.dlog(tiny(1.0d0))) then
         write(6,*)
         write(6,*) 'gser: too small gamser! Will set gamser to 0.0d0'
         write(6,*)
         gamser=0.0d0
         return
!         stop 1
        end if
!       avoid overflow of gamser (edited by TTG)
        if ((dlog(sum)-x+a*dlog(x)-gln).gt.dlog(huge(1.0d0))) then
         write(6,*) 'gser: too big argument in dexp!'
         stop 1
        end if
        
        gamser=sum*dexp(-x+a*dlog(x)-gln)
        return

        END SUBROUTINE gser
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
!       avoid underflow of gammcf
        if ((dlog(h)-x+a*dlog(x)-gln).lt.dlog(tiny(1.0d0))) then
         write(6,*)
         write(6,*) 'gcf: too small gammcf!'
         write(6,*)
         stop 1
        end if
!       avoid overflow of gammcf
        if ((dlog(h)-x+a*dlog(x)-gln).gt.dlog(huge(1.0d0))) then
         write(6,*) 'gcf: too big argument in dexp!'
         stop 1
        end if

        gammcf=dexp(-x+a*dlog(x)-gln)*h
        return

        END SUBROUTINE gcf
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

        END FUNCTION gammln
!       ========================================================================

!       ========================================================================
        function rtbis(func,x1,x2,xacc)

        implicit none
         
        integer, parameter :: JMAX=100
        double precision :: rtbis,x1,x2,xacc,func
        external func
        integer :: j
        double precision :: dx,f,fmid,xmid
        rtbis = -123.456 ! introduced by TTG (2014)
        fmid=func(x2)
        f=func(x1)
        if (f*fmid.ge.0.0d0) then
!         write(6,*) 'root must be bracketed in rtbis' ! commented out by TTG (2014)
         return ! introduced by TTG (2014)
!         stop 1 ! commented out by TTG (2014)
        end if
        if (f.lt.0.0d0) then
         rtbis=x1
         dx=x2-x1
        else
         rtbis=x2
         dx=x1-x2
        endif
        do 11 j=1,JMAX
         dx=dx*5.0d-1
         xmid=rtbis+dx
         fmid=func(xmid)
         if(fmid.le.0.)rtbis=xmid
         if(abs(dx).lt.xacc .or. fmid.eq.0.) return
11      continue
        
        write(6,*) 'too many bisections in rtbis'
        
        end function rtbis
!       ========================================================================

!       ========================================================================
        FUNCTION zbrent(func,x1,x2,tol)
        integer, parameter :: ITMAX=100
        double precision, parameter :: EPS=3.0d-8
        double precision :: zbrent,tol,x1,x2,func
        EXTERNAL func
        integer :: iter
        double precision :: a,b,c,d,e,fa,fb,fc,p,q,r,s,tol1,xm
        
        zbrent = -123.456 ! introduced by TTG (2014)
        a=x1
        b=x2
        fa=func(a)
        fb=func(b)
        e=0.0d0
        if ((fa.gt.0.0d0.and.fb.gt.0.0d0).or.(fa.lt.0.0d0.and.fb.lt.0.0d0)) then
!         write(6,*) 'root must be bracketed for zbrent' ! commented out by TTG (2014)
         return ! introduced by TTG (2014)
!         stop 1 ! commented out by TTG (2014)
        end if
        c=b
        fc=fb
        do 11 iter=1,ITMAX
          if((fb.gt.0.0d0.and.fc.gt.0.0d0).or.(fb.lt.0.0d0.and.fc.lt.0.0d0))then
            c=a
            fc=fa
            d=b-a
            e=d
          endif
          if(abs(fc).lt.abs(fb)) then
            a=b
            b=c
            c=a
            fa=fb
            fb=fc
            fc=fa
          endif
          tol1=2.0d0*EPS*abs(b)+0.5d0*tol
          xm=0.5d0*(c-b)
          if(abs(xm).le.tol1 .or. fb.eq.0.0d0)then
            zbrent=b
            return
          endif
          if(abs(e).ge.tol1 .and. abs(fa).gt.abs(fb)) then
            s=fb/fa
            if(a.eq.c) then
                p=2.0d0*xm*s
                q=1.0d0-s
            else
                q=fa/fc
                r=fb/fc
                p=s*(2.0d0*xm*q*(q-r)-(b-a)*(r-1.0d0))
                q=(q-1.0d0)*(r-1.0d0)*(s-1.0d0)
            endif
            if(p.gt.0.0d0) q=-q
            p=abs(p)
            if(2.0d0*p .lt. min(3.0d0*xm*q-abs(tol1*q),abs(e*q))) then
                e=d
                d=p/q
            else
                d=xm
                e=d
            endif
          else
            d=xm
            e=d
          endif
          a=b
          fa=fb
          if(abs(d) .gt. tol1) then
            b=b+d
          else
            b=b+sign(tol1,xm)
          endif
          fb=func(b)
11      continue
        write(6,*) 'zbrent exceeding maximum iterations'
        zbrent=b
        return

        END FUNCTION zbrent
!       ========================================================================
