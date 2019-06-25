!       (C) Thorsten Tepper Garcia 2004-05
!       ----------------------------------------------------------------
!       Subroutines & Functions
!       ----------------------------------------------------------------
        
        subroutine read_parameter
!       ----------------------------------------------------------------
!       1. Declaring variables
!       ----------------------------------------------------------------
        use variables

        implicit none

        integer :: argcount
        character*64 dummy

!       ----------------------------------------------------------------
!       2. Program start: Getting arguments
!       ----------------------------------------------------------------
        argcount = IArgC()
        if (argcount .ne. 3) then
         write(6,*) 'Computes the equivalent width [mA] of an absorption'
     &   //' line for a given transition, as a function of column density'
     &   //' and b-value. A list of the available transitions is given'
     &   //' inions.dat'
         write(6,*)
         print *,'USAGE: eqw/eqw_gauss <transition> <log col_dens> <b-value>'
         stop 1
        end if
        
        call GetArg(1,ion_name)

        call GetArg(2,dummy)
        read (dummy,'(f5.0)') log_col_dens
        
        call GetArg(3,dummy)
        read (dummy,'(f6.0)') b_value

!       ----------------------------------------------------------------
        return
        end subroutine

!       ================================================================

!       ========================================================================
        subroutine load_ion(ionname)

!       BEWARE: the atomic data thus loaded might not be consistent with
!       abs_line_profile and ifitnot!

        use variables
        
        implicit none

        character(len=10), intent(in) :: ionname

!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        select case(ionname)

!       ------------------------------------------------------------------------
!       parameters for neutral hydrogen atom (HI); Lyman-Alpha ONLY (for now!)
         case('h1')

          lambda_0 = 1215.6701d0
          fvalue = 0.416400d0
          gamma = 6.26500d+08

!       ------------------------------------------------------------------------
!       parameters for five-times ionised oxygen atom (OVI);
!       strongest transition ONLY (for now!)
         case('o6')

          lambda_0 = 1031.9261d0
          fvalue = 0.132541d0
          gamma = 4.10000d+08

         case default
          write(6,*) 'ERROR: Non-valid ion: '//trim(ionname)
          stop 1

        end select
!       ------------------------------------------------------------------------

!       ------------------------------------------------------------------------
        return

        end subroutine load_ion
!       ========================================================================

!       ================================================================

!       ================================================================
!       ----------------------------------------------------------------
!       Calculation of the equivalent width
!       ----------------------------------------------------------------
        function eqwidth(arr,arr2,points)

!       Calculates observed equivalent width by direct integration given
!       a flux arr(points) and a wavelength baseline arr2(points).
!       'arr2' HAS THE SAME RANK (IPOINTS) AS 'alambda'!

        use variables

        implicit none
         
        integer :: points, i
        double precision :: arr(IPOINTS),arr2(IPOINTS),func(IPOINTS),
     +  eqwidth

        eqwidth = 0.0D0

!       Integrating 1-flux in wavelength 
        
        do i =1,points
         
         func(i) = 1 - arr(i)
         eqwidth = eqwidth + 0.5D0*(func(i+1) + func(i))
     1   *(arr2(i+1) - arr2(i))

        end do

        return

!       ----------------------------------------------------------------
        end function
!       ----------------------------------------------------------------
!       ================================================================

c MM 16/12/99 This is a subroutine given to me by Michael Murphy, who got
c ir from Alberto Fernandez-Soto to integrate a function of one variable.
      subroutine INTEGRATE(FUN,A,B,RELERR,RESULT)

      implicit none

      double precision :: FUN,A,B,ABSERR,RELERR,RESULT,ERREST,FLAG
      integer :: NOFUN

************************************************************************
* ESTIMA LA INTEGRAL DE FUN(X) DESDE A HASTA B
************************************************************************
      double precision :: W0,W1,W2,W3,W4,AREA,X0,F0,STONE,STEP,COR11,TEMP
      double precision :: QPREV,QNOW,QDIFF,QLEFT,ESTERR,TOLERR
      double precision :: QRIGHT(31),F(16),X(16),FSAVE(8,30),XSAVE(8,30)
      integer :: LEVMIN,LEVMAX,LEVOUT,NOMAX,NOFIN,LEV,NIM,I,J
      external :: FUN
****************   INICIACION GENERAL. CONSTANTES  ********
      ABSERR=0.D0
      LEVMIN=1
      LEVMAX=30
      LEVOUT=6
      NOMAX=5000
      NOFIN=NOMAX-8*(LEVMAX-LEVOUT+2**(LEVOUT+1))

      W0=3956.D0/14175.D0
      W1=23552.D0/14175.D0
      W2=(-3712.D0)/14175.D0
      W3=41984.D0/14175.D0
      W4=(-18160.D0)/14175.D0


      FLAG=0.D0
      RESULT=0.D0
      COR11=0.D0
      ERREST=0.D0
      AREA=0.D0
      NOFUN=0
      IF(A.EQ.B)RETURN

*****************    INICIALIZACION PARA EL PRIMER INTERVALO
      LEV=0
      NIM=1
      X0=A
      X(16)=B
      QPREV=0.D0
      F0=FUN(X0)
      STONE=(B-A)/16.D0
      X(8)=(X0+X(16))/2.D0
      X(4)=(X0+X(8))/2.D0
      X(12)=(X(8)+X(16))/2.D0
      X(2)=(X0+X(4))/2.D0
      X(6)=(X(4)+X(8))/2.D0
      X(10)=(X(8)+X(12))/2.D0
      X(14)=(X(12)+X(16))/2.D0
      DO 25 J=2,16,2
          F(J)=FUN(X(J))
   25 CONTINUE
      NOFUN=9

********************     CALCULO CENTRAL
   30 X(1)=(X0+X(2))/2.D0
      F(1)=FUN(X(1))
      DO 35 J=3,15,2
          X(J)=(X(J-1)+X(J+1))/2.D0
          F(J)=FUN(X(J))
  35  CONTINUE
      NOFUN=NOFUN+8
      STEP=(X(16)-X0)/16.D0
      QLEFT=(W0*(F0+F(8)) + W1*(F(1)+F(7)) + W2*(F(2)+F(6)) +
     +      W3*(F(3)+F(5)) + W4*F(4))*STEP
      QRIGHT(LEV+1)=(W0*(F(8)+F(16))+W1*(F(9)+F(15))+W2*(F(10)+F(14))+
     +      W3*(F(11)+F(13)) + W4*F(12))*STEP
      QNOW=QLEFT+QRIGHT(LEV+1)
      QDIFF=QNOW-QPREV
      AREA=AREA+QDIFF

********************   TEST DE CONVERGENCIA DE INTERVALOS
      ESTERR=ABS(QDIFF)/1023.D0
      TOLERR=MAX(ABSERR,RELERR*ABS(AREA))*(STEP/STONE)
      IF(LEV.LT.LEVMIN) GOTO 50
      IF(LEV.GE.LEVMAX) GOTO 62
      IF(NOFUN.GT.NOFIN) GOTO 60
      IF(ESTERR.LE.TOLERR) GOTO 70

*****************  NO HAY CONVERGENCIA. LOCALIZA NUEVO INTERVALO
   50 NIM=2*NIM
      LEV=LEV+1

*****************  ALMACENA LOS ELEMENTOS PARA UN FUTURO USO
      DO 52 I=1,8
          FSAVE(I,LEV)=F(I+8)
          XSAVE(I,LEV)=X(I+8)
   52 CONTINUE

      QPREV=QLEFT
      DO 55 I=1,8
          J=-I
          F(2*J+18)=F(J+9)
          X(2*J+18)=X(J+9)
   55 CONTINUE
      GOTO 30

*********************  EL NUMERO DE LLAMADAS A FUN CASI EXCESIVO
   60 NOFIN=2*NOFIN
      LEVMAX=LEVOUT
      FLAG=FLAG+(B-X0)/(B-A)
      GOTO 70

   62 FLAG=FLAG+1.D0

********************* EL INTERVALO CONVERGE.SUMA LAS CONTRIBUCIONES
   70 RESULT=RESULT+QNOW
      ERREST=ERREST+ESTERR
      COR11=COR11+QDIFF/1023.D0

************** LOCALIZA UN NUEVO INTERVALO
   72 IF(NIM.EQ.2*(NIM/2)) GOTO 75
      NIM=NIM/2
      LEV=LEV-1
      GOTO 72
   75 NIM=NIM+1
      IF(LEV.LE.0) GOTO 80

      QPREV=QRIGHT(LEV)
      X0=X(16)
      F0=F(16)
      DO 78 I=1,8
         F(2*I)=FSAVE(I,LEV)
         X(2*I)=XSAVE(I,LEV)
   78 CONTINUE
      GOTO 30

**************   FINALIZACION
   80 RESULT=RESULT+COR11

      IF (ERREST.EQ.0.D0) RETURN
   82 TEMP=ABS(RESULT)+ERREST
      IF(TEMP.NE.ABS(RESULT)) RETURN
      ERREST=2.D0*ERREST
      GOTO 82
      end subroutine INTEGRATE
      
      subroutine lininterp2(x1,x2,y)
      integer :: j1,j2 
      double precision :: x1,x2,y,x1a(10),x2a(55),ya(10,55),dydx1,dydx2
      common/struc/ya,x2a,x1a
     
      CALL locate(x1a,10,x1,j1)
      CALL locate(x2a,55,x2,j2)

      dydx1=(ya(j1+1,j2)-ya(j1,j2))/(x1a(j1+1)-x1a(j1))
      dydx2=(ya(j1,j2+1)-ya(j1,j2))/(x2a(j2+1)-x2a(j2))  

      y = ya(j1,j2)+dydx1*(x1-x1a(j1))+dydx2*(x2-x2a(j2))

      return
      end subroutine lininterp2

      subroutine locate(xx,n,x,j)
      integer :: j,n
      double precision :: x,xx(n)
      integer :: jl,jm,ju
      jl=0
      ju=n+1
10    if(ju-jl.gt.1)then
        jm=(ju+jl)/2
        if((xx(n).gt.xx(1)).eqv.(x.gt.xx(jm)))then
          jl=jm
        else
          ju=jm
        endif
      goto 10
      endif
      j=jl
      return
      end subroutine locate


      subroutine loadsttab()
      double precision tgam(10),tka(55),ss(10,55)
      integer IGL
      parameter (IGL=80)
      double precision z2(IGL),z3(IGL),z4(IGL),wg2(IGL),wg3(IGL),wg4(IGL)
      common/struc/ss,tka,tgam
      common/gaulag/z2,z3,z4,wg2,wg3,wg4
      
      open(unit=7,file='struct.tab')
      read(7,*)(tgam(i),i=1,10)
      read(7,*)(tka(i),i=1,55)
      read(7,300)  ((ss(j,i),j=1,10),i=1,55)
      close(unit=7)

      open(unit=8,file='gaulag.tab')
      read(8,100)(j,z2(i),wg2(i),z3(i),wg3(i),z4(i),wg4(i),i=1,IGL)
      close(unit=8)

100   format(i4,6d25.15)
300   format(10f8.3)
      return
      end subroutine loadsttab

!       ----------------------------------------------------------------
!       Numerical recipies
!       ----------------------------------------------------------------

      SUBROUTINE qsimp(func,a,b,s)
      INTEGER JMAX
      double precision a,b,func,s,EPS
      EXTERNAL func
      PARAMETER (EPS=1.0d-8, JMAX=20)
CU    USES trapzd
      INTEGER j
      double precision os,ost,st
      ost=-1.0d30
      os= -1.0d30
      do 11 j=1,JMAX
        call trapzd(func,a,b,st,j)
        s=(4.*st-ost)/3.
        if (abs(s-os).lt.EPS*abs(os)) return
        os=s
        ost=st
11    continue
!      pause 'too many steps in qsimp'
      END

!       ----------------------------------------------------------------

      SUBROUTINE trapzd(func,a,b,s,n)
      INTEGER n
      double precision a,b,s,func
      EXTERNAL func
      INTEGER it,j
      double precision del,sum,tnm,x
      if (n.eq.1) then
        s=0.5*(b-a)*(func(a)+func(b))
      else
        it=2**(n-2)
        tnm=it
        del=(b-a)/tnm
        x=a+0.5*del
        sum=0.
        do 11 j=1,it
          sum=sum+func(x)
          x=x+del
11      continue
        s=0.5*(s+(b-a)*sum/tnm)
      endif
      return
      END

!       ----------------------------------------------------------------

      SUBROUTINE qromb(func,a,b,ss)
      INTEGER JMAX,JMAXP,K,KM
      double precision a,b,func,ss,EPS
      EXTERNAL func
      PARAMETER (EPS=1.0d-8, JMAX=20, JMAXP=JMAX+1, K=5, KM=K-1)
CU    USES polint,trapzd
      INTEGER j
      double precision dss,h(JMAXP),s(JMAXP)
      h(1)=1.0d0
      do 11 j=1,JMAX
        call trapzd(func,a,b,s(j),j)
        if (j.ge.K) then
          call polint(h(j-KM),s(j-KM),K,0.0d0,ss,dss)
          if (abs(dss).le.EPS*abs(ss)) return
        endif
        s(j+1)=s(j)
        h(j+1)=0.25*h(j)
11    continue
!      pause 'too many steps in qromb'
      END

!       ----------------------------------------------------------------

      SUBROUTINE polint(xa,ya,n,x,y,dy)
      INTEGER n,NMAX
      double precision dy,x,y,xa(n),ya(n)
      PARAMETER (NMAX=10)
      INTEGER i,m,ns
      double precision den,dif,dift,ho,hp,w,c(NMAX),d(NMAX)
      ns=1
      dif=abs(x-xa(1))
      do 11 i=1,n
        dift=abs(x-xa(i))
        if (dift.lt.dif) then
          ns=i
          dif=dift
        endif
        c(i)=ya(i)
        d(i)=ya(i)
11    continue
      y=ya(ns)
      ns=ns-1
      do 13 m=1,n-1
        do 12 i=1,n-m
          ho=xa(i)-x
          hp=xa(i+m)-x
          w=c(i+1)-d(i)
          den=ho-hp
          if(den.eq.0.) print*, 'failure in polint'
          den=w/den
          d(i)=hp*den
          c(i)=ho*den
12      continue
        if (2*ns.lt.n-m)then
          dy=c(ns+1)
        else
          dy=d(ns)
          ns=ns-1
        endif
        y=y+dy
13    continue
      return
      END
