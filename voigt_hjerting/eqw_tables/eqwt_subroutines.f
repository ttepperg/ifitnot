!       (C) Thorsten Tepper Garcia 2004-05
!       ----------------------------------------------------------------
!       subroutines & functions
!       ----------------------------------------------------------------
        
        subroutine read_parameter
!       ----------------------------------------------------------------
!       1. Declaring variables
!       ----------------------------------------------------------------
        use variables

        implicit none

        integer :: argcount
        character(len=64) :: dummy
!       ----------------------------------------------------------------
!       2. Program start: Getting arguments
!       ----------------------------------------------------------------
        argcount = IArgC()
              if ((argcount .lt. 5).OR.(argcount .gt. 5)) then
         write(6,*) 'Computes a table for the equivalent width of a '//
     &'transition as a function of column density (rows) and b-value'//
     &' (columns). Beware that the equivalent width is given in mA '//
     &'(mili-Angstroem)!'
         write(6,*)
         print *,'USAGE: eqw <transition> <logNHImin/step> <bmin/step>'
         stop
              end if
        
        call GetArg(1,dummy)
        read (dummy,'(A10)') ion_name

        call GetArg(2,dummy)
        read (dummy,'(F5.1)') alogNHImin
        
        call GetArg(3,dummy)
        read (dummy,'(F5.1)') stepNHI
        
        call GetArg(4,dummy)
        read (dummy,'(F5.1)') bmin
        
        call GetArg(5,dummy)
        read (dummy,'(F5.1)') stepb

!       Defining maximum values for parameters


!         if (zgal .LE. zmin .OR. zgal .GT. zmax) then
!        print*
!        write(*,'(A,F5.3,A,F5.3,A)') 'WARNING: zgal must be greater
*     1 than ',zmin,' and less equal than ',zmax,'!'
!        stop
!       else if (nLOSmax .GT. maxLOS) then
!        print*
!        print*, 'WARNING: maxLOS = ', maxLOS
!        stop
!       end if        

!       ----------------------------------------------------------------
        return
        end subroutine

!       ================================================================

!       ----------------------------------------------------------------
!       Wavelength baseline and flat spectrum
!       ----------------------------------------------------------------
        subroutine wavebase

!       Generates a wavelength baseline from alambdamin to alambdamax
!       with resolution 'alambdastep'
!       A flat, normalized ''spectrum'' is also generated; this is
!       stored into 'fluxorig', which is again modified by the
!       subroutine read_sed only if the desired, input spectrum IS NOT
!       'flat' 

!       ----------------------------------------------------------------
!       Declaring variables
!       ----------------------------------------------------------------
        use variables

        implicit none

        integer :: i, npos                                        !loop indices
        
!       ----------------------------------------------------------------
!       subroutine start
!       ----------------------------------------------------------------
!       Defining wavelength-range
             
        alambdamin = 9.0d+02            !to include the Ly-Lim for zc=0        
        alambdamax = 1.5d+03

        alambdastep = 1.0d-02     !fixes ncount, IPOINTS and awavstep
        ncount = 1                      !ncount: index for do-cycles related to
        npos = 1                !IPOINTS

        do i = 1, IPOINTS        !Initializing wavelength baseline
         alambda(i) = 0.0d0
        end do
        
        do while (alambda(ncount) .LT. alambdamax)
         alambda(npos) = alambdamin + npos*alambdastep
         npos = npos + 1
         ncount = npos - 1
        end do
        print*, ncount

        if (ncount .GT. IPOINTS) then
         print*, 'WARNING: to many wavelength-baseline points'
         stop
        end if

        return
!       ----------------------------------------------------------------
        end subroutine
!       ----------------------------------------------------------------

!       ================================================================

!       ----------------------------------------------------------------
!       Lyman Series line blanketing (Voigt-Profile Approximation)
!       ----------------------------------------------------------------
        subroutine voigtapp(trans)

!       Lyman-Series Absorption Lines are calculated according to the
!       (correct) Line-Profile Method. Since a<<1 (where a is the
!       damping parameter in the Hjerting-function H(a,v)), the line's
!       profile can be approximated by
!       
!       H(a,x) = exp(-x^2)*[1-(2a/SQRT(pi))*H_1(x)]
!       where
!       H_1(x)={(3+4x^2)(1+x^2)exp(-x^2)-(3+2x^2)sinh(x^2/(x^2))}/(2x^2)
!       with a = adamping and x = anu. 
*
!       'nlines' gives the number of lines being included

!       ----------------------------------------------------------------
!       Declaring variables
!       ----------------------------------------------------------------
        use variables

        implicit none

             double precision :: adopplerinv,adamping,alphanu,anu,anu2,h0,h1,acorr,
     +        tauneg
        integer :: i,j,trans

!       ----------------------------------------------------------------
!       Calculating absorption coefficent 'tau(lambda)'
!       ----------------------------------------------------------------

!       Calculating Profile-function for each line (nlines = 24) and
!       wavelength (at the correct epoch of absorption)

        adopplerinv = (CLIGHT/(wl_central*b))*1.0d+03
        adamping = (wl_central*gamma/(4.0d0*PI*b))*1.0d-13
!       print*, b, adamping
        alphanu = (sqrtPI*ERADIUS*aNHI*(wl_central*wl_central)*
     1  fvalue*adopplerinv)*1.0d-16

        do j = 1, ncount 
        
         anu = (alambda(j)-wl_central)*adopplerinv*1.0d-8
         anu2 = anu*anu
         h0 = DEXP(-anu2)
         h1 = 1.5d0/anu2
         if (DABS(anu).GT.1.0d-05) then     !check for the limit here

         acorr = h0-adamping*sqrtPIinv/anu2*
     1         (h0*h0*(4.0d0*anu2*anu2 + 7.0d0*anu2 + 4.0d0 + h1) - 1.0d0 - h1)

         else  
          acorr = h0-2.0d0*adamping*sqrtPIinv        
         endif
        
!       Adding up the absorption coefficent
        
          tauneg = (-alphanu)*acorr
          flux(j) = flux(j)*DEXP(tauneg)

        end do
        
        return
!       ----------------------------------------------------------------
        end subroutine
!       ----------------------------------------------------------------

!       ================================================================
!       ----------------------------------------------------------------
!       Calculation of the equivalent width
!       ----------------------------------------------------------------
        function eqwidth(arr,arr2,points)

!       Calculates omega for a redshift 'z', a flux arr(points) and a
!       wavelength baseline arr2(points).
!       'arr2' HAS THE SAME RANK (IPOINTS) AS 'alambda'!

        use variables

        implicit none
 
        integer :: points, i
        double precision ::  arr(IPOINTS),arr2(IPOINTS),func(IPOINTS),
     +        eqwidth

        eqwidth = 0.0D0

!       Integrating 1-flux in wavelength 
        
        do i =1,points
         
         func(i) = 1 - arr(i)
         eqwidth = eqwidth + 0.5D0*(func(i+1) + func(i))
     1         *(arr2(i+1) - arr2(i))

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
      double precision ::  FUN,A,B,ABSERR,RELERR,RESULT,ERREST,FLAG
      integer :: NOFUN
************************************************************************
* ESTIMA LA INTEGRAL DE FUN(X) DESDE A HASTA B
************************************************************************
      double precision ::  W0,W1,W2,W3,W4,AREA,X0,F0,STONE,STEP,COR11,TEMP
      double precision ::  QPREV,QNOW,QDifF,QLEFT,ESTERR,TOLERR
      double precision ::  QRIGHT(31),F(16),X(16),FSAVE(8,30),XSAVE(8,30)
      integer :: LEVMIN,LEVMAX,LEVOUT,NOMAX,NOFIN,LEV,NIM,I,J
      external FUN
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
      if(A.EQ.B)return

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
      do 25 J=2,16,2
          F(J)=FUN(X(J))
   25 continue
      NOFUN=9

********************     CALCULO CENTRAL
   30 X(1)=(X0+X(2))/2.D0
      F(1)=FUN(X(1))
      do 35 J=3,15,2
          X(J)=(X(J-1)+X(J+1))/2.D0
          F(J)=FUN(X(J))
  35  continue
      NOFUN=NOFUN+8
      STEP=(X(16)-X0)/16.D0
      QLEFT=(W0*(F0+F(8)) + W1*(F(1)+F(7)) + W2*(F(2)+F(6)) +
     +      W3*(F(3)+F(5)) + W4*F(4))*STEP
      QRIGHT(LEV+1)=(W0*(F(8)+F(16))+W1*(F(9)+F(15))+W2*(F(10)+F(14))+
     +      W3*(F(11)+F(13)) + W4*F(12))*STEP
      QNOW=QLEFT+QRIGHT(LEV+1)
      QDifF=QNOW-QPREV
      AREA=AREA+QDifF

********************   TEST DE CONVERGENCIA DE INTERVALOS
      ESTERR=ABS(QDifF)/1023.D0
      TOLERR=MAX(ABSERR,RELERR*ABS(AREA))*(STEP/STONE)
      if(LEV.LT.LEVMIN) goto 50
      if(LEV.GE.LEVMAX) goto 62
      if(NOFUN.GT.NOFIN) goto 60
      if(ESTERR.LE.TOLERR) goto 70

*****************  NO HAY CONVERGENCIA. LOCALIZA NUEVO INTERVALO
   50 NIM=2*NIM
      LEV=LEV+1

*****************  ALMACENA LOS ELEMENTOS PARA UN FUTURO USO
      do 52 I=1,8
          FSAVE(I,LEV)=F(I+8)
          XSAVE(I,LEV)=X(I+8)
   52 continue

      QPREV=QLEFT
      do 55 I=1,8
          J=-I
          F(2*J+18)=F(J+9)
          X(2*J+18)=X(J+9)
   55 continue
      goto 30

*********************  EL NUMERO DE LLAMADAS A FUN CASI EXCESIVO
   60 NOFIN=2*NOFIN
      LEVMAX=LEVOUT
      FLAG=FLAG+(B-X0)/(B-A)
      goto 70

   62 FLAG=FLAG+1.D0

********************* EL INTERVALO CONVERGE.SUMA LAS CONTRIBUCIONES
   70 RESULT=RESULT+QNOW
      ERREST=ERREST+ESTERR
      COR11=COR11+QDifF/1023.D0

************** LOCALIZA UN NUEVO INTERVALO
   72 if(NIM.EQ.2*(NIM/2)) goto 75
      NIM=NIM/2
      LEV=LEV-1
      goto 72
   75 NIM=NIM+1
      if(LEV.LE.0) goto 80

      QPREV=QRIGHT(LEV)
      X0=X(16)
      F0=F(16)
      do 78 I=1,8
         F(2*I)=FSAVE(I,LEV)
         X(2*I)=XSAVE(I,LEV)
   78 continue
      goto 30

**************   FINALIZACION
   80 RESULT=RESULT+COR11

      if (ERREST.EQ.0.D0) return
   82 TEMP=ABS(RESULT)+ERREST
      if(TEMP.NE.ABS(RESULT)) return
      ERREST=2.D0*ERREST
      goto 82
      end


      
      subroutine lininterp2(x1,x2,y)
      integer :: j1,j2 
      double precision :: x1,x2,y,x1a(10),x2a(55),ya(10,55),dydx1,dydx2
      common/struc/ya,x2a,x1a
     
      call locate(x1a,10,x1,j1)
      call locate(x2a,55,x2,j2)

      dydx1=(ya(j1+1,j2)-ya(j1,j2))/(x1a(j1+1)-x1a(j1))
      dydx2=(ya(j1,j2+1)-ya(j1,j2))/(x2a(j2+1)-x2a(j2))  

      y = ya(j1,j2)+dydx1*(x1-x1a(j1))+dydx2*(x2-x2a(j2))

      return
      end


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
      end


      subroutine loadsttab()
      double precision :: tgam(10),tka(55),ss(10,55)
      integer :: IGL
      parameter (IGL=80)
      double precision :: z2(IGL),z3(IGL),z4(IGL),wg2(IGL),wg3(IGL),wg4(IGL)
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
      end
