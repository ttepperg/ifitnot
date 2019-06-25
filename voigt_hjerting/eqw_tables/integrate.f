c MM 16/12/99 This is a subroutine given to me by Alberto
c Fernandez-Soto to integrate a function of one variable.
      SUBROUTINE INTEGRATE(FUN,A,B,RELERR,RESULT)
      IMPLICIT NONE
      DOUBLE PRECISION FUN,A,B,ABSERR,RELERR,RESULT,ERREST,FLAG
      INTEGER NOFUN
************************************************************************
* ESTIMA LA INTEGRAL DE FUN(X) DESDE A HASTA B
************************************************************************
      DOUBLE PRECISION W0,W1,W2,W3,W4,AREA,X0,F0,STONE,STEP,COR11,TEMP
      DOUBLE PRECISION QPREV,QNOW,QDIFF,QLEFT,ESTERR,TOLERR
      DOUBLE PRECISION QRIGHT(31),F(16),X(16),FSAVE(8,30),XSAVE(8,30)
      INTEGER LEVMIN,LEVMAX,LEVOUT,NOMAX,NOFIN,LEV,NIM,I,J
      EXTERNAL FUN
****************   INICIACION GENERAL. CONSTANTES  ********
      ABSERR=0.D0
      LEVMIN=1
      LEVMAX=30
      LEVOUT=6
      NOMAX=5000
      NOFIN=NOMAX-8*(LEVMAX-LEVOUT+2**(LEVOUT+1))

      W0=3956.D0/14175.D0
      W1=23552.D0/14175.D0
      W2=-3712.D0/14175.D0
      W3=41984.D0/14175.D0
      W4=-18160.D0/14175.D0


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
      END


      
      SUBROUTINE lininterp2(x1,x2,y)
      INTEGER j1,j2 
      REAL*8 x1,x2,y,x1a(10),x2a(55),ya(10,55),dydx1,dydx2
      common/struc/ya,x2a,x1a
     
      CALL locate(x1a,10,x1,j1)
      CALL locate(x2a,55,x2,j2)

      dydx1=(ya(j1+1,j2)-ya(j1,j2))/(x1a(j1+1)-x1a(j1))
      dydx2=(ya(j1,j2+1)-ya(j1,j2))/(x2a(j2+1)-x2a(j2))  

      y = ya(j1,j2)+dydx1*(x1-x1a(j1))+dydx2*(x2-x2a(j2))

      return
      END


      SUBROUTINE locate(xx,n,x,j)
      INTEGER j,n
      REAL*8 x,xx(n)
      INTEGER jl,jm,ju
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
      END


      subroutine loadsttab()
      real*8 tgam(10),tka(55),ss(10,55)
      integer IGL
      parameter (IGL=80)
      real*8 z2(IGL),z3(IGL),z4(IGL),wg2(IGL),wg3(IGL),wg4(IGL)
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
