!***********************#file "RADIAT.FOR" ******************************!
!************************************************************************!
!*  SUBROUTINE RADIAT                                                   *!
!*  Author : Daniel van Kraalingen                                      *!
!*           documented by Rob Groot                                    *!
!*  Date   : 15 march 1987                                              *!
!*  Purpose: This subroutine calculates for three moments during a      *!
!*           day, chosen according to a three point Gaussian integra-   *!
!*           tion routine, the sine of the solar elevation (SINB) and   *!
!*           the flux densities of the direct (PARDIR, in W/m2 and      *!
!*           diffuse (PARDIF, in W/m2) components of PAR.               *!
!*                                                                      *!
!*           References to literature:                                  *!
!*           Spitters, C.J.T., H.A.J.M. Toussaint & J.Goudriaan, 1986.  *!
!*	         Agric.and For.Met.38(1986):217-229                      *!
!*           Spitters, C.J.T. 1986.                                     *!
!*	         Agric.and For.Met.38(1986):217-229                      *!
!*                                                                      *!
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *!
!*  name     meaning                                    units  class    *!
!*  ----     -------                                    -----  -----    *!
!*  IDAY     Julian daynumber                             =      I,T    *!
!*  DAYL     astronomical daylength                       h      I      *!
!*  SINLD    intermediate variable                        =      I      *!
!*  COSLD    intermediate variable                        =      I      *!
!*  AVRAD    global radiation                         J m-2 d-1  I      *!
!*  SINB     sine of the solar elevation                  =      O      *!
!*  PARDIR   flux density of direct  component of PAR   W m-2    O      *!
!*  PARDIF   flux density of diffuse component of PAR   W m-2    O      *!
!*                                                                      *!
!*  SUBROUTINES called : none                                           *!
!*  FILE usage : none                                                   *!
!************************************************************************!

      SUBROUTINE RADIAT(IDAY,DAYL,SINLD,COSLD,AVRAD,SINB,PARDIR,
     $                  PARDIF,ATMTR)

      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)
      DIMENSION XGAUS(3), WGAUS(3)
      DIMENSION PARDIR(3),PARDIF(3),SINB(3)
      LOGICAL INIT
      PARAMETER (PI=3.1415926)
      DATA XGAUS /0.1127, 0.5000, 0.8873/
      DATA WGAUS /0.2778, 0.4444, 0.2778/


*---- sine of solar elevation (SINB), integral of SINB (DSINB)
*     and integral of SINB with correction for lower atmospheric
*     transmission at low solar elevations (DSINBE)

      AOB   =SINLD/COSLD
      DSINB =3600.*(DAYL*SINLD+24.*COSLD*SQRT(1.-AOB*AOB)/PI)
      DSINBE=3600.*(DAYL*(SINLD+0.4*(SINLD*SINLD+COSLD*COSLD*0.5))+
     $       12.0*COSLD*(2.0+3.0*0.4*SINLD)*SQRT(1.-AOB*AOB)/PI)


*---- solar constant (SC) and daily extraterrestrial radiation (ANGOT)

      SC =1370.*(1.+0.033*COS(2.*PI*IDAY/365.))
      ANGOT=SC*DSINB


*---- diffuse light fraction (FRDIF) from atmospheric
*     transmission (ATMTR)

      ATMTR=AVRAD/ANGOT
      IF (ATMTR.GT.0.75) FRDIF=0.23
      IF (ATMTR.LE.0.75.AND.ATMTR.GT.0.35) FRDIF=1.33-1.46*ATMTR
      IF (ATMTR.LE.0.35.AND.ATMTR.GT.0.07) FRDIF=1.-2.3*(ATMTR-0.07)**2
      IF (ATMTR.LE.0.07) FRDIF=1.


*---- three hours of the day are generated

      DO 10,IT=1,3

         HOUR=12.0+0.5*DAYL*XGAUS(IT)
         SINB(IT)=AMAX1(0.,SINLD+COSLD*COS(2.*PI*(HOUR+12.)/24.))

*------- diffuse PAR (PARDIF) and direct PAR (PARDIR)

         PAR   =0.5*AVRAD*SINB(IT)*(1.+0.4*SINB(IT))/DSINBE
         PARDIF(IT)=AMIN1(PAR,SINB(IT)*FRDIF*ATMTR*0.5*SC)
         PARDIR(IT)=PAR-PARDIF(IT)

10    CONTINUE


      RETURN
      END
!***********************End of "RADIAT.FOR" *****************************!
