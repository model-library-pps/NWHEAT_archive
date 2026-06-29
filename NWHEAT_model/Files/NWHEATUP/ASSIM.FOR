!************************ #file "ASSIM.FOR" *****************************!
!************************************************************************!
!*  SUBROUTINE ASSIM                                                    *!
!*  Author : Daniel van Kraalingen                                      *!
!*  Date   : 15 March 1987                                              *!
!*           Transformed for literate programming by Sjaak Wolfert      *!
!*  Date   : {TIME \@ "d MMMM, yyyy"|7 October, 1994}                                            *!
!*  Purpose: This subroutine calculates daily totals of gross           *!
!*           assimilation (DTGA) from environmental parameters.         *!
!*                                                                      *!
!*  SUBROUTINES called : none                                           *!
!*  FILE usage : none                                                   *!
!************************************************************************!
      SUBROUTINE ASSIM (DAYL,PAMAX,EFF,KDIF,LAI,PARDIR,PARDIF,
     $                  SINB,DTGA)

      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)

      !********** ASSIM Dimension declarations (#mul, #opt) *************!
      DIMENSION XGAUS(3), WGAUS(3)
      DIMENSION PARDIR(3), PARDIF(3),SINB(3)

      !********** ASSIM Parameter declarations (#mul, #opt) *************!
      LOGICAL INIT
      PARAMETER (PI=3.1415926)
      DATA XGAUS /0.1127, 0.5000, 0.8873/ !* Gaussian points  *!
      DATA WGAUS /0.2778, 0.4444, 0.2778/ !* Gaussian weights *!

      !********** ASSIM Save declarations (#mul, #opt)      *************!

      !********************** Assim body ********************************!
      !** Calculation of DTGA using DAYL, PAMAX,EFF, KDIF, LAI, PARDIR,**!
      !** PARDIF, SINB, XGAUS, WGAUS and SCV.                          **!
      TAA=0.
      DO 100,IHOUR=1,3

          !********** Assim 1: Calculation of TCA.             **********!
          ACA = 0.
          DO 10,ILAI=1,3

              !********** Assim 1.1: Calculation of LOCA.       *********!

                  !********** Assim 1.1.1: Calculation of REFS. *********!
                  SCV =0.2
                  REFH=(1.-SQRT(1.-SCV))/(1.+SQRT(1.-SCV))
                  REFS = REFH*2./(1.+1.6*SINB(IHOUR))
                  !********** End of Assim 1.1.1 ************************!

                  !********** Assim 1.1.2: Calculate KDIRBL, KDIRT. *****!
                  KDIRBL = (0.5/SINB(IHOUR))*KDIF/(0.8*SQRT(1.-SCV))
                  KDIRT  = KDIRBL*SQRT(1.-SCV)
                  !********** End of Assim 1.1.2 ************************!

                  !********** Assim 1.1.3: Definition of LAIC.  *********!
                  LAIC = LAI*XGAUS(ILAI)
                  !********** End of Assim 1.1.3 ************************!

                  !********** Assim 1.1.4  ******************************!
                  !** Calculation of VISDF, VIST,VISD.                 **!
                  VISDF = (1.-REFS)*PARDIF(IHOUR)*KDIF*EXP(-KDIF*LAIC)
                  VIST  = (1.-REFS)*PARDIR(IHOUR)*KDIRT*EXP(-KDIRT*LAIC)
                  VISD = (1.-SCV)*PARDIR(IHOUR)*KDIRBL*EXP(-KDIRBL*LAIC)
                  !********** End of Assim 1.1.4 ************************!

                  !********** Assim 1.1.5: Calculation of ASH.   ********!
                  VISSHD = VISDF+VIST-VISD
                  ASH  = PAMAX*(1.-EXP(-VISSHD*EFF/PAMAX))
                  !********** End of Assim 1.1.5 ************************!

                  !********** Assim 1.1.6: Calculation of ASUN.  ********!
                  VISPP = (1.-SCV)*PARDIR(IHOUR)/SINB(IHOUR)
                  IF (VISPP.GT.0.) THEN
                      ASUN = PAMAX*(1.-(PAMAX-ASH)*
     $                         (1.-EXP(-VISPP*EFF/PAMAX))/(EFF*VISPP))
                  ELSE
                       ASUN = ASH
                  END IF
                  !*********** End of Assim 1.1.6 ***********************!

                  !********** Assim 1.1.7: Calculation of LOCA.  ********!
                  FSLLA = EXP(-KDIRBL*LAIC)
                  LOCA  = FSLLA*ASUN+(1.-FSLLA)*ASH
                  !********** End of Assim 1.1.7 ************************!

              !********** End of Assim 1.1 ******************************!

          ACA = ACA + LOCA*WGAUS(ILAI)
10        CONTINUE

          !* Integration of ACA over LAI to obtain TCA.                *!
          TCA = ACA*LAI
          !********** End of Assim 1 ************************************!

      TAA = TAA + TCA*WGAUS(IHOUR)
100   CONTINUE

      !* Integration of TAA over DAYL to obtain DTGA.                   *!
      DTGA = TAA*DAYL

      !********************** End of Assim body *************************!

      RETURN
      END
!************************ End of "ASSIM.FOR" ****************************!
