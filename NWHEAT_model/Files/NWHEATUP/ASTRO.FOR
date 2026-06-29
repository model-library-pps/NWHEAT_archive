!************************* #file "ASTRO.FOR" ****************************!
!************************************************************************!
!* SUBROUTINE ASTRO                                                     *!
!*                                                                      *!
!* Author: Daniel van Kraalingen                                        *!
!*         documented by Rob Groot                                      *!
!*         Transformed for literate programming by Sjaak Wolfert        *!
!* Date :  {date \@ MM/dd/yy|08/12/94}                                                     *!
!* Purpose:This subroutine calculates astronomical daylength (DAYL)     *!
!*         and photoperiodically active daylength (DAYLP)               *!
!*                                                                      *!
!* SUBROUTINES called : none                                            *!
!* FILE usage : none                                                    *!
!************************************************************************!
      SUBROUTINE ASTRO (IDAY,LAT,DAYL,DAYLP,SINLD,COSLD)

      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)
      PARAMETER (PI=3.1415926)
      RAD=PI/180.       !* conversion factor from degrees to radians    *!

      !********** ASTRO Dimension declarations(#mul, #opt) **************!

      !********** ASTRO Save declarations(#mul, #opt)      **************!

      !**************************** Astro body **************************!
      !** Performs calculation of DAYL, DAYLP, SINLD, COSLD using IDAY,**!
      !** LAT and RAD.                                                 **!

          !********** Astro 1: Calculation of DEC.             **********!
          DEC=-ASIN(SIN(23.45*RAD)*COS(2.*PI*(IDAY+10.)/365.))
          !********** End of Astro 1  ***********************************!
         
          !********** Astro 2: Calculation of SINLD and COSLD. **********!
          SINLD=SIN(RAD*LAT)*SIN(DEC)
          COSLD=COS(RAD*LAT)*COS(DEC)
          !********** End of Astro 2  ***********************************!

          !********** Astro 3: Calculation of DAYL and DAYLP.  **********!
          DAYL =12.0*(1.+2.*ASIN(SINLD/COSLD)/PI)
          DAYLP=12.0*(1.+2.*ASIN((-SIN(-4.*RAD)+SINLD)/COSLD)/PI)
          !********** End of Astro 3  ***********************************!

      !**************************** End of Astro body *******************!

      RETURN
      END
!************************* End of ASTRO.FOR *****************************!
