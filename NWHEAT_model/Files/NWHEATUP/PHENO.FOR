!************************ #file "PHENO.FOR" *****************************!
!************************************************************************!
!*  SUBROUTINE PHENO                                                    *!
!*  Author : Rob Groot                                                  *!
!*  Date   : november 1985                                              *!
!*           Transformed for literate programming by Sjaak Wolfert      *!
!*  Date   : {date \@ MM/dd/yy|08/12/94}                                                   *!
!*  Purpose: This subroutine calculates the development rate of the     *!
!*           crop.                                                      *!
!*                                                                      *!
!*  SUBROUTINES and FUNCTIONS called : AFGEN, INTGRL, INSW, LIMIT       *!
!*  FILE usage : none                                                   *!
!************************************************************************!
      SUBROUTINE PHENO (TMPA,TSUM2,DAYLP,TMPSUM,DVR1,DVR2)

      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)
      REAL INTGRL,INSW

      !************** PHENO Dimension declarations (#quick) *************!
      DIMENSION VERNRT(16)

      !************** PHENO Save declarations (#quick)        ***********!
      SAVE VERN

      !**************************** Pheno body **************************!
      !** Calculation of DVR1, DVR2, TMPSUM using TMPA, DAYLP, TSUM2.  **!

          !********** Pheno 1: Defining VERNR.                 **********!
          DATA IVERNN/16/
          DATA VERNRT/-20.,0.,  -4.,0., 0.,1., 3.,1., 7.,0.8,
     $                 9.,0.4, 18.,0.0, 40.0,0./
          VERNR = AFGEN(IVERNN,VERNRT,TMPA)
          !********** End of Pheno 1 ************************************!
         
          !********** Pheno 2: Calculation of VERNF.           **********!
          VERN  = INTGRL(INSW(TMPSUM-270.,VERNR,0.),VERN)
          DATA VBASE/8./
          DATA VERSAT/46./
          VERNF = LIMIT(0.,1.,(VERN-VBASE)/(VERSAT-VBASE))
          !********** End of Pheno 2 ************************************!

          !********** Pheno 3: Calculation of FPERF.           **********!
          DATA SDAYL/20./
          BDAYL = INSW(TMPSUM-395.,0.,7.)
          PULSE = INSW(TMPSUM-125.,0.,1.)
          FPERF = (DAYLP-BDAYL)/(SDAYL-BDAYL) * PULSE
          !********** End of Pheno 3 ************************************!

          !********** Pheno 4: Calculation of TMPSUM.          **********!
          TMPB1 = AMAX1(0.,TMPA-1.)
          TMPB9 = AMAX1(0.,TMPA-9.)
          TMPCOR= INSW(TMPSUM-872.,TMPB1*VERNF*FPERF,TMPB9)
          TMP   = INSW(TMPSUM-125.,TMPB1,TMPCOR)
          TMPSUM= INTGRL(TMP,TMPSUM)
          !********** End of Pheno 4 ************************************!

          !********** Pheno 5: Calculation of DVR1 and DVR2.   **********!
          DVR1   = INSW(TMPSUM-872.,INSW(TMPSUM-125.,0.,TMP/747.),0.)
          DVR2   = INSW(TMPSUM-872.,0.,TMP/TSUM2)
          !********** End of Pheno 5 ************************************!

      !**************************** End of Pheno body *******************!

      RETURN
      END
!************************ End of PHENO.FOR ******************************!
