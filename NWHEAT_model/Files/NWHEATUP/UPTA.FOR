!***************************** #file "UPTA.FOR" *************************!
!************************************************************************!
!* PROGRAM UPTA                                                         *!
!* Author    : Rob Groot                                                *!
!* Date      : 12 Jan 1988                                              *!
!* Update    : Transformed for literate programming by Sjaak Wolfert.   *!
!*             09/02/94                                                 *!
!* Purpose   : Simulation of nitrogen balance in a system of winter     *!
!*             wheat and soil.                                          *!
!*                                                                      *!
!************************************************************************!
      PROGRAM UPTA

      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)

      !********** UPTA variable declarations (#quick)                ****!
      LOGICAL TERMIN
      !********** UPTA variable declarations (#quick)                ****!
      CHARACTER RUNNAM*5,REMARK*80,STATR*5

      !********** UPTA dimension declarations (#quick)               ****!
      DIMENSION PARDIR(3), PARDIF(3), SINB(3), FLOW(11), WCREL(10)
      DIMENSION DEPTH(10), THCKN(10), TRR(10), WCACT(10), SOILTP(10)
      DIMENSION WCACT0(10), RESOIL(10), WCWILT(10), RHOD(10), WCFLDC(10)
      DIMENSION MINOM(10), MINFM(10), NLOSS(10), NIMM(10), LRV(10)
      DIMENSION ANLAY(10)

      !********** Upta body *********************************************!
      !** Simulation of nitrogen balance in a system of winter wheat   **!
      !** and soil.                                                    **!

      DATA TERMIN /.FALSE./

          !********** Upta 1: Model initialization             **********!
          CALL INIT (IYEAR,IDAY,IFINIT,IGAP,REMARK,RUNNAM,STATR)
          !********** End of Upta 1 *************************************!

10    CONTINUE

          !********** Upta 2: Dynamic part of the model        **********!
          CALL WEATHR (IWAR,STATR,IYEAR,IDAY,IGAP,LONGIE,LATIN, ALTI,
     $                TMIN,TMAX,DTR,RAIN,VAP,WIND)

          CALL ASTRO (IDAY,LATIN,DAYL,DAYLP,SINLD,COSLD)

          CALL RADIAT (IDAY,DAYL,SINLD,COSLD,DTR,SINB,PARDIR,
     $                PARDIF,ATMTR)

          CALL WATBAL (RD,IDAY,REMARK,RUNNAM,ET0,LAITOT,
     $                RAIN,TRR,TRTOT,WCREL,RDMSOL,THCKN,DEPTH,ILAYER,
     $                TTRANS,TDRAIN,TRAIN,FLOW,TESOIL,WCACT,WCACT0,
     $                RHOD,RESOIL,WCWILT,WCFLDC,TRMAX)

          CALL NITUPT (1,IDAY,REMARK,RUNNAM,FLOW,
     $                WCACT,THCKN,ILAYER,WCACT0,LRV,IN,RDINL,
     $                TRR,RD,DVS,NDEM,ANLAY,NUPTR,NLOSS,MINFM,
     $                MINOM,NIMM)

          CALL SOLTMP (TMIN,TMAX,ILAYER,SOILTP)

          CALL OMDYN (ILAYER,WCACT,ANLAY,THCKN,WCFLDC,RHOD,SOILTP,
     $               MINFM,NLOSS,MINOM,NIMM)

          CALL NITUPT (2,IDAY,REMARK,RUNNAM,FLOW,
     $                WCACT,THCKN,ILAYER,WCACT0,LRV,IN,RDINL,
     $                TRR,RD,DVS,NDEM,ANLAY,NUPTR,NLOSS,MINFM,
     $                MINOM,NIMM)

          CALL PENMAN (ATMTR,TMIN,TMAX,DTR,WIND,VAP,E0,ES0,ET0)

          CALL WATBAL (RD,IDAY,REMARK,RUNNAM,ET0,LAITOT,
     $                RAIN,TRR,TRTOT,WCREL,RDMSOL,THCKN,DEPTH,ILAYER,
     $                TTRANS,TDRAIN,TRAIN,FLOW,TESOIL,WCACT,WCACT0,
     $                RHOD,RESOIL,WCWILT,WCFLDC,TRMAX)

          CALL NITUPT (3,IDAY,REMARK,RUNNAM,FLOW,
     $                WCACT,THCKN,ILAYER,WCACT0,LRV,IN,RDINL,
     $                TRR,RD,DVS,NDEM,ANLAY,NUPTR,NLOSS,MINFM,
     $                MINOM,NIMM)

          CALL CROP (REMARK,RUNNAM,IDAY,TMIN,RDINL,TMAX,DAYL,DAYLP,
     $              PARDIR,PARDIF,SINB,IN,ILAYER,WCREL,DEPTH,RDMSOL,
     $              THCKN,TRTOT,TRMAX,SOILTP,NUPTR,RD,LAITOT,DVS,
     $              NDEM,LRV,TERMIN)
          !********** End of Upta 2 *************************************!

      IF (TERMIN) GOTO 20

          !********** Upta 3: Update time                      **********!
          CALL TIMER (IDAY,IYEAR,IFINIT,TERMIN)
          !********** End of Upta 3 *************************************!

      GOTO 10
20    CONTINUE
      !********** End of Upta body **************************************!

      END
!***************************** End of UPTA.FOR **************************!
