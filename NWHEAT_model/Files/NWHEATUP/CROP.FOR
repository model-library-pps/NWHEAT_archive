!***************************** #file "CROP.FOR" *************************!
!************************************************************************!
!*  SUBROUTINE CROP                                                     *!
!*  Author : Rob Groot                                                  *!
!*  Date   : 26 january 1989                                            *!
!*  Update : Transformed for literate programming by Sjaak Wolfert      *!
!*           11/04/94                                                   *!
!*  Purpose: This subroutine simulates the dry matter increase and      *!
!*           the nitrogen distribution for a winter wheat crop.         *!
!*                                                                      *!
!*                                                                      *!
!*  SUBROUTINES called :                                                *!
!*  CROPR    reads crop-specific data from file CROP.DAT                *!
!*  PHENO    calculates crop development stage                          *!
!*  CRPOUT   writes crop status to output file CRPOUT.DAT               *!
!*  ASSIM    calculates gross assimilation                              *!
!*                                                                      *!
!*  FILE usage : none                                                   *!
!************************************************************************!
      SUBROUTINE CROP (REMARK,RUNNAM,IDAY,TMIN,RDINL,
     $                 TMAX,DAYL,DAYLP,PARDIR,PARDIF,SINB,IN,
     $                 ILAYER,WCREL,DEPTH,RDMSOL,THCKN,TRTOT,TRMAX,
     $                 SOILTP,NUPTR,RD,LAITOT,DVS,NDEM,LRV,TERMIN)


      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)

      !*************** CROP Variable declarations (#quick)      *********!
      LOGICAL INIT,TERMIN
      !*************** CROP Variable declarations (#quick)      *********!
      LOGICAL FLOWER
      REAL INTGRL,INSW
      CHARACTER REMARK*80,RUNNAM*5

      !*************** CROP Dimension declarations (#quick)   ***********!
      DIMENSION FRTT(22),FLT(28),RESLAT(8),TMPFT(20),TCTRT(10)
      DIMENSION SLAT(8),PGRIGT(16),TRDRTT(16),LAGT(8),NMAXST(28)
      DIMENSION NMAXRT(8),NMAXLT(8),LAISTT(8),LAIERT(8),PNARIT(14)
      DIMENSION WRDRTT(16)
      !*************** CROP Dimension declarations (#quick)  ************!
      DIMENSION DIST(10)
      !*************** CROP Dimension declarations (#quick)  ************!
      DIMENSION ART(10)
      !*************** CROP Dimension declarations (#quick)  ************!
      DIMENSION LRV(10)
      DIMENSION PARDIR(3),PARDIF(3),SINB(3),WCREL(ILAYER)
      DIMENSION DEPTH(ILAYER),THCKN(ILAYER),SOILTP(ILAYER)

      !*************** CROP Save declarations (#mul, #opt)      *********!
      SAVE

      !*************** CROP Data declarations (#quick)          *********!
      DATA INIT /.TRUE./
      !*************** CROP Data declarations (#quick)          *********!
      DATA FLOWER/.FALSE./

      !**************************** Crop body ***************************!
      !** Calculate dry matter increase, root and nitrogen distribution**!

      IF (.NOT.INIT) GOTO 100
          !********** Crop 1: Initialization                   **********!
          CALL CROPR (WLV,WRT,WLD,WRES,WST,ANRT,ANLV,TADW,LAIANT,
     $                RD,TSUM2,MAINLV,MAINST,MAINRT,MAINSO,REFTMP,
     $                Q10RM,PMXUPR,EFCPR,EFCCH,EFCPRG,EFF,KDIF,
     $                LAT,RGRRES,FC,TCNT,RNFLV,RNFST,RNFRT,RGM,
     $                DISTF,TMPFT,FRTT,FLT,SLAT,LAISTT,LAIERT,
     $                TRDRTT,WRDRTT,TCTRT,LAGT,RESLAT,PGRIGT,
     $                NMAXST,NMAXLT,NMAXRT,PNARIT,
     $                ITMPFN,IFRTN,IFLN,ISLAN,ILAISN,ILAIRN,
     $                ITRDRT,IWRDRT,ITCTRN,ILAGN,IRESLN,IPGRIN,
     $                INMXSN,INMXLN,INMXRN,IPNARN)

          NFLV = ANLV/(WLV+1.0E-9)
          NFST = ANST/(WST+1.0E-9)
          NFRT = ANRT/(WRT+1.0E-9)
          !********** End of Crop 1 *************************************!
      INIT = .FALSE.
100   CONTINUE

          !********** Crop 2: Calculate temp.sum and dev. rate.**********!
          TMPA = (TMIN+TMAX)/2.
          CALL PHENO (TMPA,TSUM2,DAYLP,TMPSUM,DVR1,DVR2)
          !********** End of Crop 2 *************************************!

      IF (TMPSUM.LT.125.) GOTO 999

          !********** Crop 3: Calculate dry matter increase.   **********!

              !********** Crop 3.1: Calculate daily assimilation.********!
              LAIST  = AFGEN(ILAISN,LAISTT,TMPSUM)*INSW(DVS-1.,1.,
     $                   LAI/LAIANT)
              LAIER  = AFGEN(ILAIRN,LAIERT,DVS)   *INSW(DVS-1.,1.,
     $                   LAI/LAIANT)
              SLA    = AFGEN (ISLAN,SLAT,DVS)
              LAI    = WLV * SLA * 1.0E-4
              LAITOT = LAI + LAIST + LAIER

              EAVT  = TMAX-0.25*(TMAX-TMIN)
              REDFOT= AFGEN(ITMPFN,TMPFT,EAVT)
              LAMAX = AMAX1(0.,1040.*NFLV-7.90)
              PAMAX = AMAX1(5.,(LAMAX*REDFOT))
              CALL ASSIM (DAYL,PAMAX,EFF,KDIF,LAI,PARDIR,PARDIF,SINB,
     $                    DTGA)
              !********** End of Crop 3.1 *******************************!

              !********** Crop 3.2: Calculate maintenance respiration.***!
              ACTLV  =(NFLV-RNFLV)/(NMAXL-RNFLV)
              ACTST  =(NFST-RNFST)/(NMAXS-RNFST)
              ACTRT  =(NFRT-RNFRT)/(NMAXR-RNFRT)

              TEF    = Q10RM**((TMPA-REFTMP)/10.)

              RMNLV  =WLV*MAINLV*TEF*ACTLV-RCTLV
              RMNST  =WST*MAINST*TEF*ACTST-RCTST
              RMNRT  =WRT*MAINRT*    ACTRT-RCTRT
              RMAINT =RMNLV+RMNST+RMNRT
              !********** End of Crop 3.2 *******************************!

              !********** Crop 3.3: Calculate act. carbohydrate flow.****!
              IF (TRTOT.GT.0) THEN
                  RED = AMIN1(1.,TRTOT/(TRMAX+1.0E-9))
              ELSE
                  RED = 1.
              ENDIF

              GPHOT = DTGA*(30./44.)*RED
              AVASS = AMAX1(0.,GPHOT-RMAINT)
              !********** End of Crop 3.3 *******************************!

              IF ((.NOT.FLOWER) .AND. (DVS.GT.1.0)) THEN
                  !********** Crop 3.4 **********************************!
                  !** Calculate number of grains, LAI, weight and      **!
                  !** nitrogen content at anthesis.                    **!
                  TADW   = WLV+WST+WRES
                  NUMGR  = 3500.E4 + 1.4E4*TADW
                  LAIANT = LAI
                  WLVANT = WLV
                  WRTANT = WRT
                  WSTANT = WST
                  NFLANT = NFLV
                  NFSANT = NFST
                  NFRANT = NFRT
                  !********** End of Crop 3.4 ***************************!
                  FLOWER=.TRUE.
              ENDIF

              !********** Crop 3.5 **************************************!
              !** Calculate protein conversion and assim. allocation.  **!
              FRT   = AFGEN(IFRTN,FRTT,DVS)
              FSH   = 1.-FRT
              FL    = FSH*AFGEN(IFLN,FLT,DVS)
              FST   = FSH-FL

              PRCLV  = 6.25*NFLV
              PRCST  = 6.25*NFST
              PRCRT  = 6.25*NFRT
              PRCGR  = INSW(DVS-1.,PRCST,5.70*NFGR)
              EFCGLV = PRCLV*EFCPR + (1.-PRCLV)*EFCCH
              EFCGST = PRCST*EFCPR + (1.-PRCST)*EFCCH
              EFCGRT = PRCRT*EFCPR + (1.-PRCRT)*EFCCH
              EFCGGR = PRCGR*EFCPRG+ (1.-PRCGR)*EFCCH
              !********** End of Crop 3.5 *******************************!

              IF (DVS.LT.1.) THEN
                  !********** Crop 3.6: Calculate growh rates. **********!
                  GRLV  = FL *AVASS*EFCGLV
                  GRRT  = FRT*AVASS*EFCGRT
                  GRST  = (FST*AVASS-GRRES)*EFCGST
                  RESLAG= AFGEN(IRESLN,RESLAT,DVS)
                  GRRES = RGRRES*RESLAG*WST
                  !********** End of Crop 3.6 ***************************!
              ELSE
                  !********** Crop 3.7 **********************************!
                  !** Calculate growth rates of grains and reserves.   **!
                  PGRIG = AFGEN(IPGRIN,PGRIGT,TMPA) * 1.E-6
                  LAG   = AFGEN(ILAGN,LAGT,DVS)
                  CDEMG = NUMGR * PGRIG * (1.0/EFCGGR) * LAG
                  RESL  = WRES/(WLV+WST+WRES)
                  TCTR  = AFGEN(ITCTRN,TCTRT,RESL)
                  CSUPG = AVASS+WRES/TCTR
                  FCHTG = AMIN1(CDEMG,CSUPG)
                  GRGR  = FCHTG*EFCGGR

                  IF(CDEMG.LT.CSUPG) THEN
                      GRRES = AVASS - CDEMG
                  ELSE
                      GRRES = -(WRES/TCTR)
                  END IF
                  !********** End of Crop 3.7 ***************************!
                  GRST  = 0.
                  GRLV  = 0.
                  GRRT  = 0.
              ENDIF
          !********** End of Crop 3 *************************************!

          !********** Crop 4: Root calculations.               **********!

              !********** Crop 4.1 Rooting depth and penetration. *******!
              DO 10,I=1,ILAYER
                  IF (RD.GT.(DEPTH(I)-0.5*THCKN(I)).AND.
     $                       RD.LE.(DEPTH(I)+0.5*THCKN(I))) IN=I
10            CONTINUE
              RDINL = RD-(DEPTH(IN-1)+0.5*THCKN(IN-1))
              !********** End of Crop 4.1 *******************************!

              IF (DVS.LT.1.) THEN
                  !********** Crop 4.2 Root extension rate.    **********!
                  IF(FRT*AVASS.GT.0.) THEN
                      IF (RD.LT.RDMSOL) THEN
                          WREDRT = AFGEN (IWRDRT,WRDRTT,WCREL(IN))
                          TREDRT = AFGEN (ITRDRT,TRDRTT,SOILTP(IN))
                          RRD = RGM * TREDRT * WREDRT
                      ELSE
                          RRD = 0.
                      END IF
                  END IF
                  !********** End of Crop 4.2 ***************************!
              ELSE
                  RRD   = 0.
              ENDIF

              !********** Crop 4.3 Root distribution.          **********!
              SUM   = 0.

              !* compartment 1 and 2 *!
              DISTUP = EXP(-DISTF*0.01*((THCKN(1)+THCKN(2))/2.))
              SUM    = SUM +DISTUP

              !* compartment 3 to IN *!
              IF (IN.GT.3) THEN
                  DO 121, I=3,IN-1
                      DIST(I) = EXP(-DISTF*0.01*DEPTH(I))
                      SUM     = SUM + DIST(I)
121               CONTINUE
              ENDIF

              !* compartment IN *!
              DIST(IN) = EXP(-DISTF*0.01*(DEPTH(IN)-0.5*THCKN(IN)
     $                      +RDINL*0.5))

              SUM = SUM + DIST(IN)
              !********** End of Crop 4.3 *******************************!

              !********** Crop 4.4 Amount of roots.            **********!
              ARTT  = 0.

              !* compartment 1 and 2 *!
              ART(1) = (WRT*DISTUP/SUM)*THCKN(1)/(THCKN(1)+THCKN(2))
              ART(2) = (WRT*DISTUP/SUM)*THCKN(2)/(THCKN(1)+THCKN(2))
              ARTT   = ART(1)+ART(2)

              !* compartment 3 to IN *!
              IF (IN.GT.3) THEN
                  DO 131, I=3,IN-1
                      ART(I) = WRT*DIST(I)/SUM
                      ARTT   = ARTT+ART(I)
131               CONTINUE
              ENDIF

              !* compartment IN *!
              ART(IN) = WRT*DIST(IN)/SUM
              ARTT    = ARTT+ART(IN)

              !* calculation check *!
              IF (ABS(ARTT-WRT).GT.1.0E-3) STOP
     $              ' ERROR IN CALC. OF ROOT DISTIB.'
              !********** End of Crop 4.4 *******************************!

              !********** Crop 4.5 Root length density.        **********!
              DO 141, I=1,IN
                  LRV(I) = ART(I) * 200. *1.E-3 / THCKN(I)
141           CONTINUE
              !********** End of Crop 4.5 *******************************!

          !********** End of Crop 4 *************************************!

          !********** Crop 5: Calculate nitrogen distribution. **********!

              !********** Crop 5.1 **************************************!
              !** Calculate max. N-levels and pot. N-accum. of grains. **!
              NMAXL  = AFGEN(INMXLN,NMAXLT,DVS)
              NMAXS  = AFGEN(INMXSN,NMAXST,DVS)
              NMAXR  = AFGEN(INMXRN,NMAXRT,DVS)
              PNARIG = AFGEN(IPNARN,PNARIT,TMPA)*1.0E-6
              !********** End of Crop 5.1 *******************************!

              !********** Crop 5.2: Calculate N-demand.        **********!
              NDEML  = AMAX1(NMAXL*WLV - ANLV,0.)
              NDEMS  = AMAX1(NMAXS*WST - ANST,0.)
              NDEMR  = AMAX1(NMAXR*WRT - ANRT,0.)
              NDEMG  = INSW(DVS-1.,0.,NUMGR*PNARIG*LAG)
              NDEMV  = NDEML + NDEMS + NDEMR

              IF (DVS.GT.1) THEN
                  NDEM = AMIN1(NDEMG,NDEMV)
              ELSE
                  NDEM = NDEMV
              ENDIF
              !********** End of Crop 5.2 *******************************!

              !********** Crop 5.3 **************************************!
              !** Calculate nitrogen uptake rate.                      **!
              RNULV  = (NDEML/(NDEMV+1.0E-9))*NUPTR
              RNUST  = (NDEMS/(NDEMV+1.0E-9))*NUPTR
              RNURT  = (NDEMR/(NDEMV+1.0E-9))*NUPTR
              !********** End of Crop 5.3 *******************************!

              !********** Crop 5.4 **************************************!
              !** Calculate N,C-translocation and N-accum.in grains.   **!
              ATNLV  = AMAX1(0.,ANLV - WLV*RNFLV)
              ATNST  = AMAX1(0.,ANST - WST*RNFST)
              ATNRT  = AMAX1(0.,ANRT - WRT*RNFRT)
              ATN    = ATNLV + ATNST + ATNRT

              NSUPG  = INSW(DVS-1.,0.,(ATN/TCNT) * TEF)
              RNAGR  = AMIN1(NDEMG,NSUPG)

              RNTRT  = RNAGR * ATNRT/(ATN+1.0E-9)
              RNTLV  = RNAGR * ATNLV/(ATN+1.0E-9)
              RNTST  = RNAGR * ATNST/(ATN+1.0E-9)
              RCTRT = 0.5 * RNTRT * 6.25
              RCTLV = 0.5 * RNTLV * 6.25
              RCTST = 0.5 * RNTST * 6.25
              !********** End of Crop 5.4 *******************************!

              !********** Crop 5.5 **************************************!
              !** Calculate max. N-uptake and N-demand.                **!
              MXUR   = PMXUPR * (1.-EXP(-0.5*(WLV+WST)/FC))

              IF (DVS.LT.1.) THEN
                  NDEM   = AMIN1(NDEM,MXUR)
              ELSE
                  NDEM   = AMIN1(NDEM,MXUR)*WRT/WRTANT
              ENDIF
              !********** End of Crop 5.5 *******************************!

              !********** Crop 5.6 **************************************!
              !** Calculate death rates and nitrogen losses.           **!
              IF (DVS.GT.1.) THEN
		  DRLV = AMAX1(WLV-WLVANT*((NFLV-RNFLV)/(NFLANT-RNFLV)),0.)
		  DRST = AMAX1(WST-WSTANT*((NFST-RNFST)/(NFSANT-RNFST)),0.)
		  DRRT = AMAX1(WRT-WRTANT*((NFRT-RNFRT)/(NFRANT-RNFRT)),0.)
              ELSE
                  DRLV = 0.
                  DRST = 0.
                  DRRT = 0.
              ENDIF

              RNLDLV = RNFLV*DRLV
              RNLDST = RNFST*DRST
              RNLDRT = RNFRT*DRRT
              !********** End of Crop 5.6 *******************************!
             

          !********** End of Crop 5 *************************************!

          !********** Crop 6: Integrations.                    **********!
 
          !* Development stages *!
          DVS1   = INTGRL(DVR1,DVS1)
          DVS2   = INTGRL(DVR2,DVS2)
          DVS    = DVS1 + DVS2

          !* Weights *!
          WLV    = INTGRL(GRLV-DRLV-RCTLV,WLV)
          WST    = INTGRL(GRST-DRST-RCTST,WST)
          WRT    = INTGRL(GRRT-DRRT-RCTRT,WRT)
          WGR    = INTGRL(GRGR,WGR)
          WRES   = AMAX1(0.,INTGRL(GRRES,WRES))
          WSD    = INTGRL(DRST,WSD)
          WLD    = INTGRL(DRLV,WLD)

          !* Rooting depth *!
          RD     = INTGRL(RRD,RD)

          !* Nitrogen amounts *!
          ANLV   = INTGRL(RNULV-RNTLV-RNLDLV,ANLV)
          ANST   = INTGRL(RNUST-RNTST-RNLDST,ANST)
          ANRT   = INTGRL(RNURT-RNTRT-RNLDRT,ANRT)
          ANGR   = INTGRL(RNAGR,ANGR)
          ANLD   = INTGRL(RNLDLV,ANLD)
          ANSD   = INTGRL(RNLDST,ANSD)
          ANRD   = INTGRL(RNLDRT,ANRD)
          !********** End of Crop 6 *************************************!

          !********** Crop 7: Some output parameters.          **********!
          WTOT   = WLV +WLD +WST +WSD +WGR +WRES
          NTOT   = ANLV+ANLD+ANST+ANSD+ANGR
          NFTOT  = NTOT/WTOT
          WSTOT  = WST + WSD + WRES
          NFSTOT = (ANST + ANSD)/(WSTOT+1.0E-9)

          NFGR   = ANGR/(WGR+1.0E-9)
          NFLV   = ANLV/(WLV+1.0E-9)
          NFST   = ANST/(WST+1.0E-9)
          NFRT   = ANRT/(WRT+1.0E-9)
          NFLD   = ANLD/(WLD+1.0E-9)
          !********** End of Crop 7 *************************************!

          !********** Crop 8: Output of results.               **********!
888       CALL CRPOUT (IDAY,WLV,WLD,WST,WRES,
     $    WSTOT,WRT,WGR,LAI,DVS,NFLV,NFLD,NFSTOT,NFGR,RD,NTOT,
     $	  WTOT,REMARK,RUNNAM,NMAXL,RED)
          !********** End of Crop 8 *************************************!

          IF (DVS.GT.2.) THEN
              TERMIN =.TRUE.
              WRITE (*,'(//2A)') ' Crop development completed, program',
     $                     '  finished'
          ENDIF

999   CONTINUE
      !**************************** End of Crop body ********************!

      RETURN
      END
!************************* End of CROP.FOR ******************************!
