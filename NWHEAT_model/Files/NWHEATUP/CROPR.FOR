!***************************** #file "CROPR.FOR" ************************!
!************************************************************************!
!* SUBROUTINE CROPR                                                     *!
!* Author     : Rob Groot                                               *!
!* Update     : Transformed for literate programming by Sjaak Wolfert.  *!
!*              08/30/94                                                *!
!* Purpose    : This subroutine reads crop specific data from the file  *!
!*              CROP.DAT.                                               *!
!************************************************************************!
      SUBROUTINE CROPR (WLV,WRT,WLD,WRES,WST,ANRT,ANLV,TADW,LAIANT,
     $           RD,TSUM2,MAINLV,MAINST,MAINRT,MAINSO,REFTMP,
     $           Q10RM,PMXUPR,EFCPR,EFCCH,EFCPRG,EFF,KDIF,
     $           LAT,RGRRES,FC,TCNT,RNFLV,RNFST,RNFRT,RGM,
     $           DISTF,TMPFT,FRTT,FLT,SLAT,LAISTT,LAIERT,
     $           TRDRTT,WRDRTT,TCTRT,LAGT,RESLAT,PGRIGT,
     $           NMAXST,NMAXLT,NMAXRT,PNARIT,
     $           ITMPFN,IFRTN,IFLN,ISLAN,ILAISN,ILAIRN,
     $           ITRDRT,IWRDRT,ITCTRN,ILAGN,IRESLN,IPGRIN,
     $           INMXSN,INMXLN,INMXRN,IPNARN)

      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)

      !*************** Dimension declarations ***************************!
      DIMENSION TMPFT(20)
      DIMENSION FRTT(22), FLT(28)
      DIMENSION SLAT(8)
      DIMENSION LAISTT(8), LAIERT(8)
      DIMENSION TRDRTT(16)
      DIMENSION WRDRTT(16)
      DIMENSION TCTRT(10)
      DIMENSION LAGT(8)
      DIMENSION RESLAT(8)
      DIMENSION PGRIGT(16)
      DIMENSION NMAXST(28), NMAXLT(8), NMAXRT(8)
      DIMENSION PNARIT(14)
      !*************** End of Dimension declarations ********************!

      !*************** Open data file with crop data ********************!
      OPEN(13,FILE='CROP.DAT',STATUS='OLD')
      !*************** End of Open data file with crop data *************!

      !*************** Read initial conditions **************************!
      CALL MOFILP(13)
      READ(13,*) WLV
      CALL MOFILP(13)
      READ(13,*) WRT
      CALL MOFILP(13)
      READ(13,*) WLD
      CALL MOFILP(13)
      READ(13,*) WRES
      CALL MOFILP(13)
      READ(13,*) WST
      CALL MOFILP(13)
      READ(13,*) ANRT
      CALL MOFILP(13)
      READ(13,*) ANLV
      CALL MOFILP(13)
      READ(13,*) TADW
      CALL MOFILP(13)
      READ(13,*) LAIANT
      CALL MOFILP(13)
      READ(13,*) RD
      !*************** End of Read initial conditions *******************!

      !*************** Read crop specific parameters ********************!
      !* temperature sum for post anthesis development
      CALL MOFILP(13)
      READ (13,*) TSUM2
      !* maintenance parameters
      CALL MOFILP(13)
      READ(13,*) MAINLV
      READ(13,*) MAINST
      READ(13,*) MAINRT
      READ(13,*) MAINSO
      !* maintenance reference temperature, and Q10
      CALL MOFILP(13)
      READ(13,*) REFTMP
      READ(13,*) Q10RM
      !* potential maximum nitrogen uptake rate (kg(N)/ha/d)
      CALL MOFILP(13)
      READ(13,*) PMXUPR
      !* Efficiency coefficients for the conversion of assimilates into
      !* proteins and into structural carbohydrates.
      CALL MOFILP(13)
      READ(13,*) EFCPR
      READ(13,*) EFCCH
      READ(13,*) EFCPRG
      !* photosynthesis parameters
      CALL MOFILP(13)
      READ(13,*) EFF
      READ(13,*) KDIF
      READ(13,*) LAT
      !* relative growth rate of reserves
      CALL MOFILP(13)
      READ(13,*) RGRRES
      !* conversion factor from weight to relative root extension
      CALL MOFILP(13)
      READ(13,*) FC
      !* time constant for nitrogen translocation
      CALL MOFILP(13)
      READ(13,*) TCNT
      !* Residual nitrogen fractions
      CALL MOFILP(13)
      READ(13,*) RNFLV
      READ(13,*) RNFST
      READ(13,*) RNFRT
      !* Potential root length growth rate
      CALL MOFILP(13)
      READ(13,*) RGM
      !* shape factor for exponential root distibution function
      CALL MOFILP(13)
      READ(13,*) DISTF
      !*************** End of Read crop specific parameters *************!

      !*************** Read function interpolation values ***************!

      !* Temperature response of photosynthesis
      CALL MOFILP(13)
      READ(13,*) ITMPFN
      READ(13,*) (TMPFT(I),TMPFT(I+1),I=1,ITMPFN-1,2)

      !* fraction of dry matter growth allocated to the diferent plant
      !* organs.
      !*Roots:
      CALL MOFILP(13)
      READ(13,*) IFRTN
      READ(13,*) (FRTT(I),FRTT(I+1),I=1,IFRTN-1,2)
      !* Leaves:
      CALL MOFILP(13)
      READ(13,*) IFLN
      READ(13,*) (FLT(I),FLT(I+1),I=1,IFLN-1,2)

      !* specific leaf area as function of development stage
      CALL MOFILP(13)
      READ(13,*) ISLAN
      READ(13,*) (SLAT(I),SLAT(I+1), I=1,ISLAN-1,2)

      !* Leaf Area Functions for stem are introduced as function of the
      !* temperaturesum, for ears as a function of DVS.
      !* !!!!!TEMPORARY CONSTRUCTION!!!!!!
      CALL MOFILP(13)
      READ(13,*) ILAISN
      READ(13,*) (LAISTT(I),LAISTT(I+1), I=1,ILAISN-1,2)
      CALL MOFILP(13)
      READ(13,*) ILAIRN
      READ(13,*) (LAIERT(I),LAIERT(I+1), I=1,ILAIRN-1,2)

      !* temperature reduction factor for root extension.
      CALL MOFILP(13)
      READ(13,*) ITRDRT
      READ(13,*) (TRDRTT(I),TRDRTT(I+1), I=1,ITRDRT-1,2)

      !* moisture reduction factor for root extension
      CALL MOFILP(13)
      READ(13,*) IWRDRT
      READ(13,*) (WRDRTT(I),WRDRTT(I+1), I=1,IWRDRT-1,2)

      !* Time constant for translocation of reserves dependent on
      !* reserve-level in plant
      CALL MOFILP(13)
      READ(13,*) ITCTRN
      READ(13,*) (TCTRT(I),TCTRT(I+1), I=1,ITCTRN-1,2)

      !* Lag phase for grain-filling in dependence of DVS
      CALL MOFILP(13)
      READ(13,*) ILAGN
      READ(13,*) (LAGT(I),LAGT(I+1), I=1,ILAGN-1,2)

      !* Lag phase for reserve flow
      CALL MOFILP(13)
      READ(13,*) IRESLN
      READ(13,*) (RESLAT(I),RESLAT(I+1), I=1,IRESLN-1,2)

      !* Potential growth rate of individual grains in dependence of
      !* temperature
      CALL MOFILP(13)
      READ(13,*) IPGRIN
      READ(13,*) (PGRIGT(I),PGRIGT(I+1), I=1,IPGRIN-1,2)

      !* Maximum nitrogen levels in leaves, stems and roots as function of
      !* development stage
      CALL MOFILP(13)
      READ(13,*) INMXSN
      READ(13,*) (NMAXST(I),NMAXST(I+1), I=1,INMXSN-1,2)
      CALL MOFILP(13)
      READ(13,*) INMXLN
      READ(13,*) (NMAXLT(I),NMAXLT(I+1), I=1,INMXLN-1,2)
      CALL MOFILP(13)
      READ(13,*) INMXRN
      READ(13,*) (NMAXRT(I),NMAXRT(I+1), I=1,INMXRN-1,2)

      !* potential nitrogen accumulation rates dependent on temperature
      CALL MOFILP(13)
      READ(13,*) IPNARN
      READ(13,*) (PNARIT(I),PNARIT(I+1), I=1,IPNARN-1,2)

      !*************** End of Read function interpolation values ********!

      RETURN
      END
!***************************** End of "CROPR.FOR" ***********************!
