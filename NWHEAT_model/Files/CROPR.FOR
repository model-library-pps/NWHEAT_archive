      SUBROUTINE CROPR (ICROP,WLV,WRT,WLD,WRES,WST,ANRT,ANLV,TADW,
     $           LAIANT,RD,TSUMEM,TSUMFI,TSUMDR,TSUMAN,TSUMMA,
     $           VBASE,VERSAT,SDAYL,IVERN,
     $           MAINLV,MAINST,MAINRT,MAINSO,REFTMP,
     $           Q10RM,PMXUPR,EFCPR,EFCCH,EFCPRG,EFF,KDIF,
     $           CO,RGRRES,FC,TCNT,RNFLV,RNFST,RNFRT,RGM,
     $           DISTF,TMPFT,FRTT,FLT,SLAT,LAISTT,LAIERT,CROPHTB,
     $           TRDRTT,WRDRTT,TCTRT,LAGT,RESLAT,PGRIGT,
     $           NMAXST,NMAXLT,NMAXRT,PNARIT,FAMAXTB,FSLATB,
     $           ITMPFN,IFRTN,IFLN,ISLAN,ILAISN,ILAIRN,ICRPN,
     $           ITRDRT,IWRDRT,ITCTRN,ILAGN,IRESLN,IPGRIN,
     $           INMXSN,INMXLN,INMXRN,IPNARN, IFAMN,IFSLN)


      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)


*------------------------ afgen array declarations -------------------------


      DIMENSION FRTT(26),FLT(32)
      DIMENSION RESLAT(12),TMPFT(28),TCTRT(14),SLAT(12)
      DIMENSION PGRIGT(20),TRDRTT(20),LAGT(12),NMAXST(32)
      DIMENSION NMAXRT(10), NMAXLT(12),LAISTT(12),LAIERT(12),PNARIT(18)
      DIMENSION WRDRTT(20),FAMAXTB(18), FSLATB(18),CROPHTB(14)


*---- open data file with crop data

      IF (ICROP .eq. 1) OPEN(14,FILE='CROP1.DAT',STATUS='OLD')
      IF (ICROP .eq. 2) OPEN(14,FILE='CROP2.DAT',STATUS='OLD')
      IF (ICROP .eq. 3) OPEN(14,FILE='CROP3.DAT',STATUS='OLD')
      IF (ICROP .eq. 4) OPEN(14,FILE='CROP4.DAT',STATUS='OLD')
      IF (ICROP .eq. 5) OPEN(14,FILE='CROP5.DAT',STATUS='OLD')
      IF (ICROP .eq. 6) OPEN(14,FILE='CROP6.DAT',STATUS='OLD')


*-------------------------- initial conditions -----------------------------

      CALL MOFILP(14)
      READ(14,*) WLV
      CALL MOFILP(14)
      READ(14,*) WRT
      CALL MOFILP(14)
      READ(14,*) WLD
      CALL MOFILP(14)
      READ(14,*) WRES
      CALL MOFILP(14)
      READ(14,*) WST
      CALL MOFILP(14)
      READ(14,*) ANRT
      CALL MOFILP(14)
      READ(14,*) ANLV
      CALL MOFILP(14)
      READ(14,*) TADW
      CALL MOFILP(14)
      READ(14,*) LAIANT
      CALL MOFILP(14)
      READ(14,*) RD


*------------------------- parameters --------------------------------------

*     temperature sums for emergence, floral initiation, and anthesis 
*      and from anthesis until maturity.

      CALL MOFILP(14)
      READ (14,*) TSUMEM
      CALL MOFILP(14)
      READ (14,*) TSUMFI
      CALL MOFILP(14)
      READ (14,*) TSUMDR
      CALL MOFILP(14)
      READ (14,*) TSUMAN
      CALL MOFILP(14)
      READ (14,*) TSUMMA

*     base and saturation number of vernalization days, saturating 
*     day length for photoperiod effects on phenology and 
*     vernalization factor

      CALL MOFILP(14)
      READ (14,*) VBASE
      CALL MOFILP(14)
      READ (14,*) VERSAT
      CALL MOFILP(14)
      READ (14,*) SDAYL
      CALL MOFILP(14)
      READ (14,*) IVERN

*     maintenance parameters

      CALL MOFILP(14)
      READ(14,*) MAINLV
      READ(14,*) MAINST
      READ(14,*) MAINRT
      READ(14,*) MAINSO

*     maintenance reference temperature, and Q10

      CALL MOFILP(14)
      READ(14,*) REFTMP
      READ(14,*) Q10RM 

*     potential maximum nitrogen uptake rate (kg(N)/ha/d)

      CALL MOFILP(14)
      READ(14,*) PMXUPR

*     Efficiency coefficients for the conversion of assimilates into
*     proteins and into structural carbohydrates. 

      CALL MOFILP(14)
      READ(14,*) EFCPR
      READ(14,*) EFCCH  
      READ(14,*) EFCPRG 

*     photosynthesis parameters and atmospheric CO2 concentration

      CALL MOFILP(14)
      READ(14,*) EFF 
      READ(14,*) KDIF 
      READ(14,*) CO

*     relative growth rate of reserves

      CALL MOFILP(14)
      READ(14,*) RGRRES 

*     conversion factor from weight to relative root extension

      CALL MOFILP(14)
      READ(14,*) FC

*     time constant for nitrogen translocation

      CALL MOFILP(14)
      READ(14,*) TCNT 

*     Residual nitrogen fractions

      CALL MOFILP(14)
      READ(14,*) RNFLV
      READ(14,*) RNFST 
      READ(14,*) RNFRT 

*     Potential root length growth rate

      CALL MOFILP(14)
      READ(14,*) RGM

*     shape factor for exponential root distibution function

      CALL MOFILP(14)
      READ(14,*) DISTF 


*------------------------- afgen function values ---------------------------

*     Temperature response of photosynthesis  

      CALL MOFILP(14)
      READ(14,*) ITMPFN
      READ(14,*) (TMPFT(I),TMPFT(I+1),I=1,ITMPFN-1,2)

*     fraction of dry matter growth allocated to the diferent plant organs 
*     Roots:

      CALL MOFILP(14)
      READ(14,*) IFRTN
      READ(14,*) (FRTT(I),FRTT(I+1),I=1,IFRTN-1,2)

*     Leaves:

      CALL MOFILP(14)
      READ(14,*) IFLN
      READ(14,*) (FLT(I),FLT(I+1),I=1,IFLN-1,2)

*     specific leaf area as function of development stage          

      CALL MOFILP(14)
      READ(14,*) ISLAN
      READ(14,*) (SLAT(I),SLAT(I+1), I=1,ISLAN-1,2)

*     Leaf Area Functions for stem are introduced as function of the
*     Temperaturesum, for ears as a function of DVS.
*     !!!!!TEMPORARY CONSTRUCTION!!!!!!

      CALL MOFILP(14)
      READ(14,*) ILAISN
      READ(14,*) (LAISTT(I),LAISTT(I+1), I=1,ILAISN-1,2)
      CALL MOFILP(14)
      READ(14,*) ILAIRN
      READ(14,*) (LAIERT(I),LAIERT(I+1), I=1,ILAIRN-1,2)

*    crop height as a function of DVS

      CALL MOFILP(14)
      READ(14,*) ICRPN
      READ(14,*) (CROPHTB(I),CROPHTB(I+1),I=1,ICRPN-1,2)

*     temperature reduction factor for root extension.

      CALL MOFILP(14)
      READ(14,*) ITRDRT
      READ(14,*) (TRDRTT(I),TRDRTT(I+1), I=1,ITRDRT-1,2)

*     moisture reduction factor for root extension 

      CALL MOFILP(14)
      READ(14,*) IWRDRT
      READ(14,*) (WRDRTT(I),WRDRTT(I+1), I=1,IWRDRT-1,2)

*     Time constant for translocation of reserves dependent on
*     reserve-level in plant

      CALL MOFILP(14)
      READ(14,*) ITCTRN
      READ(14,*) (TCTRT(I),TCTRT(I+1), I=1,ITCTRN-1,2)

*     Lag phase for grain-filling in dependence of DVS

      CALL MOFILP(14)
      READ(14,*) ILAGN
      READ(14,*) (LAGT(I),LAGT(I+1), I=1,ILAGN-1,2)

*     Lag phase for reserve flow

      CALL MOFILP(14)
      READ(14,*) IRESLN
      READ(14,*) (RESLAT(I),RESLAT(I+1), I=1,IRESLN-1,2)

*     Potential growth rate of individual grains in dependence of 
*     temperature

      CALL MOFILP(14)
      READ(14,*) IPGRIN
      READ(14,*) (PGRIGT(I),PGRIGT(I+1), I=1,IPGRIN-1,2)

*     Maximum nitrogen levels in leaves, stems and roots as function of
*     developmentstage

      CALL MOFILP(14)
      READ(14,*) INMXSN
      READ(14,*) (NMAXST(I),NMAXST(I+1), I=1,INMXSN-1,2)
      CALL MOFILP(14)
      READ(14,*) INMXLN
      READ(14,*) (NMAXLT(I),NMAXLT(I+1), I=1,INMXLN-1,2)
      CALL MOFILP(14)
      READ(14,*) INMXRN
      READ(14,*) (NMAXRT(I),NMAXRT(I+1), I=1,INMXRN-1,2)

*     potential nitrogen accumulation rates dependent on temperature

      CALL MOFILP(14)
      READ(14,*) IPNARN
      READ(14,*) (PNARIT(I),PNARIT(I+1), I=1,IPNARN-1,2)

*     correction factors dependent on atmospheric CO2 concentration
      
      CALL MOFILP(14)
      READ(14,*) IFAMN
      READ(14,*) (FAMAXTB(I), FAMAXTB(I+1), I=1, IFAMN-1,2)

      CALL MOFILP(14)
      READ(14,*) IFSLN
      READ(14,*) (FSLATB(I), FSLATB(I+1), I=1, IFSLN-1,2)
          
      CLOSE (14)

      RETURN
      END
