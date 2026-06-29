*---------------------------------------------------------------------------*
*  SUBROUTINE CROP                                                          *
*  Author : Rob Groot                                                       *
*  Date of last revision: 26 january 1989                                   *
*  Purpose: This subroutine simulates the dry matter increase and           *
*	    the nitrogen distribution for a winter wheat crop.              *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                    units  class         *
*  ----     -------                                    -----  -----         *
*  REMARK   output remark                                =                  *
*  RUNNAME  first 5 characters for name of output files         I           *
*  TMIN     minimum air temperature                      C      I           *
*  RDINL    Rooting depth in comp.where roots grow       cm     I           *
*  TMAX     maximum air temperature                      C      I           *
*  DAYL     astronomical daylength                       h      I           *
*  DAYLP    photoperiodically active daylength           h      I           *
*  PARDIR   array, flux density of direct  component of                     *
*	    PAR                                        W m-2    I           *
*  PARDIF   array, flux density of diffuse component of                     *
*	    PAR                                        W m-2    I           * 
*  SINB     array, sine of the solar elevation           =      I           *
*  IN       nr.of compartment is root growth occurs             O           *
*  ILAYER   number of soil compartments                  =      I           *
*  WCREL    array,relative watercontent of soil                             *
*	    compartment                               cm cm-3   I           *
*  DEPTH    array, middle of soil compartment measured                      *
*	    from top of soil profile                    cm      I           *
*  RDMSOL   maximum rooting depth                       cm      I           *
*  THCKN    array, thickness of soil compartments       cm      I           *
*  TRTOT    total transpiration during one day          cm      I           *
*  TRMAX    max. transpiration rate of crop           cm d-1    I           *
*  SOILTP   array, temperature of soil compartments     C       I           *
*  NUPTR    nitrogen uptake rate of the crop        kg ha-1 d-1 I           *
*  RD       rooting depth                               cm      O           *
*  LAITOT   total leaf area index of the crop         m2 m-2    O           *
*  DVS      crop development stage                       =      O           *
*  CROPHT   crop height                                  m      O           *
*  NDEM     nitrogen demand of the crop             kg ha-1 d-1 O           *
*  LRV      root density                              cm cm-3               *
*  CO       atmospheric CO2 concentration               mb      O           *
*  TERMIN   logical, simulation is finished when.TRUE.          O           *
*                                                                           *
*  SUBROUTINES called :                                                     *
*  CROPR    reads crop-specific data from file CROP.DAT                     *
*  PHENO    calculates crop development stage                               *
*  CRPOUT   writes crop status to output file CRPOUT.DAT                    *
*  ASSIM    calculates gross assimilation                                   *
*                                                                           *
*  FILE usage : none                                                        *
*---------------------------------------------------------------------------*

      SUBROUTINE CROP (REMARK,RUNNAME,IDAY,TMIN,RDINL,
     $                 TMAX,DAYL,DAYLP,PARDIR,PARDIF,SINB,IN,
     $                 ILAYER,WCREL,DEPTH,RDMSOL,THCKN,TRTOT,TRMAX,
     $                 SOILTP,NUPTR,RD,LAITOT,DVS,CROPHT,NDEM,LRV,
     $                 AVRAD,CO,TERMIN,START,INITI,STATR,ISOIL,ICROP,
     $                 IORGM,INSOIL)


      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)
      REAL INTGRL,INSW
      LOGICAL INIT,TERMIN,FLOWER,START, INITI
      CHARACTER REMARK*80,RUNNAME*5,STATR*5

      DIMENSION PARDIR(3),PARDIF(3),SINB(3),WCREL(ILAYER)
      DIMENSION DEPTH(ILAYER),THCKN(ILAYER)
      DIMENSION SOILTP(ILAYER),ART(10),LRV(10),DIST(10)

      DIMENSION FRTT(26),FLT(32)
      DIMENSION RESLAT(12),TMPFT(28),TCTRT(14),SLAT(12)
      DIMENSION PGRIGT(20),TRDRTT(20),LAGT(12),NMAXST(32)
      DIMENSION NMAXRT(10), NMAXLT(12),LAISTT(12),LAIERT(12),PNARIT(18)
      DIMENSION WRDRTT(20),FAMAXTB(18), FSLATB(18),CROPHTB(14)

      COMMON/CROPOUT/ ISOW1,ISTART1,IDANTH1, IDHALT, TWRT,TWST,TWLV,TWGR
      COMMON/CROPOUT/ TAGB1,HI,LAIMAX1,TPARAB1,RUE,NUPTAG1

      DATA FLOWER/.FALSE./

      
*--------------------------------------------------------------------------
*     initialization 
*--------------------------------------------------------------------------
      IF (INITI) INIT = .TRUE.
      IF (INITI) ISOW= IDAY
  
      IF (.NOT.INIT) GOTO 100

*---- read crop specific data from data-file CROP.DAT

      CALL CROPR (ICROP,WLV,WRT,WLD,WRES,WST,ANRT,ANLV,TADW,LAIANT,
     $            RD,TSUMEM,TSUMFI,TSUMDR,TSUMAN,TSUMMA,
     $            VBASE,VERSAT,SDAYL,IVERN,
     $            MAINLV,MAINST,MAINRT,MAINSO,REFTMP,
     $            Q10RM,PMXUPR,EFCPR,EFCCH,EFCPRG,EFF,KDIF,
     $            CO,RGRRES,FC,TCNT,RNFLV,RNFST,RNFRT,RGM,
     $            DISTF,TMPFT,FRTT,FLT,SLAT,LAISTT,LAIERT,CROPHTB,
     $            TRDRTT,WRDRTT,TCTRT,LAGT,RESLAT,PGRIGT,
     $            NMAXST,NMAXLT,NMAXRT,PNARIT,FAMAXTB,FSLATB,
     $            ITMPFN,IFRTN,IFLN,ISLAN,ILAISN,ILAIRN,ICRPN,
     $            ITRDRT,IWRDRT,ITCTRN,ILAGN,IRESLN,IPGRIN,
     $            INMXSN,INMXLN,INMXRN,IPNARN,IFAMN,IFSLN)

      NFLV = ANLV/(WLV+1.0E-9)
      NFRT = ANRT/(WRT+1.0E-9)


      FLOWER = .FALSE.
      ISTART = 0
      AVASS = 0.0
      GPHOT = 0.0
      RMAINT = 0.0
      DVS = 0.0
      DVS1 = 0.0
      DVS2 = 0.0
      WSD = 0.0
      WGR = 0.0
      WRES = 0.0
      WRD = 0.0
      ANGR = 0.0
      ANST = 0.0
      ANLD = 0.0
      ANSD = 0.0
      ANRD = 0.0
      GRST = 0.0
      DRST = 0.0
      GRRT = 0.0
      DRRT = 0.0
      GRLV = 0.0
      DRLV = 0.0
      GRGR = 0.0
      GRRES = 0.0
      RRD = 0.0
      RNAGR = 0.0
      RNULV = 0.0
      RNTLV = 0.0
      RNLDLV = 0.0
      RNUST = 0.0
      RNTST = 0.0
      RNLDST = 0.0
      RNURT = 0.0
      RNTRT = 0.0
      RNLDRT = 0.0
      NDEM = 0.0
      NUPTR = 0.0
      RCTLV = 0.0
      RCTST = 0.0
      RCTRT = 0.0

      NFST = 0.0
      TAGB = WLV + WLD + WST + WSD + WGR + WRES  
      TB = WLV + WLD + WST + WSD + WGR + WRES + WRT + WRD   
      WSTOT = WST + WSD + WRES
      SLA    = AFGEN(IFSLN,FSLATB,CO) * AFGEN(ISLAN,SLAT,DVS)* 1.0E-4 
      LAI    = WLV * SLA 
      LAITOT = LAI
      LAIMAX = 0.0
      NMAXL  = AFGEN(INMXLN,NMAXLT,DVS)
      NMAXS  = AFGEN(INMXSN,NMAXST,DVS)
      NMAXR  = AFGEN(INMXRN,NMAXRT,DVS)

      NFLD = 0.0
      NFTOT = 0.0
      NFSTOT = 0.0
      NFGR = 0.0
      NUPTAG = ANLV + ANLD + ANST + ANSD + ANGR
      NUPTT = ANLV + ANLD + ANST + ANSD + ANGR +ANRT + ANRD
      RED = 0.0
      TPARAB = 0.0
      PARABT = 0.0
      TPAR = 0.0
      AVRAD = 0.0
      RGRL= 0.006      
*---------------------------------------------------------------------------
*     Dynamic part of the model
*---------------------------------------------------------------------------
     
100   CONTINUE

*---- Subroutine PHENO is called to calculate the crop
*     development stage DVS
     
      TMPA = (TMIN+TMAX)/2.
      CALL PHENO (TMPA,TSUMEM,TSUMFI,TSUMDR,TSUMAN,TSUMMA,
     $    DAYLP,TMPSUM,DVR1,DVR2,VBASE,VERSAT,SDAYL,IVERN,INIT)
      INIT = .FALSE.
      
      IF (TMPSUM .LT. 125.) LAITOT= 0.
      IF (TMPSUM .LT. 100) GOTO 999
      IF (TMPSUM.LT.125.) START= .FALSE.
      
      IF (TMPSUM.GE.125) START= .TRUE. 

      
      CALL CRPOUT (IDAY,WLV,WLD,WST,WRES,
     $     WSTOT,WRT,WGR,LAITOT,DVS,NFLV,NFLD,NFSTOT,NFGR,RD,NUPTAG,
     $     NUPTT,TAGB,TB,TPARAB,TPAR,REMARK,RUNNAME,NMAXL,RED,
     $     TERMIN,START,TMPSUM,STATR,ISOIL,ICROP,IORGM,INSOIL)	

      
      IF (TMPSUM.LT.125.) GOTO 999
      IF (TERMIN) GOTO 999
*---------------------------------------------------------------------------   
*     canopy photosynthesis 
*---------------------------------------------------------------------------   

*---- In this version is assumed that during stem elongation the
*     LAIST increases linearly from 0 to 1, depending on the crop
*     development stage. The start of stem elongation is assumed to
*     occur at double ridge stage (TMPSUM=395). The phototsynthetic
*     capacity of stems is assumed to be the same as that of the
*     leaves. 

*      LAIST  = AFGEN(ILAISN,LAISTT,TMPSUM)*INSW(DVS-1.,1.,
*     $         AMIN1(1.,LAI/LAIANT))
*      LAIER  = AFGEN(ILAIRN,LAIERT,DVS)   *INSW(DVS-1.,1.,
*     $         AMIN1(1.,LAI/LAIANT))

      SLA    = AFGEN (ISLAN,SLAT,DVS)* 1.0E-4 * AFGEN(IFSLN,FSLATB,CO)
      GLAI= GRLV * SLA

*     growth not to exceed exponential growth curve during juvenile stage
      IF ((DVS .LT. 0.3) .AND. LAI .LT. 0.75) THEN      
      DTEFF= AMAX1(0.,TMPA - 0.)
      DELT= 1.
      GLAI = AMIN1((LAI * (EXP (RGRL*DTEFF*DELT)-1.))/DELT,
     $        GRLV * SLA)
      ENDIF

*     decrease in green leaf area by death and translocation
      DLAI= SLA * (DRLV + RCTLV) 
      LAI    = INTGRL(GLAI-DLAI,LAI) 
      
*      LAITOT = LAI + LAIST + LAIER
       LAITOT = LAI
       LAIMAX= AMAX1(LAITOT,LAIMAX)

*----- crop height (m)
       CROPHT= AFGEN(ICRPN,CROPHTB,DVS)

*---- AMAX depends on the nitrogen content of the leaves,
*     on the temperature according to Groot & Spiertz, 1989
*     and on the atmospheric CO2 concentration. 
      
      FAMAX = AFGEN(IFAMN,FAMAXTB, CO) 
      EAVT  = TMAX-0.25*(TMAX-TMIN)
      REDFOT= AFGEN(ITMPFN,TMPFT,EAVT)
      LAMAX = AMAX1(0.,1040.*NFLV-7.90) * FAMAX 
      PAMAX = AMAX1(0.5,(LAMAX*REDFOT))
*--- EFF depends on the atmospheric CO2 concentration
     
      COREF= 350.
      EFF1= EFF * ((CO - 40.)/(CO + 80.))/((COREF - 40.)/(COREF + 80.))
      CALL ASSIM (DAYL,PAMAX,EFF1,KDIF,LAITOT,PARDIR,PARDIF,SINB,
     $            DTGA,PARABT)
             
*---------------------------------------------------------------------------   
*     Rooting depth
*---------------------------------------------------------------------------   

*---- search for compartment where root growth takes place                    
*     plus penetration in deepest compartment

      DO 10,I=1,ILAYER

        IF (RD.GT.(DEPTH(I)-0.5*THCKN(I)).AND.
     $      RD.LE.(DEPTH(I)+0.5*THCKN(I))) IN=I

10    CONTINUE

      RDINL = RD-(DEPTH(IN-1)+0.5*THCKN(IN-1))


*---- rate of carbon translocation is calculated
       RCTRT = 0.5 * RNTRT * 6.25
       RCTLV = 0.5 * RNTLV * 6.25
       RCTST = 0.5 * RNTST * 6.25

*---------------------------------------------------------------------------   
*     maintenance respiration
*---------------------------------------------------------------------------   

      
      ACTLV  =AMAX1(0.,AMIN1(1.,(NFLV-RNFLV)/(NMAXL-RNFLV)))      
      ACTST  =AMAX1(0.,AMIN1(1.,(NFST-RNFST)/(NMAXS-RNFST)))
      ACTRT  =AMAX1(0.,AMIN1(1.,(NFRT-RNFRT)/(NMAXR-RNFRT)))

      TEF    = Q10RM**((TMPA-REFTMP)/10.)

      RMNLV  =WLV*MAINLV*TEF*ACTLV-RCTLV/AMAX1(0.25,EFCGGR)
      RMNST  =WST*MAINST*TEF*ACTST-RCTST/AMAX1(0.25,EFCGGR)
      RMNRT  =WRT*MAINRT*    ACTRT-RCTRT/AMAX1(0.25,EFCGGR)
      RMAINT =RMNLV+RMNST+RMNRT

*---------------------------------------------------------------------------   
*     Actual Carbohydrate flow
*---------------------------------------------------------------------------   

*---- Photosynthesis is reduced in case of water stress

      IF (TRTOT.GT.0) THEN
         RED = AMIN1(1.,TRTOT/(TRMAX+1.0E-9))
      ELSE
         RED = 1.
      ENDIF

      GPHOT = DTGA*(30./44.)*RED
      AVASS = AMAX1(0.,GPHOT-RMAINT)
      RMAINT= AMIN1(GPHOT,RMAINT) 
*---------------------------------------------------------------------------   
*     growth section
*---------------------------------------------------------------------------   

*---- Calculation of number of grains and LAI at anthesis

      IF((.NOT.FLOWER).AND.(DVS.GT.1.0)) THEN

         TADW   = WLV+WST+WRES 
         NUMGR  = 3500.E4 + 1.4E4*TADW
         LAIANT = LAI
         WLVANT = WLV
         WRTANT = WRT
         WSTANT = WST
         NFLANT = NFLV
         NFSANT = NFST
         NFRANT = NFRT

         FLOWER=.TRUE.
         IDANTH= IDAY 
      END IF


*---- Efficiency coefficients for the conversion of primary 
*     photosynthates into structural material

      PRCLV  = 6.25*NFLV
      PRCST  = AMIN1(AMAX1(0., 6.25*NFST),1.)
      PRCRT  = 6.25*NFRT
      PRCGR  = INSW(DVS-1.,PRCST,5.70*NFGR)
      EFCGLV = PRCLV*EFCPR + (1.-PRCLV)*EFCCH
      EFCGST = PRCST*EFCPR + (1.-PRCST)*EFCCH
      EFCGRT = PRCRT*EFCPR + (1.-PRCRT)*EFCCH
      EFCGGR = PRCGR*EFCPRG+ (1.-PRCGR)*EFCCH


*---- Assimilate distribution derived from field experiments

      FRT   = AFGEN(IFRTN,FRTT,DVS)
      FSH   = 1.-FRT
      FL    = FSH*AFGEN(IFLN,FLT,DVS)
      FST   = FSH*(1.-AFGEN(IFLN,FLT,DVS))

*----- check assimilate distribution
      FBAL = FRT + FL + FST - 1.0 
      IF (ABS(FBAL) .GE. 1.E-3) STOP
     $   ' assimilate distribution balance not 0, program aborted'
 
      IF (DVS.LT.1.) THEN

         RESLAG= AFGEN(IRESLN,RESLAT,DVS)

         GRLV  = FL *AVASS*EFCGLV
         GRRT  = FRT*AVASS*EFCGRT
         GRRES = RGRRES*RESLAG*WST
         GRST  = (FST*AVASS-GRRES)*EFCGST
              
         IF(FRT*AVASS.GT.0.) THEN


*---------- Rate of root extension is calculated. Extension only occurs 
*           when the rooting depth is less then the maximum rooting depth 
*           (RDMSOL). Rate of root extension depends on soil temperature
*           and soil moisture content

            IF (RD.LT.RDMSOL) THEN
               WREDRT = AFGEN (IWRDRT,WRDRTT,WCREL(IN))
               TREDRT = AFGEN (ITRDRT,TRDRTT,SOILTP(IN))
               RRD = AMIN1(RGM * TREDRT * WREDRT,RDMSOL - RD)
            ELSE
               RRD = 0.
            END IF

          ELSE
          RRD = 0.
                
         END IF

      ELSE


*------- after anthesis only grain dry matter increase occurs

         RESL  = WRES/(WLV+WST+WRES)
         TCTR  = AFGEN(ITCTRN,TCTRT,RESL)
         LAG   = AFGEN(ILAGN,LAGT,DVS)

         PGRIG = AFGEN(IPGRIN,PGRIGT,TMPA) * 1.E-6
         CDEMG = NUMGR * PGRIG * (1.0/EFCGGR) * LAG
         CSUPG = AVASS+WRES/TCTR
         FCHTG = AMIN1(CDEMG,CSUPG)
         GRGR  = FCHTG*EFCGGR

         IF(CDEMG.LT.CSUPG) THEN
            GRRES = AVASS - CDEMG 
	 ELSE
            GRRES = -(WRES/TCTR)     
         END IF

         GRST  = 0.
         GRLV  = 0.
         GRRT  = 0.
         RRD   = 0.
	
      END IF

*---- check carbon balance in plant

      CBAL = GPHOT - RMAINT -GRLV/EFCGLV -GRRT/EFCGRT -GRRES 
     $      -GRST/EFCGST -GRGR/EFCGGR
    
    
      IF (ABS(CBAL) .GE. 1.E-3) STOP
     $   '  carbon balance in plant not 0, program aborted'

*---- Calculate root distribution. ASSUME root density decreases
*     exponentially down the profile. Upper two layers are treated
*     as a single compartment in this calculation
*     This root distribution is only used when detailed nitrogen
*     uptake routine UPTA is used


      ARTT  = 0.
      SUM   = 0.
      DISTUP = EXP(-DISTF*0.01*((THCKN(1)+THCKN(2))/2.))
      SUM    = SUM +DISTUP

      IF (IN.GT.3) THEN

         DO 121, I=3,IN-1
            DIST(I) = EXP(-DISTF*0.01*DEPTH(I))
            SUM     = SUM + DIST(I)
121      CONTINUE

      ENDIF

      DIST(IN) = EXP(-DISTF*0.01*(DEPTH(IN)-0.5*THCKN(IN)
     $           +RDINL*0.5))
      SUM      = SUM + DIST(IN)

      ART(1) = (WRT*DISTUP/SUM)*THCKN(1)/(THCKN(1)+THCKN(2))
      ART(2) = (WRT*DISTUP/SUM)*THCKN(2)/(THCKN(1)+THCKN(2))
      ARTT   = ART(1)+ART(2)

      IF (IN.GT.3) THEN
         DO 131, I=3,IN-1
            ART(I) = WRT*DIST(I)/SUM
            ARTT   = ARTT+ART(I)
131      CONTINUE
      ENDIF

      ART(IN) = WRT*DIST(IN)/SUM
      ARTT    = ARTT+ART(IN)

      IF (ABS(ARTT-WRT).GT.1.0E-3) STOP
     $  ' ERROR IN CALC. OF ROOT DISTIB.'

*---- Assume a Specific Root Length of 200m/g
*     the factor 1.exp-3 is to convert kg/ha to
*     cm/cm2

      DO 141, I=1,IN
         LRV(I) = ART(I) * 200. *1.E-3 / THCKN(I)
141   CONTINUE


*---------------------------------------------------------------------------   
*     nitrogen distribution 
*---------------------------------------------------------------------------   

*---- Maximum nitrogen levels in various organs and potential 
*     N-accumulation rate of individual grains

      NMAXL  = AFGEN(INMXLN,NMAXLT,DVS)
      NMAXS  = AFGEN(INMXSN,NMAXST,DVS)
      NMAXR  = AFGEN(INMXRN,NMAXRT,DVS)
      PNARIG = AFGEN(IPNARN,PNARIT,TMPA)*1.0E-6

*---- Calculation of N-demands of the various organs

      NDEML  = AMAX1(NMAXL*WLV - ANLV,0.)
      NDEMS  = AMAX1(NMAXS*WST - ANST,0.)
      NDEMR  = AMAX1(NMAXR*WRT - ANRT,0.)
      NDEMG  = INSW(DVS-1.,0., NUMGR*PNARIG*LAG)

      IF (START .AND. ISTART .EQ. 0) ISTART = IDAY

      IF (ISTART .EQ. IDAY) THEN
         NDEMLO = NDEML
         NDEMSO = NDEMS
         NDEMRO = NDEMR
         NDEMVO = NDEMLO + NDEMSO + NDEMRO
      ENDIF

*---- N-Demand of vegetative material

      NDEMV  = NDEML + NDEMS + NDEMR

      IF (DVS.GT.1) THEN
         NDEM = AMIN1(NDEMG,NDEMV)
      ELSE
         NDEM = NDEMV
      ENDIF


*---- Amount of translocatable nitrogen of the various organs

      ATNLV  = AMAX1(0.,ANLV - WLV*RNFLV)
      ATNST  = AMAX1(0.,ANST - WST*RNFST)
      ATNRT  = AMAX1(0.,ANRT - WRT*RNFRT)
      ATN    = ATNLV + ATNST + ATNRT 

*---- N-supply for the grains

      NSUPG  = INSW(DVS-1.,0.,ATN/TCNT * TEF)

*---- Rate of nitrogen accumulation in grains

      RNAGR  = AMIN1(NDEMG,NSUPG)

*---- Rate of nitrogen translocation is calculated for the organs

      RNTRT  = RNAGR * ATNRT/(ATN+1.0E-9)
      RNTLV  = RNAGR * ATNLV/(ATN+1.0E-9)
      RNTST  = RNAGR * ATNST/(ATN+1.0E-9)


*---- nitrogen uptake rates of the organs

      RNULV  = (NDEMLO/(NDEMVO+1.0E-9))*NUPTR
      RNUST  = (NDEMSO/(NDEMVO+1.0E-9))*NUPTR
      RNURT  = (NDEMRO/(NDEMVO+1.0E-9))*NUPTR

      IF (NDEMVO .EQ. 0.) THEN
         RNULV = 0.6 * NUPTR
         RNUST = 0.2 * NUPTR
         RNURT = 0.2 * NUPTR
      ENDIF
 
*---- Death rates of the various organs

      DRST = 0.
      DRLV2 = 0.
      DRRT = 0.

*---- death rate of leaves due to water stress, high LAI,
*       ageing or nitrogen shortage
      DRLV1= (1. - RED) * WLV * 0.03
      IF (DVS.GT. 1.5)
     $ DRLV1 = (1. - RED) * WLV * 0.08

      IF (DVS.GT.1.) 
     $ DRLV2 = AMAX1(WLV-WLVANT*((NFLV-RNFLV)/(NFLANT-RNFLV)),0.)
      
      DRLV= AMAX1(DRLV1,DRLV2)     


      IF (DVS .GT. 1.)  DRRT = 0.01 * WRT 
      
      IF (DVS.GT.1.5)  DRST = 0.02 * WST


*---- nitrogen losses caused by death of vegetative material

      RNLDLV = RNFLV*DRLV
      RNLDST = RNFST*DRST
      RNLDRT = RNFRT*DRRT


*---- maximum nitrogen uptake rate by the crop
     

      MXUR   = PMXUPR * (1.-EXP(-0.5*(WLV+WST)/FC))
      
      IF (DVS.LT.1.) THEN
         NDEM   = AMIN1(NDEM,MXUR)
      ELSE
         NDEM   = AMIN1(NDEM,MXUR)*WRT/WRTANT
      ENDIF


*---------------------------------------------------------------------------   
*     Integrations
*---------------------------------------------------------------------------   
      
*---- development stages

      DVS1   = INTGRL(DVR1,DVS1)
      DVS2   = INTGRL(DVR2,DVS2)
      DVS    = DVS1 + DVS2

*---- weights
      
      WST    = INTGRL(GRST-DRST-RCTST,WST)
      WSD    = INTGRL(DRST,WSD)
      WRT    = INTGRL(GRRT-DRRT-RCTRT,WRT)
      WRD    = INTGRL(DRRT,WRD)
      WLV    = INTGRL(GRLV-DRLV-RCTLV,WLV)

      WGR    = INTGRL(GRGR,WGR)
      WLD    = INTGRL(DRLV,WLD)
      WRES   = AMAX1(0.,INTGRL(GRRES,WRES))

      RD     = INTGRL(RRD,RD)

*---- amounts of nitrogen

      ANGR   = INTGRL(RNAGR,ANGR)
      ANLV   = INTGRL(RNULV-RNTLV-RNLDLV,ANLV)
      ANLD   = INTGRL(RNLDLV,ANLD)
      ANST   = INTGRL(RNUST-RNTST-RNLDST,ANST)
     
      ANSD   = INTGRL(RNLDST,ANSD)
      ANRT   = INTGRL(RNURT-RNTRT-RNLDRT,ANRT)
      ANRD   = INTGRL(RNLDRT,ANRD)

*----- total incoming and intercepted PAR
       
       TPARAB = INTGRL(PARABT,TPARAB)
       TPAR = INTGRL(0.5*AVRAD,TPAR)
      
*---------------------------------------------------------------------------
*     some output parameters to compare model output with observed
*     field data
*---------------------------------------------------------------------------

      TB     = WLV +WLD +WST +WSD +WGR +WRES +WRT +WRD
      TAGB   = WLV +WLD +WST +WSD +WGR +WRES
      NUPTT  = ANLV+ANLD+ANST+ANSD+ANGR+ANRT+ANRD
      NUPTAG = ANLV+ANLD+ANST+ANSD+ANGR
      NFTOT  = NUPTAG/TAGB
      WSTOT  = WST + WSD + WRES             
      NFSTOT = (ANST + ANSD)/(WSTOT+1.0E-9) 
      IF (WSTOT .LE. 0.1) NFSTOT = 0.0

*---- Nitrogen fractions of the organs

      NFGR   = ANGR/(WGR+1.0E-9)
      NFLV   = ANLV/(WLV+1.0E-9)
      NFST   = ANST/(WST+1.0E-9)	
      NFRT   = ANRT/(WRT+1.0E-9)
      NFLD   = ANLD/(WLD+1.0E-9)	

*----- store old values for N demand of the various organs (only vegetative)

      NDEMLO = NDEML
      NDEMSO = NDEMS
      NDEMRO = NDEMR
      NDEMVO = NDEMLO + NDEMSO + NDEMRO

*---------------------------------------------------------------------------
*     output of simulation results 
*---------------------------------------------------------------------------

      


*---- finish condition of simulation                                    

      IF (DVS.GT.2.) THEN

         TERMIN =.TRUE.

       
         WRITE (*,'(//2A)') ' Crop development completed '

      ENDIF



999   CONTINUE

      IF (TERMIN) THEN
      TWRT= WRT + WRD
      TWST= WSTOT
      TWLV= WLV + WLD
      TWGR= WGR
      TAGB1= TAGB
      HI= WGR/TAGB
      IDHALT= IDAY
      IDANTH1= IDANTH
      ISOW1= ISOW
      ISTART1= ISTART
      LAIMAX1= LAIMAX
      NUPTAG1= NUPTAG
*-----Radiation absorbed in MJ m-2
      TPARAB1= TPARAB/10.**6
*------radiation use efficiency in g above-ground D.M./radiation abs. in MJ
      RUE= (TAGB*10.**5)/TPARAB
      ENDIF

      RETURN                                                   
      END






*---------------------------------------------------------------------------*
*  SUBROUTINE PHENO                                                         *
*  Author : Rob Groot                                                       *
*  Date   : november 1985                                                   *
*  Purpose: This subroutine calculates the development rate of the          *
*	    crop based on average daily temperatures and daylength.         *
*           References to literature :                                      *
*           Weir, A.H., P.L. Bragg, J.R. Porter & J.H. Rayner, 1984.        *
*              A winter wheat crop simulation model without water or        *
*              nutrient limitations. J.Agric. Sci.Camb. 102: 371-382.       *
*           Porter, J.R., 1984. A model of canopy development in            *
*              winter wheat. J.agric.Sci.,Camb., 102,383-392.               *
*           Reinink, K., I. Jorritsma & A. Darwinkel, 1986.                 *
*              Adaptation of the AFRC wheat phenology model for Dutch       *
*              conditions. Neth. J. Agric. Sc., 34:1-13.                    *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                    units  class         *
*  ----     -------                                    -----  -----         *
*  TMPA     average daily air temperature               C       I           *
*  TSUMEM    required degree days for emergence         C.day   I           *
*  TSUMFI    required degree days for floral initiat.   C.day   I           *
*  TSUMDR    required degree days for double ridge      C.day   I           *
*  TSUMAN    required degree days for anthesis          C.day   I           *
*  TSUMMA    required degree days from anth.to maturity C.day   I           *
*  DAYLP    photosynthetically active daylength         h       I           *
*  TMPSUM   temperature sum                             C.day   O           *
*  DVR1     pre- anthesis development rate              day-1   O           *
*  DVR2     post-anthesis development rate              day-1   O           *
*  VBASE    base number of vernalization days                   I           *
*  VERSAT   saturation number of vernalization days             I           *
*  SDAYL    saturating day length for photoperiod               I           *
*                   effects on phenology                 h      I           *
*  IVERN    0 = no vernalization effect (spring wheat),         I           *
*           1 = vernalization effect (winter wheat)                         *                                                *
*  SUBROUTINES and FUNCTIONS called : none                                  *
*  FILE usage : none                                                        *
*---------------------------------------------------------------------------*

      SUBROUTINE PHENO (TMPA,TSUMEM,TSUMFI,TSUMDR,TSUMAN,TSUMMA,
     $  DAYLP,TMPSUM,DVR1,DVR2,VBASE,VERSAT,SDAYL,IVERN,INIT)

      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)
      REAL INTGRL,INSW
      LOGICAL INIT
      DIMENSION VERNRT(16)


*---- Vernalization rate as a function of temperature

      DATA VERNRT/-20.,0.,  -4.,0., 0., 1., 3.,1., 7., 0.8, 
     $             9.,0.4,18.,0.0, 40.0,0.0/

      DATA IVERRN/16/	
    

      IF (INIT) THEN
      VERN = 0.0
      TMPSUM = 0.0
      ENDIF

*---- Vernalizationfactor

      VERNR = AFGEN(IVERRN,VERNRT,TMPA)
      VERN  = INTGRL(INSW(TMPSUM-TSUMFI,VERNR,0.),VERN)
      VERNF = LIMIT(0.,1.,(VERN-VBASE)/(VERSAT-VBASE))
      IF (IVERN .EQ. 0) VERNF = 1.

*---- Photoperiodfactor

      BDAYL = INSW(TMPSUM-TSUMDR,0.,7.)
      PULSE = INSW(TMPSUM-TSUMEM,0.,1.)
      FPERF = LIMIT(0.,1.,(DAYLP-BDAYL)/(SDAYL-BDAYL)) * PULSE


*---- accumulated temperature

      TMPB1 = AMAX1(0.,TMPA-1.)
      TMPB9 = AMAX1(0.,TMPA-9.)
      TMPCOR= INSW(TMPSUM-TSUMAN,TMPB1*VERNF*FPERF,TMPB9)
      TMP   = INSW(TMPSUM-TSUMEM,TMPB1,TMPCOR)
      TMPSUM= INTGRL(TMP,TMPSUM)


*---- developmentrate

      DVR1   = INSW(TMPSUM-TSUMAN,INSW(TMPSUM-TSUMEM,0.,TMP/
     $     (TSUMAN-TSUMEM)),0.)
      DVR2   = INSW(TMPSUM-TSUMAN,0.,TMP/TSUMMA)
 

      RETURN
      END
