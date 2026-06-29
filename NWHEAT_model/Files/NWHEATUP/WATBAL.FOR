!***********************#file "WATBAL.FOR" ******************************!
!************************************************************************!
!*  SUBROUTINE WATBAL                                                   *!
!*  Author : Rob Groot                                                  *!
!*  Date   : december 1986                                              *!
!*  Purpose: This subroutine simulates soil water status. The soil is   *!
!*	      devided into a specified number of soil compartments of    *!
!*	      variable thickness.                                        *!
!*                                                                      *!
!*	    The following processes are taken into account:              *!
!*	    - soil surface evaporation                                   *!
!*	    - distribution of soil evaporative losses over the soil      *!
!*	      compartments                                               *!
!*	    - infiltration and rainfall                                  *!
!*	    - crop transpiration                                         *!
!*                                                                      *!
!*           References to literature:                                  *!
!*             van Keulen & Seligman, 1987. Simulation of water use,    *!
!*	        nitrogen nutrition and growth of a spring wheat crop.    *!
!*             Pudoc, Wageningen, in press.                             *!
!*                                                                      *!
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *!
!*  name     meaning                                    units  class    *!
!*  ----     -------                                    -----  -----    *!
!*  RD       rooting depth                               cm      I      *!
!*  IDAY     julian daynumber                             =      I,T    *!
!*  REMARK   output remarks                               =      I      *!
!*  RUNNAM   first 5 characters for name of output files         I      *!
!*  ET0      pot. evaporation of bare soil surface     cm d-1    I      *!
!*  LAITOT   total leaf area index of the crop         m2 m-2    I      *!
!*  RAIN     daily precipitation                       cm d-1    I      *!
!*  TRR      array, transpiration rate from each of the                 *!
!*	      soil compartments                         cm d-1    I      *!
!*  TRTOT    total transpiration during one day          cm      I      *!
!*  WCREL    array, rel. watercontent of soil comp.    cm cm-3   O      *!
!*  RDMSOL   maximum rooting depth                       cm      O      *!
!*  THCKN    array, thickness of soil compartments       cm      O      *!
!*  DEPTH    array, middle of soil compartment measured                 *!
!*	      from top of soil profile                    cm      O      *!
!*  ILAYER   number of soil compartment                   =      O      *!
!*  TTRANS   total transpiration over sim. period        cm      O      *!
!*  TDRAIN   total drainage over simulated period        cm      O      *!
!*  TRAIN    total rainfall over simulated period        cm      O      *!
!*  FLOW     array, rate of waterflow between soil                      *!
!*	      compartments                              cm d-1    O      *!
!*  TESOIL   total evaporation over simulated period     cm      O      *!
!*  WCACT    array, act.water content of compartment   cm cm-1   O      *!
!*  WCACT0   array, act.water content of compartment                    *!
!*	      before routine is executed                cm cm-1   O      *!
!*  RHOD     array, bulk density of soil compartments   g cm-3   O      *!
!*  RESOIL   array, evaporation rate for each soil                      *!
!*	      compartment                                 cm      O      *!
!*  WCWILT   array, water content at wilting point for                  *!
!*	      each soil compartment                     cm cm-3   O      *!
!*  WCFLDC   array, soil moisture content at field                      *!
!*	      capacity for each soil compartment        cm cm-1   O      *!
!*  TRMAX    maximum transpiration rate of crop        cm d-1    O      *!
!*                                                                      *!
!*  SUBROUTINES called :                                                *!
!*  SOILR    reads physical soil data from data file                    *!
!*	      SOIL.DAT                                                   *!
!*  EVSOIL   calculates soil surface evaporation and distributes        *!
!*	      evaporation over soil compartments                         *!
!*  WATOUT   writes soil water status to output file WATOUT.DAT         *!
!*  FILE usage : none                                                   *!
!************************************************************************!

      SUBROUTINE WATBAL (RD,IDAY,REMARK,RUNNAM,ET0,LAITOT,
     $                   RAIN,TRR,TRTOT,WCREL,RDMSOL,THCKN,DEPTH,
     $                   ILAYER,TTRANS,TDRAIN,TRAIN,FLOW,TESOIL,WCACT,
     $                   WCACT0,RHOD,RESOIL,WCWILT,WCFLDC,TRMAX)

      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)
      REAL INTGRL
      LOGICAL INIT,STEP1
      CHARACTER REMARK*80,RUNNAM*5

      DIMENSION WCFLDC(10), WCWILT(10), WCAIR(10)
      DIMENSION WCINI(10) , WCACT(10) , WCREL(10) , WCACT0(10)
      DIMENSION RESOIL(10), TRR(10)   , THCKN(10)  , DEPTH(10)
      DIMENSION SMTB1(30) , SMTB2(30) , SMTB3(30)  ,ITEXCL(10)
      DIMENSION SMTB4(30) , SMTB5(30) , FLOW(11)   ,RHOD(10)
      DIMENSION WREDT(14) , EDPTT(10)
      PARAMETER (KDIF=.7155)
      DATA INIT/.TRUE./
      DATA STEP1/.TRUE./


*----------------------------------------------------------------------------
*     Data initialization of soil water balance
*----------------------------------------------------------------------------


      IF (.NOT.(INIT)) GO TO 200

      INIT = .FALSE.


*---- read physical soil properties from soil data file SOIL.DAT


      CALL SOILR (IWREDN,IEDPTN,ISMTB1,ISMTB2,ISMTB3,ISMTB4,ISMTB5,
     $            WREDT,EDPTT,SMTB1,SMTB2,SMTB3,SMTB4,SMTB5,THCKN,
     $            RHOD,ITEXCL,ILAYER,RDMSOL,PROP,WCINI)


*---- pF characteristics of the boundary water contents


      PFMAX =  0.
      PFFLDC = 2.
      PFWILT = 4.2


*----------------------------------------------------------------------------
*     Initialization  of water balance
*----------------------------------------------------------------------------

*---- for the soil, the water holding characteristics are
*     computed


      DO 10,I=1,ILAYER

*------- in case a compartment is of texture class one

         IF (ITEXCL(I).EQ.1) THEN
            WCFLDC(I) = AFGEN (ISMTB1,SMTB1,PFFLDC)
            WCWILT(I) = AFGEN (ISMTB1,SMTB1,PFWILT)
            WCAIR(I)  = WCWILT(I)/3.
         END IF

*------- in case a compartment is of texture class two

         IF (ITEXCL(I).EQ.2) THEN
            WCFLDC(I) = AFGEN (ISMTB2,SMTB2,PFFLDC)
            WCWILT(I) = AFGEN (ISMTB2,SMTB2,PFWILT)
            WCAIR(I)  = WCWILT(I)/3.
         END IF

*------- in case a compartment is of texture class three

         IF (ITEXCL(I).EQ.3) THEN
            WCFLDC(I) = AFGEN (ISMTB3,SMTB3,PFFLDC)
            WCWILT(I) = AFGEN (ISMTB3,SMTB3,PFWILT)
            WCAIR(I)  = WCWILT(I)/3.
         END IF

*------- in case a compartment is of texture class four

         IF (ITEXCL(I).EQ.4) THEN
            WCFLDC(I) = AFGEN (ISMTB4,SMTB4,PFFLDC)
            WCWILT(I) = AFGEN (ISMTB4,SMTB4,PFWILT)
            WCAIR(I)  = WCWILT(I)/3.
         END IF

*------- in case a compartment is of texture class five

         IF (ITEXCL(I).EQ.5) THEN
            WCFLDC(I) = AFGEN (ISMTB5,SMTB5,PFFLDC)
            WCWILT(I) = AFGEN (ISMTB5,SMTB5,PFWILT)
            WCAIR(I)  = WCWILT(I)/3.
         END IF

10    CONTINUE


*---- calculation of depth of subsequent compartments


      DEPTH(1)=0.5*THCKN(1)
      DO 20,I=2,ILAYER
         DEPTH(I)=DEPTH(I-1)+0.5*THCKN(I-1)+0.5*THCKN(I)
20    CONTINUE


*----------------------------------------------------------------------------
*     initialization of water contents
*----------------------------------------------------------------------------

*---- initialization of actual water contents cm3 cm-3


      DO 30,I=1,ILAYER
         WCACT(I) = WCINI(I)
30    CONTINUE


*----------------------------------------------------------------------------
*     dynamic section of water balance
*----------------------------------------------------------------------------


200   CONTINUE


      IF (.NOT.(STEP1)) GOTO 1000
      STEP1 = .FALSE.


*-----------------------------------------------------------------------------
*     STEP1
*
*     soil water balance is executed twice during one time step:
*     in step1 the redistribution of pricipitation is calculated
*     and moisture contents of compartments when soil is at its moist
*     are transferred to main program.
*     mineralization and immobilization are calculated at these moisture
*     contents. in step2, moisture withdrawal through transpiration and
*     evaporation are subtracted from moisture contents
*-----------------------------------------------------------------------------

*---- total amount of water in profile before calculations


      TWAT1 = 0.
      DO 40,I=1,ILAYER
        TWAT1 = TWAT1+WCACT(I)*THCKN(I)
40    CONTINUE


*---- save actual water contents in array WCACT0 for use in soil
*     nitrogen routine NITBAL


      DO 50,I=1,ILAYER
         WCACT0(I) = WCACT(I)
50    CONTINUE


*---- 'waterfall' infiltration of precipitation. This procedure is
*     executed only when infiltration occurs


      DO 60,I=1,ILAYER+1
         FLOW(I) = 0.
60    CONTINUE

      IF (RAIN.GT.0.) THEN
         FLOW(1) = RAIN
         DO 70,I=1,ILAYER


*---------- water holding capacity of the compartment (cm)


            CAP = (WCFLDC(I)-WCACT(I))*THCKN(I)


*---------- if the capacity of the compartment is smaller than the
*           infiltration, the layer is filled up to field capacity,
*           if not the layer is filled with all that is left of the
*           infiltration.


            IF (CAP.LE.FLOW(I)) THEN
               WCACT(I) = WCFLDC(I)
               FLOW(I+1)= FLOW(I)-CAP
            ELSE
               WCACT(I) = WCACT(I)+FLOW(I)/THCKN(I)
               FLOW(I+1)=0.
            END IF

70       CONTINUE

      END IF


*---- integrals for drainage and precipitation


      TDRAIN = INTGRL (FLOW(11),TDRAIN)
      TRAIN  = INTGRL (RAIN,TRAIN)


*---- relative water content, available for crop moisture uptake


      DO 75,I=1,ILAYER
         WCREL(I)=MAX(0.,(WCACT(I)-(WCWILT(I)+1.0E-9))/
     $           (WCFLDC(I)-(WCWILT(I)+1.0E-9)))
75    CONTINUE


      GOTO 2000


*----------------------------------------------------------------------------
*     STEP2
*----------------------------------------------------------------------------


1000  continue


*---- Rooting depth


*-----search for compartment where root growth takes place
*     plus penetration in deepest compartment


      IF (RD.GT.0.AND.LAITOT.GT.0.) THEN


         DO 11,I=1,ILAYER
            IF (RD.GT.(DEPTH(I)-0.5*THCKN(I)).AND.
     $      RD.LE.(DEPTH(I)+0.5*THCKN(I))) IN=I
11       CONTINUE

         RDINL = RD-(DEPTH(IN-1)+0.5*THCKN(IN-1))


*------- calculate transpiration rates


         TRMAX = ET0*(1.-EXP(-0.75*KDIF*LAITOT))
         ER = 0.
         DO 21,I=1,IN-1
            ER = ER+THCKN(I)*AFGEN(IEDPTN,EDPTT,WCREL(I))
21       CONTINUE

         ER = ER+RDINL*AFGEN(IEDPTN,EDPTT,WCREL(IN))
         TRPCM = TRMAX/ER

         TRTOT = 0.
         DO 31,I=1,ILAYER
            IF (I.LE.IN-1) THEN
               EDPTF  = AFGEN(IEDPTN,EDPTT,WCREL(I))
               WRED   = AFGEN(IWREDN,WREDT,WCREL(I))
               TRR(I) = TRPCM*EDPTF*WRED
               TRTOT  = TRTOT+TRR(I)*THCKN(I)
            ELSE
               TRR(I) = 0.
            END IF
31       CONTINUE

         EDPTF   = AFGEN(IEDPTN,EDPTT,WCREL(IN))
         WRED    = AFGEN(IWREDN,WREDT,WCREL(IN))
         TRR(IN) = TRPCM*EDPTF*WRED
         TRTOT   = TRTOT+TRR(IN)*RDINL

      ELSE

         DO 41, I=1,ILAYER
            TRR(I) = 0.
41       CONTINUE

      END IF


*---- transpiration is subtracted from the soil water status


      DO 80,I=1,ILAYER
         WCACT(I) = WCACT(I)-TRR(I)
80    CONTINUE


      IF (IN.NE.0) THEN
          WCACT(IN) = WCACT(IN)+TRR(IN)*(1.-RDINL/THCKN(IN))
      ENDIF

*---- subroutine EVSOIL is called to compute the soil evaporation
*     from each soil compartment


      CALL EVSOIL (LAITOT,ET0,ILAYER,WCACT,WCFLDC,WCAIR,THCKN,
     $             PROP,DEPTH,ESOIL,RESOIL)


*---- soil surface evaporation is subtracted from the soil water status


      DO 85,I=1,ILAYER
         WCACT(I) = WCACT(I)-RESOIL(I)
85    CONTINUE


*---- integrals of soil water balance


      TTRANS = INTGRL (TRTOT,TTRANS)
      TESOIL = INTGRL (ESOIL,TESOIL)


*---- total amount of water in profile after calculations


      TWAT2 = 0.
      DO 90,I=1,ILAYER
         TWAT2 = TWAT2+WCACT(I)*THCKN(I)
90    CONTINUE


*---- subroutine WATOUT is called to write soil water balance data to
*     output file *****WAT.OUT


      CALL WATOUT (IDAY,WCACT,TTRANS,TDRAIN,TRAIN,TESOIL,
     $		   REMARK,RUNNAM)


*---- check soil water balance: WBAL should always be zero!!!


      WBAL = ABS(TWAT1-TRTOT-ESOIL+RAIN-FLOW(ILAYER+1)-TWAT2)
      IF (WBAL.GE.1.E-4) STOP
     $   ' soil moisture balance not 0, program aborted'


      STEP1=.TRUE.


2000  CONTINUE


      RETURN
      END

*---------------------------------------------------------------------------*
*  SUBROUTINE SOILR                                                         *
*  Author : Rob Groot                                                       *
*  Date   : january 1987                                                    *
*  Purpose: This subroutine reads physical soil data from the data          *
*	    file SOIL.DAT                                                   *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                    units     class      *
*  ----     -------                                    -----     -----      *
*  ISMTB1   number of points in pF-curve, text.class 1   =         O        *
*  ISMTB2   number of points in pF-curve, text.class 2   =         O        *
*  ISMTB3   number of points in pF-curve, text.class 3   =         O        *
*  ISMTB4   number of points in pF-curve, text.class 4   =         O        *
*  ISMTB5   number of points in pF-curve, text.class 5   =         O        *
*  SMTB1    array with pF values for texture class 1     =         O        *
*  SMTB2    array with pF values for texture class 2     =         O        *
*  SMTB3    array with pF values for texture class 3     =         O        *
*  SMTB4    array with pF values for texture class 4     =         O        *
*  SMTB5    array with pF values for texture class 5     =         O        *
*  THCKN    array, thickness of soil compartments       cm         O        *
*  ITEXCL   number of texture classes                    =         O        *
*  RHOD     bulk density of soil compartments          gr/cm3      O        *
*  ILAYER   number of soil compartments                  =         O        *
*  RDMSOL   maximum rooting depth                       cm         O        *
*  PROP     proportionality factor for evaporation       =         O        *
*  WCINI    array, initial water content of the soil   cm/cm                *
*	    compartments                                 =         O        *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : soil data are read from the data file SOIL.DAT              *
*---------------------------------------------------------------------------*
   
      SUBROUTINE SOILR (IWREDN,IDPTN,ISMTB1,ISMTB2,ISMTB3,ISMTB4,
     $                  ISMTB5,WREDT,EDPTT,SMTB1,SMTB2,SMTB3,SMTB4,
     $                  SMTB5,THCKN,RHOD,ITEXCL,ILAYER,RDMSOL,PROP,
     $                  WCINI)


      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)

      DIMENSION SMTB1(30),SMTB2(30),SMTB3(30),SMTB4(30),SMTB5(30)
      DIMENSION WCINI(10),THCKN(10),ITEXCL(10),RHOD(10)
      DIMENSION WREDT(14),EDPTT(10)
      CHARACTER SOILNM*80


*---- the soil data file is opened

      OPEN (UNIT=21,FILE='SOIL.DAT',STATUS='OLD')
      CALL MOFILP(21)
      READ (21,'(A)') SOILNM
      CALL MOFILP(21)
      READ (21,*) ITEXT


*---- in case there is only one texture class, the data are read

      CALL MOFILP(21)

      IF (ITEXT.GE.1) THEN
         READ (21,*) ISMTB1
         READ (21,*) (SMTB1(I),SMTB1(I+1),I=1,ISMTB1-1,2)
      END IF


*---- in case there are two texture classes, the data are read

      CALL MOFILP(21)

      IF (ITEXT.GE.2) THEN
         READ (21,*) ISMTB2
         READ (21,*) (SMTB2(I),SMTB2(I+1),I=1,ISMTB2-1,2)
      END IF


*---- in case there are three texture classes, the data are read

      CALL MOFILP(21)

      IF (ITEXT.GE.3) THEN
         READ (21,*) ISMTB3
         READ (21,*) (SMTB3(I),SMTB3(I+1),I=1,ISMTB3-1,2)
      END IF


*---- in case there are four texture classes, the data are read

      CALL MOFILP(21)

      IF (ITEXT.GE.4) THEN
         READ (21,*) ISMTB4
         READ (21,*) (SMTB4(I),SMTB4(I+1),I=1,ISMTB4-1,2)
      END IF


*---- in case there are five texture classes, the data are read

      CALL MOFILP(21)

      IF (ITEXT.GE.5) THEN
         READ (21,*) ISMTB5
         READ (21,*) (SMTB5(I),SMTB5(I+1),I=1,ISMTB5-1,2)
      END IF


*---- number of soil compartments is read

      CALL MOFILP(21)
      READ (21,*) ILAYER


*---- thickness of the compartment and bulk density are read

      CALL MOFILP(21)
      READ (21,*) (THCKN(I),RHOD(I),I=1,ILAYER)


*---- maximum rooting depth of soil and proportionality factor are read

      CALL MOFILP(21)
      READ (21,*) RDMSOL
      CALL MOFILP(21)
      READ (21,*) PROP


*---- initial water content and the texture class to which a compartment
*     belongs are read

      CALL MOFILP(21)
      READ (21,*) (WCINI(I),ITEXCL(I),I=1,ILAYER)


*---- reduction factor for dryness of soil on water uptake by roots

      CALL MOFILP(21)
      READ (21,*) IWREDN
      READ (21,*) (WREDT(I),WREDT(I+1),I=1,IWREDN-1,2)


*---- effectiviness of roots as function of moisture content

      CALL MOFILP(21)
      READ (21,*) IDPTN
      READ (21,*) (EDPTT(I),EDPTT(I+1),I=1,IDPTN-1,2)


      RETURN
      END

*---------------------------------------------------------------------------*
*  SUBROUTINE EVSOIL                                                        *
*  Author : Daniel van Kraalingen                                           *
*  Date   : december 1986                                                   *
*  Purpose: This subroutine calculates soil evaporation and                 *
*	    distributes evaporation over soil compartments with             *
*	    exponential decrease.                                           *
*                                                                           *
*           References to literature:                                       *
*           Keulenª  Hr  vanª  1975r SimulatioŒ o‘ wate• us† an„ herbag†    *
*	        growtŠ iŒ ari„ regionsr SimulatioŒ Monographª Pudocª        *
*               Wageningenª 184pp.                                          *
*           Keulen, H.van &  N.Seligman, 1987. Simulation of water use,     *
*	        nitrogen nutrition and growth of a spring wheat crop.       *
*               Pudoc, Wageningen, in press.                                *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                        units  class     *
*  ----     -------                                        -----  -----     *
*  LAITOT   total leaf area index of the crop             m2 m-2    I       *
*  ES0      potential evaporation of a bare soil surface    cm      I       *
*  ILAYER   number of soil compartments                      =      I       *
*  WCACT    array, actual water content of soil                             *
*	    compartments                                  cm cm-1   I       *
*  WCMAX    array, maximum water content of soil                            *
*	    compartments                                  cm cm-1   I       *
*  WCFLDC   array, water content of soil compartments                       *
*	    at field capacity                             cm cm-1   I       *
*  WCAIR    array, water content of soil compartments                       *
*	    when compartments are air dry                 cm cm-1   I       *
*  THCKN    array, thickness of soil compartments           cm      I       *
*  PROP     proportionality factor for evaporation                          *
*	    distribution over soil compartments              =      I       *
*  DEPTH    array, depth of center of each soil                             *
*	    compartment measured from surface               cm      I       *
*  ESOIL    soil evaporation                              cm d-1    O       *
*  RESOIL   array, evaporation rate per soil compartm.    cm d-1    O       *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : none                                                        *
*---------------------------------------------------------------------------*

      SUBROUTINE EVSOIL (LAITOT,ES0,ILAYER,WCACT,WCFLDC,WCAIR,
     $                   THCKN,PROP,DEPTH,ESOIL,RESOIL)


      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)
      LOGICAL INIT

      DIMENSION WCACT(ILAYER) , THCKN(ILAYER), DEPTH(ILAYER),
     $          VAR(100),RESOIL(ILAYER), DRYLTB(14),WCFLDC(ILAYER),
     $          WCAIR(ILAYER)


*---- effect of relative water content in first soil compartment
*     on ratio of actual and potential soil evaporation,
*     van Keulen & Seligman, 1987 for a surface layer of 2 cm.

      DATA DRYLTB/-0.001,0., 0.,0., 0.2,0.05, 0.22,0.275, 0.33,0.9,
     $             1.,1., 1.5,1./
      IDRY = 14


*---- calculation of soil evaporation

      EVMAX  = ES0*EXP(-0.5*LAITOT)
      RELWC = (WCACT(1)-WCAIR(1))/(WCFLDC(1)-WCAIR(1))
      ESOIL = EVMAX*AFGEN(IDRY,DRYLTB,RELWC)

      SUM = 0.

      DO 10,I=1,ILAYER

         VAR(I) = (WCACT(I)-WCAIR(I))*EXP(-PROP*0.01*DEPTH(I))
         SUM = SUM+VAR(I)*THCKN(I)

10    CONTINUE


*---- calculation of evaporation rate per soil compartment (RESOIL)
*     with exponential extinction of evaporated moisture over
*     soil compartments

      DO 20,I=1,ILAYER

         IF (SUM.GT.0.) THEN
            RESOIL(I) = ESOIL*VAR(I)/SUM
         ELSE
            RESOIL(I)=0.
         END IF

20    CONTINUE


      RETURN
      END


*---------------------------------------------------------------------------*
*  SUBROUTINE WATOUT                                                        *
*  Author : Rob Groot                                                       *
*  Date   : january 1987                                                    *
*  Purpose: This subroutine writes soil water status to the                 *
*	    output file WATOUT.DAT                                          *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                    units     class      *
*  ----     -------                                    -----     -----      *
*  IDAY     julian daynumber                             =         I,T      *
*  WCACT    array, actual water content of each soil   ACT    array, actual water content of each soil   ACT    array, actual water
*	    compartment                                 cm/cm      I        *
*  TTRANS   total transpiration over simulated period    cm        I        *
*  TDRAIN   total drainage over simulated period         cm        I        *
*  TRAIN    total precipitation over sim. period         cm        I        *
*  TESOIL   total soil evaporation over sim. period      cm        I        *
*  REMARK   output remarks                               =         I        *
*  RUNNAM  first 5 characters for name of output files		  I	   *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : soil waterstatus is written to output file                  *
*	    WATOUT.DAT                                                      *
*---------------------------------------------------------------------------*
   
        SUBROUTINE WATOUT (IDAY,WCACT,
     1			   TTRANS,TDRAIN,TRAIN,TESOIL,REMARK,RUNNAM)

        IMPLICIT REAL (A-H,J-Z)
        IMPLICIT INTEGER (I)
        LOGICAL INIT
        CHARACTER REMARK*80,RUNNAM*5,OUTFIL*12
        DIMENSION WCACT(10)
        COMMON /ITIME/ ISYEAR,ISDAY
       
        DATA INIT /.TRUE./

        IF (.NOT.INIT) GOTO 100

*------	open output file #####WAT.OUT

        OUTFIL = RUNNAM//'WAT.OUT'
        OPEN (11,FILE=OUTFIL,STATUS='NEW')

        WRITE (11,'(//A,A)') ' DATA-FILE  ', OUTFIL

        WRITE (11,'(//(A,I3,A,I2))')
     1      ' Simulation starts on day ',ISDAY,', 19',ISYEAR

*------	write output remarks to file WATOUT.DAT

        WRITE (11,'(1X,A)') REMARK

**------ writes header for to output file

*	   WRITE (11,'(/A)') ' SOIL WATER STATUS'
*	   WRITE (11,'(A)')  ' ================='
*	   WRITE (11,'(3A)')
*     1     '   DAY   WCACT1  WCACT2  WCACT3  WCACT4  WCACT5  WCACT6',
*     2     '   WCACT7  WCACT8  WCACT9  WCACT10  TTRANS TDRAIN  TRAIN',
*     3     '   TESOIL'

*	INIT = .FALSE.

*100	CONTINUE

**------ write soil water status data to WATOUT.DAT during simulation


*      WRITE (11,'(1X,I5,14F8.2)')
*     1       IDAY,(WCACT(I),I=1,10),TTRANS,TDRAIN,TRAIN,TESOIL



*------ writes header for to output file

           WRITE (11,'(/A)') ' SOIL WATER STATUS'
           WRITE (11,'(A)')  ' ================='
           WRITE (11,'(3A)')
     1     '   DAY   WC0020  WC2040  WC4060  WC6080  WC80100 ',
     2     ' TTRANS TDRAIN  TRAIN',
     3     '   TESOIL'

        INIT = .FALSE.

100	CONTINUE

*------ rearrange output to layers of 20 cm each

        W0020 = (WCACT(1)*2.+WCACT(2)*8.+WCACT(3)*10.)/20.
        W2040 = (WCACT(4)+WCACT(5))/2.
        W4060 = (WCACT(6)+WCACT(7))/2.
        W6080 = (WCACT(8)+WCACT(9))/2.
        W80100=  WCACT(10)


*------ write soil water status data to WATOUT.DAT during simulation


      WRITE (11,'(1X,I5,5F8.2,4F8.2)')
     1       IDAY,W0020,W2040,W4060,W6080,W80100,
     2       TTRANS,TDRAIN,TRAIN,TESOIL


      RETURN
      END
!***********************End of "WATBAL.FOR" *****************************!
