!***********************#file "OMDYN.FOR" *******************************!
!************************************************************************!
!*  SUBROUTINE OMDYN                                                    *!
!*  Author : Rob Groot                                                  *!
!*  Date of last revision: 19-01-89                                     *!
!*  Purpose: This subroutine simulates soil organic matter dynamics     *!
!*           Old organic matter (humus) and young organic matter        *!
!*           are distinguised. Decomposition of both old and young      *!
!*           organic matter are discribed as a first order process.     *!
!*           The specific rates of decomposition are corrected for      *!
!*           the effects of soil moisture content and soil temperature  *!
!*           according to a response-surface proposed by Verbruggen     *!
!*           (1985). Decomposition of organic matter results in         *!
!*           mineralization or immobilization of nitrogen, depending    *!
!*           on the C/N-ratio of the subtrate                           *!
!*                                                                      *!
!*           References:                                                *!
!*           Johsson,Bergstrom,Jansson&Paustiaan,1988.Simulated         *!
!*           nitrogen dynamics and losses in a layered agricultural     *!
!*           soil. Ecosystems and Environment, 1988.                    *!
!*           Verbruggen,1981. Simulatie van het denitrificatie-         *!
!*           proces in de bodem. Doctoraatproefschrift nr.140, KU,      *!
!*           Leuven.                                                    *!
!*                                                                      *!
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *!
!*  name    meaning                                     units  class    *!
!*  ----    -------                                     -----  -----    *!
!*  ILAYER  number of soil compartments                   -      I      *!
!*  WCACT   water content of soil compartments         cm cm-1   I      *!
!*  ANLAY   amount of nitrogen is soil compartments    kg ha-1   I      *!
!*  THCKN   thickness of soil compartments               cm      I      *!
!*  WCFLDC  water content at field capacity            cm cm-1   I      *!
!*  RHOD    bulk density of soil compartments           g cm-3   I      *!
!*  SOILTP  temperature of soil compartments              C      I      *!
!*  MINFM   N from mineralization of crop residues     kg ha-1   O      *!
!*  NLOSS   loss of soil mineral N by immobilization   kg ha-1   O      *!
!*  MINOM   N from mineralization of old org. mat.     kg ha-1   O      *!
!*  NIMM    N immobilized in biomass                   kg ha-1   O      *!
!*                                                                      *!
!*  SUBROUTINES and FUNCTIONS called :                                  *!
!*  OMDAT   reads soil specific data on organic matter                  *!
!*          from file OMSOIL.DAT                                        *!
!*  FILE usage : none                                                   *!
!************************************************************************!


      SUBROUTINE OMDYN (ILAYER,WCACT,ANLAY,THCKN,WCFLDC,RHOD,
     $                  SOILTP,MINFM,NLOSS,MINOM,NIMM)

      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)
      LOGICAL INIT
      CHARACTER REMARK*80,RUNNAM*5

      DIMENSION WCACT(10) ,ANLAY(10) ,THCKN(10)
      DIMENSION OMLAY(10) ,MINOM(10) ,SOILTP(10)
      DIMENSION TRDMIN(10),FMLAY(10) ,NIMM(10)  ,MINFM(10)
      DIMENSION NLOSS(10) ,RHOD(10)  ,MR0(10)   ,WCFLDC(10)
      DATA INIT /.TRUE./


      IF (.NOT.INIT) GOTO 100


      INIT = .FALSE.


*--------------------------------------------------------------------------
*     initialization
*--------------------------------------------------------------------------


*---- Read soil organic matter data

      CALL OMDAT(PLAYER,PERCOM,RDROM,REFTMP,
     $           FM,RDRFM,CCFM,NCFM,DISASS,NCRB)


*---- Calculate initial amounts of old organic matter  and crop
*     residues for the soil compartments

      OMCM = PERCOM * 1.E3
      FMCM = FM/PLAYER

10    I = I+1
      DEPT = DEPT + THCKN(I)

      IF (PLAYER.GT.DEPT) THEN
         OMLAY(I) = THCKN(I)*OMCM*RHOD(I)
         FMLAY(I) = THCKN(I)*FMCM
         GOTO 10
      ELSE
         OMLAY(I) = (PLAYER-(DEPT-THCKN(I)))*OMCM*RHOD(I)
         FMLAY(I) = (PLAYER-(DEPT-THCKN(I)))*FMCM
      ENDIF


*---- calculate reference mineralization rate according to Verbruggen
*     (1985). Mineralization is assumed to be optimum at field capacity

      DO 15,I=1,ILAYER
         A0     = 0.22*(REFTMP+0.00064)**1.4425
         B0     = 0.1737*EXP(-0.119*REFTMP)+0.1107
         MR0(I) = A0*(1.-EXP(-B0*WCFLDC(I)/RHOD(I)))
15    CONTINUE


100   CONTINUE


*---------------------------------------------------------------------------
*     dynamic section
*---------------------------------------------------------------------------


*---- mineralization is a function of soil temperature and soil
*     humidity according to Verbruggen (1985). The temperature and soil
*     moisture reduction factor for mineralization (TRDMIN(I)) is
*     calculated for each soil compartment

      DO 40,I=1,ILAYER

         IF (SOILTP(I).LE.0.) THEN
            TRDMIN(I)=0.
         ELSE
            A         = 0.22*((SOILTP(I)+0.00064)**1.4425)
            B         = 0.1737*EXP(-0.119*SOILTP(I))+0.1107
            MR        = A*(1.-EXP(-B*WCACT(I)/RHOD(I)))
            TRDMIN(I) = MR/MR0(I)
         ENDIF

40    CONTINUE


*---------------------------------------------------------------------------
*     nitrogen from mineralization of crop residues (MINFM(I))
*     immobilization of nitrogen in biomass (NIMM(I))
*     loss of soil mineral nitrogen by immobilization (NLOSS(I))
*---------------------------------------------------------------------------

      DO 200,I=1,ILAYER

         IF (SOILTP(I).LE.0.) THEN

            NIMM(I)   = 0.
            NLOSS(I)  = 0.
            MINFM(I)  = 0.

         ELSE

*---------- calculate decomposition rate of fresh organic material and
*           potential mineralization rates for fresh organic material

            PDRFM = RDRFM * TRDMIN(I) * FMLAY(I)
            PMINN = PDRFM * NCFM

*---------- dissimilation:assimilation ratio for growth of microbial biomass
*           (DISASS) and N:C ratio of biomass (NCRB) have been read from
*           data-file NSOIL.DAT

            NDEMB = (PDRFM*CCFM/(DISASS+1.))*NCRB
 
            IF (NDEMB.LE.PMINN) THEN

*------------- nitrogen demand of biomass (NDEMB) is supplied by nitrogen from
*              decay of fresh organic matter

               NIMM(I)  = NDEMB
               MINFM(I) = PMINN-NDEMB
               NLOSS(I) = 0.
               FMLAY(I) = AMAX1(0.,FMLAY(I)-PDRFM)

            ELSE

               NDEMBS = AMAX1(0.,NDEMB-PMINN)

               IF (ANLAY(I).GE.NDEMBS) THEN

*---------------- if crop residues do not cover nitrogen demand of biomass,
*                 soil mineral nitrogen is partly immobilized

                  NIMM(I)  = NDEMB
                  MINFM(I) = 0.
                  NLOSS(I) = NDEMBS
                  FMLAY(I) = AMAX1(0.,FMLAY(I)-PDRFM)

               ELSE

*---------------- soil mineral nitrogen does not cover the demand.
*                 mineralization is reduced

                  NIMM(I)  = ANLAY(I)
                  MINFM(I) = 0.
                  NLOSS(I) = ANLAY(I)
                  FMLAY(I) = AMAX1(0.,(FMLAY(I)-(ANLAY(I)/NDEMBS)*
     $                       PDRFM))

               ENDIF

            ENDIF

         ENDIF

200   CONTINUE


*---------------------------------------------------------------------------
*     mineralization of old organic material (humus)
*---------------------------------------------------------------------------


*---- A C/N ratio of 10 is assumed. The total OM pool does not decrease!!
*     The rel. min. rate is from Johnsson (1987)

      DO 50,I=1,ILAYER

         IF (SOILTP(I).LE.0.) THEN
            MINOM(I) = 0.
         ELSE
            MINOM(I) = OMLAY(I)*TRDMIN(I)*RDROM*1.0E-1
         ENDIF

         MINOMP = MINOMP + MINOM(I)

50    CONTINUE


      RETURN
      END

*---------------------------------------------------------------------------*
*  SUBROUTINE OMDAT                                                         *
*  Author : Rob Groot                                                       *
*  Date   : 15 sept 1988                                                    *
*  Purpose: This subroutine reads data on soil nitrogen dynamics from       *
*           the data file OMSOIL.DAT                                        *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                       units  class      *
*  ----     -------                                       -----  -----      *
*  PLAYER   depth of plough layer                          cm      O        *
*  PERCOM   percentage organic material of humus in                         *
*	    plough layer                                    =      O        *
*  RDROM    relative decomposition  rate old org.mat.      d-1     O        *
*  REFTMP   reference temperature for mineralization        C      O        *
*  FM       amount of fresh organic matter or crop                          *
*	    residues in plouging layer                    kg/ha    O        *
*  RDRFM    relative decomposition  rate fresh org.mat.    d-1     O        *
*  CCFM     carbon content of fresh organic matter          =      O        *
*  NCFM     nitrogen content of fresh organic matter        =      O        *
*  DISASS   dissimilation:assimilation ratio for growth of                  *
*	    microbial biomass                               =      O        *
*	    and dissimilation                               =      O        *
*  NCRB     N:C ratio of biomass                            =      O        *
*  NAPDAY   array, dates of nitrogen application            =      O,T      *
*  ANAP     array, amounts of nitrogen in successive                        *
*	    nitrogen application                        kg(N)/ha   O        *
*  NRAIN    nitrogen concentration in precipitation     kg(N)/cm   O        *
*  TCND     time constant for nitrogen diffusion           d-1     O        *
*                                                                           *
*  subroutines called : none                                                *
*  FILE usage : soil nitrogen data are read from the                        *
*	    data file NSOIL.DAT                                             *
*---------------------------------------------------------------------------*

      SUBROUTINE OMDAT(PLAYER,PERCOM,RDROM,REFTMP,FM,RDRFM,CCFM,NCFM,
     $                 DISASS,NCRB)


      IMPLICIT REAL(A-H,J-Z)
      IMPLICIT INTEGER (I)


*---- open nsoil data file

      OPEN (UNIT=23,FILE='OMSOIL.DAT',STATUS='OLD')


*---- read depth of ploughing layer (cm)

      CALL MOFILP(23)
      READ(23,*) PLAYER

*---- read percentage organic material (humus) in ploughing layer

      CALL MOFILP(23)
      READ(23,*) PERCOM

*---- read relative decomposition rate of old org.mat. (kg/kg/d)

      CALL MOFILP(23)
      READ(23,*) RDROM

*---- read reference temperature for mineralization (C)

      CALL MOFILP(23)
      READ(23,*) REFTMP

*---- read amount of crop residues from previous crop

      CALL MOFILP(23)
      READ(23,*) FM

*---- read relative decomposition rate of fresh organic matter

      CALL MOFILP(23)
      READ(23,*) RDRFM

*---- read carbon content of fresh organic material

      CALL MOFILP(23)
      READ(23,*) CCFM

*---- read nitrogen content of fresh organic material

      CALL MOFILP(23)
      READ(23,*) NCFM

*---- read dissimilation:assimilation ratio for growth of microbial biomass

      CALL MOFILP(23)
      READ(23,*) DISASS

*---- read N:C ratio biomass

      CALL MOFILP(23)
      READ(23,*) NCRB


      CLOSE (23)


      RETURN
      END
!***********************End of "OMDYN.FOR" ******************************!
