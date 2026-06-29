!***********************#file "NITUPT.FOR" ******************************!
!************************************************************************!
!*  SUBROUTINE NITUPT                                                   *!
!*  Authors: Rob Groot                                                  *!
!*  Date of last revision  : 12-01-89                                   *!
!*  Purpose: This subroutine calculates the soil nitrogen status        *!
!*           For each soil compartment. Decomposition of crop           *!
!*           residues and old organic matter are calculated in a        *!
!*           seperate subroutine OMDYN, and calculated mineralization   *!
!*           and immobilization are input to this routine.              *!
!*           Mixing of soil water and nitrogen is described according   *!
!*           to Burns (1974). Nitrogen losses from soil compartments    *!
!*           occur by leaching, immobilization and nitrogen uptake      *!
!*           by the crop.                                               *!
!*           Transport of nitrogen towards roots and rate of nitrogen   *!
!*           uptake are calculated acc. de Willigen & van Noordwijk,    *!
!*           in the subroutine UPTAKE                                   *!
!*                                                                      *!
!*           References to literature:                                  *!
!*           Burns, I.G.,1984. J.soil Sci.,Vol25.No2:165-178            *!
!*                                                                      *!
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *!
!*  name     meaning                                    units  class    *!
!*  ----     -------                                    -----  -----    *!
!*  ISTEP    Integer*1, routine is executed for first,                  *!
!*           second or third time during a day IDAY       =      I,T    *!
!*  IDAY     Julian daynumber                             =      I,T    *!
!*  REMARK   output remark                                =       I     *!
!*  RUNNAM   first 5 char's of output-file name           =       I     *!
!*  FLOW     array, rate of waterflow between soil                      *!
!*	      compartments                               cm d-1    I     *!
!*  WCACT    array, actual watercontent of soil                         *!
!*	      compartments                               cm cm-1   I     *!
!*  THCKN    array, thickness of soil compartments        cm      I     *!
!*  ILAYER   number of soil compartments                  =       I     *!
!*  WCACT0   array, actual watercontent of soil                         *!
!*  LRV      root density                               cm/cm3    I     *!
!*  IN       nr. of compartment in which roots grow               I     *!
!*  RDINL    rooting depth in comp. were roots grow       cm      I     *!
!*  TRR      array, transpiration rate per soil comp.   cm d-1    I     *!
!*  RD       rooting depth                                cm      I     *!
!*  DVS      crop development stage                        =      I     *!
!*  NDEM     nitrogen demand of the crop             kg ha-1 d-1  I     *!
!*  ALAY     amount of N in soil compartments        kg (N) ha-1  O     *!
!*  NUPTR    nitrogen uptake rate of the crop        kg ha-1 d-1  O     *!
!*  NLOSS    soil mineral nitrogen lost by immob     kg ha-1 d-1  I     *!
!*  MINFM    nitrogen mineralized from FOM           kg ha-1 d-1  I     *!
!*  MINOM    nitrogen mineralized from old OM        kg ha-1 d-1  I     *!
!*  NIMM     nitrogen immobilized by growing biomass kg ha-1 d-1  I     *!
!*                                                                      *!
!*  SUBROUTINES called :                                                *!
!*  NITDAT   reads soil nitrogen data from file                         *!
!*	      NSOIL.DAT                                                  *!
!*  UPTAKE   calculates rate of nitrogen uptake                         *!
!*  NITOUT   writes soil nitrgen status to the output file NITOUT.DAT   *!
!*  FILE usage : none                                                   *!
!************************************************************************!


      SUBROUTINE NITUPT (ISTEP,IDAY,REMARK,RUNNAM,FLOW,
     $                   WCACT,THCKN,ILAYER,WCACT0,LRV,IN,RDINL,
     $                   TRR,RD,DVS,NDEM,ANLAY,NUPTR,NLOSS,MINFM,
     $                   MINOM,NIMM)


      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)
      LOGICAL INIT
      CHARACTER REMARK*80,RUNNAM*5

      DIMENSION FLOW(11)  ,WCACT(10) ,ANLAY(10) ,THCKN(10) ,WCACT0(10)
      DIMENSION TRR(10)   ,MINOM(10) ,UP(10)    ,IDAYN(5)  ,ANAP(5)
      DIMENSION NCON(10)  ,NIMM(10)  ,MINFM(10) ,NLOSS(10) ,LRV(10)
      DIMENSION TUP(10)


      DATA INIT /.TRUE./


      IF (.NOT.INIT) GOTO 100

      INIT = .FALSE.


*--------------------------------------------------------------------------
*     initialization of the soil nitrogen balance
*--------------------------------------------------------------------------


*---- Read soil nitrogen data


      CALL NITDAT (ILAYER,NTOT,ANLAY,IDAYN,ANAP,NRAIN,TCND,
     $              ADC,FMAX,INUMAP)

*     open (unit=88,file='tup.dat',status='new')

100   CONTINUE


*--------------------------------------------------------------------------
*     DYNAMIC PART
*--------------------------------------------------------------------------


*---- reset daily totals to zero

      NUPTR   = 0.0
      NS      = 0.0
      NTR     = 0.0
      NDF     = 0.0
      PNTR    = 0.0
      MINOMP  = 0.0
      NIMMP   = 0.0
      MINFMP  = 0.0
      NLOSSP  = 0.0
      LEACH   = 0.0


      IF (ISTEP.EQ.2) GOTO 999
      IF (ISTEP.EQ.3) GOTO 1111


*-------------------------------------------------------------------------
*     STEP1: calculate N-transport by moisture redistribution
*-------------------------------------------------------------------------


*---- nitrogen applications


30    IT=IT+1
      IF(IDAY.NE.IDAYN(IT).AND.IT.LT.INUMAP) THEN
         GOTO 30
      ELSE
        IF(IDAY.EQ.IDAYN(IT)) THEN
           NGIFT=ANAP(IT)
        ELSE
           NGIFT=0.
        ENDIF
        IT=0
      ENDIF


*---------------------------------------------------------------------------
*     according to the complete mixing theory (Burns, 1974), all
*     water and nitrogen entering a soil compartment is completely mixed
*     with water and nitrogen already present in the compartment, with the
*     nitrogen resulting from mineralization, and with nitrogen from
*     fertilizer application. The resulting nitrogen concentration is
*     subject to crop uptake in the transpiration stream
*---------------------------------------------------------------------------


      NTOTI = NTOT
      NTOT  = 0.0


      DO 60,I=1,ILAYER

         WATERM   = WCACT0(I)*THCKN(I)+FLOW(I)
         IF(I.EQ.1) THEN
            ANLAYM   = ANLAY(I) + FLOW(I)*NRAIN + NGIFT
         ELSE
            ANLAYM   = ANLAY(I) + FLOW(I)*NCONM
         ENDIF
         NCONM    = ANLAYM/WATERM

*------- transport out of compartment I now occurs with the concentration
*        that results from complete mixing

         ANLAY(I) = ANLAYM-FLOW(I+1)*NCONM
         NCON(I)  = ANLAY(I)/(WATERM-FLOW(I+1))
         NTOT     = NTOT + ANLAY(I)

60    CONTINUE


      LEACH = FLOW(11)*NCONM


*---- check soil nitrogen balance: NBAL1 should always be zero!!!


      NBAL = ABS(NTOTI+NGIFT+(NRAIN*FLOW(1))-LEACH-NTOT)


      IF (NBAL.GE.1.E-3) STOP
     $    ' SOIL NITROGEN BALANCE (1) NOT 0, PROGRAM ABORTED'


      GOTO 9999


*-------------------------------------------------------------------------
*     STEP2: subtract nitrogen lost by immobilization and add nitrogen
*     from mineralization
*-------------------------------------------------------------------------


999   CONTINUE


*---- ADD AMOUNTS OF NITROGEN FROM MINERALIZATION


      NTOTI = NTOT
      NTOT  = 0.0


      DO 111,I=1,ILAYER

         ANLAY(I) = ANLAY(I)+MINOM(I)+MINFM(I)-NLOSS(I)
         MINOMP   = MINOMP + MINOM(I)
         MINFMP   = MINFMP + MINFM(I)
         NLOSSP   = NLOSSP + NLOSS(I)
         NIMMP    = NIMMP  + NIMM(I)
         NTOT     = NTOT + ANLAY(I)

111   CONTINUE


*---- check soil nitrogen balance: NBAL2 should always be zero!!!


      NBAL = ABS(NTOTI+MINOMP+MINFMP-NLOSSP-NTOT)


      IF (NBAL.GE.1.E-3) STOP
     $    ' SOIL NITROGEN BALANCE (2) NOT 0, PROGRAM ABORTED'


      GOTO 9999


*-------------------------------------------------------------------------
*     STEP3: subtract nitrogen uptake by plant
*-------------------------------------------------------------------------


1111  IF (NDEM.EQ.0) GOTO 9999
     

*---------------------------------------------------------------------------
*     nitrogen uptake by nitrogen diffusion towards roots
*     and convection, related to soil moisture content according to
*     De Willigen en van Noordwijk (1987)
*---------------------------------------------------------------------------


*---- uptake is only calculated when roots are present


      IF (RD.GT.0.) THEN


*------- calculate nitrogen to be taken up by diffusion:


*        IF (DVS.LT.1.5.AND.NDEM.GT.0.) THEN
         IF (NDEM.GT.0.)THEN

            CALL UPTAKE (WCACT,THCKN,ANLAY,IN,RDINL,LRV,ILAYER,
     $                 TRR,NDEM,ADC,FMAX,UP)
        
*---------- total nitrogen uptake during one time step

            NUPTR=0.

            DO 93,I=1,10
               TUP(I) = TUP(I) + UP(I)
93	    CONTINUE

            up0020 = up(1)+up(2)+up(3)
            up2040 = up(4)+up(5)
            up4060 = up(6)+up(7)
            up6080 = up(8)+up(9)
* up80100 veranderd in up8010
            up8010 = up(10)


            DO 95, I=1,IN
               NUPTR = NUPTR+UP(I)
95          CONTINUE

*	    write (88,'(i5,7f8.2)') iday,up0020,up2040,up4060,
*     $ 			     up6080,up8010,ndem,nuptr

         ELSE

            DO 96, I=1,IN
               UP(I)=0.
96          CONTINUE

            NUPTR = 0.

         ENDIF

      ENDIF


*---------------------------------------------------------------------------
*     Amounts of nitrogen in each compartment and nitrogen uptake rate
*     of crop are calculated
*---------------------------------------------------------------------------


      NTOTI = NTOT
      NTOT  = 0.0


      DO 110,I=1,ILAYER
         ANLAY(I)= ANLAY(I)-UP(I)
         NTOT    = NTOT+ANLAY(I)
110   CONTINUE


*---- check soil nitrogen balance: NBAL3 should always be zero!!!


      NBAL = ABS(NTOTI-NUPTR-NTOT)
      IF (NBAL.GE.1.E-3) STOP
     $    ' SOIL NITROGEN BALANCE (3) NOT 0, PROGRAM ABORTED'


9999  CONTINUE


*---------------------------------------------------------------------------
*     total amount of nitrogen taken up by the crop (TNUPTC), total
*     mineralization from old organic material (TMINOM), total
*     mineralization from crop residues (TMINFM), total amount of
*     nitrogen immobilized by biomass (TIMMOB) and total amount of
*     soil nitrate lost by immobilization (TNLOSS), in kg(N).ha-1
*---------------------------------------------------------------------------


      TNUPTC = TNUPTC + NUPTR
      TMINOM = TMINOM + MINOMP
      TMINFM = TMINFM + MINFMP
      TIMMOB = TIMMOB + NIMMP
      TNLOSS = TNLOSS + NLOSSP
      TLEACH = TLEACH + LEACH

*	Update amounts of nitrogen according to measurements
*	starting at iday.eq.135
*
*	if (iday.eq.135) then
*	   anlay(1) = 1.
*	   anlay(2) = 5.
*	   anlay(3) = 6.
*	   anlay(4) = 7.
*	   anlay(5) = 0.
*	   anlay(6) = 2.
*	   anlay(7) = 2.
*	   anlay(8) = 4.
*	   anlay(9) = 4.
*	   anlay(10)= 9.
*
*	   ntot     = 40.
*
*	endif

*---------------------------------------------------------------------------
*     write soil nitrogen status to output-file
*---------------------------------------------------------------------------


      IF (ISTEP.NE.3) GOTO 303


         CALL NITOUT (IDAY,ANLAY,NTOT,NDF,
     $  	     NUPTR,TNUPTC,TMINOM,TMINFM,TIMMOB,TNLOSS,
     $		     TLEACH,FMTOT,REMARK,RUNNAM)


303   RETURN
      END


*---------------------------------------------------------------------------*
*  SUBROUTINE UPTAKE                                                        *
*  Author : Peter de Willigen                                               *
*  Date   : 10-09-1988                                                      *
*  Purpose: This subroutine calculates uptake of a nutrient from the soil.  *
*           For each soil compartment, uptake is calculated as a function   *
*           of concentration, root density and transportparameters.         *
*           The  theory of de Willigen and van Noordwijk is used in which   *
*           uptake proceeds according to plant demand if transport through  *
*           the soil allows this, otherwise the root acts as zero-sink, i.e *
*	    uptake has the maximum value under the prevailing conditions    *
*                                                                           *
*           References to literature:                                       *
*           De Willigen P. and Van Noordwijk M.,1987. Roots, plant produc-  *
*	    tion and nutrient use efficiency. Thesis, Wageningen.         *
*
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                    units  class         *
*  ----     -------                                    -----  -----         *
*  WCACT    array, actual watercontent of soil                              *
*	    compartments                               cm cm-1  I           *
*  THCKN    array, thickness of soil compartments        cm     I           *
*  ANLAY    array, amount of N in soil compartments  kg(N) ha-1 I           *
*  IN       nr.of comp.in which root growth occurs       =      I           *
*  RDINL    rooting depth in comp. were roots grow       cm     I           *
*  LRV      root density                               cm cm-3  I           *
*  ILAYER   number of soil compartments                  =      I           *
*  TRR      array, transpiration rate per soil comp.   cm d-1   I           *
*  NDEM     nitrogen demand of the crop             kg ha-1 d-1 I           *
*  NUPTR    nitrogen uptake rate of the crop        kg ha-1 d-1 O           *
*  ADC      adsorption constant                        ml cm-3  I           *
*  FMAX     max. N-influx per cm2 root area            mg cm-2  I           *
*  UP       arry,  rate of N-uptake from comp.      kg ha-1 d-1 O           *
*                                                                           *
*  SUBROUTINES called :                                                     *
*  ROOTR   reads soil root data from file ROOT.DAT                          *
*  GNUR  function                                                           *
*  FILE usage : none                                                        *
*---------------------------------------------------------------------------*

      SUBROUTINE UPTAKE (WCACT,THCKN,ANLAY,IN,RDINL,LRV,
     $                   ILAYER,TRR,NDEM,ADC,FMAX,UP)



      IMPLICIT REAL (A-H,J-Z)
      LOGICAL INIT

      DIMENSION WCACT(10) ,ANLAY(10) ,THCKN(10) ,TRR(10)   ,NU(10)
      DIMENSION LRV(10)   ,DIF(10)   ,UP(10)    ,BUF(10)   ,CONC(10)
      DIMENSION IFLAG(10)  ,MXUPT(10)

      DATA DELT /1./
      DATA INIT /.TRUE./


      IF (.NOT.INIT) GOTO 100


      INIT = .FALSE.


*---------------------------------------------------------------------------
*     initialization
*---------------------------------------------------------------------------

      PI = 4.*ATAN(1.)


*---- Read root data

      CALL ROOTR(R0,D0,A1,A2,B1)


*---- Specific surface root (root surface per unit root length)

      SPSUR = 2.*PI*R0


100   CONTINUE

*--------------------------------------------------------------------------
*     dynamic part
*--------------------------------------------------------------------------

*---- Set uptake rates from all compartments to zero

      NUPTR = 0.
      TUP   = 0.

*------------------------------tijdelijke constructie: geen wortels
*  in bovenste 2 cm ------------------------------------------------

      lrv(1)= 0.001

      DO 10 I=1,ILAYER
         UP(I) = 0.
10    CONTINUE


*---- Calculation of total root length

      TOTRL = 0.
      DO 20 I=1,IN-1
         TOTRL = TOTRL+LRV(I)*THCKN(I)
20    CONTINUE
      TOTRL = TOTRL+LRV(IN)*RDINL

      IF (TOTRL.EQ.0.) GOTO 999


*---- Calculation of Buffercapacity (BUF), Concentration (CONC), Diffusion
*     Coefficient (DIF) and dimensionless massflow (NU)

      DO 30 I=1,IN

         BUF(I) = WCACT(I)+ADC
         CONC(I)= 0.01*ANLAY(I)/(THCKN(I)*BUF(I))

         IF (WCACT(I).LT.B1) THEN
            DIF(I) = D0*WCACT(I)*((A1*B1+A2)/B1)*WCACT(I)
         ELSE
            DIF(I) = D0*WCACT(I)*(A1*WCACT(I)+A2)
         ENDIF

         NU(I) = -TRR(I)*0.5/(2.*PI*THCKN(I)*DIF(I)*LRV(I))

*------- Putting iteration flag to zero

         IFLAG(I) = 0

30    CONTINUE


*---- N-requirment in mg cm-2

      NREQ = NDEM/100.


      DO 60 IJK=1,10

         RUPTM = NREQ/TOTRL

         DO 40 I=1,IN

            IF (IFLAG(I).EQ.1) GOTO 40

            RDE = LRV(I)
            RHO = 1./(R0*SQRT(PI*RDE))
            NUI = NU(I)
            G   = GNUR(RDE,NUI,R0)
            CMIN= PI*R0**2*LRV(I)/(1.-PI*R0**2*LRV(I))*
     $	          RUPTM*G/(PI*DIF(I))
        
            IF (CONC(I).GT.CMIN) THEN
               UP(I)  = RUPTM*LRV(I)
            ELSE
               UP(I)   = LRV(I)*PI*(RHO**2-1.)*CONC(I)/G*DIF(I)
               IFLAG(I)= 1
            ENDIF

            MXUPT(I) = AMIN1(WCACT(I)*CONC(I)/DELT,FMAX*LRV(I)*SPSUR)
        
            IF (UP(I).GT.MXUPT(I)) THEN
               UP(I)   = MXUPT(I)
               IFLAG(I)= 1
            ENDIF

            NREQ = NREQ-THCKN(I)*UP(I)*IFLAG(I)

            IF (I.NE.IN) THEN
               TOTRL = TOTRL-LRV(I)*THCKN(I)*IFLAG(I)
            ELSE
               TOTRL = TOTRL-LRV(I)*RDINL*IFLAG(I)
            ENDIF

40       CONTINUE
  

         DO 50 I=1,IN-1
            TUP = TUP+THCKN(I)*UP(I)
50       CONTINUE
         TUP = TUP + RDINL*UP(IN)

         CRIT = ABS((NDEM-100.*TUP)/NDEM)

         IF (CRIT.LE.1.E-5.OR.TOTRL.LE.1.E-4) GOTO 70

60    CONTINUE


70    DO 80 I=1,IN-1
         UP(I) = 100.*THCKN(I)*UP(I)
         NUPTR = NUPTR+UP(I)
80    CONTINUE
      UP(IN) = 100.*RDINL*UP(IN)
      NUPTR= NUPTR + UP(IN)

999   RETURN

      END




      FUNCTION GNUR(W,FL,R0)

      PI=4.*ATAN(1.)
      R1=1./SQRT(PI*W)
      RHO=R1/R0
      RHO2=RHO**2
      RHOTN=RHO**(2*FL)


*---- transport by diffusion and mass flow (p.130.)


      IF (FL.LT.0.) THEN

         G1=(1.-RHO2)/2.+RHO2*(RHOTN-1.)/(2.*FL)
         G2=RHO2*(RHOTN-1.)/(2*FL*(RHO2*RHOTN-1.))*(FL+1)
         G3=(1.-RHOTN*RHO2**2)/((2*FL+4.)*(RHO2*RHOTN-1.))*(FL+1)
         GNUR=(G1+G2+G3)/(2.*(FL+1.))


*---- transport by diffusion only, transp.rate = 0. (p.125.)


      ELSE

         G1=(1.-3*RHO2)/4.
         G2=(LOG(RHO)*RHO**4)/(RHO2-1)
         GNUR=0.5*(G1+G2)

      ENDIF


      RETURN
      END








      SUBROUTINE ROOTR(R0,D0,A1,A2,B1)

      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)

*---- root data file is opened

      OPEN(UNIT=47,FILE='ROOT.DAT',STATUS='OLD')


*---- read root radius

      CALL MOFILP(47)
      READ (47,*) R0


*---- diffusion coefficient in free water (cm2/d)

      CALL MOFILP(47)
      READ (47,*) D0


*---- constants from impedance-watercontent relation

      CALL MOFILP(47)
      READ(47,*) A1,A2,B1


      CLOSE (47)


      RETURN
      END

*---------------------------------------------------------------------------*
*  SUBROUTINE NITDAT                                                        *
*  Author : Rob Groot                                                       *
*  Date   : 15 sept 1988                                                    *
*  Purpose: This subroutine reads soil nitrogen data from the data          *
*	    file NSOIL.DAT                                                  *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                       units  class      *
*  ----     -------                                       -----  -----      *
*  ILAYER   number of soil compartments                     =      I        *
*  NTOT     total nitrogen in soil profile              kg(N)/ha   O        *
*  ANLAY    array, amount of nitrogen in soil comp.     kg(N)/ha   O        *
*  IDAYN    array, dates of nitrogen application            =      O,T      *
*  ANAP     array, amounts of nitrogen in successive                        *
*	    nitrogen application                        kg(N)/ha   O        *
*  NRAIN    nitrogen concentration in precipitation     kg(N)/cm   O        *
*  TCND     time constant for N diffusion                   day    O        *
*  ADC      adsorption constant                          ml/cm3    O        *
*  FMAX     max. N-influx per cm2 root area              mg/cm2    O        *
*  INUMAP   number of nitrogen applications                 =      O        *
*                                                                           *
*  subroutines called : none                                                *
*  FILE usage : soil nitrogen data are read from the                        *
*	    data file NSOIL.DAT                                             *
*---------------------------------------------------------------------------*


      SUBROUTINE NITDAT (ILAYER,NTOT,ANLAY,IDAYN,ANAP,NRAIN,TCND,
     $                   ADC,FMAX,INUMAP)

      IMPLICIT REAL(A-H,J-Z)
      IMPLICIT INTEGER (I)

      DIMENSION ANLAY(10),IDAYN(5),ANAP(5)


*---- open nsoil data-file

      OPEN (UNIT=22,FILE='NSOIL.DAT',STATUS='OLD')


*---- total nitrogen in soil profile (kg(N)/ha)

      CALL MOFILP(22)
      READ (22,*) NTOT


*---- initial amounts of nitrogen in soil compartments
*---- in kg(N)/ha/compartment

      CALL MOFILP(22)
      READ (22,*) (ANLAY(I),I=1,ILAYER)


*---- number of nitrogen applications

      CALL MOFILP(22)
      READ (22,*) INUMAP


*---- timing of nitrogen applications, given as daynumbers

      CALL MOFILP(22)
      READ (22,*) (IDAYN(I),I=1,INUMAP)


*---- amounts of nitrogen in applications (kg/ha)

      CALL MOFILP(22)
      READ (22,*) (ANAP(I),I=1,INUMAP)


*---- read nitrogen concentration in rainwater (kg/cm)

      CALL MOFILP(22)
      READ(22,*) NRAIN


*---- read time contstant for nitrogen diffusion towards roots (days)

      CALL MOFILP(22)
      READ(22,*) TCND


*---- read adsorption constant for N (ml/cm3)

      CALL MOFILP(22)
      READ(22,*) ADC


*---- read max. N-influx per cm2 root area (mg/cm2)

      CALL MOFILP(22)
      READ(22,*) FMAX
 

      CLOSE (22)

      RETURN
      END

*---------------------------------------------------------------------------*
*  SUBROUTINE NITOUT                                                        *
*  Author : Rob Groot                                                       *
*  Date   : january 1987                                                    *
*  Purpose: This subroutine writes soil nitrogen status to the              *
*	    output file *****NIT.OUT                                        *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                       units     class   *
*  ----     -------                                       -----     -----   *
*  IDAY     julian daynumber                                =         I,T   *
*  ANLAY    array, amount of nitrogen in soil comptmnt    kg(N)/ha    I     *
*  NTOT     total amount of nitrogen in soil profile      kg(N)/ha    I     *
*  NDF      nitrogen uptake rate by diffusion             kg(N)/ha/d  I     *
*	    stream                 	                  kg(N)/ha/d  I     *
*  NUPTR    nitrogen uptake rate of the crop              kg(N)/ha/d  I     *
*  TNUPTC   total N-uptake by the crop                    kg(N)/ha    I     *
*  TMINOM   total N mineralization from humus             kg(N)/ha    I     *
*  TMINFM   total N mineralization from crop residues     kg(N)/ha    I     *
*  TIMMOB   total N immobilization  in biomass            kg(N)/ha    I     *
*  TNLOSS   total N lost from soil nitrate N by                             *
*	    immobilization                                kg(N)/ha    I     *
*  TLEACH   total N lost by leaching                      kg(N)/ha    I     *
*  FMTOT    total amount of fresh material                kg/ha       I     *
*  REMARK   output remarks                                  =         I     *
*  RUNNAM  first 5 characters for name of output files		     I	   *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : soil nitrogen status is written to output file              *
*	    NITOUT.DAT                                                      *
*---------------------------------------------------------------------------*


      SUBROUTINE NITOUT (IDAY,ANLAY,NTOT,NDF,NUPTR,TNUPTC,
     $                   TMINOM,TMINFM,TIMMOB,TNLOSS,TLEACH,FMTOT,
     $			 REMARK,RUNNAM)


      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)
      LOGICAL INIT
      CHARACTER REMARK*80,RUNNAM*5,OUTFIL*12
      DIMENSION ANLAY(10)

      COMMON /ITIME/ ISYEAR,ISDAY
      DATA INIT /.TRUE./

      IF (.NOT.INIT) GOTO 100
      INIT = .FALSE.


*---- open file #####NIT.OUT


      OUTFIL = RUNNAM//'NIT.OUT'
      OPEN (4,FILE=OUTFIL,STATUS='NEW')


      WRITE (4,'(//A,A)') ' DATA-FILE  ', OUTFIL

      WRITE (4,'(//(A,I3,A,I2))')
     $      ' Simulation starts on day ',ISDAY,', 19',ISYEAR


*---- write output remarks to file nitout.dat

      WRITE (4,'(1X,A)') REMARK


*---- write header file NITOUT.DAT

      WRITE (4,'(/A)') ' SOIL NITROGEN STATUS'
      WRITE (4,'(A)')  ' ===================='
      WRITE (4,'(4A)')
     $     ' DAY N00-20  N20-40  N40-60  N60-80  N80-100 ',
     $     '  NTOT    NDF NUPTR',
     $     '  TMNOM TMNFM TIMMO TNLOS FMTOT TLEACH'

100   CONTINUE


*---- write header file NITOUT.DAT

*      WRITE (4,'(/A)') ' SOIL NITROGEN STATUS'
*      WRITE (4,'(A)')  ' ===================='
*      WRITE (4,'(4A)')
*     $     ' DAY ANL1 ANL2 ANL3 ANL4 ANL5 ANL6',
*     $     '  ANL7 ANL8 ANL9 ANL0  NDF NUPTR',
*     $     '  TMNOM TMNFM TIMMO TNLOS FMTOT TLEACH',
*     $     ' 0-30 30-60 60-100'

*100   CONTINUE


*---- output data are rearranged to obtain output for comparison with
*     field measurements

      N0020 = ANLAY(1) + ANLAY(2) + ANLAY(3)
      N2040 = ANLAY(4) + ANLAY(4)
      N4060 = ANLAY(6) + ANLAY(7)
      N6080 = ANLAY(8) + ANLAY(9)
      N80100= ANLAY(10)

      WRITE (4,'(I4,6F8.1,2F6.2,6F6.1)')
     $       IDAY,N0020,N2040,N4060,N6080,N80100,NTOT,NDF,NUPTR,TMINOM,
     $       TMINFM,TIMMOB,TNLOSS,FMTOT,TLEACH


*---- output data are rearranged to obtain output for comparison with
*     field measurements

*      AN1 = ANLAY(1) + ANLAY(2) + ANLAY(3) + ANLAY(4)
*      AN2 = ANLAY(5) + ANLAY(6) + ANLAY(7)
*      AN3 = ANLAY(8) + ANLAY(9) + ANLAY(10)


*      WRITE (4,'(I4,10F5.1,2F6.2,9F6.1)')
*     $       IDAY,(ANLAY(I),I=1,10),NDF,NUPTR,TMINOM,
*     $       TMINFM,TIMMOB,TNLOSS,FMTOT,TLEACH,AN1,AN2,AN3


      RETURN
      END
!***********************End of "NITUPT.FOR" *****************************!
