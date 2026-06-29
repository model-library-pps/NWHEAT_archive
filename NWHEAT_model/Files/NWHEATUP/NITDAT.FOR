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
