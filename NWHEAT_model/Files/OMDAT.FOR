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

      SUBROUTINE OMDAT(IORGM,PLAYER,PERCOM,RDROM,REFTMP,FM,RDRFM,CCFM,
     $               NCFM,DISASS,NCRB)


      IMPLICIT REAL(A-H,J-Z)
      IMPLICIT INTEGER (I)


*---- open nsoil data file

      IF (IORGM .EQ. 1) OPEN (UNIT=23,FILE='OMSOIL1.DAT',STATUS='OLD')
      IF (IORGM .EQ. 2) OPEN (UNIT=23,FILE='OMSOIL2.DAT',STATUS='OLD')
      IF (IORGM .EQ. 3) OPEN (UNIT=23,FILE='OMSOIL3.DAT',STATUS='OLD')
      IF (IORGM .EQ. 4) OPEN (UNIT=23,FILE='OMSOIL4.DAT',STATUS='OLD')


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
