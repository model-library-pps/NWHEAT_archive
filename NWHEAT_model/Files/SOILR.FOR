*-------------------------------------------------------------------------*
* Copyright 2013. Wageningen University, Plant Production Systems group,  *
* P.O. Box 430, 6700 AK Wageningen, The Netherlands.                      *
* You may not use this work except in compliance with the Licence.        *
* You may obtain a copy of the Licence at:                                *
*                                                                         *
* http://models.pps.wur.nl/content/licence-agreement                      *
*                                                                         *
* Unless required by applicable law or agreed to in writing, software     *
* distributed under the Licence is distributed on an "AS IS" basis,       *
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.*
*-------------------------------------------------------------------------*

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
    
      SUBROUTINE SOILR (ISOIL,IWREDN,IDPTN,ISMTB1,ISMTB2,ISMTB3,ISMTB4,
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

      IF (ISOIL .EQ. 1) OPEN (UNIT=21,FILE='SOIL1.DAT',STATUS='OLD')
      IF (ISOIL .EQ. 2) OPEN (UNIT=21,FILE='SOIL2.DAT',STATUS='OLD')
      IF (ISOIL .EQ. 3) OPEN (UNIT=21,FILE='SOIL3.DAT',STATUS='OLD')
      IF (ISOIL .EQ. 4) OPEN (UNIT=21,FILE='SOIL4.DAT',STATUS='OLD')
      IF (ISOIL .EQ. 5) OPEN (UNIT=21,FILE='SOIL5.DAT',STATUS='OLD')
      IF (ISOIL .EQ. 6) OPEN (UNIT=21,FILE='SOIL6.DAT',STATUS='OLD')

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

    
      CLOSE (21)
      RETURN                                                            
      END
