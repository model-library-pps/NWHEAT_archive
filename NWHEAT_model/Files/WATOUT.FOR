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
*  WCACT    array, actual water content of each soil                        *
*  ACT      array, actual water content of each soil                         *
*	    compartment                                 cm/cm      I        *
*  TTRANS   total transpiration over simulated period    cm        I        *
*  TDRAIN   total drainage over simulated period         cm        I        *
*  TRAIN    total precipitation over sim. period         cm        I        *
*  TESOIL   total soil evaporation over sim. period      cm        I        *
*  TRUNOF   total water loss by runoff                   cm        I        *
*  TIRR     total water supply by irrigation             cm        I        *
*  RUNFR    fraction of rainfall lost by run off         -         I        *
*  IRR      0 = no irrigation; 1 = irrigation                      I        *
*  REMARK   output remarks                               =         I        *
*  RUNNAME  first 5 characters for name of output files            I        *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : soil waterstatus is written to output file                  *
*	    WATOUT.DAT                                                      *
*---------------------------------------------------------------------------*
    
	SUBROUTINE WATOUT (IDAY,WCACT,
     1         TTRANS,TDRAIN,TRAIN,TESOIL,TRUNOF,TIRR,RUNFR,IRRI,
     1         WTOT,WAVT,WAVR,REMARK,
     1         RUNNAME,TERMIN,START,INIT,STATR,ISOIL,ICROP,IORGM,INSOIL)

	IMPLICIT REAL (A-H,J-Z)
	IMPLICIT INTEGER (I)
	LOGICAL INIT,TERMIN,START,OPEN
	CHARACTER REMARK*80,RUNNAME*5,OUTFIL*12,STATR*5
	DIMENSION WCACT(10)
      
        COMMON /ITIME/ ISYR, ISYEAR, ISDAY
        
	DATA OPEN /.TRUE./

        IF (OPEN) THEN

*------	open output file #####WAT.OUT
        OPEN = .FALSE.
	OUTFIL = RUNNAME//'WAT.OUT'	
	OPEN (11,FILE=OUTFIL,STATUS='NEW')
        ENDIF

        IF (INIT) THEN

        WRITE (11,'(//A,A)') 'DATA-FILE  ', OUTFIL
        WRITE (11,'(//A,A)') 'Name of run: ',RUNNAME
        WRITE (11,'(//A,A,A,I3,A,I3,A,I3,A,I3,A,I3,A,F4.2)') 
     $  'Station= ',STATR,
     $  ', Soil=',ISOIL,', Crop=',ICROP,', Organic m.=',IORGM, 
     $  ', Nsoil=',INSOIL,', Irr.=',IRRI,', Runof=',RUNFR
	WRITE (11,'(/(A,I3,A,I4))')
     $      ' Simulation starts on day ',ISDAY,', ',ISYEAR

*------	write output remarks to file WATOUT.DAT

	WRITE (11,'(1X,A)') REMARK

*------ writes header for to output file 

	   WRITE (11,'(/A)') ' SOIL WATER STATUS'
	   WRITE (11,'(A)')  ' ================='
	   WRITE (11,'(3A)')
     1     '   DAY   WCACT1  WCACT2  WCACT3  WCACT4  WCACT5  WCACT6',
     2     '   WCACT7  WCACT8  WCACT9  WCACT10  TTRANS TDRAIN  TRAIN',
     3     '   TESOIL   TIRR  TRUNOF     WTOT    WAVT    WAVR'

        ICOUNT = -1       
        ISTART = 0
	INIT = .FALSE.                                                  
        
        ENDIF

*------ write soil water status data to WATOUT.DAT during simulation 

      IF (START .AND. ISTART .EQ. 0) ISTART = IDAY
      IF (IDAY .EQ. ISTART) ICOUNT = 0

      ICOUNT = ICOUNT + 1
      IF (MOD(ICOUNT,10) .EQ. 1 .OR. TERMIN) THEN

      WRITE (11,'(1X,I5,14F8.2,5F8.2)') 
     1       IDAY,(WCACT(I),I=1,10),TTRANS,TDRAIN,TRAIN,TESOIL,
     1       TIRR,TRUNOF,WTOT,WAVT,WAVR

      ENDIF

      RETURN
      END
