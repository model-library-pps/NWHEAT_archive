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

       PROGRAM CONVCL
       IMPLICIT REAL (A-H,J-Z)
       IMPLICIT INTEGER (I)
       CHARACTER*10 FILIN
       CHARACTER*15 FILE1
       CHARACTER*15 FILE2
       CHARACTER*15 FILE3

  
1      WRITE (*,'(//A$)') 
     &  ' Input number (for example 01, 02,..., 31) ==>     '
       READ (*,'(A)') FILIN
       IF (FILIN .EQ. ' ') GO TO 400

       FILE1= FILIN(1:2)//'3140D.DAT'
       FILE2= FILIN(1:2)//'.DAT'
       FILE3= '3140D'//FILIN(1:2)//'.DAT'
       
       IF (FILIN .EQ. '31') FILE2= '30.DAT'
       WRITE (*,'(//1H,3A)') FILE1,FILE2,FILE3

       OPEN(7,FILE= FILE1, STATUS='OLD') 
       OPEN(8,FILE= FILE2, STATUS= 'OLD')
       OPEN(9,FILE= FILE3, STATUS= 'NEW')

       DO 100 I=1,365

       READ(7, *) IYEAR1,IDAY1,TMIN1,TMAX1,RAIN1,RAD1
       READ (8,2000) II,IYEAR2,IDAY2,RAD2,TMIN2,TMAX2,VAP2,WIND2,RAIN2
     
       IIDAY= IDAY1 - IDAY2
       IF (IIDAY .NE. 0) WRITE (*,*) IIDAY, '  ,ERROR'

       IYEAR3=IYEAR2
       IDAY3=IDAY2
       RAD3= RAD1*1000.       
       TMIN3=TMIN1
       TMAX3= TMAX1
       RAIN3= RAIN1
       IDAY= IDAY3

       IF (IDAY .LE. 31) DTEMP= 0.35
       IF (IDAY .LE. 31) DWIND= 0.990
       IF (IDAY .GT. 31 .AND. IDAY .LE. 59) DTEMP= 7.24
       IF (IDAY .GT. 31 .AND. IDAY .LE. 59) DWIND= 0.946
       IF (IDAY .GT. 59 .AND. IDAY .LE. 90) DTEMP= 0.73
       IF (IDAY .GT. 59 .AND. IDAY .LE. 90) DWIND= 0.976
       IF (IDAY .GT. 90 .AND. IDAY .LE. 120) DTEMP= 0.71
       IF (IDAY .GT. 90 .AND. IDAY .LE. 120) DWIND= 0.993
       IF (IDAY .GT. 120 .AND. IDAY .LE. 151) DTEMP= 0.81
       IF (IDAY .GT. 120 .AND. IDAY .LE. 151) DWIND= 1.123
       IF (IDAY .GT. 151 .AND. IDAY .LE. 181) DTEMP= 1.57
       IF (IDAY .GT. 151 .AND. IDAY .LE. 181) DWIND= 1.055
       IF (IDAY .GT. 181 .AND. IDAY .LE. 212) DTEMP= 0.90
       IF (IDAY .GT. 181 .AND. IDAY .LE. 212) DWIND= 1.040
       IF (IDAY .GT. 212 .AND. IDAY .LE. 243) DTEMP= 1.91
       IF (IDAY .GT. 212 .AND. IDAY .LE. 243) DWIND= 0.946
       IF (IDAY .GT. 243 .AND. IDAY .LE. 273) DTEMP= 1.95
       IF (IDAY .GT. 243 .AND. IDAY .LE. 273) DWIND= 0.926
       IF (IDAY .GT. 273 .AND. IDAY .LE. 304) DTEMP= 0.73
       IF (IDAY .GT. 273 .AND. IDAY .LE. 304) DWIND= 1.103
       IF (IDAY .GT. 304 .AND. IDAY .LE. 334) DTEMP= 1.54
       IF (IDAY .GT. 304 .AND. IDAY .LE. 334) DWIND= 1.049
       IF (IDAY .GT. 334) DTEMP= -1.02
       IF (IDAY .GT. 334) DWIND= 0.990

       WIND3= DWIND*WIND2
       TMPA2= (TMIN2 + TMAX2)/2.
       SVAP2= 0.1 * 6.10588 * EXP (17.32491*TMPA2/(TMPA2+238.102))
       RH2= VAP2/SVAP2
       
       TMIN4= TMIN2 + DTEMP
       TMAX4= TMAX2 + DTEMP
       TMPA4= (TMIN4 + TMAX4)/2.
       SVAP4= 0.1 * 6.10588 * EXP (17.32491*TMPA4/(TMPA4+238.102))
       VAP3= RH2*SVAP4


       WRITE(9,2000) II,IYEAR3,IDAY3,RAD3,TMIN3,TMAX3,VAP3,WIND3,RAIN3

100    CONTINUE

       CLOSE (7)
       CLOSE (8)
       CLOSE (9)

       GO TO 1

400    STOP '**** END OF PROGRAM ******'

2000   FORMAT(I4,I5,I4,F7.0,2F6.1,F8.3,F6.1,F6.1)
        END
       
       
  