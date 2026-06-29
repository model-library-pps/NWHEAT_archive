       PROGRAM CONVCL
       IMPLICIT REAL (A-H,J-Z)
       IMPLICIT INTEGER (I)
       OPEN (6, FILE= 'Z')
       OPEN(7,FILE= 'DEBR87.DAT')

       DO 100 I=1,365

       READ(6, *) IDAY,IMDAY,IMONTH,IYEAR,TMAX,TMIN,TMEAN,RAIN,SUNH,
     $  RAD,VAP,RH,WIND

       IF (IDAY .LT. 135 .OR. IDAY .GT. 334) THEN
       WIND1= 3.4
       ELSE
       WIND1= 2.6
       ENDIF

       VAP1= VAP/10.
       RAD1= 1000.*RAD      
       II=1
       WRITE (7,2000) II,IYEAR,IDAY,RAD1, TMIN,TMAX,VAP1,WIND1,RAIN
100    CONTINUE

2000   FORMAT(I4,I5,I4, F7.0,2F6.1,F8.3,F6.1,F6.1)
       END