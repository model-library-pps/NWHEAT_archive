       PROGRAM CONVCL
       IMPLICIT REAL (A-H,J-Z)
       IMPLICIT INTEGER (I)
       OPEN (6, FILE= '70.DAT')
       OPEN(7,FILE= 'OXFO76.DAT')
       OPEN(8,FILE='OXFO76.NEW')

       DO 100 I=1,365

       READ(7, 2000) II, IYEAR,IDAY,RAD,TMIN,TMAX,VAP,WIND,RAIN

       READ(6, 2000) II1, IYEAR1,IDAY1,RAD1,TMIN1,TMAX1,VAP1,WIND1,RAIN1

       IF (IDAY .GE. 275) WRITE (8,2000) II,IYEAR,
     $    IDAY,RAD, TMIN,TMAX,VAP,WIND,RAIN
       IF (IDAY .LT. 275) WRITE (8,2000) II,IYEAR,
     $    IDAY, RAD, TMIN,TMAX,VAP,WIND1,RAIN
100    CONTINUE
       
       READ(7, 2000) II, IYEAR,IDAY,RAD,TMIN,TMAX,VAP,WIND,RAIN
       WRITE(8, 2000) II, IYEAR,IDAY,RAD,TMIN,TMAX,VAP,WIND,RAIN
       

2000   FORMAT(I4,I5,I4, F7.0,2F6.1,F8.3,F6.1,F6.1)
       END