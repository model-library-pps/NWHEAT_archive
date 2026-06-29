       PROGRAM CONVERT
       IMPLICIT REAL (A-H,J-Z)
       IMPLICIT INTEGER (I)
       OPEN (6, FILE= '89.txt')
       OPEN(7,FILE= 'UPPS89.NEW')

       DO 100 I=1,365

       READ(6, *) IYEAR,IDAY,TMIN,TMAX,RAIN,RAD,IWIND,VAP
       IYEAR= 1900 + IYEAR
       VAP1 = VAP/10.
       RAD1= 1000.*RAD
       WIND= REAL(IWIND)
       WIND1= 1000.*WIND/(24.*60.*60.)
       II=1
       WRITE (7,2000) II,IYEAR,IDAY,RAD1, TMIN,TMAX,VAP1,WIND1,RAIN
100    CONTINUE

2000   FORMAT(I4,I5,I4, F7.0,2F6.1,F8.3,F6.1,F6.1)
       END