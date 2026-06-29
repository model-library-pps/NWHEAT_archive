       PROGRAM CONVCL
       IMPLICIT REAL (A-H,J-Z)
       IMPLICIT INTEGER (I)
    
       OPEN(7,FILE= 'WEAT93.DAT')
       OPEN(8,FILE='JOKI93.DAT')

       DO 100 I=1,365

       READ(7, 2000) IYEAR,IDAY,RAIN,TMIN,TMAX,IRH,IWIND,RAD,X,Y
       II= 1
       RH= REAL(IRH)
       WIND= REAL(IWIND)
       IYEAR1= 1900 + IYEAR
       RAD1= RAD * 1000.
       TMEAN= 0.5*(TMIN+TMAX)
       
       ESAT1= 10.**((0.7859+0.03477*TMEAN)/(1.+0.00412*TMEAN))
       FSAT= 1. + 1.0E-6 * 1013.*(4.5+0.0006*TMEAN*TMEAN)
       ESAT= FSAT*ESAT1
       VAP1= RH*ESAT/100.

       WRITE (8,3000) II,IYEAR1,IDAY,RAD1, TMIN,TMAX,VAP1,WIND,RAIN
100    CONTINUE

2000   FORMAT(I2,I4, 3F7.1,2I3,F7.3,2F7.1)

3000   FORMAT(I4,I5,I4, F7.0,2F6.1,F8.3,F6.1,F6.1)
       
       END