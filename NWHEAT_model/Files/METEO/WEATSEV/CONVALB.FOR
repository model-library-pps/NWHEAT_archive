       PROGRAM CONVCL
       IMPLICIT REAL (A-H,J-Z)
       IMPLICIT INTEGER (I)
       CHARACTER LOC*4

       OPEN (6, FILE= 'ALBA91.OLD')
       OPEN(7,FILE= 'ALBA91.DAT')

       DO 100 I=1,365
       READ (6,1000) LOC,IYEAR,IDAY,RAD,TMAX,TMIN,RAIN,X
1000   FORMAT (A4,I3, I4, F6.2, F6.1, F6.1, F6.1,F7.2)
*        READ (6,*) LOC,IYEAR,IDAY,RAD,TMAX,TMIN,RAIN,X     
  
       IF (IDAY .LE. 31) VAP1= 1.014
       IF (IDAY .LE. 31) WIND1= 1.4
       IF (IDAY .GT. 31 .AND. IDAY .LE. 59) VAP1= 1.076
       IF (IDAY .GT. 31 .AND. IDAY .LE. 59) WIND1= 1.7
       IF (IDAY .GT. 59 .AND. IDAY .LE. 90) VAP1= 1.267
       IF (IDAY .GT. 59 .AND. IDAY .LE. 90) WIND1= 2.0
       IF (IDAY .GT. 90 .AND. IDAY .LE. 120) VAP1= 1.406
       IF (IDAY .GT. 90 .AND. IDAY .LE. 120) WIND1= 1.7
       IF (IDAY .GT. 120 .AND. IDAY .LE. 151) VAP1= 1.566
       IF (IDAY .GT. 120 .AND. IDAY .LE. 151) WIND1= 1.7
       IF (IDAY .GT. 151 .AND. IDAY .LE. 181) VAP1= 1.886
       IF (IDAY .GT. 151 .AND. IDAY .LE. 181) WIND1= 1.7
       IF (IDAY .GT. 181 .AND. IDAY .LE. 212) VAP1= 2.079
       IF (IDAY .GT. 181 .AND. IDAY .LE. 212) WIND1= 1.7
       IF (IDAY .GT. 212 .AND. IDAY .LE. 243) VAP1= 2.180
       IF (IDAY .GT. 212 .AND. IDAY .LE. 243) WIND1= 1.4
       IF (IDAY .GT. 243 .AND. IDAY .LE. 273) VAP1= 1.984
       IF (IDAY .GT. 243 .AND. IDAY .LE. 273) WIND1= 1.4
       IF (IDAY .GT. 273 .AND. IDAY .LE. 304) VAP1= 1.703
       IF (IDAY .GT. 273 .AND. IDAY .LE. 304) WIND1= 1.4
       IF (IDAY .GT. 304 .AND. IDAY .LE. 334) VAP1= 1.253
       IF (IDAY .GT. 304 .AND. IDAY .LE. 334) WIND1= 1.4
       IF (IDAY .GT. 334) VAP1= 1.073
       IF (IDAY .GT. 334) WIND1= 1.4

       IIYEAR= 1900 + IYEAR
       RAD1= 1000.*RAD      
       II=1
       WRITE (7,2000) II,IIYEAR,IDAY,RAD1,TMIN,TMAX,VAP1,WIND1,RAIN
100    CONTINUE

2000   FORMAT(I4,I5,I4, F7.0,2F6.1,F8.3,F6.1,F6.1)
       END