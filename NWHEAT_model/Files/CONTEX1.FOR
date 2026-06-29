       PROGRAM CONVCL
       IMPLICIT REAL (A-H,J-Z)
       IMPLICIT INTEGER (I)
       CHARACTER*10 FILIN
      
       CHARACTER*15 FILE2
       CHARACTER*15 FILE3
       CHARACTER*100 LINE




  
 1      WRITE (*,'(//A$)') 
     &  ' Input number (for example 01, 02,..., 31) ==>     '
       READ (*,'(A)') FILIN
       IF (FILIN .EQ. ' ') GO TO 400


       FILE2= 'BAS'//FILIN(1:2)//'.DAT'
       FILE3= 'BASE'//FILIN(1:2)//'.DAT'
       
       
       WRITE (*,'(//1H,3A)') FILE2,FILE3

       
       OPEN(8,FILE= FILE2, STATUS= 'OLD')
       OPEN(9,FILE= FILE3, STATUS= 'NEW')

*       CALL MOFILP (8)
        
       DO 50 I=1,18
       READ (8,'(A)') LINE
       WRITE (9,'(A)') LINE
50     CONTINUE

       READ (8,*) LONGI, LATI, ALTI, A,B
       WRITE (9,1000) LONGI, LATI, ALTI, A,B

       DO 100 I=1,365
       READ (8,2000) II,IYEAR2,IDAY2,RAD2,TMIN2,TMAX2,VAP2,WIND2,RAIN2
   
       IYEAR3=IYEAR2
       IDAY3=IDAY2
       RAD3= RAD2       
       TMIN3=TMIN2
       TMAX3= TMAX2
       RAIN3= RAIN2
       IDAY= IDAY3

       VAP3= VAP2
       WIND3= WIND2

       WRITE(9,2000) II,IYEAR3,IDAY3,RAD3,TMIN3,TMAX3,VAP3,WIND3,RAIN3

100    CONTINUE

       
       CLOSE (8)
       CLOSE (9)

       GO TO 1

400    STOP '**** END OF PROGRAM ******'

1000   FORMAT(F7.1,F6.1,F6.0,2F6.2)
2000   FORMAT(I4,I5,I4,F7.0,2F6.1,F8.3,F6.1,F6.1)
        END
       
       
  