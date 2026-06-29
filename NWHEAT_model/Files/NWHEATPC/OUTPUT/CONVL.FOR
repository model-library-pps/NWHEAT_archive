       PROGRAM CONVCL
       IMPLICIT REAL (A-H,J-Z)
       IMPLICIT INTEGER (I)
       CHARACTER*10 FILIN
       CHARACTER*15 FILE1
       CHARACTER*15 FILE2
       CHARACTER*15 FILE3

  
1      WRITE (*,'(//A$)') 
     &  ' Input name (for example ROBASMP1) ==>     '
       READ (*,'(A)') FILIN
       IF (FILIN .EQ. ' ') GO TO 400

       FILE1= FILIN(1:8)//'.DAT'
       
       FILE3= FILIN(1:8)//'.WOL'
       
       
       WRITE (*,'(//1H,3A)') FILE1, FILE3

       OPEN(7,FILE= FILE1, STATUS='OLD') 
       
       OPEN(9,FILE= FILE3, STATUS= 'NEW')

       DO 100 I=1,30

       READ(7, *) IYEAR,ISOW,IEMERG,IANTH,IHALT,WRT,WLV,WST,WGR,TAGB
        
       TAGB1=TAGB/1000.
       WGR1= WGR/1000.
       

       WRITE(9,2000) I,IYEAR,ISOW,IANTH,IHALT,TAGB1,WGR1

100    CONTINUE

       CLOSE (7)
     
       CLOSE (9)

       GO TO 1

400    STOP '**** END OF PROGRAM ******'

2000   FORMAT(I4,I6,3I5,2F7.2)
        END
       
       
  