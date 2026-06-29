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
*  WCACT    array, actual water content of each soil   ACT    array, actual water content of each soil   ACT    array, actual water content of each soil                        *
*	    compartment                                 cm/cm      I        *
*  TTRANS   total transpiration over simulated period    cm        I        *
*  TDRAIN   total drainage over simulated period         cm        I        *
*  TRAIN    total precipitation over sim. period         cm        I        *
*  TESOIL   total soil evaporation over sim. period      cm        I        *
*  REMARK   output remarks                               =         I        *
*  RUNNAME  first 5 characters for name of output files            I        *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : soil waterstatus is written to output file                  *
*	    WATOUT.DAT                                                      *
*---------------------------------------------------------------------------*
    
	SUBROUTINE WATOUT (IDAY,WCACT,
     1                     TTRANS,TDRAIN,TRAIN,TESOIL,REMARK,RUNNAME)

	IMPLICIT REAL (A-H,J-Z)
	IMPLICIT INTEGER (I)
	LOGICAL INIT
	CHARACTER REMARK*80,RUNNAME*5,OUTFIL*12
	DIMENSION WCACT(10)
        COMMON /ITIME/ ISYEAR,ISDAY
        
	DATA INIT /.TRUE./

        IF (.NOT.INIT) GOTO 100

*------	open output file #####WAT.OUT

	OUTFIL = RUNNAME//'WAT.OUT'	
	OPEN (11,FILE=OUTFIL,STATUS='NEW')

        WRITE (11,'(//A,A)') ' DATA-FILE  ', OUTFIL

	WRITE (11,'(//(A,I3,A,I2))')
     1      ' Simulation starts on day ',ISDAY,', 19',ISYEAR

*------	write output remarks to file WATOUT.DAT

	WRITE (11,'(1X,A)') REMARK

**------ writes header for to output file 

*	   WRITE (11,'(/A)') ' SOIL WATER STATUS'
*	   WRITE (11,'(A)')  ' ================='
*	   WRITE (11,'(3A)')
*     1     '   DAY   WCACT1  WCACT2  WCACT3  WCACT4  WCACT5  WCACT6',
*     2     '   WCACT7  WCACT8  WCACT9  WCACT10  TTRANS TDRAIN  TRAIN',
*     3     '   TESOIL'

*	INIT = .FALSE.                                                  

*100	CONTINUE

**------ write soil water status data to WATOUT.DAT during simulation 


*      WRITE (11,'(1X,I5,14F8.2)') 
*     1       IDAY,(WCACT(I),I=1,10),TTRANS,TDRAIN,TRAIN,TESOIL



*------ writes header for to output file 

	   WRITE (11,'(/A)') ' SOIL WATER STATUS'
	   WRITE (11,'(A)')  ' ================='
	   WRITE (11,'(3A)')
     1     '   DAY   WC0020  WC2040  WC4060  WC6080  WC80100 ',
     2     ' TTRANS TDRAIN  TRAIN',
     3     '   TESOIL'

	INIT = .FALSE.                                                  

100	CONTINUE

*------ rearrange output to layers of 20 cm each

	W0020 = (WCACT(1)*2.+WCACT(2)*8.+WCACT(3)*10.)/20.
	W2040 = (WCACT(4)+WCACT(5))/2.
	W4060 = (WCACT(6)+WCACT(7))/2.
	W6080 = (WCACT(8)+WCACT(9))/2.
	W80100=  WCACT(10)


*------ write soil water status data to WATOUT.DAT during simulation 


      WRITE (11,'(1X,I5,5F8.2,4F8.2)') 
     1       IDAY,W0020,W2040,W4060,W6080,W80100,
     2       TTRANS,TDRAIN,TRAIN,TESOIL


      RETURN
      END
