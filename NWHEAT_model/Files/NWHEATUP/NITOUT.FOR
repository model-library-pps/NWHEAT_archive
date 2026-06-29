*---------------------------------------------------------------------------*
*  SUBROUTINE NITOUT                                                        *
*  Author : Rob Groot                                                       *
*  Date   : january 1987                                                    *
*  Purpose: This subroutine writes soil nitrogen status to the              *
*	    output file *****NIT.OUT                                        *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                       units     class   *
*  ----     -------                                       -----     -----   *
*  IDAY     julian daynumber                                =         I,T   *
*  ANLAY    array, amount of nitrogen in soil comptmnt    kg(N)/ha    I     *
*  NTOT     total amount of nitrogen in soil profile      kg(N)/ha    I     *
*  NDF      nitrogen uptake rate by diffusion             kg(N)/ha/d  I     *
*	    stream                 	                  kg(N)/ha/d  I     *
*  NUPTR    nitrogen uptake rate of the crop              kg(N)/ha/d  I     *
*  TNUPTC   total N-uptake by the crop                    kg(N)/ha    I     *
*  TMINOM   total N mineralization from humus             kg(N)/ha    I     *
*  TMINFM   total N mineralization from crop residues     kg(N)/ha    I     *
*  TIMMOB   total N immobilization  in biomass            kg(N)/ha    I     *
*  TNLOSS   total N lost from soil nitrate N by                             *
*	    immobilization                                kg(N)/ha    I     *
*  TLEACH   total N lost by leaching                      kg(N)/ha    I     *
*  FMTOT    total amount of fresh material                kg/ha       I     *
*  REMARK   output remarks                                  =         I     *  
*  RUNNAME  first 5 characters for name of output files               I     *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : soil nitrogen status is written to output file              *
*	    NITOUT.DAT                                                      *
*---------------------------------------------------------------------------*


      SUBROUTINE NITOUT (IDAY,ANLAY,NTOT,NDF,NUPTR,TNUPTC,
     $                   TMINOM,TMINFM,TIMMOB,TNLOSS,TLEACH,FMTOT,
     $                   REMARK,RUNNAME)                   


      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)
      LOGICAL INIT
      CHARACTER REMARK*80,RUNNAME*5,OUTFIL*12
      DIMENSION ANLAY(10)

      COMMON /ITIME/ ISYEAR,ISDAY
      DATA INIT /.TRUE./

      IF (.NOT.INIT) GOTO 100
      INIT = .FALSE.                                                  


*---- open file #####NIT.OUT 


      OUTFIL = RUNNAME//'NIT.OUT'
      OPEN (4,FILE=OUTFIL,STATUS='NEW')


      WRITE (4,'(//A,A)') ' DATA-FILE  ', OUTFIL

      WRITE (4,'(//(A,I3,A,I2))')
     $      ' Simulation starts on day ',ISDAY,', 19',ISYEAR


*---- write output remarks to file nitout.dat

      WRITE (4,'(1X,A)') REMARK


*---- write header file NITOUT.DAT

      WRITE (4,'(/A)') ' SOIL NITROGEN STATUS'
      WRITE (4,'(A)')  ' ===================='
      WRITE (4,'(4A)')
     $     ' DAY N00-20  N20-40  N40-60  N60-80  N80-100 ',
     $     '  NTOT    NDF NUPTR',
     $     '  TMNOM TMNFM TIMMO TNLOS FMTOT TLEACH'

100   CONTINUE


*---- write header file NITOUT.DAT

*      WRITE (4,'(/A)') ' SOIL NITROGEN STATUS'
*      WRITE (4,'(A)')  ' ===================='
*      WRITE (4,'(4A)')
*     $     ' DAY ANL1 ANL2 ANL3 ANL4 ANL5 ANL6',
*     $     '  ANL7 ANL8 ANL9 ANL0  NDF NUPTR',
*     $     '  TMNOM TMNFM TIMMO TNLOS FMTOT TLEACH',
*     $     ' 0-30 30-60 60-100'

*100   CONTINUE


*---- output data are rearranged to obtain output for comparison with
*     field measurements

      N0020 = ANLAY(1) + ANLAY(2) + ANLAY(3)
      N2040 = ANLAY(4) + ANLAY(4) 
      N4060 = ANLAY(6) + ANLAY(7)
      N6080 = ANLAY(8) + ANLAY(9)
      N80100= ANLAY(10)

      WRITE (4,'(I4,6F8.1,2F6.2,6F6.1)')
     $       IDAY,N0020,N2040,N4060,N6080,N80100,NTOT,NDF,NUPTR,TMINOM,
     $       TMINFM,TIMMOB,TNLOSS,FMTOT,TLEACH


*---- output data are rearranged to obtain output for comparison with
*     field measurements

*      AN1 = ANLAY(1) + ANLAY(2) + ANLAY(3) + ANLAY(4)
*      AN2 = ANLAY(5) + ANLAY(6) + ANLAY(7)
*      AN3 = ANLAY(8) + ANLAY(9) + ANLAY(10)


*      WRITE (4,'(I4,10F5.1,2F6.2,9F6.1)')
*     $       IDAY,(ANLAY(I),I=1,10),NDF,NUPTR,TMINOM,
*     $       TMINFM,TIMMOB,TNLOSS,FMTOT,TLEACH,AN1,AN2,AN3


      RETURN
      END
