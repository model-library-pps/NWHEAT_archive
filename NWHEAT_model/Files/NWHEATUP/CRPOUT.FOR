!***************************** #file "CRPOUT.FOR" ***********************!
!************************************************************************!
!* SUBROUTINE CRPOUT                                                    *!
!* Author : Rob Groot                                                   *!
!* Date   : january 1987                                                *!
!* Update : Transformed for literate programming by Sjaak Wolfert.      *!
!*          {TIME \@ "MM/dd/yy"|08/30/94}                                                    *!
!* Purpose: This subroutine writes crop status to the                   *!
!*	     output file CRPOUT.DAT                                      *!
!*                                                                      *!
!* SUBROUTINES called : none                                            *!
!* FILE usage : crop status is written to output file CRPOUT.DAT        *!
!************************************************************************!
      SUBROUTINE CRPOUT (IDAY,WLV,WLD,WST,WRES,WSTOT,WRT,WGR,
     $                   LAI,DVS,NFLV,NFLD,NFSTOT,NFGR,RD,NTOT,WTOT,
     $			 REMARK,RUNNAM,NMAXL,RED)

      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)
      LOGICAL INIT
      CHARACTER REMARK*80,RUNNAM*5,OUTFIL*12
      COMMON /ITIME/ ISYEAR,ISDAY
      DATA INIT /.TRUE./

      IF(.NOT.INIT) GOTO 100

          !********** Initilization.                           **********!
	  OUTFIL = RUNNAM//'CRP.OUT'
          OPEN(13,FILE=OUTFIL,STATUS='NEW')
          WRITE (13,'(//A,A)') ' DATA-FILE  ', OUTFIL
          WRITE (13,'(//(A,I3,A,I2))')
     $          ' Simulation starts on day ',ISDAY,', 19',ISYEAR
          WRITE (13,'(1X,A)') REMARK
          WRITE (13,'(/A)') ' CROP PRODUCTION'
          WRITE (13,'(A)')  ' ==============='
          !********** End of Initilization.******************************!

      INIT = .FALSE.
100   CONTINUE

          !********** Conversion to percentages.               **********!
          NPLV   = NFLV*100.
          NPLD   = NFLD*100.
          NPSTOT = NFSTOT*100.
          NPGR   = NFGR*100.
          NPMAXL = NMAXL*100
          !********** End of Conversion to percentages.******************!

          !********** Write parameters to file.                **********!
          ICOUNT = ICOUNT+1
          IF (MOD(ICOUNT,30).EQ.1) THEN
              WRITE (13,'(3A)')
     $              ' DAY    WLV    WLD    WST   WRES  WSTOT   WRT',
     $              '    WGR    WTOT   LAI   DVS   RD   NPLV  NPLD',
     $              '  NPSTOT NPGR  NTOT  RED'
          ENDIF

          WRITE (13,'(1X,I4,8F7.0,2F6.2,F6.1,4F6.2,F6.1,F6.2))')
     $           IDAY,WLV,WLD,WST,WRES,WSTOT,WRT,
     $           WGR,WTOT,LAI,DVS,RD,NPLV,NPLD,NPSTOT,NPGR,NTOT,RED
          !********** End of Write parameters to file. ******************!

      RETURN
      END
!***************************** End of CRPOUT.FOR ************************!
