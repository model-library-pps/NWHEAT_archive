!***********************#file "TIMER.FOR" *******************************!
!************************************************************************!
!*  SUBROUTINE TIMER                                                    *!
!*  Author : Rob Groot                                                  *!
!*  Date   : december 1986                                              *!
!*  Purpose: This subroutine updates TIME with DELT.                    *!
!*                                                                      *!
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *!
!*  name     meaning                                    units  class    *!
!*  ----     -------                                    -----  -----    *!
!*  TIME     running value for time                       =      I,T    *!
!*  DELT     time step of integration                   days     I,T    *!
!*  FINTIM   time at which simulation is finished         =      I,T    *!
!*  STYEAR   year in which simulation starts              =      I,T    *!
!*  IDAY     julian daynumber                             =      O,T    *!
!*  YEAR     running value for year                       =      O,T    *!
!*  TERMIN   logical, simulation is finished when.TRUE.          O      *!
!*                                                                      *!
!*  SUBROUTINES called : none                                           *!
!*  FILE usage : none                                                   *!
!************************************************************************!

      SUBROUTINE TIMER(IDAY,IYEAR,IFINIT,TERMIN)

      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)
      LOGICAL TERMIN


*---- write daynumber to screen

      CALL CLS
      WRITE (*,*) IDAY


*---- update daynumber

      IDAY = IDAY+1
      IRUN = IRUN+1


*---- end of simulation is induced when finish time is reached

      IF (IRUN.EQ.IFINIT) THEN
         TERMIN=.TRUE.
         WRITE (*,'(///2A)') ' finish conditions reached,',
     1                      ' simulation terminated'
      ENDIF


*---- January 1 is defined as DAY=1

      IF (IDAY.GT.365) THEN
         IDAY = 1
         IYEAR = IYEAR+1
      END IF


      RETURN
      END
!***********************End of "TIMER.FOR" ******************************!
