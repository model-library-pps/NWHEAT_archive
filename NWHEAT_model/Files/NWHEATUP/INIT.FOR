!***********************#file "INIT.FOR" ********************************!
!************************************************************************!
!*  SUBROUTINE INIT                                                     *!
!*  Author : Rob Groot                                                  *!
!*  Date   : 02-jan-1989                                                *!
!*  Purpose: This subroutine reads data required to run SOIL-program    *!
!*           interactively from  screen                                 *!
!*                                                                      *!
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *!
!*  name    meaning                                     units  class    *!
!*  ----    -------                                     -----  -----    *!
!*  IYEAR   year in which simulation starts               -      T,O    *!
!*  IDAY    day on which simulation starts                d      T,O    *!
!*  IFINIT  maximum number of days for single run         d      T,O    *!
!*  IGAP    The maximum difference in days that is allowed              *!
!*          between subsequent data file records.         d      T,O    *!
!*  REMARK  remark, printed on top of output files        -      O      *!
!*  RUNNAM first 5 char's of output file-name (CHAR*!5)   -      O      *!
!*  STATR   name of weather station (CHAR*5)              -      O      *!
!*                                                                      *!
!*  SUBROUTINES and FUNCTIONS called : none                             *!
!*                                                                      *!
!*  FILE usage : none                                                   *!
!************************************************************************!
      SUBROUTINE INIT (IYEAR,IDAY,IFINIT,IGAP,REMARK,RUNNAM,STATR)

      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)
      CHARACTER REMARK*80,RUNNAM*5,STATR*5
      COMMON /ITIME/ ISYEAR,ISDAY

*---- maximum difference in days that is allowed between subsequent
*     data in weather-data-files (igap)
      IGAP  = 10

      CALL CLS

      WRITE (*,'(////////A)') ' Crop & soil nitrogen dynamics'
      WRITE (*,'(/////A)') ' start year for simulation         : '
      READ  (*,'(I4)') IYEAR
     
      WRITE (*,'(a)')	   ' start day  for simulation         : '
      READ  (*,'(I3)') IDAY

      WRITE (*,'(A)') ' length of simulation (days)       : '
      READ  (*,'(I4)') IFINIT

      WRITE (*,'(A)') ' weather station (max.5 characters): '
      READ  (*,'(a)') STATR

      WRITE (*,'(a)') ' output filename (max.5 characters): '
      READ  (*,'(a)') RUNNAM

      WRITE (*,'(A)') ' output remarks                    : '
      READ  (*,'(a)') REMARK

      IYEAR  = IYEAR - 1900
      ISYEAR = IYEAR
      ISDAY  = IDAY

      RETURN
      END
!***********************End of "INIT.FOR" *******************************!
