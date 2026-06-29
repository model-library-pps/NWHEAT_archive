*--------------------------------------------------------------------*
*  SUBROUTINE INIT                                                   *
*  Author : Rob Groot                                                *
*  Date   : 02-jan-1989                                              *
*  Purpose: This subroutine reads data required to run SOIL-program  *
*           interactively from  screen                               *
*                                                                    *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)   *
*  name    meaning                                     units  class  *
*  ----    -------                                     -----  -----  *
*  IIYEAR   year in which simulation starts               -      T,O  *
*  INYEAR  number of simulation runs (i.e. years)        -      T,O  *
*  IDAY    day on which simulation starts                d      T,O  *
*  IFINIT  maximum number of days for single run         d      T,O  *
*  IGAP    The maximum difference in days that is allowed            *
*          between subsequent data file records.         d      T,O  *
*  REMARK  remark, printed on top of output files        -      O    *
*  RUNNAME first 5 char's of output file-name (CHAR*5)   -      O    *
*  STATR   name of weather station (CHAR*5)              -      O    *
*  CONTIN  more weather stations                                O    *
*  SENSP   sensitivity parameter for precipitation       -      O    *
*  SENSR    idem for radiation                           -      O    *
*  SENSW    idem for windspeed                           -      O    *
*  SENSV    idem for vapour pressure                     -      O    *
*  SENST    idem for temperature                         -      O    *
*  ISOIL   number of soil type                                  O    *
*  ICROP   number of crop species                               O    *
*  INSOIL  number of soil nitrogen  data base                   O    *
*  IORGM   number of soil organic matter data base              O    *
*  IPM     0 = Penman transpiration; 1 = Penman Monteith transpiration *
*  IOPT    1 = AB weather format; 2 = repaired DBMETEO format        *
*  IRRI    0 = no irrigation; 1 = irrigation                         *
*  RUNFR   fraction of rainfall lost by run off                      *
*  WTRDIR  directory where weather data are stored                   *                         *
*  SUBROUTINES and FUNCTIONS called : none                           *
*                                                                    *
*  FILE usage : none                                                 *
*--------------------------------------------------------------------*

      SUBROUTINE INIT (IIYR,INYEAR,IDAY,IFINIT,IGAP,REMARK,RUNNAME,
     $  STATR,CONTIN,SENSP,SENSV,SENSW,SENSR,SENST,
     $  ISOIL,ICROP,IORGM,INSOIL,IPM,
     $  IOPT,IRRI,RUNFR,WTRDIR)


      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)
      CHARACTER REMARK*80,RUNNAME*5,STATR*5,WTRDIR*80
      CHARACTER ANS*1,CONTIN*1
      LOGICAL BATCH,START    
      DATA START /.TRUE./    
      DATA BATCH /.FALSE./

*---- maximum difference in days that is allowed between subsequent
*     data in weather-data-files (igap)

      IPM= 0
      IGAP  = 10
      ISOIL= 1
      ICROP= 1
      IORGM= 1
      INSOIL= 1
      IOPT= 1 
      IRRI= 0     
      WTRDIR= ' '
      SENSP= 1.
      SENSV= 1.
      SENSW= 1.
      SENSR= 1.
      SENST= 0.
      RUNFR= 0.
      CALL CLS

      IF (.NOT. START) GOTO 10 

*    ask user if batch processing is required
     
      WRITE (*, '(/A$)') '  Do you want to run from a batch file',
     $  '  yes (Y) or no (N) ?            ' 
      READ (*,'(A)') ANS
      IF (ANS .EQ. 'Y' .OR. ANS .EQ. 'y') THEN
      BATCH = .TRUE.
      ENDIF

      IF (BATCH) THEN 
*     batch input
      OPEN (99,FILE= 'BATCH.INP',STATUS= 'OLD')
      CALL MOFILP (99)
      START= .FALSE.
      ENDIF      

10    IF (BATCH) READ (99, *) RUNNAME, IIYEAR, INYEAR, IDAY, IFINIT, 
     $   STATR, SENSP, SENSV, SENSW, SENSR, SENST, 
     $   ISOIL, ICROP, IORGM, INSOIL, IPM,
     $   IOPT, IRRI, RUNFR,WTRDIR, REMARK, CONTIN
      

      IF (BATCH)  GOTO 100
      CONTIN= 'N'     
      WRITE (*,'(////////A)') ' Crop & soil nitrogen dynamics'
      WRITE (*,'(/////A$)') ' start year for simulation         : '
      READ  (*,'(I4)') IIYEAR

      WRITE (*,'(A$)') ' simulation during how many years      : '
      READ  (*,'(I4)') INYEAR
      
      WRITE (*,'(a$)')      ' start day  for simulation         : '
      READ  (*,'(I3)') IDAY

      WRITE (*,'(A$)') ' length of simulation (days)       : '
      READ  (*,'(I4)') IFINIT

      WRITE (*,'(A$)') ' type of weather format, ',
     $  '  AB format (1) or DBMETEO format (2)  : '
      READ (*,'(I1)') IOPT

      WRITE (*,'(A$)') ' weather station (max.5 characters): '
      READ  (*,'(a)') STATR

      WRITE (*,'(a$)') ' output filename (max.5 characters): '
      READ  (*,'(a)') RUNNAME

      WRITE (*,'(A$)') ' output remarks                    : '
      READ  (*,'(a)') REMARK

100   CONTINUE

      IIYR  = IIYEAR - 1900
     

      RETURN
      END
