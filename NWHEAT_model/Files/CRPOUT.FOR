*---------------------------------------------------------------------------*
*  SUBROUTINE CRPOUT                                                        *
*  Author : Rob Groot                                                       *
*  Date   : january 1987                                                    *
*  Purpose: This subroutine writes crop status to the                       *
*	    output file CRPOUT.DAT                                          *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                    units     class      *
*  ----     -------                                    -----     -----      *
*  IDAY     julian daynumber                             =         I,T      *
*  WLV      weight of leaves                           kg/ha       I        *
*  WLD      weight of dead leaves                      kg/ha       I        *
*  WST      weight of stems                            kg/ha       I        *
*  WRES     weight of reserves                         kg/ha       I        *
*  WSTOT    total stem weight (WST+WRES+WSD)           kg/ha       I        *
*  WRT      weight of roots                            kg/ha       I        *
*  WGR      weight of grain                            kg/ha       I        *
*  LAI      leaf area index                            m2/m2       I        *
*  DVS      crop development stage                       =         I        *
*  NFLV     nitrogen fraction of leaves                  =         I        *
*  NFLD     nitrogen fraction of dead leaves             =         I        *
*  NFSTOT   nitrogen fraction of measured stem weight    =         I        * 
*  NFGR     nitrogen fraction of the grain               =         I        *
*  RD       rooting depth                                cm        I        *
*  NTOT     total nitrogen uptake by the crop          kg/ha       I        *
*  WTOT     total above ground dry matter              kg/ha       I        *
*  REMARK   output remarks                               =         I        *
*  RUNNAME  first 5 characters for name of output files            I        *
*  NMAXL    max. N-content leaves                      kg/kg       I        *
*  RED      reduction factor for photosynthesis          =         I        *
*	    as a result of water stress                  =         I        *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : crop status is written to output file                       *
*	    CRPOUT.DAT                                                      *
*---------------------------------------------------------------------------*

      SUBROUTINE CRPOUT (IDAY,WLV,WLD,WST,WRES,WSTOT,WRT,WGR,
     $                   LAI,DVS,NFLV,NFLD,NFSTOT,NFGR,RD,NUPTAG,NUPTT,
     $                   TAGB,TB,TPARAB,TPAR,REMARK,RUNNAME,NMAXL,
     $                   RED,TERMIN,START,TMPSUM,STATR,ISOIL,ICROP,
     $                   IORGM,INSOIL)

      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)
      LOGICAL TERMIN, START, OPEN
      CHARACTER REMARK*80,RUNNAME*5,OUTFIL*12,STATR*5
      COMMON /ITIME/ ISYR, ISYEAR, ISDAY
      DATA OPEN /.TRUE./
      

      IF (OPEN) THEN

*---- open output file CRPOUT.DAT

      OUTFIL = RUNNAME//'CRP.OUT'
      OPEN(13,FILE=OUTFIL,STATUS='NEW')
      
      OPEN = .FALSE.
      ENDIF

      IF (.NOT. START) ISTART = 0
      IF (TMPSUM .LT. 125.) GO TO 999

      IF (START .AND. ISTART .EQ. 0) ISTART = IDAY

      IF (IDAY .EQ. ISTART) THEN
*---- write filename and start date on top of output-file

      WRITE (13,'(//A,A)') 'DATA-FILE  ', OUTFIL
      WRITE (13,'(//A,A)') 'Name of run: ', RUNNAME 
      WRITE (13,'(//A,A,A,I3,A,I3,A,I3,A,I3)') 'Station=  ',STATR,
     $   ', Soil=',ISOIL,', Crop=',ICROP,', Organic m.=',IORGM, 
     $   ', Nsoil=', INSOIL
      WRITE (13,'(/(A,I3,A,I4))')
     $      ' Simulation starts on day ',ISDAY,', ',ISYEAR


*---- Write output remarks to file CRPOUT.DAT

      WRITE (13,'(1X,A)') REMARK


*---- writes header for to output file CRPOUT.DAT
      WRITE (13,'(/A)') ' CROP PRODUCTION'
      WRITE (13,'(A)')  ' ==============='


         WRITE (13,'(3A)')
     $  ' DAY    WLV    WLD    WST   WRES  WSTOT   WRT',
     $  '    WGR    TAGB    TB    LAI   DVS   RD   NPLV  NPLD  ',
     $  'NPSTOT NPGR NUPTA NUPTT  RED    PARAB    PAR'

      
            
      IDANTH = 0
      ICOUNT = 0
      
      ENDIF



*---- Express N-contents as percentages
   
      NPLV   = NFLV*100.
      NPLD   = NFLD*100.
      NPSTOT = NFSTOT*100.            
      NPGR   = NFGR*100.
      NPMAXL = NMAXL*100

*-----express PAR in MJ/m2
      TPARAB1 = TPARAB/1000000.
      TPAR1   = TPAR/1000000.
 
      IF (DVS .GE. 1. .AND. IDANTH .EQ. 0) IDANTH = IDAY

      ICOUNT = ICOUNT+1

*      IF (MOD(ICOUNT,30).EQ.1) THEN

*         WRITE (13,'(3A)')
*     $  ' DAY    WLV    WLD    WST   WRES  WSTOT   WRT',
*     $  '    WGR    WTOT   LAI   DVS   RD   NPLV  NPLD  NPSTOT NPGR',
*     $  '  NTOT  RED'

*      ENDIF
    
        
      IF (MOD(ICOUNT,10) .EQ. 1 .OR. IDAY .EQ. IDANTH .OR. TERMIN) THEN

      WRITE (13,'(1X,I4,9F7.0,2F6.2,F6.1,4F6.2,2F6.1,F6.2,2F8.1))')
     $       IDAY,WLV,WLD,WST,WRES,WSTOT,WRT,
     $       WGR,TAGB,TB,LAI,DVS,RD,NPLV,NPLD,NPSTOT,NPGR,NUPTAG,
     $       NUPTT,RED,TPARAB1,TPAR1

      ENDIF
      IF (IDAY .EQ. ISTART) ICOUNT = ICOUNT - 1
999   CONTINUE
      RETURN
      END
