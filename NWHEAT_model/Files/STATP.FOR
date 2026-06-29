      SUBROUTINE STATP (RUNNAME,REMARK,STATR,ISOIL,ICROP,IORGM,
     $    INSOIL,TERMY)
*-------------------------------------------------------------------------------------------------------*
*    STATP :  statistics of production limited by availability of water and nutrients.                  *
*    Subroutine for statistical treatment of yield variables simulated in a series of                   *
*    runs with the NWHEAT model.                                                                        *
*    The yield data refer to one crop and a time series of daily weather data from one weather station. *
*-------------------------------------------------------------------------------------------------------*

      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)
      LOGICAL TERMY,OPEN,INIT
      CHARACTER RUNNAME*5,NWSTAT*30,STATR*5,REMARK*80

      COMMON /ITIME/ ISYR, ISYEAR, ISDAY
      COMMON /CROPOUT/ ISOW1,ISTART1,IDANTH1,IDHALT,TWRT,TWST,TWLV,TWGR
      COMMON /CROPOUT/ TAGB1,HI,LAIMAX1,TPARAB1,RUE,NUPTAG1
      COMMON /WATBOUT/ TTRANS1,TDRAIN1,TRAIN1,TESOIL1,DWAVT
      COMMON /WATBOUT/ TRUNOF1,TIRR1,RUNFR1,IRRI1
      COMMON /NITBOUT/ TNUPTC1,TMINOM1,TMINFM1,TBIOM1
      COMMON /NITBOUT/ TNIMM1,TLEACH1,TNRAIN1,TNGIFT1,DFMTOT,DNTOT

      DATA OPEN /.FALSE./
      DATA INIT /.TRUE./

      IF (.NOT. OPEN)  THEN
      OPEN= .TRUE.

*------ open output file
       NWSTAT = RUNNAME//'.STA'
       OPEN (80,FILE=NWSTAT,STATUS='NEW', ACCESS='SEQUENTIAL')
       ENDIF

        IF (.NOT. INIT) GOTO 100

*------ initialization number of runs and means
      
       IWRUN= 0
       MSOW= 0.
       MSTAR= 0.
       MANT= 0.
       MHALT= 0.
       MWRT= 0.
       MWLV= 0.
       MWST= 0.
       MWGR= 0.
       MAGB= 0.
       MHI= 0.
       MLAI= 0.
       MPARA= 0.
       MRUE= 0.
       MDWA= 0.
       MRAIN= 0.
       MEV= 0.
       MTRA= 0.
       MDRAI= 0.
       MIRR= 0.
       MRUNO= 0.
       MWUE= 0.
       MMINO= 0.
       MMINF= 0.
       MDNTO= 0.
       MNRAI= 0.
       MNGIF= 0.
       MNUP= 0.
       MLEAC= 0.
       MIMM= 0.
       MBIOM= 0.
       MNUE= 0.
       MDFMT= 0.


*------ initialization sums
      
       SUMSOW= 0.
       SUMSTAR= 0.
       SUMANT= 0.
       SUMHALT= 0.
       SUMWRT= 0.
       SUMWLV= 0.
       SUMWST= 0.
       SUMWGR= 0.
       SUMAGB= 0.
       SUMHI= 0.
       SUMLAI= 0.
       SUMPARA= 0.
       SUMRUE= 0.
       SUMDWA= 0.
       SUMRAIN= 0.
       SUMEV= 0.
       SUMTRA= 0.
       SUMDRAI= 0.
       SUMIRR= 0.
       SUMRUNO= 0.
       SUMWUE= 0.
       SUMMINO= 0.
       SUMMINF= 0.
       SUMDNTO= 0.
       SUMNRAI= 0.
       SUMNGIF= 0.
       SUMNUP= 0.
       SUMLEAC= 0.
       SUMIMM= 0.
       SUMBIOM= 0.
       SUMNUE= 0.
       SUMDFMT= 0.


*------ initialization square sums
       
       S2MSOW= 0.
       S2MSTAR= 0.
       S2MANT= 0.
       S2MHALT= 0.
       S2MWRT= 0.
       S2MWLV= 0.
       S2MWST= 0.
       S2MWGR= 0.
       S2MAGB= 0.
       S2MHI= 0.
       S2MLAI= 0.
       S2MPARA= 0.
       S2MRUE= 0.
       S2MDWA= 0.
       S2MRAIN= 0.
       S2MEV= 0.
       S2MTRA= 0.
       S2MDRAI= 0.
       S2MIRR= 0.
       S2MRUNO= 0.
       S2MWUE= 0.
       S2MMINO= 0.
       S2MMINF= 0.
       S2MDNTO= 0.
       S2MNRAI= 0.
       S2MNGIF= 0.
       S2MNUP= 0.
       S2MLEAC= 0.
       S2MIMM= 0.
       S2MBIOM= 0.
       S2MNUE= 0.
       S2MDFMT= 0.


*------ initialization standard deviations
      
       SDSOW= 0.
       SDSTAR= 0.
       SDANT= 0.
       SDHALT= 0.
       SDWRT= 0.
       SDWLV= 0.
       SDWST= 0.
       SDWGR= 0.
       SDAGB= 0.
       SDHI= 0.
       SDLAI= 0.
       SDPARA= 0.
       SDRUE= 0.
       SDDWA= 0.
       SDRAIN= 0.
       SDEV= 0.
       SDTRA= 0.
       SDDRAI= 0.
       SDIRR= 0.
       SDRUNO= 0.
       SDWUE= 0.
       SDMINO= 0.
       SDMINF= 0.
       SDDNTO= 0.
       SDNRAI= 0.
       SDNGIF= 0.
       SDNUP= 0.
       SDLEAC= 0.
       SDIMM= 0.
       SDBIOM= 0.
       SDNUE= 0.
       SDDFMT= 0.


*------ initialization variation coefficients
     
       VCSOW= 0.
       VCSTAR= 0.
       VCANT= 0.
       VCHALT= 0.
       VCWRT= 0.
       VCWLV= 0.
       VCWST= 0.
       VCWGR= 0.
       VCAGB= 0.
       VCHI= 0.
       VCLAI= 0.
       VCPARA= 0.
       VCRUE= 0.
       VCDWA= 0.
       VCRAIN= 0.
       VCEV= 0.
       VCTRA= 0.
       VCDRAI= 0.
       VCIRR= 0.
       VCRUNO= 0.
       VCWUE= 0.
       VCMINO= 0.
       VCMINF= 0.
       VCDNTO= 0.
       VCNRAI= 0.
       VCNGIF= 0.
       VCNUP= 0.
       VCLEAC= 0.
       VCIMM= 0.
       VCBIOM= 0.
       VCNUE= 0.
       VCDFMT= 0.


*------ print headings
       
        WRITE (80,'(///,2A)') 
     $   '*** RESULTS OF CROP GROWTH MODEL NWHEAT ***',
     $   ' SUMMARY OF OUTPUT FOR WINTER WHEAT AND YIELD STATISTICS ***'

        WRITE (80,'(/,A,A,A,A,A,I3,A,I3,A,I3,A,I3,A,I3, A,F4.2)') 
     $   'Name of run: ',
     $   RUNNAME,', Station= ', STATR,', Soil= ', ISOIL,', Crop= ',
     $   ICROP,', Organic M.= ', IORGM, ', Nsoil= ',INSOIL,
     $   ', IRR= ',IRRI1,', Runof= ',RUNFR1

        WRITE (80,'(1X,A)') REMARK

        WRITE (80,'(//,4A)')
     $  'YEAR     SOW EMERG  ANTH  HALT   WRT   WLV    WST   WGR ',   
     $  '   TAGB     HI  LAIM PARAB  RUE   DWAV RAIN  ESOIL   TRANS',
     $  ' DRAIN RUNOF IRR  WUE MINOM MINFM DNTOT NRAIN NGIFT NUPT  ',
     $  'NLEACH NIMMO  NUE  DNFM  NBIOM' 


100     CONTINUE

        INIT= .FALSE.
   

        IF (TERMY) GOTO 200 
     
*----harvest year
        IHYEAR= ISYEAR +1

*----water use efficiency (g DM above-gr./kg water transpired)
*----nitrogen use efficiency (kg DM above gr./kg N in above gr. plant)
*----components of water balance in mm
*----components of nitrogen balance in kg N/ha
*----crop production in kg DM/ha
*----PAR intercepted in MJ/m2
*----radiation use efficiency in g D.M. above-gr./MJ PAR intercepted

        WUE= TAGB1/((TTRANS1 + TESOIL1) * 100.)
        NUE= TAGB1/NUPTAG1
        DWAVT2= DWAVT*10.
        TRAIN2= TRAIN1*10.
        TESOIL2= TESOIL1*10.
        TTRANS2= TTRANS1*10.
        TDRAIN2= TDRAIN1*10.
        TRUNOF2= TRUNOF1*10.
        TIRR2=   TIRR1*10.

*------ one line of output for each year

        WRITE (80,'(I4,2X,4I6,5F7.0,2F6.2,F6.0,F6.2,3F6.0,3X,
     $   4F5.0,F6.2,11F6.1)')  
     $   IHYEAR, ISOW1,ISTART1,IDANTH1,IDHALT,TWRT,TWLV,TWST,TWGR,
     $   TAGB1,HI,LAIMAX1,TPARAB1,RUE,DWAVT2,TRAIN2,TESOIL2,TTRANS2,
     $   TDRAIN2,TRUNOF2,TIRR2,WUE,TMINOM1,TMINFM1,
     $   DNTOT,TNRAIN1,TNGIFT1,NUPTAG1,TLEACH1,TNIMM1,NUE,DFMTOT,TBIOM1
     

*-----counting the number of years and summing the variables

         IWRUN=   IWRUN+1
         SUMSOW=  SUMSOW + ISOW1
         SUMSTAR= SUMSTAR + ISTART1
         SUMANT=  SUMANT + IDANTH1
         SUMHALT= SUMHALT + IDHALT
         SUMWRT=  SUMWRT + TWRT
         SUMWLV=  SUMWLV + TWLV
         SUMWST=  SUMWST + TWST
         SUMWGR=  SUMWGR + TWGR
         SUMAGB=  SUMAGB + TAGB1
         SUMHI=   SUMHI + HI
         SUMLAI=  SUMLAI + LAIMAX1
         SUMPARA= SUMPARA + TPARAB1
         SUMRUE=  SUMRUE + RUE
         SUMDWA=  SUMDWA + DWAVT2
         SUMRAIN= SUMRAIN + TRAIN2
         SUMEV=   SUMEV + TESOIL2
         SUMTRA=  SUMTRA + TTRANS2
         SUMDRAI= SUMDRAI + TDRAIN2
         SUMIRR=  SUMIRR + TIRR2
         SUMRUNO= SUMRUNO + TRUNOF2
         SUMWUE=  SUMWUE + WUE
         SUMMINO= SUMMINO + TMINOM1
         SUMMINF= SUMMINF + TMINFM1
         SUMDNTO= SUMDNTO + DNTOT
         SUMNRAI= SUMNRAI + TNRAIN1
         SUMNGIF= SUMNGIF + TNGIFT1
         SUMNUP=  SUMNUP + NUPTAG1
         SUMLEAC= SUMLEAC + TLEACH1
         SUMIMM=  SUMIMM + TNIMM1
         SUMBIOM= SUMBIOM + TBIOM1
         SUMNUE=  SUMNUE + NUE
         SUMDFMT= SUMDFMT + DFMTOT

*-----Square sums for the same variables
    
         S2MSOW=  S2MSOW +  ISOW1*ISOW1
         S2MSTAR= S2MSTAR + ISTART1*ISTART1
         S2MANT=  S2MANT + IDANTH1*IDANTH1
         S2MHALT= S2MHALT + IDHALT*IDHALT
         S2MWRT=  S2MWRT +  TWRT*TWRT
         S2MWLV=  S2MWLV +  TWLV*TWLV
         S2MWST=  S2MWST +  TWST*TWST
         S2MWGR=  S2MWGR +  TWGR*TWGR
         S2MAGB=  S2MAGB +  TAGB1*TAGB1
         S2MHI=   S2MHI +  HI*HI
         S2MLAI=  S2MLAI + LAIMAX1*LAIMAX1
         S2MPARA= S2MPARA + TPARAB1*TPARAB1
         S2MRUE=  S2MRUE + RUE*RUE
         S2MDWA=  S2MDWA + DWAVT2*DWAVT2
         S2MRAIN= S2MRAIN + TRAIN2*TRAIN2
         S2MEV=   S2MEV + TESOIL2*TESOIL2
         S2MTRA=  S2MTRA + TTRANS2*TTRANS2
         S2MDRAI= S2MDRAI + TDRAIN2*TDRAIN2
         S2MIRR=  S2MIRR + TIRR2*TIRR2
         S2MRUNO= S2MRUNO + TRUNOF2*TRUNOF2
         S2MWUE=  S2MWUE + WUE*WUE
         S2MMINO= S2MMINO + TMINOM1*TMINOM1
         S2MMINF= S2MMINF + TMINFM1*TMINFM1
         S2MDNTO= S2MDNTO + DNTOT*DNTOT
         S2MNRAI= S2MNRAI + TNRAIN1*TNRAIN1
         S2MNGIF= S2MNGIF + TNGIFT1*TNGIFT1
         S2MNUP=  S2MNUP  + NUPTAG1*NUPTAG1
         S2MLEAC= S2MLEAC + TLEACH1*TLEACH1
         S2MIMM=  S2MIMM + TNIMM1*TNIMM1
         S2MBIOM= S2MBIOM + TBIOM1*TBIOM1
         S2MNUE=  S2MNUE + NUE*NUE
         S2MDFMT= S2MDFMT + DFMTOT*DFMTOT

         IF (.NOT. TERMY) RETURN

*------elaboration for statistical analysis

200    CONTINUE

*------Means     

       MSOW=  SUMSOW/IWRUN
       MSTAR= SUMSTAR/IWRUN
       MANT=  SUMANT/IWRUN
       MHALT= SUMHALT/IWRUN       
       MWRT=  SUMWRT/IWRUN
       MWLV=  SUMWLV/IWRUN
       MWST=  SUMWST/IWRUN
       MWGR=  SUMWGR/IWRUN
       MAGB=  SUMAGB/IWRUN
       MHI=   SUMHI/IWRUN
       MLAI=  SUMLAI/IWRUN
       MPARA= SUMPARA/IWRUN
       MRUE=  SUMRUE/IWRUN
       MDWA=  SUMDWA/IWRUN
       MRAIN= SUMRAIN/IWRUN
       MEV=   SUMEV/IWRUN
       MTRA=  SUMTRA/IWRUN
       MDRAI= SUMDRAI/IWRUN
       MIRR=  SUMIRR/IWRUN
       MRUNO= SUMRUNO/IWRUN
       MWUE=  SUMWUE/IWRUN
       MMINO= SUMMINO/IWRUN
       MMINF= SUMMINF/IWRUN
       MDNTO= SUMDNTO/IWRUN
       MNRAI= SUMNRAI/IWRUN
       MNGIF= SUMNGIF/IWRUN
       MNUP=  SUMNUP/IWRUN
       MLEAC= SUMLEAC/IWRUN
       MIMM=  SUMIMM/IWRUN
       MBIOM= SUMBIOM/IWRUN
       MNUE= SUMNUE/IWRUN
       MDFMT= SUMDFMT/IWRUN

       IF (IWRUN .LT. 3) GOTO 300

*------standard deviation

       SDSOW=  SQRT(AMAX1(0.,(S2MSOW-(SUMSOW*SUMSOW)/IWRUN)/IWRUN))
       SDSTAR= SQRT(AMAX1(0.,(S2MSTAR-(SUMSTAR*SUMSTAR)/IWRUN)/IWRUN))
       SDANT=  SQRT(AMAX1(0.,(S2MANT-(SUMANT*SUMANT)/IWRUN)/IWRUN))
       SDHALT= SQRT(AMAX1(0.,(S2MHALT-(SUMHALT*SUMHALT)/IWRUN)/IWRUN))
       SDWRT=  SQRT(AMAX1(0.,(S2MWRT-(SUMWRT*SUMWRT)/IWRUN)/IWRUN))
       SDWLV=  SQRT(AMAX1(0.,(S2MWLV-(SUMWLV*SUMWLV)/IWRUN)/IWRUN))
       SDWST=  SQRT(AMAX1(0.,(S2MWST-(SUMWST*SUMWST)/IWRUN)/IWRUN))
       SDWGR=  SQRT(AMAX1(0.,(S2MWGR-(SUMWGR*SUMWGR)/IWRUN)/IWRUN))
       SDAGB=  SQRT(AMAX1(0.,(S2MAGB-(SUMAGB*SUMAGB)/IWRUN)/IWRUN))
       SDHI=   SQRT(AMAX1(0.,(S2MHI-(SUMHI*SUMHI)/IWRUN)/IWRUN))
       SDLAI=  SQRT(AMAX1(0.,(S2MLAI-(SUMLAI*SUMLAI)/IWRUN)/IWRUN))
       SDPARA= SQRT(AMAX1(0.,(S2MPARA-(SUMPARA*SUMPARA)/IWRUN)/IWRUN))
       SDRUE=  SQRT(AMAX1(0.,(S2MRUE-(SUMRUE*SUMRUE)/IWRUN)/IWRUN))
       SDDWA=  SQRT(AMAX1(0.,(S2MDWA-(SUMDWA*SUMDWA)/IWRUN)/IWRUN))
       SDRAIN= SQRT(AMAX1(0.,(S2MRAIN-(SUMRAIN*SUMRAIN)/IWRUN)/IWRUN))
       SDEV=   SQRT(AMAX1(0.,(S2MEV-(SUMEV*SUMEV)/IWRUN)/IWRUN))
       SDTRA=  SQRT(AMAX1(0.,(S2MTRA-(SUMTRA*SUMTRA)/IWRUN)/IWRUN))
       SDDRAI= SQRT(AMAX1(0.,(S2MDRAI-(SUMDRAI*SUMDRAI)/IWRUN)/IWRUN))
       SDIRR=  SQRT(AMAX1(0.,(S2MIRR-(SUMIRR*SUMIRR)/IWRUN)/IWRUN))
       SDRUNO= SQRT(AMAX1(0.,(S2MRUNO-(SUMRUNO*SUMRUNO)/IWRUN)/IWRUN))
       SDWUE=  SQRT(AMAX1(0.,(S2MWUE-(SUMWUE*SUMWUE)/IWRUN)/IWRUN))
       SDMINO= SQRT(AMAX1(0.,(S2MMINO-(SUMMINO*SUMMINO)/IWRUN)/IWRUN))
       SDMINF= SQRT(AMAX1(0.,(S2MMINF-(SUMMINF*SUMMINF)/IWRUN)/IWRUN))
       SDDNTO= SQRT(AMAX1(0.,(S2MDNTO-(SUMDNTO*SUMDNTO)/IWRUN)/IWRUN))
       SDNRAI= SQRT(AMAX1(0.,(S2MNRAI-(SUMNRAI*SUMNRAI)/IWRUN)/IWRUN))
       SDNGIF= SQRT(AMAX1(0.,(S2MNGIF-(SUMNGIF*SUMNGIF)/IWRUN)/IWRUN))
       SDNUP=  SQRT(AMAX1(0.,(S2MNUP-(SUMNUP*SUMNUP)/IWRUN)/IWRUN))
       SDLEAC= SQRT(AMAX1(0.,(S2MLEAC-(SUMLEAC*SUMLEAC)/IWRUN)/IWRUN))
       SDIMM=  SQRT(AMAX1(0.,(S2MIMM-(SUMIMM*SUMIMM)/IWRUN)/IWRUN))
       SDBIOM= SQRT(AMAX1(0.,(S2MBIOM-(SUMBIOM*SUMBIOM)/IWRUN)/IWRUN))
       SDNUE= SQRT(AMAX1(0.,(S2MNUE-(SUMNUE*SUMNUE)/IWRUN)/IWRUN))
       SDDFMT= SQRT(AMAX1(0.,(S2MDFMT-(SUMDFMT*SUMDFMT)/IWRUN)/IWRUN))

*-----variation coefficient (%)

       VCSOW= 100.*SDSOW/AMAX1(0.01,ABS(MSOW))
       VCSTAR= 100.*SDSTAR/AMAX1(0.01,ABS(MSTAR))
       VCANT= 100.*SDANT/AMAX1(0.01,ABS(MANT))
       VCHALT= 100.*SDHALT/AMAX1(0.01,ABS(MHALT))
       VCWRT= 100.*SDWRT/AMAX1(0.01,ABS(MWRT))
       VCWLV= 100.*SDWLV/AMAX1(0.01,ABS(MWLV))
       VCWST= 100.*SDWST/AMAX1(0.01,ABS(MWST))
       VCWGR= 100.*SDWGR/AMAX1(0.01,ABS(MWGR))
       VCAGB= 100.*SDAGB/AMAX1(0.01,ABS(MAGB))
       VCHI= 100.*SDHI/AMAX1(0.01,ABS(MHI))
       VCLAI= 100.*SDLAI/AMAX1(0.01,ABS(MLAI))
       VCPARA= 100.*SDPARA/AMAX1(0.01,ABS(MPARA))
       VCRUE= 100.*SDRUE/AMAX1(0.01,ABS(MRUE))
       VCDWA= 100.*SDDWA/AMAX1(0.01,ABS(MDWA))
       VCRAIN= 100.*SDRAIN/AMAX1(0.01,ABS(MRAIN))
       VCEV= 100.*SDEV/AMAX1(0.01,ABS(MEV))
       VCTRA= 100.*SDTRA/AMAX1(0.01,ABS(MTRA))
       VCDRAI= 100.*SDDRAI/AMAX1(0.01,ABS(MDRAI))
       VCIRR= 100.*SDIRR/AMAX1(0.01,ABS(MIRR))
       VCRUNO= 100.*SDRUNO/AMAX1(0.01,ABS(MRUNO))
       VCWUE= 100.*SDWUE/AMAX1(0.01,ABS(MWUE))
       VCMINO= 100.*SDMINO/AMAX1(0.01,ABS(MMINO))
       VCMINF= 100.*SDMINF/AMAX1(0.01,ABS(MMINF))
       VCDNTO= 100.*SDDNTO/AMAX1(0.01,ABS(MDNTO))
       VCNRAI= 100.*SDNRAI/AMAX1(0.01,ABS(MNRAI))
       VCNGIF= 100.*SDNGIF/AMAX1(0.01,ABS(MNGIF))
       VCNUP= 100.*SDNUP/AMAX1(0.01,ABS(MNUP))
       VCLEAC= 100.*SDLEAC/AMAX1(0.01,ABS(MLEAC))
       VCIMM= 100.*SDIMM/AMAX1(0.01,ABS(MIMM))
       VCBIOM= 100.*SDBIOM/AMAX1(0.01,ABS(MBIOM))
       VCNUE= 100.*SDNUE/AMAX1(0.01,ABS(MNUE))
       VCDFMT= 100.*SDDFMT/AMAX1(0.01,ABS(MDFMT))

300     CONTINUE



*------statistical results on file

        WRITE (80, '(/A,I3,A)') ' MEANS over ',IWRUN,' years  :'
        WRITE (80,'(6X,4F6.0,5F7.0,2F6.2,F6.0,F6.2,3F6.0,3X,
     $   4F5.0,F6.2,11F6.1)')  
     $   MSOW,MSTAR,MANT,MHALT,MWRT,MWLV,MWST,MWGR,
     $   MAGB,MHI,MLAI,MPARA,MRUE,MDWA,
     $   MRAIN,MEV,MTRA,MDRAI,MRUNO,MIRR,MWUE,MMINO,MMINF,
     $   MDNTO,MNRAI,MNGIF,MNUP,MLEAC,MIMM,MNUE,MDFMT,MBIOM

        WRITE (80, '(A)')  ' STANDARD DEVIATIONS  :'
        WRITE (80,'(6X,4F6.0,5F7.0,2F6.2,F6.0,F6.2,3F6.0,3X,
     $   4F5.0,F6.2,11F6.1)')  
     $   SDSOW,SDSTAR,SDANT,SDHALT,SDWRT,SDWLV,SDWST,SDWGR,
     $   SDAGB,SDHI,SDLAI,SDPARA,SDRUE,SDDWA,
     $   SDRAIN,SDEV,SDTRA,SDDRAI,SDRUNO,SDIRR,SDWUE,SDMINO,SDMINF,
     $   SDDNTO,SDNRAI,SDNGIF,SDNUP,SDLEAC,SDIMM,SDNUE,SDDFMT,SDBIOM

        WRITE (80, '(A)')  ' VARIATION COEFFICIENTS (%)  : '       
        WRITE (80,'(6X,4F6.1,5F7.1,2F6.1,F6.1,F6.1,3F6.1,3X,
     $   4F5.1,F6.1,11F6.1)')  
     $   VCSOW,VCSTAR,VCANT,VCHALT,VCWRT,VCWLV,VCWST,VCWGR,
     $   VCAGB,VCHI,VCLAI,VCPARA,VCRUE,VCDWA,
     $   VCRAIN,VCEV,VCTRA,VCDRAI,VCRUNO,VCIRR,VCWUE,VCMINO,VCMINF,
     $   VCDNTO,VCNRAI,VCNGIF,VCNUP,VCLEAC,VCIMM,VCNUE,VCDFMT,VCBIOM

         INIT= .TRUE.
    

         END