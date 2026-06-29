*-------------------------------------------------------------------------*
* Copyright 2013. Wageningen University, Plant Production Systems group,  *
* P.O. Box 430, 6700 AK Wageningen, The Netherlands.                      *
* You may not use this work except in compliance with the Licence.        *
* You may obtain a copy of the Licence at:                                *
*                                                                         *
* http://models.pps.wur.nl/content/licence-agreement                      *
*                                                                         *
* Unless required by applicable law or agreed to in writing, software     *
* distributed under the Licence is distributed on an "AS IS" basis,       *
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.*
*-------------------------------------------------------------------------*

*--------------------------------------------------------------------
*
*     PROGRAM NWHEAT.FOR 
*     author : rob groot
*     date of last revision : 02-jan-1988
*     model simulates the following processes:
*
*           -  redistribution of precipitation
*           -  moisture withdrawal by crop transpiration
*           -  moisture withdrawal by soil surface 
*           -  soil surface evaporation
*           -  drainage
*           -  transport and leaching of nitrate
*           -  nitrogen transport towards roots by diffusion
*           -  nitrogen transport towards roots by mass flow
*           -  crop nitrogen uptake
*           -  decomposition of fresh and old organic matter
*           -  immobilisation of mineralisation of nitrogen, depending
*           -  on C/N-ratio of substrate
*           -  crop development, crop growth, dry matter distibtution
*              and nitrogen distribution
*
*     purpose: model provides a framework to validate assumptions
*              about nutrient transport, organic matter dynamics,
*              nitrogen uptake as function of N-supply for various
*              crops, leaching of nitrogen for different N-application
*              strategies or for different cropping systems,
*              etc., etc., etc., etc...........
*
*--------------------------------------------------------------------

      PROGRAM NWHEAT

      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)
      CHARACTER RUNNAME*5,REMARK*80,STATR*5,CONTIN*1,WTRDIR*80
      COMMON /ITIME/ ISYR, ISYEAR, ISDAY

      DIMENSION PARDIR(3) ,PARDIF(3) ,SINB(3)   ,FLOW(11)  ,WCREL(10)
      DIMENSION DEPTH(10) ,THCKN(10) ,TRR(10)   ,WCACT(10) ,SOILTP(10)
      DIMENSION WCACT0(10),RESOIL(10),WCWILT(10),RHOD(10)  ,WCFLDC(10)
      DIMENSION MINOM(10) ,MINFM(10) ,NIMM(10) ,NBIOM(10)  ,LRV(10)
      DIMENSION ANLAY(10)
      LOGICAL TERMIN,START,INITI,TERMY      
      
*-----iunit number for weather
      DATA IUWE/65/

*---- initialize program
*     LOOP for different weather stations

      
1      CALL INIT (IIYR,INYEAR,IDSOW,IFINIT,IGAP,REMARK,RUNNAME,
     $  STATR,CONTIN,SENSP,SENSV,SENSW,SENSR,SENST,
     $  ISOIL,ICROP,IORGM,INSOIL,IPM,
     $  IOPT,IRRI,RUNFR,WTRDIR)

       TERMY= .FALSE.

*---- LOOP for a number of years

      IYEAR = IIYR

5     CONTINUE
            
      TERMIN = .FALSE.
      START = .FALSE.
      IDAY = IDSOW
      ISDAY = IDSOW
      ISYR = IYEAR 
      ISYEAR= IYEAR + 1900
      INITI = .TRUE.

*---- LOOP for the entire simulation period

        
10       CONTINUE

*------- read weather data
         IF (IOPT .EQ. 1) THEN
         CALL WEATHR (IWAR,WTRDIR,STATR,IYEAR,IDAY,IGAP,LONGIE,LATIN,
     $                ALTI,TMIN,TMAX,DTR,RAIN,VAP,WIND)
         ELSE
         CALL DBMRD (WTRDIR,STATR,IUWE,4,IYEAR,IDAY,LATIN,ALTI,
     $                TMIN,TMAX,DTR,VAP,WIND,RAIN)
         ENDIF

* ----- to prevent temperature effect on relative humidity
          
          TMPA = (TMIN + TMAX)/2.
          SVAP1  = 6.10588 * EXP (17.32491*TMPA/(TMPA+238.102))
          VAP= AMIN1(VAP,SVAP1)
          RH= VAP/SVAP1

* ------  sensitivity analyses
        
*         TMIN= TMIN + SENST
*         TMAX= TMAX + SENST  
         
*         TMPA = (TMIN + TMAX)/2.
*         SVAP2  = 6.10588 * EXP (17.32491*TMPA/(TMPA+238.102))
*         VAP= RH*SVAP2
         
               
*          VAP= AMIN1(SVAP1, VAP * SENSV)
*          WIND= WIND * SENSW
*          DTR= DTR * SENSR
*          RAIN= RAIN * SENSP
          
*------- calculate daylength, atmospheric transmission and 
*        radiation distribution (direct and diffuse radiation)


         CALL ASTRO (IDAY,LATIN,DAYL,DAYLP,SINLD,COSLD)                     
         CALL RADIAT (IDAY,DAYL,SINLD,COSLD,DTR,SINB,PARDIR,
     $                PARDIF,ATMTR,AVRAD)


*------- calculate soil moisture and soil nitrogen redistribution

         
         CALL WATBAL (1,RD,IDAY,REMARK,RUNNAME,ETC,ES0,LAITOT,RAIN,
     $                TRR,TRTOT,WCREL,RDMSOL,THCKN,DEPTH,ILAYER,
     $                TTRANS,TDRAIN,TRAIN,FLOW,TESOIL,WCACT,WCACT0,
     $                IRRI,RUNFR,RHOD,RESOIL,WCWILT,WCFLDC,TRMAX,FLOWR,
     $                TERMIN,START,INITI,STATR,
     $                ISOIL,ICROP,IORGM,INSOIL,IPM)
          
*------- calculate N-transport by moisture distribution


         CALL NITBAL (1,IDAY,REMARK,RUNNAME,FLOW,WCACT,THCKN,
     $                ILAYER,WCACT0,IN,RDINL,TRR,RD,DVS,NDEM,WCWILT,
     $                ANLAY,NUPTR,NIMM,MINFM,MINOM,NBIOM,FMTOT,FLOWR,
     $                TERMIN,START,INITI,STATR,
     $                ISOIL,ICROP,IORGM,INSOIL)


*------- calculate (or estimate) soil temperatures, to be used
*        in calculation of mineralization


         CALL SOLTMP (TMIN,TMAX,ILAYER,SOILTP,INITI)


*------- calculate organic matter dynamics when soil is at its moist
*        (mineralisation - immobilization )

         CALL OMDYN (IORGM,ILAYER,WCACT,ANLAY,THCKN,WCFLDC,RHOD,SOILTP,
     $               MINFM,NIMM,MINOM,NBIOM,FMTOT,INITI)


*------- subtract nitrogen lost by immobilization and add nitrogen
*        from mineralization 


         CALL NITBAL (2,IDAY,REMARK,RUNNAME,FLOW,WCACT,THCKN,
     $                ILAYER,WCACT0,IN,RDINL,TRR,RD,DVS,NDEM,WCWILT,
     $                ANLAY,NUPTR,NIMM,MINFM,MINOM,NBIOM,FMTOT,FLOWR,
     $                TERMIN,START,INITI,STATR,
     $                ISOIL,ICROP,IORGM,INSOIL)


*------- calculate nitrification (if required!) Note that nitrifiers
*        have access to ammonia before crop


*        CALL NITRI


*------- calculate potential soil evaporation and crop transpiration


         CALL PENMAN (ALTI,ATMTR,TMIN,TMAX,AVRAD,WIND,VAP,CO,E0,ES0,ETC)

         IF (IPM .EQ. 1) CALL BGLEAF (ATMTR,TMIN,TMAX,AVRAD,WIND,
     $     VAP,ALTI,CROPHT,LAITOT,CO,ETC)


*------- subtract evaporation and transpiration from soil water
*        status


         CALL WATBAL (2,RD,IDAY,REMARK,RUNNAME,ETC,ES0,LAITOT,RAIN,
     $                TRR,TRTOT,WCREL,RDMSOL,THCKN,DEPTH,ILAYER,
     $                TTRANS,TDRAIN,TRAIN,FLOW,TESOIL,WCACT,WCACT0,
     $                IRRI,RUNFR,RHOD,RESOIL,WCWILT,WCFLDC,TRMAX,FLOWR,
     $                TERMIN,START,INITI,STATR,
     $                ISOIL,ICROP,IORGM,INSOIL,IPM)


*------- subtract nitrogen uptake by plant 


         CALL NITBAL (3,IDAY,REMARK,RUNNAME,FLOW,WCACT,THCKN,
     $                ILAYER,WCACT0,IN,RDINL,TRR,RD,DVS,NDEM,WCWILT,
     $                ANLAY,NUPTR,NIMM,MINFM,MINOM,NBIOM,FMTOT,FLOWR,
     $                TERMIN,START,INITI,STATR,
     $                ISOIL,ICROP,IORGM,INSOIL)


*------- calculate crop growth including crop nitrogen uptake,
*        transpiration

       IF (TERMIN)  
     $   CALL CROP (REMARK,RUNNAME,IDAY,TMIN,RDINL,TMAX,DAYL,DAYLP,
     $              PARDIR,PARDIF,SINB,IN,ILAYER,WCREL,DEPTH,RDMSOL,
     $              THCKN,TRTOT,TRMAX,SOILTP,NUPTR,RD,LAITOT,DVS,
     $              CROPHT,NDEM,LRV,AVRAD,CO,TERMIN,START,INITI,STATR,
     $              ISOIL,ICROP,IORGM,INSOIL)


         IF (TERMIN) CALL STATP (RUNNAME,REMARK,STATR,
     $        ISOIL,ICROP,IORGM,INSOIL,TERMY)

         IF (TERMIN) GOTO 20

         CALL CROP (REMARK,RUNNAME,IDAY,TMIN,RDINL,TMAX,DAYL,DAYLP,
     $              PARDIR,PARDIF,SINB,IN,ILAYER,WCREL,DEPTH,RDMSOL,
     $              THCKN,TRTOT,TRMAX,SOILTP,NUPTR,RD,LAITOT,DVS,
     $              CROPHT,NDEM,LRV,AVRAD,CO,TERMIN,START,INITI,STATR,
     $              ISOIL,ICROP,IORGM,INSOIL)

     
*------- when finish conditions are reached (TERMIN .TRUE.)



         IF (TERMIN) GOTO 15   


*------- update daynumber and year


         CALL TIMER (IDAY,IYEAR,IFINIT,TERMIN,INITI)

         INITI = .FALSE.                
       
15      GOTO 10

20      CONTINUE
      
         

         IF (IYEAR .NE. IIYR + INYEAR)  GO TO 5

         TERMY= .TRUE.
         CALL STATP (RUNNAME,REMARK,STATR,ISOIL,ICROP,IORGM,
     $    INSOIL,TERMY)

         IF (CONTIN .EQ. 'Y' .OR. CONTIN .EQ. 'y') GOTO 1
      END
