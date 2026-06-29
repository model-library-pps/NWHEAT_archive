*--------------------------------------------------------------------
*
*	PROGRAM SOIL.FOR 
*	author : rob groot
*	date of last revision : 02-jan-1989
*	model simulates the following processes:
*
*	      -  redistribution of precipitation
*	      -  moisture withdrawal by crop transpiration
*	      -  moisture withdrawal by soil surface 
*	      -  soil surface evaporation
*	      -  drainage
*	      -  transport and leaching of nitrate
*	      -  nitrogen transport towards roots by diffusion
*	      -  nitrogen transport towards roots by mass flow
*	      -  crop nitrogen uptake
*	      -  decomposition of fresh and old organic matter
*	      -  immobilisation of mineralisation of nitrogen, depending
*	      -  on C/N-ratio of substrate
*	      -  crop development, crop growth, dry matter distibtution
*	         and nitrogen distribution
*
*	purpose: model provides a framework to validate assumptions
*      	   about nutrient transport, organic matter dynamics,
*      	   nitrogen uptake as function of N-supply for various
*      	   crops, leaching of nitrogen for different N-application
*      	   strategies or for different cropping systems,
*      	   etc., etc., etc., etc...........
*
*--------------------------------------------------------------------

	PROGRAM SOIL
	
	IMPLICIT REAL (A-H,J-Z)
	IMPLICIT INTEGER (I)
	CHARACTER RUNNAM*5,REMARK*80,STATR*5

	DIMENSION PARDIR(3),PARDIF(3),SINB(3),FLOW(11),WCREL(10)
	DIMENSION DEPTH(10),THCKN(10),TRR(10),WCACT(10),SOILTP(10)
	DIMENSION WCACT0(10),RESOIL(10),WCWILT(10),RHOD(10),WCFLDC(10)
	DIMENSION MINOM(10),MINFM(10),NLOSS(10),NIMM(10),LRV(10)
	DIMENSION ANLAY(10)
	LOGICAL TERMIN

	DATA TERMIN /.FALSE./


*------	initialize program


	CALL INIT (IYEAR,IDAY,IFINIT,IGAP,REMARK,RUNNAM,STATR)


*------	LOOP for the entire simulation period


10	CONTINUE

*--------- read weather data


	   CALL WEATHR (IWAR,STATR,IYEAR,IDAY,IGAP,LONGIE,LATIN,ALTI,
     $                  TMIN,TMAX,DTR,RAIN,VAP,WIND)


*--------- calculate daylength, atmospheric transmission and 
*          radiation distribution (direct and diffuse radiation)


	   CALL ASTRO (IDAY,LATIN,DAYL,DAYLP,SINLD,COSLD)                     
	   CALL RADIAT (IDAY,DAYL,SINLD,COSLD,DTR,SINB,PARDIR,
     $                  PARDIF,ATMTR)


*--------- calculate soil moisture and soil nitrogen redistribution


	   CALL WATBAL (RD,IDAY,REMARK,RUNNAM,ET0,LAITOT,
     $               RAIN,TRR,TRTOT,WCREL,RDMSOL,THCKN,DEPTH,ILAYER,
     $               TTRANS,TDRAIN,TRAIN,FLOW,TESOIL,WCACT,WCACT0,
     $               RHOD,RESOIL,WCWILT,WCFLDC,TRMAX)
	

*--------- calculate N-transport by moisture distribution


	   CALL NITBAL (1,IDAY,REMARK,RUNNAM,FLOW,WCACT,THCKN,
     $                ILAYER,WCACT0,IN,RDINL,TRR,RD,DVS,NDEM,WCWILT,
     $                ANLAY,NUPTR,NLOSS,MINFM,MINOM,NIMM)


*--------- calculate (or estimate) soil temperatures, to be used
*          in calculation of mineralization


	   CALL SOLTMP (TMIN,TMAX,ILAYER,SOILTP)


*--------- calculate organic matter dynamics when soil is at its moist
*          (mineralisation - immobilization )


	   CALL OMDYN (ILAYER,WCACT,ANLAY,THCKN,WCFLDC,RHOD,SOILTP,
     $               MINFM,NLOSS,MINOM,NIMM)


*--------- subtract nitrogen lost by immobilization and add nitrogen
*          from mineralization 


	   CALL NITBAL (2,IDAY,REMARK,RUNNAM,FLOW,WCACT,THCKN,
     $                ILAYER,WCACT0,IN,RDINL,TRR,RD,DVS,NDEM,WCWILT,
     $                ANLAY,NUPTR,NLOSS,MINFM,MINOM,NIMM)


*--------- calculate nitrification (if required!) Note that nitrifiers
*          have access to ammonia before crop


*	   CALL NITRI


*--------- calculate potential soil evaporation


	   CALL PENMAN (ATMTR,TMIN,TMAX,DTR,WIND,VAP,E0,ES0,ET0)


*--------- subtract evaporation and transpiration from soil water
*          status


	   CALL WATBAL (RD,IDAY,REMARK,RUNNAM,ET0,LAITOT,
     $                RAIN,TRR,TRTOT,WCREL,RDMSOL,THCKN,DEPTH,ILAYER,
     $                TTRANS,TDRAIN,TRAIN,FLOW,TESOIL,WCACT,WCACT0,
     $                RHOD,RESOIL,WCWILT,WCFLDC,TRMAX)


*--------- subtract nitrogen uptake by plant 


	   CALL NITBAL (3,IDAY,REMARK,RUNNAM,FLOW,WCACT,THCKN,
     $                ILAYER,WCACT0,IN,RDINL,TRR,RD,DVS,NDEM,WCWILT,
     $                ANLAY,NUPTR,NLOSS,MINFM,MINOM,NIMM)


*--------- calculate crop growth including crop nitrogen uptake,
*          transpiration



	   CALL CROP (REMARK,RUNNAM,IDAY,TMIN,RDINL,TMAX,DAYL,DAYLP,
     $              PARDIR,PARDIF,SINB,IN,ILAYER,WCREL,DEPTH,RDMSOL,
     $              THCKN,TRTOT,TRMAX,SOILTP,NUPTR,RD,LAITOT,DVS,
     $              NDEM,LRV,TERMIN)

     
*--------- when finish conditions are reached (TERMIN .TRUE.)
*          program is finished


	   IF (TERMIN) GOTO 20   


*--------- update daynumber and year


	   CALL TIMER (IDAY,IYEAR,IFINIT,TERMIN)

         
	GOTO 10

20	CONTINUE

	END

