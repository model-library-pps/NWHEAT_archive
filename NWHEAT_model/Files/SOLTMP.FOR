

*--------------------------------------------------------------------------*
*  SUBROUTINE SOLTMP                                                       *
*  Author : J.J.R. Groot                                                   *
*  Date   : 11-february-1987                                               *
*  Purpose: This subroutine generates temperatures of different soil       *
*	    compartments, based on the assumption that the temperature-    *
*           fluctuations are smaller in deeper soil compartments, and that *
*	    the temperature of a compartment can be described by a delay-  *
*	    function of the average daily air temperature                  *
*                                                                          *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)         *
*  name    meaning                                     units  class        *
*  ----    -------                                     -----  -----        *
*  tmpa    average air temperature                       C      I          *
*  ilayer  number of soil compartments                   -      I          *
*  soiltp  array with generated temperature per                            *
*          compartment                                   C      O          *
*                                                                          *
*  subroutines called : none                                               *
*  file usage : none                                                       *
*--------------------------------------------------------------------------*

      subroutine soltmp (tmin,tmax,ilayer,soiltp,INITI)

      implicit real (a-h,j-z)
      implicit integer (i)
      LOGICAL INITI,INIT
      dimension soiltp(10),ilag(10),avtmp(1000)
      
*---- temperature delay for soil compartments

      data ilag/1,4,8,16,26,38,52,68,84,100/

      IF (INITI) INIT = .TRUE.
      IF (.NOT. INIT) GOTO 5
      IDAY = 0
5     CONTINUE    

*---- average daily temperature

      tmpa = (tmin+tmax)/2.

      iday  = iday + 1
      avtmp(iday) = tmpa

      do 100,il = 1,ilayer

*-------  when the delay is one day, compartment-temperature equals 
*         average air temperature

         if (ilag(il).eq.1) then
            soiltp(il) = avtmp(iday)
         else

*--------- calculate temperature of compartment IL. For days prior to 
*          itialization, average daily air temperatures are not 
*          available. Therefore the temperature of all soil compartments
*          at the start of the simulation is assumed to be equal to the air
*          temperature 

           istart = max(iday-ilag(il)+1,1)
           iniday = max(ilag(il)-iday,0)

           sumtmp = 0.
           do 10,i = istart, iday
              sumtmp = sumtmp + avtmp(i)
10         continue

           soiltp(il) = ((iniday*avtmp(1)+sumtmp)/ilag(il))

        endif

100   continue

      return
      end
