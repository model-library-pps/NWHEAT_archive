*---- Calculate root distribution. ASSUME root density decreases
*     exponentially down the profile. Upper two layers are treated
*     as a single compartment in this calculation
*     This root distribution is only used when detailed nitrogen
*     uptake routine UPTA is used

Known variables:

THCKN   array with thickness of soil compartments     [cm] 
IN      soil compartment where root growth occurs     [-] 
DEPTH   array with middle of soil measureded from
	top of soil profile                           [cm]     
RDINL   how far the roots are penetrated in IN        [cm]
WRT     weight of the roots                           [kg/ha]
LRV     root density                                  [kg/ha/d]

Unknown:
ARTT
ART
DISTUP
SUM
DIST
DISTF


*-----------------------------
      ARTT  = 0.
      SUM   = 0.
      DISTUP = EXP(-DISTF*0.01*((THCKN(1)+THCKN(2))/2.))
      SUM    = SUM +DISTUP

      IF (IN.GT.3) THEN

	 DO 121, I=3,IN-1
	    DIST(I) = EXP(-DISTF*0.01*DEPTH(I))
	    SUM     = SUM + DIST(I)
121      CONTINUE

      ENDIF

      DIST(IN) = EXP(-DISTF*0.01*(DEPTH(IN)-0.5*THCKN(IN)
     $           +RDINL*0.5))
      SUM      = SUM + DIST(IN)

      ART(1) = (WRT*DISTUP/SUM)*THCKN(1)/(THCKN(1)+THCKN(2))
      ART(2) = (WRT*DISTUP/SUM)*THCKN(2)/(THCKN(1)+THCKN(2))
      ARTT   = ART(1)+ART(2)

      IF (IN.GT.3) THEN
	 DO 131, I=3,IN-1
	    ART(I) = WRT*DIST(I)/SUM
	    ARTT   = ARTT+ART(I)
131      CONTINUE
      ENDIF

      ART(IN) = WRT*DIST(IN)/SUM
      ARTT    = ARTT+ART(IN)

      IF (ABS(ARTT-WRT).GT.1.0E-3) STOP
     $  ' ERROR IN CALC. OF ROOT DISTIB.'

*---- Assume a Specific Root Length of 200m/g
*     the factor 1.exp-3 is to convert kg/ha to
*     cm/cm2

      DO 141, I=1,IN
	 LRV(I) = ART(I) * 200. *1.E-3 / THCKN(I)
141   CONTINUE

*--------------------------------
