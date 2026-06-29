!***********************#file "SERVICE.FOR" *****************************!
!************************************************************************!
!*  REAL FUNCTION INTGRL                                                *!
!*                                                                      *!
!*  Date   : 18-FEB-1987                                                *!
!*  Purpose: The INTGRL (Integral) function updates state variables     *!
!*           on the basis of their rate of change. The initial value    *!
!*           of the integral is given in the calling routine or         *!
!*           calling program                                            *!
!*                                                                      *!
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *!
!*  name    meaning                                     units  class    *!
!*  ----    -------                                     -----  -----    *!
!*  GR      rate of change of state variable              =      I      *!
!*  H       input value of state variable that is                       *!
!*          updated                                       =      I      *!
!*  INTGRL  output value of state variable that is                      *!
!*          updated                                       =      O      *!
!************************************************************************!

      REAL FUNCTION INTGRL(GR,H)

      IMPLICIT REAL (A-Z)
      DELT = 1.


*---- multiplication of rate and time step and addition to previous
*     value

      INTGRL = H+DELT*GR

      RETURN
      END







*---------------------------------------------------------------------------*
* FUNCTION LIMIT                                                            *
* Function limits X between P1 and P2                                       *
*---------------------------------------------------------------------------*

      REAL FUNCTION LIMIT(P1,P2,X)


      IMPLICIT REAL (A-Z)

      IF (P1.GT.P2) THEN
         WRITE (*,'(A)') ' LIMIT function arguments error'
         WRITE (*,'(A,F8.2,A,F8.2)')
     $           ' Lower bound =',P1,';  Upper bound =',P2
         WRITE (*,'(A)') ' Run deleted!!'
         STOP
      END IF

      IF (X.GT.P2) THEN
         LIMIT = P2
      ELSE
         IF (X.LT.P1) THEN
            LIMIT = P1
         ELSE
            LIMIT = X
         END IF
      END IF

      RETURN
      END

*---------------------------------------------------------------------------*
*  REAL FUNCTION AFGEN                                                      *
*  Author : Daniel  van Kraalingen                                          *
*  Date   : 18-FEB-1987                                                     *
*  Purpose: This function is a linear interpolation function.  The          *
*           function does not extrapolate : in case of X below or           *
*           above the region defined by TABLE, the first                    *
*           respectively the last Y-value is returned and a message         *
*           is generated.                                                   *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name    meaning                                     units  class         *
*  ----    -------                                     -----  -----         *
*  AFGEN  function name, result of the interpolation     =      O           *
*  TABLE  A one-dimensional array with paired            =      I           *
*         data: x,y,x,y, etc.                                               *
*  ILTAB  The number of elements of the array            -      I           *
*         TABLE                                                             *
*  X      The value at which interpolation should        =      I           *
*         take place                                                        *
*                                                                           *
*  FATAL ERROR CHECKS (execution terminated, message)                       *
*  condition                                                                *
*  ---------                                                                *
*  TABLE(I) < TABLE(I-2) , for I odd                                        *
*  ILTAB odd                                                                *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : none                                                        *
*---------------------------------------------------------------------------*

      REAL FUNCTION AFGEN (ILTAB,TABLE,X)
      IMPLICIT REAL (A-Z)
      INTEGER I, IUP, ILTAB
      DIMENSION TABLE(ILTAB)


*---- check on odd ILTAB

      IF (MOD(ILTAB,2).NE.0) THEN
         WRITE (*,'(A,I4/,A)')
     $        ' ERROR in function AFGEN : ILTAB=',ILTAB,
     $        ' ILTAB must be even !'
         STOP
      END IF

      IUP = 0


      DO 10 I=3,ILTAB,2

*------- check on ascending order of X-values in function

         IF (TABLE(I).LE.TABLE(I-2)) THEN
            WRITE (*,'(A,I4/,A,I4,A/,A)')
     $           ' X-coordinates not in ascending order at element',I,
     $           ' AFGEN-function contains',ILTAB,' points',
     $           ' Run deleted!'
            STOP
         END IF

         IF (IUP.EQ.0.AND.TABLE(I).GE.X) IUP = I

10    CONTINUE


      IF (X.LT.TABLE(1)) THEN
         WRITE (*,'(A/A,I4,A/A,G8.2)')
     $           ' Interpolation below defined region!!',
     $           ' AFGEN-function contains ',ILTAB,' points,',
     $           ' Interpolation at X=',X
         AFGEN = TABLE(2)
         GOTO 40
      END IF


      IF (X.GT.TABLE(ILTAB-1)) THEN
         WRITE (*,'(A/A,I4,A/A,G8.2)')
     $           ' Interpolation above defined region!!',
     $           ' AFGEN-function contains ',ILTAB,' points,',
     $           ' Interpolation at X=',X
         AFGEN = TABLE(ILTAB)
         GO TO 40
      END IF


*---- normal interpolation

      SLOPE = (TABLE(IUP+1)-TABLE(IUP-1))/(TABLE(IUP)-TABLE(IUP-2))
      AFGEN = TABLE(IUP-1)+(X-TABLE(IUP-2))*SLOPE

40    RETURN
      END




*--------------------------------------------------------------------------*
*  SUBROUTINE MOFILP                                                       *
*  Author : Daniel van Kraalingen                                          *
*  Date   : december 1986                                                  *
*  Purpose: This subroutine moves the filepointer across comment lines     *
*           of data files and places the file pointer at the first non     *
*           comment record. Comment lines are marked by an asterisk (!)    *
*           in their first column.                                         *
*                                                                          *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)         *
*  name    meaning                                     units  class        *
*  ----    -------                                     -----  -----        *
*  IUNIT   unit number from which data are read          -      I          *
*                                                                          *
*  SUBROUTINES called : none                                               *
*  FILE usage : none                                                       *
*--------------------------------------------------------------------------*

      SUBROUTINE MOFILP (IUNIT)

      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)
      CHARACTER LINE*80,CHR*1

*---- the file with unit number IUNIT is read as long as the first
*     character of a record is an asterisk (*).

      CHR = '!'
10    IF (CHR.EQ.'!') THEN
         READ (IUNIT,'(A)') LINE
         CHR = LINE(1:1)
         GOTO 10
      ENDIF

*---- the filepointer backspaced one line

      BACKSPACE (IUNIT)

      RETURN
      END


*---------------------------------------------------------------------------*
      INTEGER FUNCTION ILEN (STRING)

*     this subroutine determines the significant length (ILEN)
*     of a string (STRING), if the string is empty a zero is returned

*     ILEN, returned length (O)
*     STRING, input string  (I)

*     Author: Daniel van Kraalingen
*     Date: Aug 87


      CHARACTER*(*) STRING

      DO 10 ILEN=LEN(STRING),1,-1
         IF (STRING(ILEN:ILEN).NE.' ') RETURN
10    CONTINUE

      RETURN
      END
*---------------------------------------------------------------------------*

*---------------------------------------------------------------------------*
      SUBROUTINE CLS

      PRINT '(1X,A1,A3)', CHAR(27),'[2J'

      RETURN
      END
*---------------------------------------------------------------------------*





*---------------------------------------------------------------------------*
*  FUNCTION INSW                                                            *
*  Purpose: Input switch depending on sign of X1                            *
*---------------------------------------------------------------------------*

      FUNCTION INSW (X1,X2,X3)

      IMPLICIT REAL (A-Z)

      IF (X1.LT.0.) THEN
         INSW = X2
      ELSE
         INSW = X3
      END IF

      RETURN
      END
!***********************End of "SERVICE.FOR" ****************************!
