MODULE GEMPRM_FUNC
    
CONTAINS

!  Macro definitions

!G_MAX(a,b)	= ( ( (a) > (b) ) ? (a) :  (b) )
FUNCTION G_MAX(a,b)

G_MAX = MAX(a,b)

END FUNCTION

!G_MIN(a,b)	= ( ( (a) < (b) ) ? (a) :  (b) )
FUNCTION G_MIN(a,b)

G_MIN = MIN(a,b)

END FUNCTION

!G_ABS(a)	= ( ( (a) >= 0.0F  ) ? (a) : -(a) )
REAL FUNCTION G_ABS(a)

G_ABS = ABS(a)

END FUNCTION

!G_NINT(x)	= (((x)<0.0F) ? 
!             ((((x)-(float)((int)(x)))<=-.5F)?(int)((x)-.5F):(int)(x)) : 
!             ((((x)-(float)((int)(x)))>=.5F)?(int)((x)+.5F):(int)(x)))
FUNCTION G_NINT(x)

G_NINT = NINT(x)

END FUNCTION

! *  This macro returns a double, though the parameters can be any
! *  consitent type

REAL FUNCTION G_DIST(x1, y1, x2, y2)

G_DIST = (dsqrt (DBLE (((x2 - x1) * (x2 - x1)) +  &
			           ((y2 - y1) * (y2 - y1)))))

END FUNCTION

! *  This macro is for comparing floating point equality

FUNCTION G_DIFF(x, y) RESULT( Output )
LOGICAL :: Output
REAL :: x,y,z

z = 0.000001

Output = ( G_ABS(x-y) < 0.000001 )
!Output = Compare_Float( x, y, z )

END FUNCTION

FUNCTION G_DIFFT(x,y,t) RESULT( Output )
REAL :: x,y,t
LOGICAL :: Output
Output = ( G_ABS(x-y) < t )
!Output = Compare_Float( x, y, t )

END FUNCTION

! *  This macro performs a modulus for floating point types

FUNCTION G_MOD(x,y) RESULT( Output )
integer :: Output
!G_MOD = ( int( (x)/(y) ) - int( (x)/(y) ) )
Output = INT(AMOD(x,y))

END FUNCTION

REAL FUNCTION ERMISS(xxxx)

ERMISS = ( G_ABS(xxxx - -9999.0) < 0.1 )

END FUNCTION

! *  Convert Degrees-minutes to Degrees-decimal

REAL FUNCTION DMTODD(xxxx)

DMTODD = INT( (xxxx/100.0) + DMOD(DBLE(xxxx),100.0) / 60.0 )

END FUNCTION

! *  Convert Degrees-decimal to Degrees-minutes

REAL FUNCTION DDTODM(xxxx)

DDTODM = ( (int(xxxx)*100.0) + G_NINT( int(((xxxx)-int(xxxx))*100.0) * .60 ) )

END FUNCTION

FUNCTION Compare_Float( x, y, ulp ) RESULT( Compare )
  REAL,              INTENT( IN )  :: x
  REAL,              INTENT( IN )  :: y
  REAL, OPTIONAL,    INTENT( IN )  :: ulp
  LOGICAL :: Compare
  REAL :: Rel

  Rel = 1.0
  
  IF ( PRESENT( ulp ) ) THEN
    Rel = ulp
  END IF
  
  Compare = ABS( x - y ) < ( Rel * SPACING( MAX(ABS(x),ABS(y)) ) )
  
END FUNCTION Compare_Float

FUNCTION SC(xxxx) RESULT(IOUT_4)

  INTEGER :: scale, IOUT_4
  REAL :: xxxx

  IOUT_4 = G_NINT( xxxx * scale ) * 2

END FUNCTION SC

    
END MODULE

