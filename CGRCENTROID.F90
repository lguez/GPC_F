SUBROUTINE cgr_centroid(x, y, np, xcent, ycent, area, iret)
! --------------------------------------------------
!/************************************************************************
! * cgr_centroid        *
! *         *
! * This function computes the area and centroid (or center of mass) *
! * of the given polygon.      *
! *         *
! * Reference:        *
! * Graphics Gems IV, "Centroid of a Polygon", Gerard Bashein and *
! * Paul R. Detmer, pp 3-5.      *
! *         *
! * cgr_centroid ( x, y, np, xcent, ycent, area, iret )   *
! *         *
! * Input parameters:       *
! * x [np]  float  X coordinates of polygon *
! * y [np]  float  Y coordinates of polygon *
! * *np         int         Number of point in polygon      *
! *         *
! * Output parameters:       *
! * *xcent  float  X coordinate of centroid *
! * *ycent  float  Y coordinate of centroid *
! * *area  float  Area of polygon   *
! * *iret  int  Return code   *
! *       -1 = Not enough points *
! *       -2 = Area is zero  *
! *         *
! **         *
!DONE
!USE GEMINC
USE GEMPRM

!IMPLICIT NONE
! - - - arg types - - -
  INTEGER :: np,iret                                                     
  REAL(4) :: x(*) 
  REAL(4) :: y(*) 
  REAL(4) :: xcent,ycent                                                    
  REAL(4) :: area                                                    
! - - - local declarations - - -
  INTEGER :: i,j
  REAL(4) :: ai,atmp,xtmp,ytmp,z
!  LOGICAL :: G_DIFF
  LOGICAL :: LOutput

 LOGICAL DEBUG
    COMMON /PDEBUG/ DEBUG

!------------------------------------------------------------------------
  IF (DEBUG) write(25,*)'cgr_centroid START ',np

! - - - begin - - -
  atmp = 0.0
  xtmp = 0.0
  ytmp = 0.0
  iret = 0

!    /*
!     * Check for at least 3 points to make a polygon.
!     */

  IF (  np < 3 ) THEN
    iret = -1
    xcent = RMISSD
    ycent = RMISSD
    area  = RMISSD
    GOTO 9999
  END IF

  i = (np)-1
  j = 1

!    /*
!     * Compute the summation of the area and the first moments.
!     */

  DO WHILE (j <  np)
    ai = x(i) * y(j) - x(j) * y(i)
    atmp = atmp + ai
    xtmp = xtmp + ( ( x(j) + x(i) ) * ai)
    ytmp = ytmp + ( ( y(j) + y(i) ) * ai)
    i = j                                                                       
    j = j+1      
!    write(25,*)'cgr_centroid: j,ai,atmp,x/ytmp = ',j-1,ai,atmp,xtmp,ytmp
  END DO

!  write(25,*)'cgr_centroid: atmp = ',atmp
  
!    /*
!     * Compute the area of the polygon.
!     */

   area = ABS( atmp / 2.0 )

!   write(25,*)'cgr_centroid: area = ',area
   
!    /*
!     * Compute the location of the centroid of the polygon.
!     */

  z = 0.000001

  LOutput = ( ABS(atmp - 0.0) < 0.000001 )
  
!  write(25,*)'cgr_centroid: LOutput = ',LOutput
  
!  IF ( .NOT. G_DIFF(atmp, 0.0) ) THEN
  IF( .NOT. LOutput) THEN
    xcent = xtmp / ( 3.0 * atmp )
    ycent = ytmp / ( 3.0 * atmp )
  ELSE
    xcent = 0
    ycent = 0

    DO i = 1, np
      xcent = xcent + x(i)
      ycent = ycent + y(i)
    END DO

    xcent = xcent / (np)
    ycent = ycent / (np)
    iret = -2
  END IF

9999 CONTINUE

  IF (DEBUG) write(25,*)'cgr_centroid END ',xcent, ycent, area

END SUBROUTINE
!  0 Errors detected
