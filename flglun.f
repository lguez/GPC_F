 SUBROUTINE FL_GLUN  ( lun, iret )
C************************************************************************
C* FL_GLUN        *
C*          *
C* This subroutine gets a logical unit number that can be used for *
C* file access.  It is used to eliminate conflicts in assigning  *
C* logical unit numbers.      *
C*          *
C* FL_GLUN  ( LUN, IRET )      *
C*          *
C* Output parameters:       *
C* LUN  INTEGER  Logical unit number   *
C* IRET  INTEGER  Return code   *
C*        0 = normal return  *
C*     -21 = no more luns  *
C**         *
C************************************************************************
c INCLUDE  'GEMPRM.PRM'
c INCLUDE  'GMBDTA.CMN'

 integer lun 

 INTEGER  IFILENUM
 LOGICAL  LFILENUM(100)
 COMMON/FILEINFO/IFILENUM,LFILENUM
 
      LOGICAL DEBUG
    COMMON /PDEBUG/ DEBUG
C------------------------------------------------------------------------
       IF (DEBUG) WRITE (25,*)'FL_GLUN START '
 iret = -21
C
C* Get logical unit number from table.
C
 lun  = -1
 knt  = 1
c DO WHILE  ( ( knt .le. 10 ) .and. ( lun .eq. -1 ) )
c   IF  ( lungem ( knt ) .eq. 0 )  THEN
c  lun = 10 + knt
c  lungem (knt) = 1
c  iret = 0
c   ELSE
c  knt = knt + 1
c   END IF
c END DO

!
!  TO FIND AN AVAILABLE FILE POINTER, CYCLE THROUGH
!  LFILENUM. THE FIRST AVAILABLE (.FALSE.) WILL BE USED 
!  & SET TO .TRUE. SO IT CAN'T BE USED BY ANY OTHER
!  TASKS.
!
   DO I=100,200
     IF(.NOT.LFILENUM(I)) THEN
    lun = I
    LFILENUM(I) = .TRUE.
    iret = 0
    EXIT
  ENDIF
   ENDDO

C*
      WRITE (25,*)'FL_GLUN END ',LUN,iret
      
 RETURN
 END
