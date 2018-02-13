! --------------------------------------------------
SUBROUTINE cfl_clos(fptr,iret)
! --------------------------------------------------
!/************************************************************************
! * cfl_clos        *
! *         *
! * This function closes the specified file.  Closing a file that has *
! * been opened as a temporary file will cause the file to be removed. *
! *         *
! * cfl_clos ( fptr, iret )      *
! *         *
! * Input parameters:       *
! * *fptr  FILE  File pointer   *
! *         *
! * Output parameters:       *
! * *iret  int  Return code   *
! *      -6 = No file has been opened *
! **         *

!USE GEMINC
USE GEMPRM
USE MSVCRT
use ISO_C_BINDING

IMPLICIT NONE
! - - - arg types - - -
  type(C_PTR) :: fptr
  INTEGER :: iret                                                     
! - - - local declarations - - -
  INTEGER :: ier,errno,ifptr,fpclose

! - - - begin - - -
  iret = 0
  ifptr = transfer(fptr,0_C_INTPTR_T)

  IF ( ifptr == 0 ) THEN
    iret = -6
    GOTO 9999
  END IF

  fpclose = fclose(fptr)
  ier = transfer(fpclose,0_C_INTPTR_T)

  IF ( ier /= 0 ) CALL cfl_iret( errno, iret, ier )

  9999 CONTINUE

  RETURN

END SUBROUTINE
!INCLUDE 'C2F_LIB.F90'  
!  0 Errors detected
