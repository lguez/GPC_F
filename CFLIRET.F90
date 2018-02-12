SUBROUTINE cfl_iret(ierrno,iflerr,iret)
! --------------------------------------------------
!/************************************************************************
! * cfl_iret								*
! *									*
! * This function translates a non-zero error value from a C I/O call	*
! * into a GEMPAK "CFL" error code.					*
! *									*
! * cfl_iret ( ierrno, iflerr, iret )					*
! *									*
! * Input parameters:							*
! *	 ierrno		int		Status from I/O operation	*
! *									*
! * Output parameters:							*
! *	*iflerr		int		GEMPAK "CFL" error number	*
! *					 -1 = File does not exist	*
! *					 -2 = Cannot open file		*
! *					 -3 = Cannot read file		*
! *					 -4 = Cannot write file		*
! *					 -5 = File already exists	*
! *					 -6 = No file has been opened	*
! *					 -7 = Cannot write / read	*
! *					 -8 = Permission denied		*
! *					 -9 = Invalid type of I/O	*
! *					-10 = Is a directory		*
! *					-11 = Is not a directory	*
! *	*iret		int		Return code			*
! *					   0 = Normal			*
! **									*

!USE GEMINC
USE GEMPRM

IMPLICIT NONE
! - - - arg types - - -
  INTEGER :: ierrno,iflerr                                                   
  INTEGER :: iret                                                    
! - - - local declarations - - -
  CHARACTER,POINTER :: osname

! - - - begin - - -
  iret = 0
  iflerr = ierrno
  
  IF ( ierrno == 0 ) THEN
    GOTO 9999
  END IF

!/*
! *	Determine operating system from environment variable.
! *	Translate ierrno into CFL-error.
! */

  IF ( ierrno == 2 )  THEN
    iflerr = -1
  ELSE IF ( (ierrno == 24) .OR. (ierrno == 63) ) THEN
    iflerr = -2
  ELSE IF ( (ierrno == 26) .OR. (ierrno == 28) ) THEN
    iflerr = -4
  ELSE IF ( ierrno == 17 ) THEN
    iflerr = -5
  ELSE IF ( ierrno == 9 ) THEN
    iflerr = -6
  ELSE IF ( ierrno == 5 ) THEN
    iflerr = -7
  ELSE IF ( (ierrno == 1) .OR. (ierrno == 13) .OR. (ierrno == 30) ) THEN
    iflerr = -8
  ELSE IF ( ierrno == 60 ) THEN
    iflerr = -9
  ELSE IF ( ierrno == 21 ) THEN
    iflerr = -10
  ELSE IF ( ierrno == 20 ) THEN
    iflerr = -11
  ELSE
!    osname = getenv( "OS" )
!    IF ( memcmp(osname, "HPUX", 4) == 0 ) THEN
!
!      IF ( ierrno == 53 ) THEN
!        iflerr = -2
!      ELSE IF ( ierrno == 51 ) THEN
!        iflerr = -3
!      ELSE IF ( ierrno == 50 ) THEN
!        iflerr = -9
!      END IF
!
!    ELSE IF ( memcmp(osname, "IRIX", 4) == 0 ) THEN
!      IF ( ierrno == 61 )  iflerr = -3
!    ELSE IF ( memcmp(osname, "SunOS", 5) == 0 ) THEN
!      IF ( ierrno == 61 )  iflerr = -3
!    END IF

  END IF

  9999 CONTINUE

END SUBROUTINE
!  0 Errors detected
