!FUNCTION cfl_ropn(filnam,defdir,iret)  RESULT (output_4)
subroutine cfl_ropn(output_4,filnam,defdir,iret)
! --------------------------------------------------
!/************************************************************************
! * cfl_ropn								*
! *									*
! * This function opens a file for reading.				*
! *									*
! * The file is located by searching in the following order:		*
! *									*
! *	1. filnam (as given)						*
! *	2. defdir/filnam						*
! *									*
! * FILE *cfl_ropn ( filnam, defdir, iret )				*
! *									*
! * Input parameters:							*
! *	*filnam		char		File name			*
! *	*defdir		char		Default directory		*
! *									*
! * Output parameters:							*
! *	*iret		int		    Return code			*
! *	*cfl_ropn	FILE		File pointer			*
! **									*

!
!   TRY FL_SOPN ?								*
!
use ISO_C_BINDING
!USE geminc
USE gemprm
USE MSVCRT

! - - - arg types - - -
!  INTEGER :: output_4 
  type(C_PTR) :: output_4                                                                                                                                                                  
  INTEGER :: iret                                                    
  CHARACTER (LEN=*) :: filnam                                                   
  CHARACTER (LEN=*) :: defdir                                                   
! - - - local declarations - - -
  INTEGER :: ier,lflen,errno
  type(C_PTR) fpo,fpc
  INTEGER :: ifpo
  CHARACTER(LEN=1024 + 101,KIND=C_CHAR) :: fullname
  character(LEN=20,KIND=C_CHAR) mode
  character(LEN=20,KIND=C_CHAR) C_format


  	LOGICAL DEBUG
   	COMMON /PDEBUG/ DEBUG

!------------------------------------------------------------------------
   IF (DEBUG) write(25,*)'cfl_ropn START '
! - - - begin - - -
  iret = 0
  ifpo = 0

!  CALL fl_inqr( filnam, defdir, lflen, fullname, iret )
   CALL cfl_inqr( filnam, defdir, lflen, fullname, iret )
  filnam = filnam // C_NULL_CHAR

!write(25,*)'cfl_ropn: fullname = ',fullname
  IF (  iret == 0 ) THEN
    mode = 'r' // C_NULL_CHAR
    fpo = fopen( fullname, mode )	!<- C CODE?
	ifpo = transfer(fpo,0_C_INTPTR_T)
    
    IF ( ifpo == 0 ) THEN
      CALL cfl_iret( ifpo, iret, ier )
    END IF

  END IF

  output_4 = fpo
!  output_4 = ifpo
  
  IF (DEBUG) write(25,*)'cfl_ropn END ',IRET,ifpo
    
  RETURN
  
END 
!  0 Errors detected
