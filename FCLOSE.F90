FUNCTION fclose(fptr)  RESULT (output_4)
! --------------------------------------------------
!/************************************************************************
! * fclose								*
! *									*
! * This function closes a file and clears the pointer				*
! *									*
! * The file is located by searching in the following order:		*
! *									*
! *	1. filnam (as given)						*
! *	2. defdir/filnam						*
! *									*
! * FUNCTION fclose (fptr)  RESULT (output_4)			*
! *									*
! * Input parameters:							*
! *	*filnam		char		File name			*
! *	*defdir		char		Default directory		*
! *									*
! * Output parameters:							*
! *	*iret		int		Return code			*
! *	*cfl_ropn	FILE		File pointer			*
! **									*

IMPLICIT NONE
! - - - arg types - - -
  INTEGER :: output_4,fptr                                                                                                                                                                                   
! - - - local declarations - - -
  INTEGER :: ier

  INTEGER :: IFILENUM
  LOGICAL :: LFILENUM(100)
  COMMON/FILEINFO/IFILENUM,LFILENUM
! - - - begin - - -

!
!  CLOSING A FILE, SET THE POINTER TO .FALSE. IF CLOSE COMMAND
!  FAILS, REPORT THE ERROR.
!  ERROR *SHOULD* BE 0
!

  CLOSE (fptr, ERR=10)

  IF(IER.EQ.0) THEN
    IFILENUM = 0
	LFILENUM(fptr) = .FALSE.
  ENDIF
  
10 continue
  output_4 = IER

  RETURN
  END
