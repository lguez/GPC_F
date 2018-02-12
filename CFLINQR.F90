SUBROUTINE cfl_inqr(filnam,defdir,flen,newfil,iret)
! --------------------------------------------------

!!
!!  NOTE*** USE FL_INQR INSTEAD
!!

!/************************************************************************
! * cfl_inqr								*
! *									*
! * This function determines whether a file exists and the file size.	 *
! *									*
! * The file is located by searching in the following order (environment *
! * variables may be used as part of the paths):				*
! *									*
! *	1. filnam (as given)						*
! *	2. defdir/filnam						*
! *									*
! * cfl_inqr ( filnam, defdir, flen, newfil, iret )			*
! *									*
! * Input parameters:							*
! *	*filnam		char		File name			*
! *	*defdir		char		Default directory		*
! *									*
! * Output parameters:							*
! *	*flen		long		File size			*
! *	*newfil		char		Expanded file name		*
! *	*iret		int		Return code			*
! *					  0 = Normal, file exists	*
! *					 -1 = File does not exist	*
! **									*

!USE GEMINC
USE GEMPRM

IMPLICIT NONE
! - - - arg types - - -
  INTEGER :: flen,iret                                                     
  CHARACTER (LEN=*) :: filnam                                                   
  CHARACTER (LEN=*) :: defdir                                                   
  CHARACTER (LEN=*) :: newfil                                                   
! - - - local declarations - - -
  INTEGER :: ier,ier1
  CHARACTER (LEN=LLPATH) :: newname
  LOGICAL	::	Lexists
!  TYPE (STAT) :: stbuf

 LOGICAL DEBUG
 COMMON /PDEBUG/ DEBUG

!------------------------------------------------------------------------
   IF (DEBUG) WRITE (25,*)'cfl_inqr: START ',filnam
! - - - begin - - -
   iret = 0
   flen = 0

!  CALL css_envr( filnam, newname, ier )
!  CALL ss_envr( filnam, newname, ier )

!   newfil = newname
   newfil = filnam

!C
!C*	Do the INQUIRE with the modified file name.  If the file is
!C*	found, we are done.
!C

	INQUIRE ( FILE = newfil, EXIST = Lexists, SIZE = flen, IOSTAT = ier1 )
    
    WRITE (25,*)'cfl_inqr: Lexists 1 = ',Lexists,IER1
      
!  ier1 = 0
  IF (ier1 /= 0 .OR. .NOT.Lexists ) THEN

    IF ( defdir /= ' ' ) THEN
!      CALL css_envr( defdir, newfil, ier )
!      CALL ss_envr( defdir, newfil, ier )

      newfil = newfil(1:LEN_TRIM(newfil)) // "\"
      newfil = newfil(1:LEN_TRIM(newfil)) // newname
 
      WRITE (25,*)'cfl_inqr: INQUIRE - 2 ',newfil
  	  INQUIRE ( FILE = newfil, EXIST = Lexists, SIZE = flen, IOSTAT = ier1 )

      WRITE (25,*)'cfl_inqr: Lexists 2 = ',Lexists,IER1
!      ier1 = stat( newfil, stbuf )
    END IF

  ELSE
    

  END IF

  IF ( ier1 == 0 ) THEN
 !    flen = INT(stbuf%st_size)
      WRITE (25,*)'cfl_inqr - ERROR = 0'
  ELSE
!    CALL cfl_iret( errno, iret, ier )
      WRITE (25,*)'cfl_inqr - ERROR NO = 0...'
  END IF

IF (DEBUG) WRITE (25,*)'cfl_inqr: END '

END SUBROUTINE
!  0 Errors detected
