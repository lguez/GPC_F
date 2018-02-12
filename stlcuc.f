	SUBROUTINE ST_LCUC  ( string, outstr, iret )
C************************************************************************
C* ST_LCUC   								*
C*									*
C* This subroutine converts lower-case characters in a string to 	*
C* upper case.  The input and output string may be the same variable.	*
C*									*
C* ST_LCUC  ( STRING, OUTSTR, IRET )					*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String 				*
C*									*
C* Output parameters:							*
C*	OUTSTR		CHAR*		String in upper case		*
C*	IRET 		INTEGER		Return code			*
C*				   	 0 = normal return 		*
C**									*
!DONE
C************************************************************************
	CHARACTER*(*)	string, outstr
      
      LOGICAL DEBUG
   	COMMON /PDEBUG/ DEBUG
! - - - begin - - -
        IF (DEBUG) write(25,*)'ST_LCUC START'
C*---------------------------------------------------------------------
	iret = 0
C
C*	Move input string into output string and get length.
C
	outstr = string
	isize = LEN ( outstr )
C
C*	Loop through all characters.
C
	DO  i = 1, isize
C
C*	    Check to see if this is a lower case letter.
C
	  IF  ( ( outstr (i:i) .ge. 'a' ) .and. 
     +	    ( outstr (i:i) .le. 'z' ) )  THEN
C
C*		Get value of character, subtract 32 to convert to
C*		upper case and reinsert in string.
C
		j = ICHAR  ( outstr (i:i) )
		j = j - 32
		outstr (i:i) = CHAR (j)
	  ENDIF

      END DO
C*

      IF (DEBUG) write(25,*)'ST_LCUC END'
      
      RETURN
      END
