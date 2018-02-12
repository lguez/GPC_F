	SUBROUTINE ST_UCLC  ( string, outstr, iret )
C************************************************************************
C* ST_UCLC   								*
C*									*
C* This subroutine converts upper-case characters in a string to 	*
C* lower case.  The input and output strings may be the same 		*
C* variable.								*
C*									*
C* ST_UCLC  ( STRING, OUTSTR, IRET )					*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String 				*
C*									*
C* Output parameters:							*
C*	OUTSTR		CHAR*		String in upper case		*
C*	IRET 		INTEGER		Return code			*
C*				   	 0 = normal return 		*
C**									*
C************************************************************************
	CHARACTER*(*)	string, outstr
      
      LOGICAL DEBUG
   	COMMON /PDEBUG/ DEBUG
! - - - begin - - -
      IF (DEBUG) write(25,*)'ST_UCLC START'
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
	DO i = 1, isize
C
C*	  Check to see if this is a lower case letter.
C
	  IF  ( ( outstr (i:i) .ge. 'A' ) .and. 
     +	    ( outstr (i:i) .le. 'Z' ) )  THEN
C
C*		Get value of character, add 32 to convert to
C*		lower case and reinsert in string.
C
		j = ICHAR ( outstr(i:i) )
		j = j + 32
		outstr (i:i) = CHAR(j)
	  END IF
	
      END DO
C*
      IF (DEBUG) write(25,*)'ST_UCLC END'
      
      RETURN
      END
