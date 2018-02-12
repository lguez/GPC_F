	SUBROUTINE ST_NUMB  ( string, ival, iret )
C************************************************************************
C* ST_NUMB                                       			*
C*									*
C* This subroutine converts a string into an integer.			* 
C*									*
C* ST_NUMB  ( STRING, IVAL, IRET )					*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String				*
C*									*
C* Output parameters:							*
C*	IVAL		INTEGER		Integer value      		*
C*	IRET		INTEGER		Return code			*
C*				 	  0 = normal return		*
C*					 -2 = conversion error		*
C** 									*
C************************************************************************
c	USE GEMPRM

C	INCLUDE		'GEMPRM.PRM'
	PARAMETER	( IMISSD = -9999   )
C!						Missing integer value
C*
	CHARACTER*(*) 	string
C*
	CHARACTER	sss*12, c*1
	LOGICAL		good, plus
      
	LOGICAL DEBUG
   	COMMON /PDEBUG/ DEBUG

C------------------------------------------------------------------------
      IF (DEBUG) write(25,*)'ST_NUMB START'
      
	iret = -2
	ival = IMISSD
C
C*	Remove blanks from string.
C
	CALL ST_RMBL  ( string, sss, lens, ier )
C
C*	Check for empty string.
C
	IF ( lens .eq. 0 )  GOTO 9999
C
C*	If last character is period, remove it.
C
	IF ( sss (lens:lens) .eq. '.' )  THEN
	   sss (lens:lens) = ' '
	   lens = lens - 1
	END IF

	IF  ( lens .eq. 0 )  RETURN
C
C*	Check for + or - in first character.
C
	IF ( sss (1:1) .eq. '+' )  THEN
	  ibeg = 2
	  plus = .true.
	ELSE IF  ( sss (1:1) .eq. '-' )  THEN
	  ibeg = 2
	  plus = .false.
	ELSE
	  ibeg = 1
	  plus = .true.
	END IF

	IF ( ibeg .gt. lens )  RETURN
C
C*	Now loop through all characters and turn into integer.
C
	ival0 = ICHAR ( '0' )
	ival  = 0
	good  = .true.
	i     = ibeg
	
	DO WHILE  ( good .and. ( i .le. lens ) )
	  c     = sss (i:i)
	  ivalc = ICHAR ( c ) - ival0
	  IF  ( ( ivalc .ge. 0 ) .and. ( ivalc .le. 9 ) )  THEN
		ival  = ival * 10 + ivalc
		i     = i + 1
	  ELSE
		good  = .false.
	  END IF
	
	END DO
C
C*	Check for good value and add sign.
C
	IF ( .not. good )  THEN
	  ival = IMISSD
	ELSE IF  ( plus )  THEN
	  iret = 0
	ELSE
	  iret = 0
	  ival = -ival
	END IF
C*
9999  CONTINUE

      IF (DEBUG) write(25,*)'ST_NUMB END'
      
	RETURN
	END
