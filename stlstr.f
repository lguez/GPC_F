	SUBROUTINE ST_LSTR  ( string, lens, iret )
C************************************************************************
C* ST_LSTR								*
C*									*
C* This subroutine returns the number of characters in a string 	*
C* disregarding trailing null characters, tabs and spaces.		*
C*									*
C* ST_LSTR  ( STRING, LENS, IRET )						*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String 				*
C*									*
C* Output parameters:							*
C*	LENS		INTEGER 	Length of string		*
C*	IRET		INTEGER		Return code			*
C*				 	 0 = normal return 		*
C**									*
C************************************************************************
!	USE GEMINC
c	USE GEMPRM

C	INCLUDE		'GEMPRM.PRM'
C!
C!	ASCII character constants
C!
        CHARACTER*1     CHNULL, CHCTLA, CHCTLC, CHTAB,  CHLF,   CHFF
        CHARACTER*1     CHCR,   CHCAN,  CHESC,  CHFS,   CHGS,   CHRS
        CHARACTER*1     CHUS,   CHSPAC, CHTLDA
C!
        PARAMETER       ( CHNULL = CHAR (0) )
C!                                              Null
        PARAMETER       ( CHCTLA = CHAR (1) )
C!                                              Control A
	PARAMETER	( CHCTLC = CHAR (3) )
C!						Control C
	PARAMETER	( CHTAB  = CHAR (9) )
C!						Tab
	PARAMETER	( CHLF   = CHAR (10) )
C!						Line feed
	PARAMETER	( CHFF   = CHAR (12) )
C!						Form feed
	PARAMETER	( CHCR   = CHAR (13) )
C!						Carriage return
	PARAMETER	( CHCAN  = CHAR (24) )
C!						Cancel (CAN)
	PARAMETER	( CHESC  = CHAR (27) )
C!						Escape
	PARAMETER	( CHFS   = CHAR (28) )
C!						FS
	PARAMETER	( CHGS   = CHAR (29) )
C!						GS
	PARAMETER	( CHRS   = CHAR (30) )
C!						Record Separator
	PARAMETER	( CHUS   = CHAR (31) )
C!						US
	PARAMETER	( CHSPAC = CHAR (32) )
C!						Space
	PARAMETER	( CHTLDA = CHAR (126) )
C!						Tilda

C*
      CHARACTER*(*)	string
C*
	CHARACTER*1	c
      
      LOGICAL DEBUG
   	COMMON /PDEBUG/ DEBUG
! - - - begin - - -
!        IF (DEBUG) write(25,*)'ST_LSTR START'
C*------------------------------------------------------------------------
	lens = 0
	iret = 0
C
C*	Get the actual length of the string.
C
	lens = LEN  ( string )
	IF  ( lens .eq. 0 )  GOTO 9999
C
C*	Start at last character and loop backwards.
C
	ip = lens
	DO WHILE  ( ip .gt. 0 )
C
C*	  Get current value of string and check for space, null, tab.
c
	  c = string ( ip : ip )
	  IF  ( ( c .eq. CHSPAC ) .or. ( c .eq. CHNULL ) .or.
     +	    ( c .eq. CHTAB  ) )  THEN
		lens = lens - 1
		ip   = ip - 1
	  ELSE
		ip   = 0
	  END IF

	END DO
C*
9999  CONTINUE

!      IF (DEBUG) write(25,*)'ST_LSTR end'
      
	RETURN
	END
