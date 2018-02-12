	SUBROUTINE ST_RMBL  ( string, outstr, length, iret )
C************************************************************************
C* ST_RMBL								*
C*									*
C* This subroutine removes spaces and tabs from a string.  The input	*
C* and output strings may be the same variable.				*
C*									*
C* ST_RMBL  ( STRING, OUTSTR, LENGTH, IRET )				*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String				*
C*									*
C* Output parameters:							*
C*	OUTSTR		CHAR*		String without blanks		*
C*	LENGTH		INTEGER		Length of output string		*
C*	IRET		INTEGER		Return code			*
C*				   	 0 = normal return 		*
C**									*
C************************************************************************
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
	CHARACTER*(*)	string, outstr
C*
	CHARACTER	c*1, sss*160, ttt*160
      
       LOGICAL DEBUG
   	COMMON /PDEBUG/ DEBUG
! - - - begin - - -
      IF (DEBUG) write(25,*)'ST_RMBL START'
C-----------------------------------------------------------------------
	iret   = 0
	length = 0
	lenin  = LEN ( string )
	lenst  = MIN ( lenin, 160 )
	sss    = string
	ttt    = ' '
C
C*	Get length of input string.
C
	CALL ST_LSTR  ( sss (:lenst), lens, iret )
C
C*	Check each character to see if it is a blank.
C
	DO i = 1, lens
	  c = sss (i:i)

	  IF  ( ( c .ne. CHSPAC ) .and. ( c .ne. CHTAB ) )  THEN
		length = length + 1
		ttt ( length : length ) = c
	  END IF

	END DO
C*
	outstr = ttt
C*
      IF (DEBUG) write(25,*)'ST_RMBL END'
      
	RETURN
	END
