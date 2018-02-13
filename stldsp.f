 SUBROUTINE ST_LDSP  ( string, outstr, ncout, iret )
C************************************************************************
C* ST_LDSP        *
C*         *
C* This subroutine deletes the leading spaces and tabs in a string. *
C* The input and output strings may be the same variable.  *
C*         *
C* ST_LDSP  ( STRING, OUTSTR, NCOUT, IRET )    *
C*         *
C* Input parameters:       *
C* STRING  CHAR*  String    *
C*         *
C* Output parameters:       *
C* OUTSTR  CHAR*  Output string   *
C* NCOUT  INTEGER  Number of characters output *
C* IRET  INTEGER  Return code   *
C*       0 = normal return  *
C**         *
C************************************************************************
c INCLUDE  'GEMPRM.PRM'
c USE GEMPRM
C!
C! ASCII character constants
C!
        CHARACTER*1     CHNULL, CHCTLA, CHCTLC, CHTAB,  CHLF,   CHFF
        CHARACTER*1     CHCR,   CHCAN,  CHESC,  CHFS,   CHGS,   CHRS
        CHARACTER*1     CHUS,   CHSPAC, CHTLDA
C!
        PARAMETER       ( CHNULL = CHAR (0) )
C!                                              Null
        PARAMETER       ( CHCTLA = CHAR (1) )
C!                                              Control A
 PARAMETER ( CHCTLC = CHAR (3) )
C!      Control C
 PARAMETER ( CHTAB  = CHAR (9) )
C!      Tab
 PARAMETER ( CHLF   = CHAR (10) )
C!      Line feed
 PARAMETER ( CHFF   = CHAR (12) )
C!      Form feed
 PARAMETER ( CHCR   = CHAR (13) )
C!      Carriage return
 PARAMETER ( CHCAN  = CHAR (24) )
C!      Cancel (CAN)
 PARAMETER ( CHESC  = CHAR (27) )
C!      Escape
 PARAMETER ( CHFS   = CHAR (28) )
C!      FS
 PARAMETER ( CHGS   = CHAR (29) )
C!      GS
 PARAMETER ( CHRS   = CHAR (30) )
C!      Record Separator
 PARAMETER ( CHUS   = CHAR (31) )
C!      US
 PARAMETER ( CHSPAC = CHAR (32) )
C!      Space
 PARAMETER ( CHTLDA = CHAR (126) )
C!      Tilda

C*
 CHARACTER*(*) string, outstr
C*
 CHARACTER stbuf*160, c*1
      
      LOGICAL DEBUG
    COMMON /PDEBUG/ DEBUG
! - - - begin - - -
!        IF (DEBUG) write(25,*)'ST_LDSP START'
C*-------------------------------------------------------------------------
 lenin = LEN ( string )
 lenst = MIN ( lenin, 160 )
 stbuf = string
 iret  = 0
C
C* Get length of string.
C
 CALL ST_LSTR  ( stbuf (:lenst), lens, iret )
C
C* If length is non-zero, find first non space.
C
 IF  ( lens .eq. 0 )  THEN
   ncout  = 0
   outstr = ' '
 ELSE
   jp = 1
   c  = stbuf ( jp:jp )
C
   DO WHILE  ( ( ( c .eq. CHSPAC ) .or. ( c .eq. CHTAB ) .or.
     +       ( c .eq. CHNULL ) ) .and. ( jp .le. lens ) )
  jp = jp + 1
  IF  ( jp .le. lens )  c = stbuf ( jp:jp )
   ENDDO
C
C*   Compute length and fill output string.
C
   ncout = lens - jp + 1
   IF  ( ncout .gt. 0 )  THEN
  outstr = stbuf ( jp : lens )
   ELSE
  outstr = ' '
   END IF

      ENDIF
C*  
!      IF (DEBUG) write(25,*)'ST_LDSP END'
      
 RETURN
 END
