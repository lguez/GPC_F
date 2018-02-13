SUBROUTINE cfl_trln(ifptr,bufsiz,buffer,iret)
! --------------------------------------------------
!/************************************************************************
! * cfl_trln        *
! *         *
! * This function reads a line of data from a GEMPAK table file into *
! * BUFFER.  It terminates the line after the last non-blank character *
! * with a null character.  This function skips over comments and  *
! * blank lines.        *
! *         *
! * A GEMPAK table file is a text file that may have comment records *
! * within the file.  A comment record is a record where the first *
! * non-blank character is an exclamation point.    *
! *         *
! * This routine has an internal buffer size of LENBUF=256.  If the *
! * file which is being read contains records longer than 256, the *
! * chances are great that the file is not a GEMPAK table file and an *
! * error is returned.       *
! *         *
! * cfl_trln ( ifptr, bufsiz, buffer, iret )    *
! *         *
! * Input parameters:       *
! * *ifptr  FILE File pointer    *
! * bufsiz  int Size of buffer    *
! *         *
! * Output parameters:       *
! * *buffer  char Text string from file   *
! * *iret  int Return code    *
! *      0 = Normal; full record returned *
! *      1 = Record length exceeds passed-in *
! *     buffer size;    *
! *     partial record returned  *
! *      4 = EOF reached   *
! *     -3 = Read failure   *
! *     -6 = No file has been opened  *
! *    -24 = Record > internal buffer size *
! **         *

!USE GEMINC
USE GEMPRM

IMPLICIT NONE
! - - - arg types - - -
  INTEGER :: ifptr,bufsiz                                                   
  INTEGER :: iret  
  CHARACTER(*) :: buffer
! - - - local declarations - - -
  INTEGER :: lenbuf2,found,nonblank,b1,ier
  INTEGER :: LENBUF
  CHARACTER (LEN=256) :: readbuf,strspn
  CHARACTER :: tchar,NULL
  CHARACTER,POINTER :: string
  LOGICAL :: leof,lerror
  
  LOGICAL DEBUG
    COMMON /PDEBUG/ DEBUG
! - - - begin - - -
    write(25,*)'cfl_trln START ',iret,ifptr
    
   LENBUF = 256
   iret = 0
   NULL = ' '
   leof = .false.
   lerror = .false.

  IF ( ifptr == 0 ) THEN
     iret = -6
    GOTO 9999
  END IF

! /*
!  *  Read records until a data record is found.
!  */

  found = 0

  write(25,*)'cfl_trln: READ FILE'
  
  DO WHILE ( found == 0 )

!      write(25,*)'cfl_trln: READING...'
!     /*
!      *  Read the record and check for errors.
!      */

    read (ifptr,'(A)',SIZE=LENBUF,err=10,end=11,EOR=15, ADVANCE='NO' ) readbuf
    goto 15
10  continue
    lerror = .true.
    goto 15
    
11  continue
    leof = .true.
    
15  continue
    IF ( leof .AND. .NOT.lerror ) THEN
       iret = 4
      GOTO 9999
    END IF

    IF ( lerror ) THEN
       iret = -3
      GOTO 9999
    END IF

!     /*
!      *  Check the length of returned record.
!      */

    lenbuf2 = LEN(TRIM(ADJUSTL(readbuf)))
    
!    write(25,*)'cfl_trln: lenbuf2,LENBUF = ',lenbuf2,LENBUF
!    write(25,*)'cfl_trln: ',READBUF

    IF ( lenbuf2 >= 256 ) THEN
       iret = -24
      GOTO 9999
    ELSEIF ( lenbuf2 == 0 ) THEN ! EMPTY LINE READ...
      iret = 2
      GOTO 9999
    END IF

!     /*
!      *  Check first non-blank character.  
!      *  If valid, copy string and return; otherwise continue.
!      */

!    nonblank = strspn(readbuf, " \t")
    tchar = readbuf(nonblank+1:nonblank+1)
    
!    write(25,*)'cfl_trln: tchar = ',tchar

!    IF ( tchar /= '.NOT.' .AND. tchar /= CHAR(10) .AND. &
!   tchar /= CHAR(000)) THEN
    IF ( tchar /= '!' .AND. tchar /= CHAR(10) .AND. &
   tchar /= CHAR(000)) THEN
    found = 1

      IF ( lenbuf2 < bufsiz ) THEN
        buffer = readbuf
!        CALL st_lstr( buffer, b1, ier )
!        buffer(:b1+1) = CHAR(000)
      ELSE
        iret = 1
        b1 = bufsiz - 1
        buffer = readbuf
!        buffer(:b1+1) = CHAR(000)
      END IF

    END IF

!    write(25,*)'cfl_trln: found = ',found
  END DO

9999 CONTINUE

  write(25,*)'cfl_trln END ',iret

END SUBROUTINE
!INCLUDE 'C2F_LIB.F90'  
!  0 Errors detected
