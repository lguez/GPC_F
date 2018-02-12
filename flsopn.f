	SUBROUTINE FL_SOPN  ( filnam, lun, iret )
C************************************************************************
C* FL_SOPN								*
C* 									*
C* This subroutine opens an existing sequential file and returns a 	*
C* logical unit number to be used to access the file.  The file is 	*
C* opened READONLY.							*
C* 									*
C* FL_SOPN  ( FILNAM, LUN, IRET )					*
C* 									*
C* Input parameters:							*
C*	FILNAM		CHAR*		File name 			*
C* 									*
C* Output parameters:							*
C*	LUN		INTEGER		Logical unit number 		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = cannot open file		*
C**									*

C************************************************************************
C	INCLUDE		'GEMPRM.PRM'
      USE         GEMPRM
C*
	CHARACTER*(*)	filnam
C*
	CHARACTER	file*132
	LOGICAL		exist
      
	LOGICAL DEBUG
   	COMMON /PDEBUG/ DEBUG
C------------------------------------------------------------------------
      IF (DEBUG) WRITE (25,*)'FL_SOPN START '
      
C*	Get a logical unit number to use.  Error if none available.
C
	CALL FL_GLUN  ( lun, iret )
	IF  ( iret .ne. 0 .or. lun eq 0 )  goto 9999
C
C*	Call to FL_INQR will perform all the case tests and translate
C*	environmental variables.
C
!      WRITE (25,*)'FL_SOPN: filnam = ',filnam
!	CALL FL_INQR  ( filnam, exist, file, ier )
      
      file = filnam
      exist = .TRUE.
C
C*	If the file exists, open it.  If not, attempt open in order to
C*	get correct error number from this machine.
C
!      WRITE (25,*)'FL_SOPN: file = ',file
!      WRITE (25,*)'FL_SOPN: exist = ',exist
      
	IF  ( exist )  THEN
	    OPEN ( UNIT = lun, FILE = file,  STATUS = 'OLD',
     +		   ACCESS = 'SEQUENTIAL',  IOSTAT = iostat ) 
	  ELSE
	    OPEN ( UNIT = lun, FILE = filnam,  STATUS = 'OLD',
     +		   ACCESS = 'SEQUENTIAL',  IOSTAT = iostat ) 
        END IF
        
!        WRITE (25,*)'FL_SOPN: iostat = ',iostat
C
C*	If open failed, get error message and free lun.
C
	IF  ( iostat .ne. 0 )  THEN
	    iret = -1
	    CALL FL_FLUN ( lun, ier )
	    lun = 0
	  ELSE
	    iret = 0
        END IF
C*
9999  continue
    
      WRITE (25,*)'FL_SOPN END ',LUN,iret
      
	RETURN
	END
