	FUNCTION CFL_TBOP  ( table, ctype, LUN, iret ) RESULT (output_4)
!	SUBROUTINE FL_TBOP  ( table, type, lun, iret )
C************************************************************************
C* FL_TBOP								*
C* 									*
C* This subroutine opens an existing table file.  A table file is a	*
C* sequential file that may have comment records at the beginning	*
C* of the file.  Comment records are records with an exclamation point	*
C* as the first non-blank character.  Leading comment records are 	*
C* skipped and the file is positioned for reading at the first valid 	*
C* data record.  The file is opened for READONLY access.  		*
C* 									*
C* The TABLE file name is split into the path and filename and the file	*
C* is located by searching in the following order:			*
C* 									*
C*	1. filename (local)						*
C* 	2. path/filename (TABLE as given)				*
C* 	3. $NCDESK/type/filename					*
C* 	4. $NCSITE/type/filename					*
C*	5. $GEMTBL/type/filename					*
C* 									*
C* FL_TBOP  ( TABLE, TYPE, LUN, IRET )					*
C* 									*
C* Input parameters:							*
C*	TABLE		CHAR*		Table file name 		*
C*	CTYPE		CHAR*		File name type			*
C*									*
C* Output parameters:							*
C*	LUN		INTEGER		Logical unit number 		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* D. Keiser/GSC	12/95	Copied from FL_TOPN			*
C* S. Jacobs/NCEP	 6/98	Added check for blank table name	*
C* A. Hardy/GSC		12/00   Added search paths NCDESK and NCSITE	*
C* S. Jacobs/NCEP	 5/01	Changed to call FL_TINQ			*
C************************************************************************
	USE gemprm

!	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER   output_4,lun                                                                                                                                                                                   
	INTEGER   iret                                                    

	CHARACTER*(*)	table, ctype
C
	CHARACTER	name*132
	LOGICAL		exist
C------------------------------------------------------------------------
       WRITE (25,*)'CFL_TBOP: START ',table
       
       I1 = LEN_TRIM(table)
       I2 = LEN_TRIM(ctype)
       
       WRITE (25,*)'CFL_TBOP: table = ',I1,table(:I1)
       WRITE (25,*)'CFL_TBOP: ctype  = ',I2,ctype(:I2)
	iret = 0
C
      
!	CALL FL_TINQ  ( table, type, exist, name, iret )
!
!     "table" is the input file name,
!     "name" is the output file name...
      name = table
      
       WRITE (25,*)'CFL_TBOP: 1 ',NAME
	CALL FL_SOPN  ( name, lun, iret )
      
       WRITE(25,*)'CFL_TBOP: 2 ',LUN,iret
C
C*	Skip comment records.
C
	IF  ( iret .eq. 0 )  THEN
	  CALL FL_TDAT  ( lun, iret )
        
         WRITE (25,*)'CFL_TBOP: IRET'
C
C*	    Check for errors during advance to data.
C
	  IF  ( iret .ne. 0 )  THEN
!		CALL FL_CLOS  ( lun, ier )
          CALL CFL_CLOS  ( lun, ier )
		lun = 0
	  END IF
      END IF

       WRITE (25,*)'CFL_TBOP: END - ',LUN,IRET
       
	output_4 = lun

100   CONTINUE
C*
	RETURN
	END
