module cfl_clos_m

  IMPLICIT NONE

contains

  SUBROUTINE cfl_clos(fptr, iret)

    ! This function closes the specified file. Closing a file that has
    ! been opened as a temporary file will cause the file to be
    ! removed.

    USE MSVCRT, only: fclose
    use, intrinsic:: ISO_C_BINDING

    ! Input parameters: 
    ! fptr FILE File pointer 

    ! Output parameters: 
    ! iret int Return code 
    ! -6 = No file has been opened 

    ! - - - arg types - - -
    type(C_PTR) :: fptr
    INTEGER :: iret
    ! - - - local declarations - - -
    INTEGER ifptr, fpclose

    ! - - - begin - - -
    iret = 0
    ifptr = transfer(fptr, 0_C_INTPTR_T)

    IF (ifptr == 0) THEN
       iret = -6
       GOTO 9999
    END IF

    fpclose = fclose(fptr)

9999 CONTINUE

  END SUBROUTINE cfl_clos

end module cfl_clos_m
