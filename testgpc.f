PROGRAM TESTGPC

  ! This program tests the GPC contributed library public functions.

  use, intrinsic:: ISO_C_BINDING

  USE TESTGPC_1, only: F_GPC_POLYGON, F_GPC_VERTEX_LIST, F_gpc_op
  USE module_GPC, only: C_GPC_POLYGON, C_GPC_VERTEX_LIST, C_GPC_VERTEX

  implicit none

  interface
     subroutine C_fopen(ifp, filename, mode, ier) bind(C, name='C_fopen')
       import
       implicit none
       type(C_PTR) ifp
       character(KIND=C_CHAR), intent(IN):: filename(*)
       character(KIND=C_CHAR), intent(IN):: mode(*)
       integer(C_int), intent(inout):: ier
     end subroutine C_fopen

     function fclose(stream) bind(C, name='fclose')
       import
       implicit none
       integer(C_INT) fclose
       type(C_PTR), value:: stream
     end function fclose
  end interface

  INTEGER ii, cont, numsub, which, ier, operation, readholeflag
  integer writeholeflag
  INTEGER nverts, pagflg, hole
  REAL xv(80000), yv(80000)
  INTEGER fpclose, ifpread

  CHARACTER(LEN=8, KIND=C_CHAR) select
  CHARACTER(LEN=4, KIND=C_CHAR) mode
  CHARACTER(LEN=256, KIND=C_CHAR) filnam
  CHARACTER(LEN=80) buffer
  CHARACTER(LEN=8) errgrp
  type(C_PTR) fpopen
  type(C_GPC_POLYGON) C_subject_polygon, C_clip_polygon, C_result_polygon
  type(C_GPC_VERTEX_LIST) C_verticies

  INTEGER(SELECTED_INT_KIND(4)), PARAMETER:: GPC_DIFF = 0, GPC_INT = 1, &
       GPC_XOR = 2, GPC_UNION = 3

  TYPE(F_GPC_POLYGON) subject_polygon, clip_polygon, result_polygon
  TYPE(F_GPC_VERTEX_LIST) contour

  INTEGER IFILENUM
  LOGICAL LFILENUM(200)
  real all, area, CLIP, G_FALSE, G_NORMAL, G_TRUE, subject
  integer i, iresult, iret
  COMMON /FILEINFO/IFILENUM, LFILENUM

  LOGICAL DEBUG
  COMMON /PDEBUG/ DEBUG

  !-------------------------------------------------------------------------
  
  OPEN(25, FILE='TESTGPC.TXT', STATUS='UNKNOWN')
  WRITE(25, *)'START TESTGPC'

  ! FLAG TO PRINT DEBUG MESSAGES IN WINGRIDDS.LOG

  DEBUG = .TRUE. ! UNCOMMENT IF YOU WANT DEBUG MESSAGES
  !DEBUG = .FALSE. ! UNCOMMENT IF YOU DO NOT WANT DEBUG MESSAGES

  ! FALSIFY THE FILE POINTER ARRAY TO SHOW ALL FILES CLOSED & AVAILABLE

  DO I=100, 200
     LFILENUM(I) = .FALSE.
  ENDDO

  C_subject_polygon%num_contours = 0
  C_subject_polygon%hole = C_NULL_PTR
  C_clip_polygon%num_contours = 0
  C_result_polygon%num_contours = 0
  C_verticies%num_vertices = 0

  subject_polygon%num_contours = 0
  subject_polygon%hole = 0
  clip_polygon%num_contours = 0
  result_polygon%num_contours = 0
  contour%num_vertices = 0

  nverts = 0

  ! NOTICE - IN THIS EXAMPLE, BOTH THE SUBJECT POLYGON AND THE CLIP POLYGON ARE
  ! READ AUTOMATICLY SO YOU DO NOT HAVE TO USE THE "1 = GPC_READ_POLYGON"
  ! OPTION.

  ! filnam = 'arrows.gpf'
  filnam = 'britain.gpf' // C_NULL_CHAR
  mode = 'r'// C_NULL_CHAR

  readholeflag = 0
  which = 0
  Call C_fopen(fpopen, filnam, mode, ier)
  CALL gpc_read_polygon(fpopen, readholeflag, C_subject_polygon)

  fpclose = fclose(fpopen)

  WRITE(25, *)'TESTGPC: Subject Contours = ', C_subject_polygon%num_contours

  ! filnam = 'britain.gpf
  filnam = 'arrow.gpf' // C_NULL_CHAR
  readholeflag = 0
  which = 1
  Call C_fopen(fpopen, filnam, mode, ier)
  CALL gpc_read_polygon(fpopen, readholeflag, C_clip_polygon)

  fpclose = fclose(fpopen)

  WRITE(25, *)'TESTGPC: Clip Contours = ', C_clip_polygon%num_contours

  DO WHILE (cont == G_FALSE)
     numsub = 0
     WRITE(*, 901) ""
     WRITE(*, 902) " ORIGINAL GPC PUBLIC FUNCTIONS "
     WRITE(*, 902) " 1 = GPC_READ_POLYGON 2 = GPC_WRITE_POLYGON "
     WRITE(*, 902) " 3 = GPC_ADD_CONTOUR 4 = GPC_POLYGON_CLIP "
     WRITE(*, 902) " 5 = GPC_FREE_POLYGON "
     WRITE(*, 902) " GPC PUBLIC FUNCTION ADD-ONS "
     WRITE(*, 902) " 11 = GPC_CREATE_VERTEX_LIST "
     WRITE(*, 902) " 12 = GPC_GET_VERTEX_LIST "
     WRITE(*, 902) " 13 = GPC_GET_VERTEX_AREA "
     WRITE(*, 902) " HELP "
     WRITE(*, 902) " 99 = HELP on INPUT FILE FORMATS "
     WRITE(*, 902) ""
     WRITE(*, 902) "Select a subroutine number or type EXIT: "
     READ *, select

     SELECT CASE (select(1:1))

     CASE ('e', 'E')
        cont = G_TRUE

     CASE DEFAULT
        read(unit = select, fmt = *) numsub

        write(25, *)'****numsub = ', numsub

     END SELECT

     IF (numsub == 1) THEN
        WRITE(*, 902) "Make sure your polygon file is in the proper format:"
        WRITE(*, 902) "<num-contours>"
        WRITE(*, 902) "<num-vertices-in-first-contour>"
        WRITE(*, 902) "[<first-contour-hole-flag>]"
        WRITE(*, 902) "<vertex-list>"
        WRITE(*, 902) "<num-vertices-in-second-contour>"
        WRITE(*, 902) "[<second-contour-hole-flag>]"
        WRITE(*, 902) "<vertex-list> "
        WRITE(*, 902) "etc."
        WRITE(*, 902) "Enter filename to read polygon from: "
        READ(*, 902) filnam
        WRITE(*, 903) "Enter whether file format contains hole flags (", &
             G_FALSE, "-FALSE, ", G_TRUE, "-TRUE):"
        read *, readholeflag
        WRITE(*, 903) "Enter which polygon (", SUBJECT, "-SUBJECT, ", CLIP, &
             "-CLIP):"
        read *, which

        Call C_fopen(fpopen, filnam, mode, ier)

        WRITE(25, *)'TESTGPC IER = ', IER, IFILENUM

        IF (ier == G_NORMAL) THEN

           IF (which == SUBJECT) THEN
              WRITE(25, *)'TESTGPC: READ SUBJECT POLYGON'
              CALL gpc_read_polygon(fpopen, readholeflag, C_subject_polygon)

           ELSE IF (which == CLIP) THEN
              WRITE(25, *)'TESTGPC READ CLIP POLYGON'
              CALL gpc_read_polygon(fpopen, readholeflag, C_clip_polygon)

           ELSE
              WRITE(*, 902) "Invalid polygon type"
           END IF

           CALL cfl_clos(fpopen, ier)

        ELSE
           WRITE(*, 902) "Unable to open file ", filnam
        END IF

     END IF

     IF (numsub == 2) THEN
        WRITE(*, 902) "Enter filename to write polygon to: "
        READ(*, 902) filnam
        WRITE(*, 903) "Enter the write hole flag (", G_FALSE, "-FALSE, ", &
             G_TRUE, "-TRUE):"
        read *, writeholeflag
        WRITE(*, 905) "Enter which polygon (", SUBJECT, "-SUBJECT, ", CLIP, &
             "-CLIP, ", IRESULT, "-RESULT):"
        read *, which

        WRITE(25, *)'TESTGPC: attempting to write a polygon...'
        mode = 'w'// C_NULL_CHAR
        filnam=TRIM(ADJUSTL(filnam))//char(0)
        Call C_fopen(fpopen, filnam, mode, ier)

        IF (ier == G_NORMAL) THEN
           writeholeflag = 0

           IF (which == SUBJECT) THEN
              WRITE(25, *)'TESTGPC: write subject'
              CALL gpc_write_polygon(fpopen, writeholeflag, C_subject_polygon)
           ELSE IF (which == CLIP) THEN
              WRITE(25, *)'TESTGPC: write clip'
              CALL gpc_write_polygon(fpopen, writeholeflag, C_clip_polygon)
           ELSE IF (which == IRESULT) THEN
              WRITE(25, *)'TESTGPC: write result'
              CALL gpc_write_polygon(fpopen, writeholeflag, C_result_polygon)
           ELSE
              WRITE(*, 902) "Invalid polygon type"
           END IF

           CALL cfl_clos(fpopen, ier)

        ELSE
           WRITE(*, 902) "Unable to open file ", filnam
        END IF

     END IF

     IF (numsub == 3) THEN

        IF (nverts == 0) THEN
           WRITE(*, 902) "Must first create a vertex list (option 11)"
        ELSE
           WRITE(*, 905) "Enter which polygon (", SUBJECT, "-SUBJECT, ", CLIP, &
                "-CLIP, ", IRESULT, "-RESULT) to add vertex list to:"
           read *, which
           WRITE(*, 903) "Enter the hole flag (", G_TRUE, "-HOLE, ", G_FALSE, &
                "-NOT A HOLE):"
           read *, hole

           ! hole = 0

           IF (which == SUBJECT) THEN
              CALL gpc_add_contour(C_subject_polygon, C_verticies, hole)
           ELSE IF (which == CLIP) THEN
              CALL gpc_add_contour(C_clip_polygon, C_verticies, hole)
           ELSE
              WRITE(*, 902) "Invalid polygon"
           END IF

        END IF

     END IF

     IF (numsub == 4) THEN
        WRITE(*, 906) "Enter operation (", GPC_DIFF, "-GPC_DIFF, ", GPC_INT, &
             "-GPC_INT, ", GPC_XOR, "-GPC_XOR, ", GPC_UNION, "-GPC_UNION):"
        read *, operation
        WRITE(25, *)'TESTGPC: operation = ', operation
        WRITE(25, *)'TESTGPC: calling gpc_polygon_clip...'
        CALL gpc_polygon_clip(operation, C_subject_polygon, C_clip_polygon, &
             C_result_polygon)
     END IF

     IF (numsub == 5) THEN
        WRITE(*, 907, ADVANCE='NO') "Enter which polygon (", SUBJECT, &
             "-SUBJECT, ", CLIP, "-CLIP, ", IRESULT, "-RESULT, ", ALL, &
             "-ALL) ", "to free contours:", " "
        read *, which

        IF (which == SUBJECT .OR. which == ALL) THEN
           IF (C_subject_polygon%num_contours /= 0) &
                CALL gpc_free_polygon(C_subject_polygon)
        ELSE IF (which == CLIP .OR. which == ALL) THEN
           IF (C_clip_polygon%num_contours /= 0) &
                CALL gpc_free_polygon(C_clip_polygon)
        ELSE IF (which == IRESULT .OR. which == ALL) THEN
           IF (C_result_polygon%num_contours /= 0) &
                CALL gpc_free_polygon(C_result_polygon)
        END IF

     END IF

     IF (numsub == 11) THEN
        nverts = 0

        WRITE(25, *)'TESTGPC: @11 - nverts 1 = ', nverts
        if (nverts == 0) then
           WRITE(*, *) "Enter either the number of points in polygon ", &
                " (to be followed by entering the points), ", &
                " or a filename to read points from: "

           READ(*, 902) filnam
           CALL st_numb(filnam, nverts, ier)

           WRITE(25, *)'TESTGPC: @11 - nverts 2 = ', nverts, ier

           IF (ier == 0) THEN

              DO ii = 1, nverts
                 READ(*, 908) xv(ii), yv(ii)
              END DO

              IF (C_verticies%num_vertices /= 0) &
                   call gpc_free_vertex(C_verticies)

              CALL gpc_cvlist(nverts, xv, yv, C_verticies, ier)

           ELSE

              nverts = 0

              WRITE(*, 909) "Note that the file format is simply a list of ", &
                   "coordinate pairs separated by whitespace. The number of ", &
                   "points will be counted automatically. For instance, a ", &
                   "file containing: 0 0 0 1 1 1 yields a vertex list of ", &
                   "three points."

              WRITE(25, 909) "Note that the file format is simply a list of ", &
                   "coordinate pairs separated by whitespace. The number of ", &
                   "points will be counted automatically. For instance, a ", &
                   "file containing: 0 0 0 1 1 1 yields a vertex list of ", &
                   "three points."

              call FL_SOPN(filnam, ifpread, ier)

              WRITE(25, *)'TESTGPC: @11 fpread = ', ifpread

              IF (ier == G_NORMAL) THEN
                 CALL cfl_trln(ifpread, 80, buffer, ier)

                 DO WHILE (ier == 0)
                    nverts = nverts + 1
                    READ(buffer, '(F20.15, 2x, F20.15)') xv(nverts), yv(nverts)

                    CALL cfl_trln(ifpread, 80, buffer, ier)
                 END DO

                 WRITE(*, 910) "EOF reached in file ", filnam, &
                      ", number of vertices = ", nverts
                 ! CALL cfl_clos(ifpread, ier)
                 close(ifpread)

                 IF (C_verticies%num_vertices /= 0) &
                      call gpc_free_vertex(C_verticies)

                 CALL gpc_cvlist(nverts, xv, yv, C_verticies, ier)

              END IF

           END IF

        ELSE

           IF (C_verticies%num_vertices /= 0) call gpc_free_vertex(C_verticies)

           CALL gpc_cvlist(nverts, xv, yv, C_verticies, ier)

        ENDIF

     END IF

     IF (numsub == 12) THEN
        CALL gpc_gvlist(C_verticies, nverts, xv, yv, ier)
        WRITE(*, 911) "gpc_gvlist, ier = ", ier
        WRITE(*, 911) "Number of vertices = ", nverts

        DO ii = 1, nverts
           WRITE(*, 912) ii, " - ", xv(ii), " ", yv(ii)
        END DO

     END IF

     IF (numsub == 13) THEN
        call gpc_gvarea(C_verticies, area)
        WRITE(*, 913) "Area of contour is ", area
     END IF

     IF (numsub == 99) THEN
        pagflg = G_FALSE
        errgrp = "TESTGPC"
        ! CALL ip_help(errgrp, pagflg, ier, LEN_TRIM(errgrp))
     END IF

  END DO

  IF (C_subject_polygon%num_contours /= 0) &
       CALL gpc_free_polygon(C_subject_polygon)
  IF (C_clip_polygon%num_contours /= 0) CALL gpc_free_polygon(C_clip_polygon)
  IF (C_result_polygon%num_contours /= 0) &
       CALL gpc_free_polygon(C_result_polygon)
  IF (contour%num_vertices /= 0) DEALLOCATE(contour%vertex)

901 FORMAT (/A)
902 FORMAT (99A)
903 FORMAT (A, I0, A, I0, A)
905 FORMAT (A, I0, A, I0, A, I0, A)
906 FORMAT (A, I0, A, I0, A, I0, A, I0, A)
907 FORMAT (A, I0, A, I0, A, I0, A, I0, A, /)
908 FORMAT (F0.6, A, F0.6)
909 FORMAT (A/A/A/A/A/A/A)
910 FORMAT (A, A, A, I0)
911 FORMAT (A, I0)
912 FORMAT (I0, A, F0.6, A, F0.6)
913 FORMAT (A, F0.6)

END PROGRAM TESTGPC
