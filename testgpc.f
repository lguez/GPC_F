PROGRAM TESTGPC

  ! This program tests the GPC contributed library public functions.

  use, intrinsic:: ISO_C_BINDING

  use cfl_clos_m, only: cfl_clos
  use jumble, only: new_unit
  USE module_GPC, only: C_GPC_POLYGON, C_GPC_VERTEX_LIST, gpc_read_polygon, &
       gpc_write_polygon, gpc_add_contour, gpc_polygon_clip, gpc_free_polygon, &
       gpc_free_vertex, gpc_cvlist, gpc_gvlist, gpc_gvarea
  use shapelib, only: shpfileobject, shpobject
  use shapelib_03, only: shp_open_03, shp_read_object_03

  implicit none

  INTEGER, PARAMETER :: SUBJECT=0
  INTEGER, PARAMETER :: CLIP=1
  INTEGER, PARAMETER :: IRESULT=2
  INTEGER, PARAMETER :: ALL=3

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

  INTEGER ii, numsub, which, ier, operation, readholeflag
  integer writeholeflag
  INTEGER nverts, hole
  REAL xv(80000), yv(80000)
  INTEGER fpclose, ifpread

  CHARACTER(LEN=4, KIND=C_CHAR) mode
  CHARACTER(LEN=256, KIND=C_CHAR) filnam
  CHARACTER(LEN=80) buffer
  type(C_PTR) fpopen
  type(C_GPC_POLYGON) C_subject_polygon, C_clip_polygon, C_result_polygon
  type(C_GPC_VERTEX_LIST) C_verticies

  INTEGER(SELECTED_INT_KIND(4)), PARAMETER:: GPC_DIFF = 0, GPC_INT = 1, &
       GPC_XOR = 2, GPC_UNION = 3

  real area
  INTEGER ::  G_NORMAL = 0
  INTEGER ::  G_TRUE =    1  ! TRUE 
  INTEGER ::  G_FALSE =    0  ! FALSE 
  TYPE(shpfileobject) hshp
  TYPE(shpobject) psobject

  !-------------------------------------------------------------------------

  print *, 'START TESTGPC'

  C_subject_polygon%num_contours = 0
  C_subject_polygon%hole = C_NULL_PTR
  C_clip_polygon%num_contours = 0
  C_result_polygon%num_contours = 0
  C_verticies%num_vertices = 0

  nverts = 0

  ! NOTICE - IN THIS EXAMPLE, BOTH THE SUBJECT POLYGON AND THE CLIP POLYGON ARE
  ! READ AUTOMATICLY SO YOU DO NOT HAVE TO USE THE "1 = GPC_READ_POLYGON"
  ! OPTION.

  ! filnam = 'arrows.gpf'
  filnam = 'britain.gpf' // C_NULL_CHAR
  mode = 'r'// C_NULL_CHAR

  readholeflag = 0
  which = 0
  call shp_open_03(filnam, "rb", hshp)
  call shp_read_object_03(hshp, 0, psobject)
  Call C_fopen(fpopen, filnam, mode, ier)
  CALL gpc_read_polygon(fpopen, readholeflag, C_subject_polygon)

  fpclose = fclose(fpopen)

  print *, 'TESTGPC: Subject Contours = ', C_subject_polygon%num_contours

  ! filnam = 'britain.gpf
  filnam = 'arrow.gpf' // C_NULL_CHAR
  readholeflag = 0
  which = 1
  Call C_fopen(fpopen, filnam, mode, ier)
  CALL gpc_read_polygon(fpopen, readholeflag, C_clip_polygon)

  fpclose = fclose(fpopen)

  print *, 'TESTGPC: Clip Contours = ', C_clip_polygon%num_contours

  print *, " ORIGINAL GPC PUBLIC FUNCTIONS "
  print *, " 1 = GPC_READ_POLYGON 2 = GPC_WRITE_POLYGON "
  print *, " 3 = GPC_ADD_CONTOUR 4 = GPC_POLYGON_CLIP "
  print *, " 5 = GPC_FREE_POLYGON "
  print *, " GPC PUBLIC FUNCTION ADD-ONS "
  print *, " 11 = GPC_CREATE_VERTEX_LIST "
  print *, " 12 = GPC_GET_VERTEX_LIST "
  print *, " 13 = GPC_GET_VERTEX_AREA "
  READ *, numsub

  select case (numsub)
  case (1)
     print *, "Make sure your polygon file is in the proper format:"
     print *, "<num-contours>"
     print *, "<num-vertices-in-first-contour>"
     print *, "[<first-contour-hole-flag>]"
     print *, "<vertex-list>"
     print *, "<num-vertices-in-second-contour>"
     print *, "[<second-contour-hole-flag>]"
     print *, "<vertex-list> "
     print *, "etc."
     print *, "Enter filename to read polygon from: "
     READ *, filnam
     print *, "Enter whether file format contains hole flags (", &
          G_FALSE, "-FALSE, ", G_TRUE, "-TRUE):"
     read *, readholeflag
     print *, "Enter which polygon (", SUBJECT, "-SUBJECT, ", CLIP, &
          "-CLIP):"
     read *, which

     Call C_fopen(fpopen, filnam, mode, ier)

     print *, 'TESTGPC IER = ', IER

     IF (ier == G_NORMAL) THEN

        IF (which == SUBJECT) THEN
           print *, 'TESTGPC: READ SUBJECT POLYGON'
           CALL gpc_read_polygon(fpopen, readholeflag, C_subject_polygon)

        ELSE IF (which == CLIP) THEN
           print *, 'TESTGPC READ CLIP POLYGON'
           CALL gpc_read_polygon(fpopen, readholeflag, C_clip_polygon)

        ELSE
           print *, "Invalid polygon type"
        END IF

        CALL cfl_clos(fpopen, ier)

     ELSE
        print *, "Unable to open file ", filnam
     END IF
  case (2)
     print *, "Enter filename to write polygon to: "
     read *, filnam
     print *, "Enter the write hole flag (", G_FALSE, "-FALSE, ", &
          G_TRUE, "-TRUE):"
     read *, writeholeflag
     print *, "Enter which polygon (", SUBJECT, "-SUBJECT, ", CLIP, &
          "-CLIP, ", IRESULT, "-RESULT):"
     read *, which

     print *, 'TESTGPC: attempting to write a polygon...'
     mode = 'w'// C_NULL_CHAR
     filnam=TRIM(ADJUSTL(filnam))//char(0)
     Call C_fopen(fpopen, filnam, mode, ier)

     IF (ier == G_NORMAL) THEN
        writeholeflag = 0

        IF (which == SUBJECT) THEN
           print *, 'TESTGPC: write subject'
           CALL gpc_write_polygon(fpopen, writeholeflag, C_subject_polygon)
        ELSE IF (which == CLIP) THEN
           print *, 'TESTGPC: write clip'
           CALL gpc_write_polygon(fpopen, writeholeflag, C_clip_polygon)
        ELSE IF (which == IRESULT) THEN
           print *, 'TESTGPC: write result'
           CALL gpc_write_polygon(fpopen, writeholeflag, C_result_polygon)
        ELSE
           print *, "Invalid polygon type"
        END IF

        CALL cfl_clos(fpopen, ier)

     ELSE
        print *, "Unable to open file ", filnam
     END IF
  case (3)

     IF (nverts == 0) THEN
        print *, "Must first create a vertex list (option 11)"
     ELSE
        print *, "Enter which polygon (", SUBJECT, "-SUBJECT, ", CLIP, &
             "-CLIP, ", IRESULT, "-RESULT) to add vertex list to:"
        read *, which
        print *, "Enter the hole flag (", G_TRUE, "-HOLE, ", G_FALSE, &
             "-NOT A HOLE):"
        read *, hole

        ! hole = 0

        IF (which == SUBJECT) THEN
           CALL gpc_add_contour(C_subject_polygon, C_verticies, hole)
        ELSE IF (which == CLIP) THEN
           CALL gpc_add_contour(C_clip_polygon, C_verticies, hole)
        ELSE
           print *, "Invalid polygon"
        END IF

     END IF
  case (4)
     print *, "Enter operation (", GPC_DIFF, "-GPC_DIFF, ", GPC_INT, &
          "-GPC_INT, ", GPC_XOR, "-GPC_XOR, ", GPC_UNION, "-GPC_UNION):"
     read *, operation
     print *, 'TESTGPC: operation = ', operation
     print *, 'TESTGPC: calling gpc_polygon_clip...'
     CALL gpc_polygon_clip(operation, C_subject_polygon, C_clip_polygon, &
          C_result_polygon)
  case (5)
     print *, "Enter which polygon (", SUBJECT, &
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
  case (11)
     nverts = 0

     print *, 'TESTGPC: @11 - nverts 1 = ', nverts
     if (nverts == 0) then
        WRITE(*, *) "Enter either the number of points in polygon ", &
             " (to be followed by entering the points), ", &
             " or a filename to read points from: "

        read *, filnam
        read(unit = filnam, fmt = *, iostat = ier) nverts

        print *, 'TESTGPC: @11 - nverts 2 = ', nverts, ier

        IF (ier == 0) THEN

           DO ii = 1, nverts
              READ *, xv(ii), yv(ii)
           END DO

           IF (C_verticies%num_vertices /= 0) &
                call gpc_free_vertex(C_verticies)

           CALL gpc_cvlist(nverts, xv, yv, C_verticies, ier)

        ELSE

           nverts = 0

           PRINT *, "Note that the file format is simply a list of ", &
                "coordinate pairs separated by whitespace. The number of ", &
                "points will be counted automatically. For instance, a ", &
                "file containing: 0 0 0 1 1 1 yields a vertex list of ", &
                "three points."
           call new_unit(ifpread)
           open(ifpread, file = filnam, action = "read", status = "old", &
                position = "rewind", iostat = ier)

           print *, 'TESTGPC: @11 fpread = ', ifpread

           IF (ier == G_NORMAL) THEN
              read(ifpread, fmt = *) buffer

              DO WHILE (ier == 0)
                 nverts = nverts + 1
                 READ(buffer, '(F20.15, 2x, F20.15)') xv(nverts), yv(nverts)

                 read(ifpread, fmt = *) buffer
              END DO

              print *, "EOF reached in file ", filnam, &
                   ", number of vertices = ", nverts
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
  case (12)
     CALL gpc_gvlist(C_verticies, nverts, xv, yv, ier)
     print *, "gpc_gvlist, ier = ", ier
     print *, "Number of vertices = ", nverts

     DO ii = 1, nverts
        print *, ii, " - ", xv(ii), " ", yv(ii)
     END DO
  case (13)
     call gpc_gvarea(C_verticies, area)
     print *, "Area of contour is ", area
  END select

  IF (C_subject_polygon%num_contours /= 0) &
       CALL gpc_free_polygon(C_subject_polygon)
  IF (C_clip_polygon%num_contours /= 0) CALL gpc_free_polygon(C_clip_polygon)
  IF (C_result_polygon%num_contours /= 0) &
       CALL gpc_free_polygon(C_result_polygon)

END PROGRAM TESTGPC
