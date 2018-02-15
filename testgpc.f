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
  INTEGER nverts
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
     WRITE(*, *) "Enter either the number of points in polygon ", &
          " (to be followed by entering the points), ", &
          " or a filename to read points from: "
     read *, filnam
     read(unit = filnam, fmt = *, iostat = ier) nverts

     IF (ier == 0) THEN
        DO ii = 1, nverts
           READ *, xv(ii), yv(ii)
        END DO

        IF (C_verticies%num_vertices /= 0) call gpc_free_vertex(C_verticies)
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

        IF (ier == 0) THEN
           read(ifpread, fmt = *) buffer

           DO WHILE (ier == 0)
              nverts = nverts + 1
              READ(buffer, fmt = *) xv(nverts), yv(nverts)
              read(ifpread, fmt = *) buffer
           END DO

           print *, "EOF reached in file ", filnam, &
                ", number of vertices = ", nverts
           close(ifpread)
           IF (C_verticies%num_vertices /= 0) call gpc_free_vertex(C_verticies)
           CALL gpc_cvlist(nverts, xv, yv, C_verticies, ier)
        END IF
     END IF
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
