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

  INTEGER which, ier, operation, readholeflag
  INTEGER nverts
  INTEGER fpclose

  CHARACTER(LEN=4, KIND=C_CHAR) mode
  CHARACTER(LEN=256, KIND=C_CHAR) filnam
  type(C_PTR) fpopen
  type(C_GPC_POLYGON) C_subject_polygon, C_clip_polygon, C_result_polygon
  type(C_GPC_VERTEX_LIST) C_verticies

  INTEGER(SELECTED_INT_KIND(4)), PARAMETER:: GPC_DIFF = 0, GPC_INT = 1, &
       GPC_XOR = 2, GPC_UNION = 3

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
  print *, "Enter operation (", GPC_DIFF, "-GPC_DIFF, ", GPC_INT, &
       "-GPC_INT, ", GPC_XOR, "-GPC_XOR, ", GPC_UNION, "-GPC_UNION):"
  read *, operation
  print *, 'TESTGPC: operation = ', operation
  print *, 'TESTGPC: calling gpc_polygon_clip...'
  CALL gpc_polygon_clip(operation, C_subject_polygon, C_clip_polygon, &
       C_result_polygon)
  IF (C_subject_polygon%num_contours /= 0) &
       CALL gpc_free_polygon(C_subject_polygon)
  IF (C_clip_polygon%num_contours /= 0) CALL gpc_free_polygon(C_clip_polygon)
  IF (C_result_polygon%num_contours /= 0) &
       CALL gpc_free_polygon(C_result_polygon)

END PROGRAM TESTGPC
