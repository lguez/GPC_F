PROGRAM TESTGPC

  ! This program tests the GPC contributed library public functions.

  use, intrinsic:: ISO_C_BINDING

  use jumble, only: new_unit
  USE module_GPC, only: C_GPC_POLYGON, gpc_polygon_clip, gpc_free_polygon
  use shapelib, only: shpfileobject, shpobject
  use shapelib_03, only: shp_open_03, shp_read_object_03

  implicit none

  INTEGER operation
  CHARACTER(LEN=256) filnam
  type(C_GPC_POLYGON) C_subject_polygon, C_clip_polygon, C_result_polygon
  INTEGER(SELECTED_INT_KIND(4)), PARAMETER:: GPC_DIFF = 0, GPC_INT = 1, &
       GPC_XOR = 2, GPC_UNION = 3
  TYPE(shpfileobject) hshp
  TYPE(shpobject) psobject

  !-------------------------------------------------------------------------

  ! filnam = 'arrows.gpf'
  filnam = 'britain.gpf' // C_NULL_CHAR

  call shp_open_03(filnam, "rb", hshp)
  call shp_read_object_03(hshp, 0, psobject)
  C_subject_polygon%num_contours = 1
  C_subject_polygon%hole = C_NULL_PTR
  
  ! filnam = 'britain.gpf
  filnam = 'arrow.gpf' // C_NULL_CHAR
  C_clip_polygon%num_contours = 0

  print *, "Enter operation (", GPC_DIFF, "-GPC_DIFF, ", GPC_INT, &
       "-GPC_INT, ", GPC_XOR, "-GPC_XOR, ", GPC_UNION, "-GPC_UNION):"
  read *, operation
  print *, 'TESTGPC: operation = ', operation
  print *, 'TESTGPC: calling gpc_polygon_clip...'
  C_result_polygon%num_contours = 0
  CALL gpc_polygon_clip(operation, C_subject_polygon, C_clip_polygon, &
       C_result_polygon)
  IF (C_subject_polygon%num_contours /= 0) &
       CALL gpc_free_polygon(C_subject_polygon)
  IF (C_clip_polygon%num_contours /= 0) CALL gpc_free_polygon(C_clip_polygon)
  IF (C_result_polygon%num_contours /= 0) &
       CALL gpc_free_polygon(C_result_polygon)

END PROGRAM TESTGPC
