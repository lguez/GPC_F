PROGRAM TESTGPC

  ! This program tests the GPC contributed library public functions.

  use, intrinsic:: ISO_C_BINDING

  use jumble, only: new_unit
  USE GPC_f, only: GPC_VERTEX, GPC_VERTEX_LIST, GPC_POLYGON, gpc_polygon_clip, &
       gpc_free_polygon
  use shapelib, only: shpfileobject, shpobject, shpclose
  use shapelib_03, only: shp_open_03, shp_read_object_03

  implicit none

  INTEGER operation, i
  type(GPC_POLYGON) subject_polygon, clip_polygon, result_polygon
  INTEGER(SELECTED_INT_KIND(4)), PARAMETER:: GPC_DIFF = 0, GPC_INT = 1, &
       GPC_XOR = 2, GPC_UNION = 3
  TYPE(shpfileobject) hshp
  TYPE(shpobject) psobject
  type(GPC_VERTEX), allocatable, target:: v(:)
  type(GPC_VERTEX_LIST), target:: l

  !-------------------------------------------------------------------------

  call shp_open_03('britain', "rb", hshp)
  call shp_read_object_03(hshp, 0, psobject)
  call shpclose(hshp)
  v = [(GPC_VERTEX(psobject%padfx(i), psobject%padfy(i)), &
       i = 1, psobject%nvertices)]
  l = GPC_VERTEX_LIST(psobject%nvertices, c_loc(v))
  subject_polygon%num_contours = 1
  subject_polygon%hole = C_NULL_PTR
  
  clip_polygon%num_contours = 0

  print *, "Enter operation (", GPC_DIFF, "-GPC_DIFF, ", GPC_INT, &
       "-GPC_INT, ", GPC_XOR, "-GPC_XOR, ", GPC_UNION, "-GPC_UNION):"
  read *, operation
  print *, 'TESTGPC: operation = ', operation
  print *, 'TESTGPC: calling gpc_polygon_clip...'
  result_polygon%num_contours = 0
  CALL gpc_polygon_clip(operation, subject_polygon, clip_polygon, &
       result_polygon)
  IF (subject_polygon%num_contours /= 0) &
       CALL gpc_free_polygon(subject_polygon)
  IF (clip_polygon%num_contours /= 0) CALL gpc_free_polygon(clip_polygon)
  IF (result_polygon%num_contours /= 0) CALL gpc_free_polygon(result_polygon)

END PROGRAM TESTGPC
