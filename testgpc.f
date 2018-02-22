PROGRAM TESTGPC

  ! This program tests the GPC library.

  use, intrinsic:: ISO_C_BINDING

  USE GPC_f, only: GPC_VERTEX, GPC_VERTEX_LIST, GPC_POLYGON, gpc_polygon_clip, &
       GPC_OP, GPC_DIFF, GPC_INT, GPC_XOR, GPC_UNION
  use jumble, only: get_command_arg_dyn
  use shapelib, only: shpfileobject, shpobject, shpclose
  use shapelib_03, only: shp_open_03, shp_read_object_03

  implicit none

  INTEGER(GPC_OP) set_operation
  integer i, j
  type(GPC_POLYGON) subject_polygon, clip_polygon, result_polygon
  TYPE(shpfileobject) hshp
  TYPE(shpobject) psobject
  type(GPC_VERTEX), allocatable, target:: v_subject(:), v_clip(:)
  type(GPC_VERTEX), pointer:: v_result(:)
  type(GPC_VERTEX_LIST), target:: l_subject, l_clip
  type(GPC_VERTEX_LIST), pointer:: l_result(:)
  integer(c_int), pointer:: hole(:)
  character(len = :), allocatable:: filename

  !-------------------------------------------------------------------------

  call get_command_arg_dyn(1, filename)
  call shp_open_03(filename, "rb", hshp)
  call shp_read_object_03(hshp, 0, psobject)
  call shpclose(hshp)
  v_subject = [(GPC_VERTEX(psobject%padfx(i), psobject%padfy(i)), &
       i = 1, psobject%nvertices - 1)]
  l_subject = GPC_VERTEX_LIST(psobject%nvertices - 1, c_loc(v_subject))
  subject_polygon = gpc_polygon(num_contours = 1, hole = c_null_ptr, &
       contour = c_loc(l_subject))
  
  call get_command_arg_dyn(2, filename)
  call shp_open_03(filename, "rb", hshp)
  call shp_read_object_03(hshp, 0, psobject)
  call shpclose(hshp)
  v_clip = [(GPC_VERTEX(psobject%padfx(i), psobject%padfy(i)), &
       i = 1, psobject%nvertices - 1)]
  l_CLIP = GPC_VERTEX_LIST(psobject%nvertices - 1, c_loc(v_CLIP))
  clip_polygon = gpc_polygon(num_contours = 1, hole = c_null_ptr, &
       contour = c_loc(l_CLIP))

  print *, "Enter operation (", GPC_DIFF, "-GPC_DIFF, ", GPC_INT, &
       "-GPC_INT, ", GPC_XOR, "-GPC_XOR, ", GPC_UNION, "-GPC_UNION):"
  read *, set_operation
  CALL gpc_polygon_clip(set_operation, subject_polygon, clip_polygon, &
       result_polygon)
  print *, "result_polygon%num_contours = ", result_polygon%num_contours
  call c_f_pointer(result_polygon%hole, hole, [result_polygon%num_contours])
  print *, "result_polygon%hole = ", hole == 1_c_int
  call c_f_pointer(result_polygon%contour, l_result, &
       [result_polygon%num_contours])

  do i = 1, result_polygon%num_contours
     print *, "l_result(", i, ")%num_vertices = ", l_result(i)%num_vertices
     call c_f_pointer(l_result(i)%vertex, v_result, [l_result(i)%num_vertices])

     do j = 1, l_result(i)%num_vertices
        print *, "v_result(", j, ") = ", v_result(j)
     end do
  end do

END PROGRAM TESTGPC
