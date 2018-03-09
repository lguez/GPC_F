PROGRAM TEST_GPC

  ! This program tests the GPC library.

  use, intrinsic:: ISO_C_BINDING

  USE GPC_f, only: GPC_VERTEX, GPC_VERTEX_LIST, GPC_POLYGON, gpc_polygon_clip, &
       GPC_OP, GPC_DIFF, GPC_INT, GPC_XOR, GPC_UNION
  use jumble, only: get_command_arg_dyn
  use nr_util, only: assert
  use shapelib, only: shpfileobject, shpobject, shpclose, shpt_polygon, &
       shpdestroyobject
  use shapelib_03, only: shp_open_03, shp_read_object_03, shp_create_03, &
       shp_append_object_03

  implicit none

  INTEGER(GPC_OP) set_operation
  integer i, j, ishape
  type(GPC_POLYGON) subject_polygon, clip_polygon, result_polygon
  TYPE(shpfileobject) hshp
  TYPE(shpobject) psobject
  type gpc_vertex_list_f
     type(GPC_VERTEX), allocatable:: v(:)
  end type gpc_vertex_list_f
  type(gpc_vertex_list_f), allocatable, target:: l_subject_f(:), l_clip_f(:)
  type(GPC_VERTEX), pointer:: v_result(:)
  type(GPC_VERTEX_LIST), allocatable, target:: l_subject(:), l_clip(:)
  type(GPC_VERTEX_LIST), pointer:: l_result(:)
  integer(c_int), pointer:: hole(:)
  character(len = :), allocatable:: filename
  integer, allocatable:: pan_part_start(:) ! (result_polygon%num_contours)
  REAL(c_double), allocatable:: padf(:, :) ! (2, 0:)


  !-------------------------------------------------------------------------

  call get_command_arg_dyn(1, filename, &
       "Required arguments: shapefile shapefile")
  call shp_open_03(filename, "rb", hshp)
  call shp_read_object_03(hshp, 0, psobject)
  call shpclose(hshp)
  call assert(psobject%nparts >= 1, "psobject%nparts >= 1")
  call assert(psobject%panpartstart(1) == 0, "psobject%panpartstart(1)")
  allocate(l_subject_f(psobject%nparts), l_subject(psobject%nparts))

  do i = 1, psobject%nparts - 1
     allocate(l_subject_f(i)%v(psobject%panpartstart(i + 1) - 1 &
          - psobject%panpartstart(i)))
     l_subject_f(i)%v = [(GPC_VERTEX(psobject%padfx(j), psobject%padfy(j)), &
          j = psobject%panpartstart(i) + 1, psobject%panpartstart(i + 1) - 1)]
     l_subject(i) = GPC_VERTEX_LIST(num_vertices = size(l_subject_f(i)%v), &
          vertex = c_loc(l_subject_f(i)%v))
  end do

  allocate(l_subject_f(psobject%nparts)%v(psobject%nvertices - 1 &
       - psobject%panpartstart(psobject%nparts)))
  l_subject_f(psobject%nparts)%v = [(GPC_VERTEX(psobject%padfx(j), &
       psobject%padfy(j)), &
       j = psobject%panpartstart(psobject%nparts) + 1, psobject%nvertices - 1)]
  l_subject(psobject%nparts) &
       = GPC_VERTEX_LIST(num_vertices = size(l_subject_f(psobject%nparts)%v), &
       vertex = c_loc(l_subject_f(psobject%nparts)%v))
  call shpdestroyobject(psobject)
  subject_polygon = gpc_polygon(num_contours = size(l_subject), &
       hole = c_null_ptr, contour = c_loc(l_subject))

  call get_command_arg_dyn(2, filename, &
       "Required arguments: shapefile shapefile")
  call shp_open_03(filename, "rb", hshp)
  call shp_read_object_03(hshp, 0, psobject)
  call shpclose(hshp)
  call assert(psobject%nparts >= 1, "psobject%nparts >= 1")
  call assert(psobject%panpartstart(1) == 0, "psobject%panpartstart(1)")
  allocate(l_clip_f(psobject%nparts), l_clip(psobject%nparts))

  do i = 1, psobject%nparts - 1
     allocate(l_clip_f(i)%v(psobject%panpartstart(i + 1) - 1 &
          - psobject%panpartstart(i)))
     l_clip_f(i)%v = [(GPC_VERTEX(psobject%padfx(j), psobject%padfy(j)), &
          j = psobject%panpartstart(i) + 1, psobject%panpartstart(i + 1) - 1)]
     l_clip(i) = GPC_VERTEX_LIST(num_vertices = size(l_clip_f(i)%v), &
          vertex = c_loc(l_clip_f(i)%v))
  end do

  allocate(l_clip_f(psobject%nparts)%v(psobject%nvertices - 1 &
       - psobject%panpartstart(psobject%nparts)))
  l_clip_f(psobject%nparts)%v = [(GPC_VERTEX(psobject%padfx(j), &
       psobject%padfy(j)), &
       j = psobject%panpartstart(psobject%nparts) + 1, psobject%nvertices - 1)]
  l_clip(psobject%nparts) &
       = GPC_VERTEX_LIST(num_vertices = size(l_clip_f(psobject%nparts)%v), &
       vertex = c_loc(l_clip_f(psobject%nparts)%v))
  call shpdestroyobject(psobject)
  clip_polygon = gpc_polygon(num_contours = size(l_clip), hole = c_null_ptr, &
       contour = c_loc(l_clip))

  print *, "Enter operation (", GPC_DIFF, "difference,", GPC_INT, &
       "intersection,", GPC_XOR, "exclusive or,", GPC_UNION, "union):"
  read *, set_operation
  CALL gpc_polygon_clip(set_operation, subject_polygon, clip_polygon, &
       result_polygon)
  print *, "result_polygon%num_contours = ", result_polygon%num_contours

  if (result_polygon%num_contours /= 0) then
     allocate(pan_part_start(result_polygon%num_contours))
     pan_part_start(1) = 0
     call c_f_pointer(result_polygon%hole, hole, [result_polygon%num_contours])
     print *, "result_polygon%hole = ", hole == 1_c_int
     call c_f_pointer(result_polygon%contour, l_result, &
          [result_polygon%num_contours])
     print *, "l_result%num_vertices = ", l_result%num_vertices

     allocate(padf(2, &
          0:sum(l_result%num_vertices) + result_polygon%num_contours - 1))
     ! (one repeated vertex for each ring)

     call shp_create_03("test_GPC", shpt_polygon, hshp)

     do i = 2, result_polygon%num_contours
        pan_part_start(i) = pan_part_start(i - 1) &
             + l_result(i - 1)%num_vertices + 1
        ! (last vertex of each ring repeats the first vertex)
     end do

     do i = 1, result_polygon%num_contours
        call c_f_pointer(l_result(i)%vertex, v_result, &
             [l_result(i)%num_vertices])

        do j = 1, l_result(i)%num_vertices
           padf(:, pan_part_start(i) + j - 1) = [v_result(j)%x, v_result(j)%y]
        end do

        padf(:, pan_part_start(i) + l_result(i)%num_vertices) &
             = padf(:, pan_part_start(i))
     end do

     call shp_append_object_03(ishape, hshp, shpt_polygon, padf, pan_part_start)
     call shpclose(hshp)
  end if

END PROGRAM TEST_GPC
