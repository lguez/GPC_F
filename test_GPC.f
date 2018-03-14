PROGRAM TEST_GPC

  ! This program tests the GPC library.

  use, intrinsic:: ISO_C_BINDING

  USE GPC_f, only: GPC_OP, GPC_DIFF, GPC_INT, GPC_XOR, GPC_UNION
  use gpc_polygon_clip_f_m, only: POLYGON, gpc_polygon_clip_f
  use jumble, only: get_command_arg_dyn
  use nr_util, only: assert
  use shapelib, only: shpfileobject, shpobject, shpclose, shpt_polygon, &
       shpdestroyobject
  use shapelib_03, only: shp_open_03, shp_read_object_03, shp_create_03, &
       shp_append_object_03
  use shpobj2pol_m, only: shpobj2pol

  implicit none

  INTEGER(GPC_OP) set_op
  integer i, ishape
  type(POLYGON) subject_pol, clip_pol, result_pol
  TYPE(shpfileobject) hshp
  TYPE(shpobject) psobject
  character(len = :), allocatable:: filename
  integer, allocatable:: pan_part_start(:) ! (result_pol%nparts)
  REAL(c_double), allocatable:: padf(:, :) ! (2, 0:)

  !-------------------------------------------------------------------------

  call get_command_arg_dyn(1, filename, &
       "Required arguments: shapefile shapefile")
  call shp_open_03(filename, "rb", hshp)
  call shp_read_object_03(hshp, 0, psobject)
  call shpclose(hshp)
  call assert(psobject%nparts >= 1, "psobject%nparts >= 1")
  call assert(psobject%panpartstart(1) == 0, "psobject%panpartstart(1)")
  subject_pol = shpobj2pol(psobject)
  call shpdestroyobject(psobject)

  call get_command_arg_dyn(2, filename, &
       "Required arguments: shapefile shapefile")
  call shp_open_03(filename, "rb", hshp)
  call shp_read_object_03(hshp, 0, psobject)
  call shpclose(hshp)
  call assert(psobject%nparts >= 1, "psobject%nparts >= 1")
  call assert(psobject%panpartstart(1) == 0, "psobject%panpartstart(1)")
  clip_pol = shpobj2pol(psobject)
  call shpdestroyobject(psobject)

  print *, "Enter operation (", GPC_DIFF, "difference,", GPC_INT, &
       "intersection,", GPC_XOR, "exclusive or,", GPC_UNION, "union):"
  read *, set_op
  CALL gpc_polygon_clip_f(set_op, subject_pol, clip_pol, result_pol)
  print *, "result_pol%nparts = ", result_pol%nparts
  print *, "result_pol%hole = ", result_pol%hole
  print *, "result_pol%part%n_points = ", result_pol%part%n_points

  if (result_pol%nparts /= 0) then
     allocate(pan_part_start(result_pol%nparts))
     pan_part_start(1) = 0
     allocate(padf(2, 0:sum(result_pol%part%n_points) - 1))
     call shp_create_03("test_GPC", shpt_polygon, hshp)

     do i = 2, result_pol%nparts
        pan_part_start(i) = pan_part_start(i - 1) &
             + result_pol%part(i - 1)%n_points
     end do

     do i = 1, result_pol%nparts
        padf(:, pan_part_start(i):pan_part_start(i) &
             + result_pol%part(i)%n_points - 1) = result_pol%part(i)%points
     end do

     call shp_append_object_03(ishape, hshp, shpt_polygon, padf, pan_part_start)
     call shpclose(hshp)
  end if

END PROGRAM TEST_GPC
