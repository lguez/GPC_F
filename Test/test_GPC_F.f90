PROGRAM TEST_GPC_f

  ! This program tests the GPC_F library.

  use, intrinsic:: ISO_C_BINDING

  USE GPC_f, only: GPC_OP, GPC_DIFF, GPC_INT, GPC_XOR, GPC_UNION, POLYGON, &
       gpc_polygon_clip_f, shpobj2pol
  use jumble, only: get_command_arg_dyn, assert
  use shapelib, only: shpfileobject, shpobject, shpclose, shpt_polygon, &
       shpdestroyobject
  use shapelib_03, only: shp_open_03, shp_read_object_03, shp_create_03, &
       shp_append_object_03

  implicit none

  INTEGER(GPC_OP) set_op
  integer i, ishape
  type(POLYGON) subj_pol, clip_pol, res_pol
  TYPE(shpfileobject) hshp
  TYPE(shpobject) psobject
  character(len = :), allocatable:: filename
  integer, allocatable:: pan_part_start(:) ! (res_pol%nparts)
  REAL(c_double), allocatable:: padf(:, :) ! (2, 0:)

  !-------------------------------------------------------------------------

  call get_command_arg_dyn(1, filename, &
       "Required arguments: shapefile shapefile")
  call shp_open_03(hshp, filename, "rb")
  call shp_read_object_03(hshp, 0, psobject)
  call shpclose(hshp)
  call assert(psobject%nparts >= 1, "psobject%nparts >= 1")
  call assert(psobject%panpartstart(1) == 0, "psobject%panpartstart(1)")
  subj_pol = shpobj2pol(psobject)
  call shpdestroyobject(psobject)

  call get_command_arg_dyn(2, filename, &
       "Required arguments: shapefile shapefile")
  call shp_open_03(hshp, filename, "rb")
  call shp_read_object_03(hshp, 0, psobject)
  call shpclose(hshp)
  call assert(psobject%nparts >= 1, "psobject%nparts >= 1")
  call assert(psobject%panpartstart(1) == 0, "psobject%panpartstart(1)")
  clip_pol = shpobj2pol(psobject)
  call shpdestroyobject(psobject)

  print *, "Enter operation (", GPC_DIFF, "difference,", GPC_INT, &
       "intersection,", GPC_XOR, "exclusive or,", GPC_UNION, "union):"
  read *, set_op
  CALL gpc_polygon_clip_f(set_op, subj_pol, clip_pol, res_pol)
  print *, "res_pol%nparts = ", res_pol%nparts
  print *, "res_pol%hole = ", res_pol%hole
  print *, "res_pol%part%n_points = ", res_pol%part%n_points

  if (res_pol%nparts /= 0) then
     allocate(pan_part_start(res_pol%nparts))
     pan_part_start(1) = 0
     allocate(padf(2, 0:sum(res_pol%part%n_points) - 1))
     call shp_create_03("test_GPC", shpt_polygon, hshp)

     do i = 2, res_pol%nparts
        pan_part_start(i) = pan_part_start(i - 1) + res_pol%part(i - 1)%n_points
     end do

     do i = 1, res_pol%nparts
        padf(:, pan_part_start(i):pan_part_start(i) &
             + res_pol%part(i)%n_points - 1) = res_pol%part(i)%points
     end do

     call shp_append_object_03(ishape, hshp, shpt_polygon, padf, pan_part_start)
     call shpclose(hshp)
  end if

END PROGRAM TEST_GPC_F
