module gpc_polygon_clip_f_m

  use contour_531, only: polyline
  
  implicit none

  type polygon
     integer nparts

     type(polyline), allocatable:: part(:) ! (nparts)
     ! Each polyline must be closed.
     
     logical, allocatable:: hole(:) ! (nparts)
  end type polygon

  ENUM, BIND(C)
     ENUMERATOR GPC_DIFF, GPC_INT, GPC_XOR, GPC_UNION
  END ENUM

  integer, parameter:: GPC_OP = kind(GPC_DIFF)

  private polyline
  
contains

  subroutine gpc_polygon_clip_f(set_op, subj_pol, clip_pol, res_pol)

    ! A lot of machinery here. In particular, code is repeated to
    ! construct spc and cpc because, in Fortran 2003, the argument of
    ! c_loc cannot be an array with pointer attribute. Note that in
    ! type GPC_POLYGON, the first vertex of each ring must not be
    ! repeated at the end of the ring, contrary to the convention in
    ! type polygon.

    use, intrinsic:: ISO_C_BINDING

    INTEGER(GPC_OP), intent(in):: set_op
    ! set operation, must be one of GPC_DIFF, GPC_INT, GPC_XOR, GPC_UNION
    
    type(polygon), intent(in):: subj_pol, clip_pol ! subject and clip polygons
    type(polygon), intent(out):: res_pol ! result polygon

    ! Local:

    type, bind(c):: gpc_vertex
       real(c_double) x, y
    end type gpc_vertex

    type, bind(c):: gpc_vertex_list
       integer(c_int) num_vertices
       type(c_ptr) vertex
    end type gpc_vertex_list

    type, bind(c):: gpc_polygon
       integer(c_int) num_contours
       type(c_ptr) hole, contour
    end type gpc_polygon

    type gpc_vertex_list_f
       type(GPC_VERTEX), allocatable:: v(:)
    end type gpc_vertex_list_f

    interface
       subroutine gpc_polygon_clip(set_operation, subject_polygon, &
            clip_polygon, result_polygon) bind(c)
         import gpc_op, gpc_polygon
         implicit none
         integer(gpc_op), value, intent(in):: set_operation
         type(gpc_polygon), intent(inout):: subject_polygon, clip_polygon
         type(gpc_polygon), intent(out):: result_polygon
       end subroutine gpc_polygon_clip

       subroutine gpc_free_polygon(p) bind(c)
         import gpc_polygon
         implicit none
         type(gpc_polygon), intent(inout):: p
       end subroutine gpc_free_polygon
    end interface

    integer i, j

    type(GPC_POLYGON) spc, cpc, rpc
    ! subject, clip and result polygons, C-interoperable

    type(gpc_vertex_list_f), allocatable, target:: l_subj_f(:), l_clip_f(:)
    ! subject and clip vertex lists, not C-interoperable

    type(GPC_VERTEX_LIST), allocatable, target:: l_subj_c(:), l_clip_c(:)
    type(GPC_VERTEX_LIST), pointer:: l_res_c(:)
    ! subject, clip and result vertex lists, C-interoperable

    type(GPC_VERTEX), pointer:: v_res(:)
    integer(c_int), pointer:: hole_res(:)
    integer(c_int), allocatable, target:: hole_subj(:), hole_clip(:)

    !-------------------------------------------------------------------------

    ! Construct spc:

    allocate(l_subj_f(subj_pol%nparts), l_subj_c(subj_pol%nparts), &
         hole_subj(subj_pol%nparts))

    hole_subj = merge(1_c_int, 0_c_int, subj_pol%hole)

    do i = 1, subj_pol%nparts
       allocate(l_subj_f(i)%v(subj_pol%part(i)%n_points - 1))
       ! (do not repeat the first point)

       l_subj_f(i)%v = [(GPC_VERTEX(subj_pol%part(i)%points(1, j), &
            subj_pol%part(i)%points(2, j)), &
            j = 1, subj_pol%part(i)%n_points - 1)]
       l_subj_c(i) = GPC_VERTEX_LIST(num_vertices = size(l_subj_f(i)%v), &
            vertex = c_loc(l_subj_f(i)%v))
    end do

    spc = gpc_polygon(num_contours = size(l_subj_c), hole = c_loc(hole_subj), &
         contour = c_loc(l_subj_c))

    ! Construct cpc:

    allocate(l_clip_f(clip_pol%nparts), l_clip_c(clip_pol%nparts), &
         hole_clip(clip_pol%nparts))

    hole_clip = merge(1_c_int, 0_c_int, clip_pol%hole)

    do i = 1, clip_pol%nparts
       allocate(l_clip_f(i)%v(clip_pol%part(i)%n_points - 1))
       ! (do not repeat the first point)

       l_clip_f(i)%v = [(GPC_VERTEX(clip_pol%part(i)%points(1, j), &
            clip_pol%part(i)%points(2, j)), &
            j = 1, clip_pol%part(i)%n_points - 1)]
       l_clip_c(i) = GPC_VERTEX_LIST(num_vertices = size(l_clip_f(i)%v), &
            vertex = c_loc(l_clip_f(i)%v))
    end do

    cpc = gpc_polygon(num_contours = size(l_clip_c), hole = c_loc(hole_clip), &
         contour = c_loc(l_clip_c))

    CALL gpc_polygon_clip(set_op, spc, cpc, rpc)

    ! Construct res_pol:

    res_pol%nparts = rpc%num_contours
    allocate(res_pol%part(res_pol%nparts), res_pol%hole(res_pol%nparts))

    if (rpc%num_contours /= 0) then
       call c_f_pointer(rpc%hole, hole_res, [rpc%num_contours])
       res_pol%hole = hole_res == 1_c_int
       call c_f_pointer(rpc%contour, l_res_c, [rpc%num_contours])

       res_pol%part%n_points = l_res_c%num_vertices + 1
       ! last vertex must repeart the first

       res_pol%part%closed = .true.

       do i = 1, res_pol%nparts
          associate (n => res_pol%part(i)%n_points)
            allocate(res_pol%part(i)%points(2, n))
            call c_f_pointer(l_res_c(i)%vertex, v_res, [n - 1])

            do j = 1, n - 1
               res_pol%part(i)%points(:, j) = [v_res(j)%x, v_res(j)%y]
            end do

            res_pol%part(i)%points(:, n) = res_pol%part(i)%points(:, 1)
          end associate
       end do
    end if

    call gpc_free_polygon(rpc)

  end subroutine gpc_polygon_clip_f

END module gpc_polygon_clip_f_m
