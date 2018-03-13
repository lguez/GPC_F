module gpc_polygon_clip_f_m

  use contour_531, only: polyline
  
  implicit none

  type polygon
     integer nparts

     type(polyline), allocatable:: part(:) ! (nparts)
     ! Each polyline must be closed.
     
     logical, allocatable:: hole(:) ! (nparts)
  end type polygon

  private polyline
  
contains

  subroutine gpc_polygon_clip_f(set_op, subject_pol, clip_pol, result_pol)

    use, intrinsic:: ISO_C_BINDING

    USE GPC_f, only: GPC_VERTEX, GPC_VERTEX_LIST, GPC_POLYGON, &
         gpc_polygon_clip, GPC_OP

    INTEGER(GPC_OP), intent(in):: set_op
    type(polygon), intent(in):: subject_pol, clip_pol
    type(polygon), intent(out):: result_pol

    ! Local:

    integer i, j
    type(GPC_POLYGON) subj_p, clip_p, res_p

    type gpc_vertex_list_f
       type(GPC_VERTEX), allocatable:: v(:)
    end type gpc_vertex_list_f

    type(gpc_vertex_list_f), allocatable, target:: l_subj_f(:), l_clip_f(:)
    type(GPC_VERTEX), pointer:: v_res(:)
    type(GPC_VERTEX_LIST), allocatable, target:: l_subj(:), l_clip(:)
    type(GPC_VERTEX_LIST), pointer:: l_res(:)
    integer(c_int), pointer:: hole_res(:)
    integer(c_int), allocatable, target:: hole_subj(:), hole_clip(:)

    !-------------------------------------------------------------------------

    ! Construct subj_p:

    allocate(l_subj_f(subject_pol%nparts), l_subj(subject_pol%nparts), &
         hole_subj(subject_pol%nparts))

    hole_subj = merge(1_c_int, 0_c_int, subject_pol%hole)

    do i = 1, subject_pol%nparts
       allocate(l_subj_f(i)%v(subject_pol%part(i)%n_points - 1))
       ! (do not repeat the first point)

       l_subj_f(i)%v = [(GPC_VERTEX(subject_pol%part(i)%points(1, j), &
            subject_pol%part(i)%points(2, j)), &
            j = 1, subject_pol%part(i)%n_points - 1)]
       l_subj(i) = GPC_VERTEX_LIST(num_vertices = size(l_subj_f(i)%v), &
            vertex = c_loc(l_subj_f(i)%v))
    end do

    subj_p = gpc_polygon(num_contours = size(l_subj), hole = c_loc(hole_subj), &
         contour = c_loc(l_subj))

    ! Construct clip_p:

    allocate(l_clip_f(clip_pol%nparts), l_clip(clip_pol%nparts), &
         hole_clip(clip_pol%nparts))

    hole_clip = merge(1_c_int, 0_c_int, clip_pol%hole)

    do i = 1, clip_pol%nparts
       allocate(l_clip_f(i)%v(clip_pol%part(i)%n_points - 1))
       ! (do not repeat the first point)

       l_clip_f(i)%v = [(GPC_VERTEX(clip_pol%part(i)%points(1, j), &
            clip_pol%part(i)%points(2, j)), &
            j = 1, clip_pol%part(i)%n_points - 1)]
       l_clip(i) = GPC_VERTEX_LIST(num_vertices = size(l_clip_f(i)%v), &
            vertex = c_loc(l_clip_f(i)%v))
    end do

    clip_p = gpc_polygon(num_contours = size(l_clip), hole = c_loc(hole_clip), &
         contour = c_loc(l_clip))

    CALL gpc_polygon_clip(set_op, subj_p, clip_p, res_p)

    ! Construct result_pol:

    result_pol%nparts = res_p%num_contours
    allocate(result_pol%part(result_pol%nparts), &
         result_pol%hole(result_pol%nparts))

    if (res_p%num_contours /= 0) then
       call c_f_pointer(res_p%hole, hole_res, [res_p%num_contours])
       result_pol%hole = hole_res == 1_c_int
       call c_f_pointer(res_p%contour, l_res, [res_p%num_contours])

       result_pol%part%n_points = l_res%num_vertices + 1
       ! last vertex must repeart the first

       result_pol%part%closed = .true.

       do i = 1, result_pol%nparts
          associate (n => result_pol%part(i)%n_points)
            allocate(result_pol%part(i)%points(2, n))
            call c_f_pointer(l_res(i)%vertex, v_res, [n - 1])

            do j = 1, n - 1
               result_pol%part(i)%points(:, j) = [v_res(j)%x, v_res(j)%y]
            end do

            result_pol%part(i)%points(:, n) = result_pol%part(i)%points(:, 1)
            deallocate(v_res)
          end associate
       end do

       deallocate(hole_res, l_res)
    end if

end subroutine gpc_polygon_clip_f

END module gpc_polygon_clip_f_m
