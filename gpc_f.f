module gpc_f

  ! Fortran interface to GPC.

  use, intrinsic:: iso_c_binding

  implicit none

  private
  public GPC_VERTEX, GPC_VERTEX_LIST, GPC_POLYGON, gpc_polygon_clip, &
       gpc_free_polygon, GPC_OP, GPC_DIFF, GPC_INT, GPC_XOR, GPC_UNION

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

  ENUM, BIND(C)
     ENUMERATOR GPC_DIFF, GPC_INT, GPC_XOR, GPC_UNION
  END ENUM

  integer, parameter:: GPC_OP = kind(GPC_DIFF)

  interface
     subroutine gpc_free_polygon(polygon) bind(c)
       import gpc_polygon
       implicit none
       type(gpc_polygon) polygon
     end subroutine gpc_free_polygon

     subroutine gpc_polygon_clip(set_operation, subject_polygon, clip_polygon, &
          result_polygon) bind(c)
       import gpc_op, gpc_polygon
       implicit none
       integer(gpc_op), value, intent(in):: set_operation
       type(gpc_polygon), intent(inout):: subject_polygon, clip_polygon
       type(gpc_polygon), intent(out):: result_polygon
     end subroutine gpc_polygon_clip
  end interface

end module gpc_f
