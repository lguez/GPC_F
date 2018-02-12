module module_gpc

  use, intrinsic :: ISO_C_BINDING

  implicit none

  private
  public C_GPC_VERTEX, C_GPC_VERTEX_LIST, C_GPC_POLYGON
  public gpc_read_polygon, gpc_write_polygon, gpc_add_contour, gpc_polygon_clip
  public gpc_free_polygon, gpc_cvlist, gpc_gvlist, gpc_gvarea, gpc_free_vertex

  TYPE, BIND(C) :: C_GPC_VERTEX
     REAL(C_DOUBLE) :: x
     REAL(C_DOUBLE) :: y
  END TYPE C_GPC_VERTEX

  TYPE, BIND(C) :: C_GPC_VERTEX_LIST
     INTEGER(C_INT) :: num_vertices
     TYPE(C_PTR) :: C_vertex
  END TYPE C_GPC_VERTEX_LIST

  TYPE, BIND(C) :: C_GPC_POLYGON
     integer(C_INT) num_contours
     type(C_PTR) hole
     type(C_PTR) C_contour
  END TYPE C_GPC_POLYGON

  interface
     SUBROUTINE gpc_free_polygon(polygon) &
          BIND(C, name='gpc_free_polygon')
       IMPORT :: C_GPC_POLYGON
       implicit none
       TYPE(C_GPC_POLYGON) polygon
     end SUBROUTINE gpc_free_polygon

     SUBROUTINE gpc_read_polygon(infile_ptr, read_hole_flags, polygon) &
          BIND(C, name='gpc_read_polygon')
       IMPORT :: C_PTR, C_INT, C_GPC_POLYGON
       implicit none
       TYPE(C_PTR), value :: infile_ptr
       integer(C_INT), value :: read_hole_flags
       TYPE(C_GPC_POLYGON) polygon
     end SUBROUTINE gpc_read_polygon

     SUBROUTINE gpc_write_polygon(infile_ptr, write_hole_flags, polygon) &
          BIND(C, name='gpc_write_polygon')
       IMPORT :: C_PTR, C_INT, C_GPC_POLYGON
       implicit none
       TYPE(C_PTR), value :: infile_ptr
       integer(C_INT), value :: write_hole_flags
       TYPE(C_GPC_POLYGON) polygon
     end SUBROUTINE gpc_write_polygon

     SUBROUTINE gpc_add_contour(polygon, vertex_list, hole) &
          BIND(C, name='gpc_add_contour')
       IMPORT :: C_INT, C_PTR, C_GPC_POLYGON, C_GPC_VERTEX_LIST
       implicit none
       TYPE(C_GPC_POLYGON) polygon
       TYPE(C_GPC_VERTEX_LIST) vertex_list
       integer(C_INT), value :: hole
     end SUBROUTINE gpc_add_contour

     SUBROUTINE gpc_polygon_clip(set_operation, subj, clip, result) &
          BIND(C, name='gpc_polygon_clip')
       IMPORT :: C_GPC_OP, C_GPC_POLYGON
       implicit none
       INTEGER(C_GPC_OP), value :: set_operation
       TYPE(C_GPC_POLYGON) subj
       TYPE(C_GPC_POLYGON) clip
       TYPE(C_GPC_POLYGON) result
     end SUBROUTINE gpc_polygon_clip

     ! GPC FUNCTION ADD-ONS

     SUBROUTINE gpc_cvlist(npoly, px, py, vertex_list, iret) &
          BIND(C, name='gpc_cvlist')
       IMPORT :: C_INT, C_FLOAT, C_GPC_VERTEX_LIST
       implicit none
       integer(C_INT), value :: npoly
       real(C_FLOAT), 		intent(inout) :: px(*)
       real(C_FLOAT), 		intent(inout) :: py(*)
       TYPE(C_GPC_VERTEX_LIST) vertex_list
       integer(C_INT), intent(inout) :: iret
     end SUBROUTINE gpc_cvlist

     SUBROUTINE gpc_gvlist(vertex_list, npoly, px, py, iret) &
          BIND(C, name='gpc_gvlist')
       IMPORT :: C_INT, C_FLOAT, C_GPC_VERTEX_LIST
       implicit none
       TYPE(C_GPC_VERTEX_LIST) vertex_list
       integer(C_INT), intent(inout) :: npoly
       real(C_FLOAT), 		intent(inout) :: px(*)
       real(C_FLOAT), 		intent(inout) :: py(*)
       integer(C_INT), intent(inout) :: iret
     end SUBROUTINE gpc_gvlist

     SUBROUTINE gpc_gvarea(vertex_list, area) &
          BIND(C, name='gpc_gvarea')
       IMPORT :: C_FLOAT, C_GPC_VERTEX_LIST
       implicit none
       TYPE(C_GPC_VERTEX_LIST) vertex_list
       real(C_FLOAT), 	intent(inout) :: area
     end SUBROUTINE gpc_gvarea

     SUBROUTINE gpc_free_vertex(vertex_list) &
          BIND(C, name='gpc_free_vertex')
       IMPORT :: C_GPC_VERTEX_LIST
       implicit none
       TYPE(C_GPC_VERTEX_LIST) vertex_list
     end SUBROUTINE gpc_free_vertex
  end interface

  ENUM, BIND(C)
     ENUMERATOR :: GPC_DIFF, GPC_INT, GPC_XOR, GPC_UNION
  END ENUM

  integer, parameter :: C_GPC_OP = kind(GPC_DIFF)

end module module_gpc
