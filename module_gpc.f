module module_gpc

  use, intrinsic:: ISO_C_BINDING

  implicit none

  private
  public C_GPC_VERTEX, C_GPC_VERTEX_LIST, C_GPC_POLYGON, gpc_polygon_clip, &
       gpc_free_polygon

  TYPE, BIND(C):: C_GPC_VERTEX
     REAL(C_DOUBLE) x
     REAL(C_DOUBLE) y
  END TYPE C_GPC_VERTEX

  TYPE, BIND(C):: C_GPC_VERTEX_LIST
     INTEGER(C_INT) num_vertices
     TYPE(C_PTR) C_vertex
  END TYPE C_GPC_VERTEX_LIST

  TYPE, BIND(C):: C_GPC_POLYGON
     integer(C_INT) num_contours
     type(C_PTR) hole
     type(C_PTR) contour
  END TYPE C_GPC_POLYGON

  ENUM, BIND(C)
     ENUMERATOR GPC_DIFF, GPC_INT, GPC_XOR, GPC_UNION
  END ENUM

  integer, parameter:: C_GPC_OP = kind(GPC_DIFF)

  interface
     SUBROUTINE gpc_free_polygon(polygon) &
          BIND(C, name = 'gpc_free_polygon')
       IMPORT C_GPC_POLYGON
       implicit none
       TYPE(C_GPC_POLYGON) polygon
     end SUBROUTINE gpc_free_polygon

     SUBROUTINE gpc_polygon_clip(set_operation, subj, clip, result) &
          BIND(C, name = 'gpc_polygon_clip')
       IMPORT C_GPC_OP, C_GPC_POLYGON
       implicit none
       INTEGER(C_GPC_OP), value:: set_operation
       TYPE(C_GPC_POLYGON) subj
       TYPE(C_GPC_POLYGON) clip
       TYPE(C_GPC_POLYGON) result
     end SUBROUTINE gpc_polygon_clip
  end interface

end module module_gpc
