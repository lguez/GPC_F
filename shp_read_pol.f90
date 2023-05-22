module shp_read_pol_m

  implicit none

contains

  subroutine shp_read_pol(p, hshp, ishape)

    use gpc_polygon_clip_f_m, only: POLYGON
    use shapelib, only: shpfileobject, shpobject, shpdestroyobject
    use shapelib_03, only: shp_read_object_03
    use shpobj2pol_m, only: shpobj2pol

    type(polygon), intent(out):: p
    type(shpfileobject), intent(in):: hshp
    integer, intent(in):: ishape ! 0-based

    ! Local:
    TYPE(shpobject) psobject


    !-------------------------------------------------------------

    call shp_read_object_03(hshp, ishape, psobject)
    p = shpobj2pol(psobject)
    call shpdestroyobject(psobject)

  end subroutine shp_read_pol

end module shp_read_pol_m
