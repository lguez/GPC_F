module shpobj2pol_m

  implicit none

contains

  pure type(polygon) function shpobj2pol(psobj) result(p)

    ! Copy a variable of type shpobject, defined in FortranGIS, to a
    ! new variable of type polygon.

    use gpc_polygon_clip_f_m, only: POLYGON
    use shapelib, only: shpobject

    type(shpobject), intent(in):: psobj

    ! Local:
    integer i, j

    !---------------------------------------------------------------

    p%nparts = psobj%nparts
    allocate(p%hole(p%nparts), p%part(p%nparts))
    p%hole = .false.
    p%part%closed = .true.

    do i = 1, p%nparts - 1
       p%part(i)%n_points = psobj%panpartstart(i + 1) - psobj%panpartstart(i)
    end do

    p%part(p%nparts)%n_points = psobj%nvertices &
         - psobj%panpartstart(psobj%nparts)
    j = 1

    do i = 1, p%nparts
       allocate(p%part(i)%points(2, p%part(i)%n_points))
       p%part(i)%points(1, :) = psobj%padfx(j:j + p%part(i)%n_points - 1)
       p%part(i)%points(2, :) = psobj%padfy(j:j + p%part(i)%n_points - 1)
       j = j + p%part(i)%n_points
    end do

  end function shpobj2pol

end module shpobj2pol_m
