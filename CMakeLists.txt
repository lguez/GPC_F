cmake_minimum_required(VERSION 3.13)
project(GPC_F_project Fortran)

add_library(gpc_f gpc_polygon_clip_f.f shpobj2pol.f gpc_f.f)

set_target_properties(gpc_f PROPERTIES Fortran_FORMAT FREE)
target_link_libraries(gpc_f PUBLIC fortrangis contour_531 GPC)
include(ConfigureCompilerFlags)
find_path(fortrangis_INCLUDE_DIR shapelib.mod)
target_include_directories(gpc_f PRIVATE ${fortrangis_INCLUDE_DIR})
