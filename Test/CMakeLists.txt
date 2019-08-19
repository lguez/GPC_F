find_library(fortrangis_LIBRARY fortrangis)
find_library(fortranc_LIBRARY fortranc)
find_library(shapelib_LIBRARY shp)
find_library(GPC_LIBRARY GPC)

add_executable(test_GPC test_GPC.f90)
target_include_directories(test_GPC PRIVATE ${fortrangis_INCLUDE_DIR})

target_link_libraries(test_GPC shapelib_03 ${fortrangis_LIBRARY}
  ${fortranc_LIBRARY} ${shapelib_LIBRARY} contour_531 jumble nr_util
  gpc_f ${GPC_LIBRARY})
