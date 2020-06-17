if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
  find_package(Shapelib_03 CONFIG REQUIRED)
endif()

add_executable(test_GPC_F test_GPC_F.f90)

target_link_libraries(test_GPC_F PRIVATE shapelib_03 contour_531 jumble nr_util
  gpc_f)
