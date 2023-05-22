if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
  find_package(Jumble CONFIG REQUIRED)
endif()

add_executable(test_GPC_F test_GPC_F.f90)

target_link_libraries(test_GPC_F PRIVATE Shapelib_03::shapelib_03
  Contour_531::contour_531 Jumble::jumble gpc_f)
