cmake_minimum_required(VERSION 3.13)
project(GPC_F Fortran)

add_library(gpc_f gpc_polygon_clip_f.f90 shpobj2pol.f90 gpc_f.f90)

set_target_properties(gpc_f PROPERTIES Fortran_MODULE_DIRECTORY
  ${PROJECT_BINARY_DIR}/modules)
target_include_directories(gpc_f PUBLIC
  $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/modules>)

if(NOT TARGET contour_531)
  find_package(Contour_531 CONFIG REQUIRED)
endif()

if(NOT TARGET GPC)
  find_package(GPC CONFIG REQUIRED)
endif()

target_link_libraries(gpc_f PUBLIC contour_531 PRIVATE GPC)

set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR})
include(ConfigureCompilerFlags)
find_path(fortrangis_INCLUDE_DIR shapelib.mod REQUIRED)
target_include_directories(gpc_f PUBLIC ${fortrangis_INCLUDE_DIR})
add_subdirectory(Test EXCLUDE_FROM_ALL)

include(CMakePackageConfigHelpers)
configure_package_config_file(Config.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake
    INSTALL_DESTINATION lib/cmake/${PROJECT_NAME})

if(CMAKE_Fortran_COMPILER_ID MATCHES NAG|Intel)
  set(install_include_dir include/${PROJECT_NAME})
  install(DIRECTORY ${PROJECT_BINARY_DIR}/modules/ DESTINATION
    ${install_include_dir})
else()
  set(install_include_dir include)
  install(FILES ${PROJECT_BINARY_DIR}/modules/gpc_f.mod TYPE INCLUDE)
endif()

install(TARGETS gpc_f EXPORT ${PROJECT_NAME}Targets LIBRARY
  INCLUDES DESTINATION ${install_include_dir})
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake
  DESTINATION lib/cmake/${PROJECT_NAME})
install(EXPORT ${PROJECT_NAME}Targets DESTINATION lib/cmake/${PROJECT_NAME})
export(EXPORT ${PROJECT_NAME}Targets)
