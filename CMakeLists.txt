cmake_minimum_required(VERSION 3.16)
project(GPC_F LANGUAGES Fortran)

# Start finding things

if(NOT TARGET contour_531)
  find_package(Contour_531 CONFIG REQUIRED)
endif()

if(NOT TARGET GPC)
  find_package(GPC CONFIG REQUIRED)
endif()

if(NOT TARGET shapelib_03)
  find_package(Shapelib_03 CONFIG REQUIRED)
endif()

if(NOT TARGET fortrangis::fortranGIS)
  option(FETCH "Download and build FortranGIS")

  if(FETCH)
    include(FetchContent)
    FetchContent_Declare(fortrangis GIT_REPOSITORY
      https://github.com/ARPA-SIMC/fortrangis)
    FetchContent_MakeAvailable(fortrangis)
  else()
    find_package(fortrangis CONFIG REQUIRED)
  endif()
endif()

# End finding things

add_library(gpc_f gpc_polygon_clip_f.f90 shpobj2pol.f90 gpc_f.f90
  shp_read_pol.f90)
set_target_properties(gpc_f PROPERTIES Fortran_MODULE_DIRECTORY
  ${PROJECT_BINARY_DIR}/modules)
target_include_directories(gpc_f PUBLIC
  $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/modules>)
target_link_libraries(gpc_f PUBLIC Contour_531::contour_531
  fortrangis::fortranGIS PRIVATE GPC::GPC Shapelib_03::shapelib_03)
add_subdirectory(Test EXCLUDE_FROM_ALL)
configure_file(${PROJECT_NAME}Config.cmake.in ${PROJECT_NAME}Config.cmake @ONLY)

# Start installation part

if(CMAKE_Fortran_COMPILER_ID MATCHES NAG|Intel)
  set(install_include_dir include/${PROJECT_NAME})
  install(DIRECTORY ${PROJECT_BINARY_DIR}/modules/ DESTINATION
    ${install_include_dir})
else()
  set(install_include_dir include)
  install(FILES ${PROJECT_BINARY_DIR}/modules/gpc_f.mod TYPE INCLUDE)
endif()

install(TARGETS gpc_f EXPORT ${PROJECT_NAME}Targets INCLUDES
  DESTINATION ${install_include_dir})
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake
  DESTINATION lib/cmake/${PROJECT_NAME})
install(EXPORT ${PROJECT_NAME}Targets DESTINATION lib/cmake/${PROJECT_NAME})

# End installation part

export(EXPORT ${PROJECT_NAME}Targets)
