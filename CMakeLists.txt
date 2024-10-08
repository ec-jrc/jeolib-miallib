###############################################################################
# Author(s): Pieter.Kempeneers@ec.europa.eu
# Copyright (C) 2000-2023 European Union (Joint Research Centre)

# This file is part of miallib.

# miallib is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# miallib is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with miallib.  If not, see <https://www.gnu.org/licenses/>.
###############################################################################

## need to install
# libopenblas-dev
# libfftw3-mpi-dev

cmake_minimum_required(VERSION 3.15)

# The project's name and version
project(miallib)

# set(MIALLIB_LIB_NAME miallib_generic)
set(MIALLIB_LIB_NAME miallib)

set (MIALLIB_VERSION_MAJOR 1)
set (MIALLIB_VERSION_MINOR 1)
set (MIALLIB_VERSION_PATCH 6)
set (MIALLIB_VERSION "${MIALLIB_VERSION_MAJOR}.${MIALLIB_VERSION_MINOR}.${MIALLIB_VERSION_PATCH}")
set (MIALLIB_SOVERSION "${MIALLIB_VERSION_MAJOR}")

set(PROCESS_IN_PARALLEL TRUE CACHE BOOL "Choose if miallib should be run in parallel")

# build a CPack driven installer package
INCLUDE (InstallRequiredSystemLibraries)
SET(CPACK_COMPONENTS_ALL libraries headers)
SET(CPACK_RESOURCE_FILE_LICENSE
     "${CMAKE_CURRENT_SOURCE_DIR}/COPYING")
SET(CPACK_PACKAGE_VERSION_MAJOR "${MIALLIB_VERSION_MAJOR}")
SET(CPACK_PACKAGE_VERSION_MINOR "${MIALLIB_VERSION_MINOR}")
SET(CPACK_PACKAGE_VERSION_PATCH "${MIALLIB_VERSION_PATCH}")
SET(CPACK_GENERATOR "DEB")
SET(CPACK_DEBIAN_PACKAGE_MAINTAINER "Pieter Kempeneers")
SET(CPACK_DEBIAN_PACKAGE_SHLIBDEPS ON)
# Set files to ignore
list(APPEND CPACK_SOURCE_IGNORE_FILES "_CPack_Packages")
list(APPEND CPACK_SOURCE_IGNORE_FILES ".gz")
list(APPEND CPACK_SOURCE_IGNORE_FILES ".bz2")
list(APPEND CPACK_SOURCE_IGNORE_FILES ".zip")
list(APPEND CPACK_SOURCE_IGNORE_FILES ".svn")
list(APPEND CPACK_SOURCE_IGNORE_FILES ".git")
list(APPEND CPACK_SOURCE_IGNORE_FILES "HOWTORELEASE.txt")
list(APPEND CPACK_SOURCE_IGNORE_FILES "CMakeCache.txt")
list(APPEND CPACK_SOURCE_IGNORE_FILES "CPackConfig.cmake")
list(APPEND CPACK_SOURCE_IGNORE_FILES "schemas")
list(APPEND CPACK_SOURCE_IGNORE_FILES "/build/;~$;${CPACK_SOURCE_IGNORE_FILES}")

SET(CPACK_DEB_COMPONENT_INSTALL YES)
INCLUDE(CPack)
CPACK_ADD_COMPONENT(libraries)
CPACK_ADD_COMPONENT(headers)
SET(CPACK_DEB_COMPONENT_INSTALL YES)
SET(CPACK_COMPONENTS_ALL libraries headers)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake/modules")
message(STATUS "cmake module path: ${CMAKE_MODULE_PATH}")

set(MIALLIB_SRC_DIR ${PROJECT_SOURCE_DIR}/core/c)

#simple way to get all src files, but not recommended
# file(GLOB SRC_FILES ${PROJECT_SOURCE_DIR}/core/*.c")
# set(SOURCES
#   ${SRC_FILES}
#   )

set(MIALLIB_H
  ${MIALLIB_SRC_DIR}/miallib_convolve.h
  ${MIALLIB_SRC_DIR}/miallib_dem.h
  ${MIALLIB_SRC_DIR}/miallib_dist.h
  ${MIALLIB_SRC_DIR}/miallib_erodil.h
  ${MIALLIB_SRC_DIR}/miallib_format.h
  ${MIALLIB_SRC_DIR}/miallib_geodesy.h
  ${MIALLIB_SRC_DIR}/miallib_geometry.h
  ${MIALLIB_SRC_DIR}/miallib_hmt.h
  ${MIALLIB_SRC_DIR}/miallib_imem.h
  ${MIALLIB_SRC_DIR}/miallib_io.h
  ${MIALLIB_SRC_DIR}/miallib_label.h
  ${MIALLIB_SRC_DIR}/miallib_miscel.h
  ${MIALLIB_SRC_DIR}/miallib_opclo.h
  ${MIALLIB_SRC_DIR}/miallib_pointop.h
  ${MIALLIB_SRC_DIR}/miallib_proj.h
  ${MIALLIB_SRC_DIR}/miallib_segment.h
  ${MIALLIB_SRC_DIR}/miallib_stats.h
  ${MIALLIB_SRC_DIR}/miallib_swig.h
  ${MIALLIB_SRC_DIR}/mialtypes.h
  ${MIALLIB_SRC_DIR}/myhull.h
  ${MIALLIB_SRC_DIR}/op.h
  )

set(SOURCES
  ${MIALLIB_H}
  ${MIALLIB_SRC_DIR}/aflood.c
  ${MIALLIB_SRC_DIR}/alphacc.c
  ${MIALLIB_SRC_DIR}/alphatree.c
  ${MIALLIB_SRC_DIR}/alphatreetoCCs.c
  ${MIALLIB_SRC_DIR}/base.h
  ${MIALLIB_SRC_DIR}/b_def.h
  ${MIALLIB_SRC_DIR}/borderdetection.c
  ${MIALLIB_SRC_DIR}/borderdetection.h
  ${MIALLIB_SRC_DIR}/bresenham.c
  ${MIALLIB_SRC_DIR}/b_undef.h
  ${MIALLIB_SRC_DIR}/bytesex.h
  ${MIALLIB_SRC_DIR}/ced.c
  ${MIALLIB_SRC_DIR}/classification.c
  ${MIALLIB_SRC_DIR}/colconv.c
  ${MIALLIB_SRC_DIR}/complete.c
  ${MIALLIB_SRC_DIR}/compose.c
  ${MIALLIB_SRC_DIR}/convolve.c
  ${MIALLIB_SRC_DIR}/dbscan.c
  ${MIALLIB_SRC_DIR}/dcluster.c
  ${MIALLIB_SRC_DIR}/d_def.h
  ${MIALLIB_SRC_DIR}/dendro.c
  ${MIALLIB_SRC_DIR}/determineSize.c
  ${MIALLIB_SRC_DIR}/determineSize.h
  ${MIALLIB_SRC_DIR}/dirmean.c
  ${MIALLIB_SRC_DIR}/dist.c
  ${MIALLIB_SRC_DIR}/douglas-peucker.c
  ${MIALLIB_SRC_DIR}/douglas-peucker.h
  ${MIALLIB_SRC_DIR}/d_undef.h
  ${MIALLIB_SRC_DIR}/edgeweight.c
  ${MIALLIB_SRC_DIR}/efedt.c
  ${MIALLIB_SRC_DIR}/epc.c
  ${MIALLIB_SRC_DIR}/erodil.c
  ${MIALLIB_SRC_DIR}/fah.c
  ${MIALLIB_SRC_DIR}/fah.h
  ${MIALLIB_SRC_DIR}/f_def.h
  ${MIALLIB_SRC_DIR}/fifo.c
  ${MIALLIB_SRC_DIR}/fifo.h
  ${MIALLIB_SRC_DIR}/fillocarve.c
  ${MIALLIB_SRC_DIR}/flatdir.c
  ${MIALLIB_SRC_DIR}/flow.c
  ${MIALLIB_SRC_DIR}/format.c
  ${MIALLIB_SRC_DIR}/f_undef.h
  ${MIALLIB_SRC_DIR}/g32_def.h
  ${MIALLIB_SRC_DIR}/g32_undef.h
  ${MIALLIB_SRC_DIR}/g_def.h
  ${MIALLIB_SRC_DIR}/geodist.c
  ${MIALLIB_SRC_DIR}/geom.c
  ${MIALLIB_SRC_DIR}/ggeo.c
  ${MIALLIB_SRC_DIR}/grid.c
  ${MIALLIB_SRC_DIR}/gsl.c
  ${MIALLIB_SRC_DIR}/g_undef.h
  ${MIALLIB_SRC_DIR}/herkbl.c
  ${MIALLIB_SRC_DIR}/herk.c
  ${MIALLIB_SRC_DIR}/histo.c
  ${MIALLIB_SRC_DIR}/htop.c
  ${MIALLIB_SRC_DIR}/hull.c
  ${MIALLIB_SRC_DIR}/hullti.c
  ${MIALLIB_SRC_DIR}/i32_def.h
  ${MIALLIB_SRC_DIR}/i32_undef.h
  ${MIALLIB_SRC_DIR}/i64_def.h
  ${MIALLIB_SRC_DIR}/i64_undef.h
  ${MIALLIB_SRC_DIR}/imem.c
  ${MIALLIB_SRC_DIR}/imio2.c
  ${MIALLIB_SRC_DIR}/imio.c
  ${MIALLIB_SRC_DIR}/imio_gdal.c
  ${MIALLIB_SRC_DIR}/imio.h
  ${MIALLIB_SRC_DIR}/imstat.c
  ${MIALLIB_SRC_DIR}/indexx.c
  ${MIALLIB_SRC_DIR}/label.c
  ${MIALLIB_SRC_DIR}/labelccdissim.c
  ${MIALLIB_SRC_DIR}/labelccfastrim.c
  ${MIALLIB_SRC_DIR}/labelccmi.c
  ${MIALLIB_SRC_DIR}/labelccms.c
  ${MIALLIB_SRC_DIR}/labelccmsdissim.c
  ${MIALLIB_SRC_DIR}/labelccvar.c
  ${MIALLIB_SRC_DIR}/labelci.c
  ${MIALLIB_SRC_DIR}/labelcims.c
  ${MIALLIB_SRC_DIR}/labelvertex.c
  ${MIALLIB_SRC_DIR}/lerodil.c
  ${MIALLIB_SRC_DIR}/linepool.c
  ${MIALLIB_SRC_DIR}/ll_def.h
  ${MIALLIB_SRC_DIR}/ll_undef.h
  ${MIALLIB_SRC_DIR}/mblincomb.c
  ${MIALLIB_SRC_DIR}/mcisrg.c
  ${MIALLIB_SRC_DIR}/mcisrg.h
  ${MIALLIB_SRC_DIR}/miallib.h
  ${MIALLIB_SRC_DIR}/miscel.c
  ${MIALLIB_SRC_DIR}/mmlabel.c
  ${MIALLIB_SRC_DIR}/mslabel.c
  ${MIALLIB_SRC_DIR}/mslabel.h
  ${MIALLIB_SRC_DIR}/msmm.c
  ${MIALLIB_SRC_DIR}/mspa.c
  ${MIALLIB_SRC_DIR}/myhull.cc
  ${MIALLIB_SRC_DIR}/myhull.h
  ${MIALLIB_SRC_DIR}/natngbint.c
  ${MIALLIB_SRC_DIR}/newlabelcc.c
  ${MIALLIB_SRC_DIR}/oiht.c
  ${MIALLIB_SRC_DIR}/oiiz.c
  ${MIALLIB_SRC_DIR}/oiws.c
  ${MIALLIB_SRC_DIR}/outeredge.c
  ${MIALLIB_SRC_DIR}/ovlmatrix.c
  ${MIALLIB_SRC_DIR}/partition.c
  ${MIALLIB_SRC_DIR}/partorp.c
  ${MIALLIB_SRC_DIR}/phase_correlation.c
  ${MIALLIB_SRC_DIR}/pointop.c
  ${MIALLIB_SRC_DIR}/pqueue.c
  ${MIALLIB_SRC_DIR}/pqueueExact.c
  ${MIALLIB_SRC_DIR}/pqueueExact.h
  ${MIALLIB_SRC_DIR}/pqueue.h
  ${MIALLIB_SRC_DIR}/projection.c
  ${MIALLIB_SRC_DIR}/propagate.c
  ${MIALLIB_SRC_DIR}/rank.c
  ${MIALLIB_SRC_DIR}/recons.c
  ${MIALLIB_SRC_DIR}/regionMean.c
  ${MIALLIB_SRC_DIR}/regionMean.h
  ${MIALLIB_SRC_DIR}/registration.c
  ${MIALLIB_SRC_DIR}/remsens.c
  ${MIALLIB_SRC_DIR}/rminmax.c
  ${MIALLIB_SRC_DIR}/s_def.h
  ${MIALLIB_SRC_DIR}/segmentation.c
  ${MIALLIB_SRC_DIR}/segmentation.h
  ${MIALLIB_SRC_DIR}/setreg.c
  ${MIALLIB_SRC_DIR}/setreglut.c
  ${MIALLIB_SRC_DIR}/setshft.c
  ${MIALLIB_SRC_DIR}/shade.c
  ${MIALLIB_SRC_DIR}/shm.c
  ${MIALLIB_SRC_DIR}/simplifyLine.c
  ${MIALLIB_SRC_DIR}/simplifyLine.h
  ${MIALLIB_SRC_DIR}/skel.c
  ${MIALLIB_SRC_DIR}/skelodthin.c
  ${MIALLIB_SRC_DIR}/srg.c
  ${MIALLIB_SRC_DIR}/s_undef.h
  ${MIALLIB_SRC_DIR}/switch.c
  ${MIALLIB_SRC_DIR}/transition.c
  ${MIALLIB_SRC_DIR}/u32_def.h
  ${MIALLIB_SRC_DIR}/u32_undef.h
  ${MIALLIB_SRC_DIR}/u64_def.h
  ${MIALLIB_SRC_DIR}/u64_undef.h
  ${MIALLIB_SRC_DIR}/uc_def.h
  ${MIALLIB_SRC_DIR}/uc_undef.h
  ${MIALLIB_SRC_DIR}/us_def.h
  ${MIALLIB_SRC_DIR}/us_undef.h
  ${MIALLIB_SRC_DIR}/uswilk.c
  ${MIALLIB_SRC_DIR}/vectorize.c
  ${MIALLIB_SRC_DIR}/vectorize.h
  ${MIALLIB_SRC_DIR}/writeShapeFile.c
  ${MIALLIB_SRC_DIR}/writeShapeFile.h
  ${MIALLIB_SRC_DIR}/writeSVG.c
  ${MIALLIB_SRC_DIR}/writeSVG.h
  ${MIALLIB_SRC_DIR}/wsfah.c
  ${MIALLIB_SRC_DIR}/wshed.c
  )
# core/c/banner.h

message(STATUS "SOURCES: ${SOURCES}")
# Setup output directories.
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)


option (BUILD_SHARED_LIBS "Build with shared library" ON)
###############################################################################
# Define the miallib library.
# add_library( ${MIALLIB_LIB_NAME} ${SRC_FILES})
add_library(${MIALLIB_LIB_NAME} ${SOURCES})
###############################################################################

SET_TARGET_PROPERTIES(${MIALLIB_LIB_NAME}
  PROPERTIES
  SOVERSION ${MIALLIB_SOVERSION}
  VERSION ${MIALLIB_VERSION}
  NAMESPACE ${MIALLIB_LIB_NAME}::
  )

include_directories("core/c")

find_package(PkgConfig REQUIRED)

# Setup configure file
# configure a header file to pass some of the CMake settings to the source code

configure_file ( "cmake-config.h.in" "${CMAKE_CURRENT_BINARY_DIR}/core/c/config_miallib.h")
# configure_file ( "cmake-config.in" "${CMAKE_CURRENT_BINARY_DIR}/miallib-config" @ONLY )
include_directories("${CMAKE_CURRENT_BINARY_DIR}/core/c")# needed for config_miallib.h
add_definitions(-DHAVE_CONFIG_H)

include(GNUInstallDirs)
set(INCLUDE_INSTALL_DIR ${CMAKE_INSTALL_INCLUDEDIR}/miallib
  CACHE PATH "Location of header files" )
set(LIBRARY_INSTALL_DIR ${CMAKE_INSTALL_LIBDIR}
  CACHE PATH "Location of library files" )

#Generating Package Files
include(CMakePackageConfigHelpers)

configure_package_config_file(
  miallib-config.cmake.in
  ${CMAKE_CURRENT_BINARY_DIR}/miallib-config.cmake
  INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/miallib
  PATH_VARS INCLUDE_INSTALL_DIR LIBRARY_INSTALL_DIR
  )

# Generate MIALLIBConfigVersion.cmake
write_basic_package_version_file(
  ${CMAKE_CURRENT_BINARY_DIR}/miallib-config-version.cmake
  VERSION ${MIALLIB_VERSION}
  COMPATIBILITY AnyNewerVersion
  )

configure_file ( "cmake.pc.in" "miallib.pc"  @ONLY)

find_package(ZLIB REQUIRED)
target_link_libraries(${MIALLIB_LIB_NAME} ${ZLIB_LIBRARIES})
include_directories(${ZLIB_INCLUDE_DIRS})
message(STATUS "Found ZLIB_INCLUDE_DIRS: ${ZLIB_INCLUDE_DIRS}")

find_package(GDAL 3.00 REQUIRED)
target_link_libraries(${MIALLIB_LIB_NAME} ${GDAL_LIBRARIES})
include_directories(${GDAL_INCLUDE_DIRS})

find_package(GeoTIFF)
message(STATUS "Found GEOTIFF_INCLUDE_DIRS: ${GEOTIFF_INCLUDE_DIRS}")
include_directories(${GEOTIFF_INCLUDE_DIR})
target_link_libraries(${MIALLIB_LIB_NAME} ${GEOTIFF_LIBRARIES})

find_package(TIFF)
target_link_libraries(${MIALLIB_LIB_NAME} tiff)

find_package(PROJ REQUIRED IMPORTED)
message(STATUS "Found PROJ INCLUDE DIRS: ${PROJ_INCLUDE_DIRS}")
target_link_libraries(${MIALLIB_LIB_NAME} ${PROJ_LIBRARIES})


message(STATUS "Found DL_LIBS: ${CMAKE_DL_LIBS}")
target_link_libraries(${MIALLIB_LIB_NAME} ${CMAKE_DL_LIBS})


### BEGIN FFTW

pkg_search_module(FFTW REQUIRED fftw3 IMPORTED_TARGET)
include_directories(PkgConfig::FFTW)
link_libraries(PkgConfig::FFTW)

#find_package(FFTW REQUIRED)
message(STATUS "Found FFTW_LIBRARIES: ${FFTW_LIBRARIES}")
message(STATUS "Found FFTW_INCLUDE_DIRS: ${FFTW_INCLUDE_DIRS}")
target_link_libraries(${MIALLIB_LIB_NAME} ${FFTW_LIBRARIES})

if(PROCESS_IN_PARALLEL)
  find_package(OpenMP REQUIRED)
  set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
  set (CMAKE_C_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
  set(PARALLEL_BOOL 1)
  message(STATUS "parallel processing: ${PARALLEL_BOOL}")
  SET(CMAKE_C_FLAGS "-DOPENMP=${PARALLEL_BOOL}")
  target_link_libraries(${MIALLIB_LIB_NAME} OpenMP::OpenMP_CXX)
  # string(REPLACE ".so" "_omp.so" FFTW_OMP_LIBRARIES "${FFTW_LIBRARIES}")
  message(STATUS "FFTW_OMP_LIBRARIES: ${FFTW_OMP_LIBRARIES}")
  target_link_libraries(${MIALLIB_LIB_NAME} ${FFTW_OMP_LIBRARIES})
else(PROCESS_IN_PARALLEL)
  set(PARALLEL_BOOL 0)
  message(STATUS "parallel processing: ${PARALLEL_BOOL}")
endif(PROCESS_IN_PARALLEL)

find_package(PkgConfig REQUIRED)

set(BLA_VENDOR OpenBLAS)
set(CMAKE_C_FLAGS -fopenmp)
#find_package(BLAS REQUIRED)

find_package(GSL REQUIRED)
if(GSL_FOUND)
  message(STATUS "Found GSL: ${GSL_LIBRARIES}")
  include_directories(${GSL_INCLUDE_DIRS})
  add_definitions(-DGSL_DLL)
  target_link_libraries(${MIALLIB_LIB_NAME} ${GSL_LIBRARIES})
endif()


find_package(SHAPELIB REQUIRED)
message(STATUS "Found SHAPELIB libraries: ${SHAPELIB_LIBRARIES}")
message(STATUS "Found SHAPELIB include dir: ${SHAPELIB_INCLUDE_DIRS}")
include_directories(${SHAPELIB_INCLUDE_DIRS})
target_link_libraries(${MIALLIB_LIB_NAME} ${SHAPELIB_LIBRARIES})
add_definitions(-DSHAPE_DLL)

INSTALL(TARGETS ${MIALLIB_LIB_NAME}
  EXPORT "${MIALLIB_LIB_NAME}"
  COMPONENT libraries
  LIBRARY DESTINATION lib)
INSTALL(FILES ${MIALLIB_H} DESTINATION include/miallib/)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/miallib-config.cmake
  ${CMAKE_CURRENT_BINARY_DIR}/miallib-config-version.cmake
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/miallib
  COMPONENT headers
  )
INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/miallib.pc
        DESTINATION lib/pkgconfig
)
