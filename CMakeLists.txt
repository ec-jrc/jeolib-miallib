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
cmake_minimum_required(VERSION 3.5)

# The project's name and version
project(miallib)

set(MIALLIB_LIB_NAME miallib_generic)
# enable_testing()
# include(CTest)

set (MIALLIB_VERSION_MAJOR 1)
set (MIALLIB_VERSION_MINOR 1)
set (MIALLIB_VERSION_PATCH 0)
set (MIALLIB_VERSION "${MIALLIB_VERSION_MAJOR}.${MIALLIB_VERSION_MINOR}.${MIALLIB_VERSION_PATCH}")
set (MIALLIB_SOVERSION "${MIALLIB_VERSION_MAJOR}")

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake/modules")
message(STATUS "cmake module path: ${CMAKE_MODULE_PATH}")

# file(GLOB SRC_FILES "core/c/*.c")
# set(SOURCES
#   ${SRC_FILES}
#   )

set(SOURCES
  core/c/aflood.c
  core/c/alphacc.c
  core/c/alphatree.c
  core/c/alphatreetoCCs.c
  # core/c/banner.h
  core/c/base.h
  core/c/b_def.h
  core/c/borderdetection.c
  core/c/borderdetection.h
  core/c/bresenham.c
  core/c/b_undef.h
  core/c/bytesex.h
  core/c/ced.c
  core/c/chull.c
  core/c/classification.c
  core/c/colconv.c
  core/c/complete.c
  core/c/compose.c
  core/c/convolve.c
  core/c/dbscan.c
  core/c/dcluster.c
  core/c/d_def.h
  core/c/dendro.c
  core/c/determineSize.c
  core/c/determineSize.h
  core/c/dirmean.c
  core/c/dist.c
  core/c/douglas-peucker.c
  core/c/douglas-peucker.h
  core/c/d_undef.h
  core/c/edgeweight.c
  core/c/efedt.c
  core/c/epc.c
  core/c/erodil.c
  core/c/fah.c
  core/c/fah.h
  core/c/f_def.h
  core/c/fifo.c
  core/c/fifo.h
  core/c/fillocarve.c
  core/c/flatdir.c
  core/c/flow.c
  core/c/format.c
  core/c/f_undef.h
  core/c/g32_def.h
  core/c/g32_undef.h
  core/c/g_def.h
  core/c/geodist.c
  core/c/geom.c
  core/c/ggeo.c
  core/c/grid.c
  core/c/gsl.c
  core/c/g_undef.h
  core/c/herkbl.c
  core/c/herk.c
  core/c/histo.c
  core/c/htop.c
  core/c/hull.c
  core/c/hullti.c
  core/c/i32_def.h
  core/c/i32_undef.h
  core/c/i64_def.h
  core/c/i64_undef.h
  core/c/imem.c
  core/c/imio2.c
  core/c/imio.c
  core/c/imio_gdal.c
  core/c/imio.h
  core/c/imstat.c
  core/c/indexx.c
  core/c/label.c
  core/c/labelccdissim.c
  core/c/labelccfastrim.c
  core/c/labelccmi.c
  core/c/labelccms.c
  core/c/labelccmsdissim.c
  core/c/labelccvar.c
  core/c/labelci.c
  core/c/labelcims.c
  core/c/labelvertex.c
  core/c/lerodil.c
  core/c/linepool.c
  core/c/ll_def.h
  core/c/ll_undef.h
  core/c/Makefile
  core/c/mblincomb.c
  core/c/mcisrg.c
  core/c/mcisrg.h
  core/c/miallib_convolve.h
  core/c/miallib_dem.h
  core/c/miallib_dist.h
  core/c/miallib_erodil.h
  core/c/miallib_format.h
  core/c/miallib_generic.pc
  core/c/miallib_geodesy.h
  core/c/miallib_geometry.h
  core/c/miallib.h
  core/c/miallib_hmt.h
  core/c/miallib_imem.h
  core/c/miallib_io.h
  core/c/miallib_label.h
  core/c/miallib_miscel.h
  core/c/miallib_opclo.h
  core/c/miallib_pointop.h
  core/c/miallib_proj.h
  core/c/miallib_segment.h
  core/c/miallib_stats.h
  core/c/miallib_swig.h
  core/c/mial.pc
  core/c/mialtypes.h
  core/c/miscel.c
  core/c/mmlabel.c
  core/c/mslabel.c
  core/c/mslabel.h
  core/c/msmm.c
  core/c/mspa.c
  core/c/myhull.c
  core/c/natngbint.c
  core/c/newlabelcc.c
  core/c/oiht.c
  core/c/oiiz.c
  core/c/oiws.c
  core/c/op.h
  core/c/outeredge.c
  core/c/ovlmatrix.c
  core/c/partition.c
  core/c/partorp.c
  core/c/phase_correlation.c
  core/c/pointop.c
  core/c/pqueue.c
  core/c/pqueueExact.c
  core/c/pqueueExact.h
  core/c/pqueue.h
  core/c/projection.c
  core/c/propagate.c
  core/c/rank.c
  core/c/recons.c
  core/c/regionMean.c
  core/c/regionMean.h
  core/c/registration.c
  core/c/remsens.c
  core/c/rminmax.c
  core/c/s_def.h
  core/c/segmentation.c
  core/c/segmentation.h
  core/c/setreg.c
  core/c/setreglut.c
  core/c/setshft.c
  core/c/shade.c
  core/c/shm.c
  core/c/simplifyLine.c
  core/c/simplifyLine.h
  core/c/skel.c
  core/c/skelodthin.c
  core/c/srg.c
  core/c/s_undef.h
  core/c/switch.c
  core/c/transition.c
  core/c/u32_def.h
  core/c/u32_undef.h
  core/c/u64_def.h
  core/c/u64_undef.h
  core/c/uc_def.h
  core/c/uc_undef.h
  core/c/us_def.h
  core/c/us_undef.h
  core/c/uswilk.c
  core/c/vectorize.c
  core/c/vectorize.h
  core/c/writeShapeFile.c
  core/c/writeShapeFile.h
  core/c/writeSVG.c
  core/c/writeSVG.h
  core/c/wsfah.c
  core/c/wshed.c
  )


# Setup output directories.
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)


option (BUILD_SHARED_LIBS "Build with shared library" ON)
###############################################################################
# Define the miallib library.
# add_library( ${MIALLIB_LIB_NAME} ${SRC_FILES})
add_library( ${MIALLIB_LIB_NAME} ${SOURCES})
###############################################################################

SET_TARGET_PROPERTIES(${MIALLIB_LIB_NAME}
  PROPERTIES
  SOVERSION ${MIALLIB_SOVERSION}
  VERSION ${MIALLIB_VERSION}
  )

find_package(GDAL 3.00 REQUIRED)
target_link_libraries(${MIALLIB_LIB_NAME} ${GDAL_LIBRARIES})
include_directories(${GDAL_INCLUDE_DIRS})

find_package(TIFF)
target_link_libraries(${MIALLIB_LIB_NAME} tiff)

# find_package(PROJ REQUIRED CONFIG)
# target_link_libraries(${MIALLIB_LIB_NAME} PROJ::proj)

target_link_libraries(${MIALLIB_LIB_NAME} ${CMAKE_DL_LIBS})

find_package(OpenMP REQUIRED)
if (OPENMP_FOUND)
  set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
endif(OPENMP_FOUND)

### BEGIN FFTW
find_package(PkgConfig REQUIRED)
pkg_check_modules(FFTW IMPORTED_TARGET REQUIRED fftw3)

if( NOT FFTW_ROOT AND DEFINED ENV{FFTWDIR} )
    set( FFTW_ROOT $ENV{FFTWDIR} )
endif()
find_library(
        FFTW_DOUBLE_THREADS_LIB
        NAMES "fftw3_threads"
        PATHS ${PKG_FFTW_LIBRARY_DIRS} ${LIB_INSTALL_DIR}
        )


if (FFTW_DOUBLE_THREADS_LIB)
    set(FFTW_DOUBLE_THREADS_LIB_FOUND TRUE)
    set(FFTW_LIBRARIES ${FFTW_LIBRARIES} ${FFTW_DOUBLE_THREADS_LIB})
    add_library(FFTW::DoubleThreads INTERFACE IMPORTED)
    set_target_properties(FFTW::DoubleThreads
        PROPERTIES INTERFACE_INCLUDE_DIRECTORIES "${FFTW_INCLUDE_DIRS}"
        INTERFACE_LINK_LIBRARIES "${FFTW_DOUBLE_THREADS_LIB}"
        )
else()
    set(FFTW_DOUBLE_THREADS_LIB_FOUND FALSE)
endif()

target_link_libraries(${MIALLIB_LIB_NAME} OpenMP::OpenMP_CXX PkgConfig::FFTW)
target_link_libraries(${MIALLIB_LIB_NAME} OpenMP::OpenMP_CXX PkgConfig::FFTW ${FFTW_DOUBLE_THREADS_LIB})
### END FFTW

find_package(GSL REQUIRED)
if(GSL_FOUND)
  message(STATUS "Found GSL: ${GSL_LIBRARIES}")
  include_directories(${GSL_INCLUDE_DIRS})
  add_definitions(-DGSL_DLL)
  target_link_libraries(${MIALLIB_LIB_NAME} ${GSL_LIBRARIES})
endif()


# find_package(SHP REQUIRED)
# target_link_libraries(${MIALLIB_LIB_NAME} shp)
find_package(SHAPELIB REQUIRED)
# message(STATUS "Found SHAPE library: ${SHAPELIB_LIBRARY}")
message(STATUS "Found SHAPELIB libraries: ${SHAPELIB_LIBRARIES}")
message(STATUS "Found SHAPELIB include dir: ${SHAPELIB_INCLUDE_DIRS}")
message(STATUS "Found SHAPELIB_LIBRARY: ${SHAPELIB_LIBRARY}")
include_directories(${SHAPELIB_INCLUDE_DIRS})
target_link_libraries(${MIALLIB_LIB_NAME} ${SHAPELIB_LIBRARIES})
add_definitions(-DSHAPE_DLL)

INSTALL (TARGETS ${MIALLIB_LIB_NAME} LIBRARY DESTINATION lib)
