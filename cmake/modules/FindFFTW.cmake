# - Find the FFTW library
#
# Usage:
#   find_package(FFTW [REQUIRED] [QUIET] )
#
# It sets the following variables:
#   FFTW_FOUND               ... true if fftw is found on the system
#   FFTW_LIBRARIES           ... full path to fftw library
#   FFTW_INCLUDES            ... fftw include directory
#   FFTW_OMP_PRESENT         ... OpenMP specific libraries are present and included in FFTW_LIBRARIES
#
# The following variables will be checked by the function
#   FFTW_USE_STATIC_LIBS    ... if true, only static libraries are found
#   FFTW_ROOT               ... if set, the libraries are exclusively searched
#                               under this path
#   FFTW_LIBRARY            ... fftw library to use
#   FFTW_INCLUDE_DIR        ... fftw include directory
#   ENABLE_OPENMP_FFTW      ... if true, OpenMP specific libraries will be searched and used if possible
#   ENABLE_OPENMP           ... used as a default value of ENABLE_OPENMP_FFTW, if not defined
#

#If environment variable FFTWDIR is specified, it has same effect as FFTW_ROOT
if( NOT FFTW_ROOT AND ENV{FFTWDIR} )
  set( FFTW_ROOT $ENV{FFTWDIR} )
endif()

# Check if we can use PkgConfig
find_package(PkgConfig)

#Determine from PKG
if( PKG_CONFIG_FOUND AND NOT FFTW_ROOT )
  pkg_check_modules( PKG_FFTW QUIET "fftw3" )
endif()

#Check whether to search static or dynamic libs
set( CMAKE_FIND_LIBRARY_SUFFIXES_SAV ${CMAKE_FIND_LIBRARY_SUFFIXES} )

if( ${FFTW_USE_STATIC_LIBS} )
  set( CMAKE_FIND_LIBRARY_SUFFIXES ${CMAKE_STATIC_LIBRARY_SUFFIX} )
else()
  set( CMAKE_FIND_LIBRARY_SUFFIXES ${CMAKE_SHARED_LIBRARY_SUFFIX} )
endif()

if( FFTW_ROOT )

  #find libs
  find_library(
    FFTW_LIB
    NAMES "fftw3"
    PATHS ${FFTW_ROOT}
    PATH_SUFFIXES "lib" "lib64" "lib/x86_64-linux-gnu"
    NO_DEFAULT_PATH
  )

  find_library(
    FFTWF_LIB
    NAMES "fftw3f"
    PATHS ${FFTW_ROOT}
    PATH_SUFFIXES "lib" "lib64" "lib/x86_64-linux-gnu"
    NO_DEFAULT_PATH
  )

  find_library(
    FFTWL_LIB
    NAMES "fftw3l"
    PATHS ${FFTW_ROOT}
    PATH_SUFFIXES "lib" "lib64" "lib/x86_64-linux-gnu"
    NO_DEFAULT_PATH
  )

  find_library(
    FFTW_LIB_OMP
    NAMES "fftw3_omp"
    PATHS ${FFTW_ROOT}
    PATH_SUFFIXES "lib" "lib64" "lib/x86_64-linux-gnu"
    NO_DEFAULT_PATH
  )

  find_library(
    FFTWF_LIB_OMP
    NAMES "fftw3f_omp"
    PATHS ${FFTW_ROOT}
    PATH_SUFFIXES "lib" "lib64" "lib/x86_64-linux-gnu"
    NO_DEFAULT_PATH
  )


  #find includes
  find_path(
    FFTW_INCLUDES
    NAMES "fftw3.h"
    PATHS ${FFTW_ROOT}
    PATH_SUFFIXES "include"
    NO_DEFAULT_PATH
  )

else()

  find_library(
    FFTW_LIB
    NAMES "fftw3"
    PATHS ${PKG_FFTW_LIBRARY_DIRS} ${LIB_INSTALL_DIR}
  )

  find_library(
    FFTWF_LIB
    NAMES "fftw3f"
    PATHS ${PKG_FFTW_LIBRARY_DIRS} ${LIB_INSTALL_DIR}
  )

  find_library(
    FFTW_LIB_OMP
    NAMES "fftw3_omp"
    PATHS ${PKG_FFTW_LIBRARY_DIRS} ${LIB_INSTALL_DIR}
  )

  find_library(
    FFTWF_LIB_OMP
    NAMES "fftw3f_omp"
    PATHS ${PKG_FFTW_LIBRARY_DIRS} ${LIB_INSTALL_DIR}
  )

  find_library(
    FFTWL_LIB
    NAMES "fftw3l"
    PATHS ${PKG_FFTW_LIBRARY_DIRS} ${LIB_INSTALL_DIR}
  )

  find_path(
    FFTW_INCLUDES
    NAMES "fftw3.h"
    PATHS ${PKG_FFTW_INCLUDE_DIRS} ${INCLUDE_INSTALL_DIR}
  )

endif( FFTW_ROOT )

set(FFTW_LIBRARIES ${FFTW_LIB} ${FFTWF_LIB})

if(FFTWL_LIB)
  set(FFTW_LIBRARIES ${FFTW_LIBRARIES} ${FFTWL_LIB})
endif()

# we need ENABLE_OPENMP_FFTW
set (ENABLE_OPENMP_FFTW ON)
message(STATUS "ENABLE_OPENMP_FFTW is ${ENABLE_OPENMP_FFTW}")
# option (ENABLE_OPENMP_FFTW "Enable usage of OpenMP in FFTW, if possible" ${ENABLE_OPENMP})
# set (ENABLE_OPENMP_FFTW "Enable usage of OpenMP in FFTW, if possible" ${ENABLE_OPENMP})

if (FFTW_LIB_OMP AND ENABLE_OPENMP_FFTW)
  set(FFTW_LIBRARIES ${FFTW_LIB_OMP} ${FFTW_LIBRARIES})
endif()

if (FFTWF_LIB_OMP AND ENABLE_OPENMP_FFTW)
  set(FFTW_LIBRARIES ${FFTWF_LIB_OMP} ${FFTW_LIBRARIES})
endif()

if ( (FFTW_LIB_OMP OR FFTWF_LIB_OMP) AND ENABLE_OPENMP_FFTW )
  set(FFTW_OMP_PRESENT YES)
  message(STATUS "Usage of OpenMP is enabled in FFTW")
  if ( NOT FFTW_LIB_OMP OR NOT FFTWF_LIB_OMP )
      message (FATAL_ERROR "It looks like only single or double precision version of OpenMP FFTW extensions is present")
  endif()
else()
  set(FFTW_OMP_PRESENT NO)
  if (ENABLE_OPENMP_FFTW)
    message(STATUS "OpenMP-specific FFTW libraries are not found")
  else()
    message(STATUS "Usage of OpenMP is disabled in FFTW")
  endif()
endif()

set( CMAKE_FIND_LIBRARY_SUFFIXES ${CMAKE_FIND_LIBRARY_SUFFIXES_SAV} )

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(FFTW DEFAULT_MSG
                                  FFTW_INCLUDES FFTW_LIBRARIES)

mark_as_advanced(FFTW_INCLUDES FFTW_LIBRARIES FFTW_LIB FFTWF_LIB FFTWL_LIB FFTW_LIB_OMP FFTWF_LIB_OMP)
