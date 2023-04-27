find_path( SHAPELIB_INCLUDE_DIR shapefil.h PATH_SUFFIXES libshp ${SHAPELIB_FIND_OPTS})
find_library( SHAPELIB_LIBRARY shp ${SHAPELIB_FIND_OPTS})
include( FindPackageHandleStandardArgs )
FIND_PACKAGE_HANDLE_STANDARD_ARGS( SHAPELIB SHAPELIB_INCLUDE_DIR SHAPELIB_LIBRARY )
set(SHAPELIB_LIBRARIES ${SHAPELIB_LIBRARY} )
set(SHAPELIB_INCLUDE_DIRS ${SHAPELIB_INCLUDE_DIR} )
