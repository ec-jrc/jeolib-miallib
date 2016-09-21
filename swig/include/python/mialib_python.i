# python specific interface file







// provide support for numpy array with mial


%{
  //#define NPY_NO_DEPRECATED_API NPY_1_7_API_VERSION
#include "Python.h"
#include "numpy/arrayobject.h"
%}

%typemap(in,numinputs=1) (PyArrayObject *psArray)
{
  /* %typemap(in,numinputs=1) (PyArrayObject  *psArray) */
  if ($input != NULL && PyArray_Check($input))
  {
      $1 = (PyArrayObject*)($input);
  }
  else
  {
      PyErr_SetString(PyExc_TypeError, "not a numpy array");
      SWIG_fail;
  }
}


%inline %{
  ERROR_TYPE RasterIOMIALib( IMAGE* im, PyArrayObject *psArray) {
    psArray->data = memcpy((void *)(psArray->data), (void *)GetImPtr(im), GetImNx(im)*GetImNy(im)*(GetImBitPerPixel(im)/8) );
    return NO_ERROR;
  }

  ERROR_TYPE _ConvertNumPyArrayToMIALibIMAGE( IMAGE *im, PyArrayObject *psArray) {
    im->p_im=memcpy( (void *)GetImPtr(im), (void *)(psArray->data), GetImNx(im)*GetImNy(im)*(GetImBitPerPixel(im)/8));
    return NO_ERROR; 
  }
%}

%pythoncode %{
import numpy

t_UCHAR    =  3
t_SHORT    =  4
t_USHORT   =  5
t_INT32    =  6
t_UINT32   =  7
t_INT64    =  8
t_UINT64   =  9
t_FLOAT    = 10
t_MIAFLOAT = 10
t_DOUBLE   = 11

def ImDataToNumPyTypeCode(IMAGE_data_type):
    """Converts a given MIALib image data  type code into the correspondent
   numpy array data type code."""
    if IMAGE_data_type == t_UCHAR:
      return numpy.uint8
    elif IMAGE_data_type == t_USHORT:
      return numpy.uint16
    elif IMAGE_data_type == t_SHORT:
      return numpy.int16
    elif IMAGE_data_type == t_UINT32:
      return numpy.uint32
    elif IMAGE_data_type == t_INT32:
      return numpy.int32
    elif IMAGE_data_type == t_UINT64:
      return numpy.uint64
    elif IMAGE_data_type == t_INT64:
      return numpy.int64
    elif IMAGE_data_type == t_FLOAT:
      return numpy.float32
    elif IMAGE_data_type == t_DOUBLE:
      return numpy.float64
    else:
        raise TypeError("provided IMAGE_data_type with available IMAGE data types")

def NumPyToImDataTypeCode(numeric_type):
    """Converts a given numpy array data type code into the correspondent
    MIALib image data type code."""
    if not isinstance(numeric_type, (numpy.dtype,type)):
        raise TypeError("Input must be a valid numpy Array data type")

    if numeric_type == numpy.uint8:
      return t_UCHAR
    elif numeric_type == numpy.uint16:
      return t_USHORT
    elif numeric_type == numpy.int16:
      return t_SHORT
    elif numeric_type == numpy.uint32:
      return t_UINT32
    elif numeric_type == numpy.int32:
      return t_INT32
    elif numeric_type == numpy.uint64:
      return t_UINT64
    elif numeric_type == numpy.int64:
      return t_INT64
    elif numeric_type == numpy.float32:
      return t_FLOAT
    elif numeric_type == numpy.float64:
      return t_DOUBLE
    else:
        raise TypeError("provided numeric_type not compatible with available IMAGE data types")

def ImDataToNumPyTypeCode(ImDataType):
    """Returns the numpy Array data type code matching a given an MIALib
    image data type."""
    if not isinstance(ImDataType, int):
        raise TypeError("Input must be an integer value")

    if ImDataType == t_UCHAR:
        return numpy.uint8
    elif ImDataType == t_USHORT:
        return numpy.uint16
    elif ImDataType == t_SHORT:
        return numpy.int16
    elif ImDataType == t_UINT32:
        return numpy.uint32
    elif ImDataType == t_INT32:
        return numpy.int32
    elif ImDataType == t_UINT64:
        return numpy.uint64
    elif ImDataType == t_INT64:
        return numpy.int64
    elif ImDataType == t_FLOAT:
        return numpy.float32
    elif ImDataType == t_DOUBLE:
        return numpy.float64
    else:
        return None

def ConvertToNumPyArray( im ):
    """Pure python implementation of converting a MIALib image
    into a numpy array.  Data values are copied!"""

    buf_obj = numpy.empty([im.ny,im.nx], dtype = ImDataToNumPyTypeCode(im.DataType))

    if RasterIOMIALib(im, buf_obj) != NO_ERROR:
       return None

    return buf_obj

def ConvertNumPyArrayToMIALibImage( psArray ):
    """Pure python implementation of converting a numpy array into
    a MIALib image.  Data values are copied!"""

    im=_mialmodule.create_image(NumPyToImDataTypeCode(psArray.dtype),psArray.shape[0],psArray.shape[1],1)

    if _ConvertNumPyArrayToMIALibIMAGE(im, psArray) != NO_ERROR:
        return None

    return im

%}

%pythoncode %{

def MyGDALRead(fn, band_number=0):
    """Read a specific band_number from a file name

    Keyword arguments:
    fn -- string for file name with path
    band_number -- integer indicating band_number (default is 0)
    """
    return jipl.GDALRead(fn, band_number, 0, 0, getnx(fn), getny(fn), getnx(fn), getny(fn))
    


def writeGeofnTiffOneStripPerLine(i0, fn, ref_fn):
    jipl.writeGeoTiffOneStripPerLine(i0, fn, getepsg(ref_fn), getulcx(ref_fn), getulcy(ref_fn), getscale(ref_fn), 1, 0, 0, 0, "")


def writeMBGeofnTiffOneStripPerLine(imarray, n, fn, ref_fn):
    jipl.writeMBGeoTiffOneStripPerLine(rgb, n, fn, getepsg(ref_fn), getulcx(ref_fn), getulcy(ref_fn), getscale(ref_fn), 1, 0, 0, 0, "")

def getnx(fn):
    gdi=jipl.GDALInfoJIP(fn)
    if gdi:
        return int(jipl.getpixval(gdi, 6).d_val)
        return None

def getny(fn):
    gdi=jipl.GDALInfoJIP(fn)
    if gdi:
        return int(jipl.getpixval(gdi, 7).d_val)
        return None

def getulcx(fn):
    gdi=jipl.GDALInfoJIP(fn)
    if gdi:
        return jipl.getpixval(gdi, 0).d_val
        return None

def getulcy(fn):
    gdi=jipl.GDALInfoJIP(fn)
    if gdi:
        return jipl.getpixval(gdi, 3).d_val
        return None

def getepsg(fn):
    gdi=jipl.GDALInfoJIP(fn)
    if gdi:
        return int(jipl.getpixval(gdi, 9).d_val)
        return None

def getscale(fn):
    gdi=jipl.GDALInfoJIP(fn)
    if gdi:
        return jipl.getpixval(gdi, 1).d_val
        return None
%}


