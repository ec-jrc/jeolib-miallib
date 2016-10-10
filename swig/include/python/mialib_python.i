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

    im=_mialib.create_image(NumPyToImDataTypeCode(psArray.dtype),psArray.shape[0],psArray.shape[1],1)

    if _ConvertNumPyArrayToMIALibIMAGE(im, psArray) != NO_ERROR:
        return None

    return im

%}
