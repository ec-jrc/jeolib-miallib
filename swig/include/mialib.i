/* mial.i */

%include constraints.i



// %feature("docstring");

%define DOCSTRING
"This is an initial test module for the JIPL (Joint Image Processing Library)
developed in the framework of the JEODPP of the EO&SS@BD pilot project.
Emphasis is on morphological image analysis functionalities.
Contact: Pierre.Soille@jrc.ec.europa.eu"
%enddef

%feature("autodoc", "1");

// It consists of wrappers of C code underlying mialisp orginally developed
// by Pierre Soille over the years since 1988.

%module(docstring=DOCSTRING) mialib


// see https://stackoverflow.com/questions/11435102/is-there-a-good-way-to-produce-documentation-for-swig-interfaces
%import "../../../core/build/doc/xml/mial_doxy2swig.i"



%{
/* Put header files here or function declarations like below */
#include "mialib_swig.h"
#include "op.h"
%}




// See info on cpointers
// http://www.swig.org/Doc3.0/SWIGDocumentation.html#Library_nn3


//%include cpointer.i
//%pointer_functions(IMAGE,imagep)
//%pointer_functions(IMAGE *,imap)


// definition of array pointers
// for use, e.g., in writing multiband image
%include carrays.i
%array_functions(IMAGE *, imap)
%array_functions(int , intp) // used for example for box array
%array_functions(double , doublep)


// %nodefaultctor image;      // No default constructor 
// %nodefaultdtor image;      // No default destructor




// renaming:

/* imem.c */
/* %rename(imInfo) iminfo; */
/* %rename(imToArray) imtoarray; */
/* %rename(arrayToIm) arraytoim; */
/* %rename(setPixVal) setpixval; */
/* %rename(getPixVal) getpixval; */
/* %rename(createImArray) create_imarray; */

// rename the C declarations
// %rename("%(lowercamelcase)s", %$isfunction) ""; // foo_bar -> fooBar; FooBar -> fooBar


// %rename("nd_%s", regextarget=1, fullname=1) "IMAGE \*\(.*";



// new object with their constructor and destructor
//%newobject *IMAGE();
//%newobject *G_TYPE();

 //%typemap(newfree) IMAGE * "free_image($1);";

// 20160922
// define each mialib function returning a new IMAGE as a new object
// this triggers the setting of 'SWIG_POINTER_OWN' for the new IMAGE
// rather than '0' previously
// (note that for the destructor ~IMAGE() the setting is 'SWIG_POINTER_NEW')


%include mialib_newobjects.i


// 20160923
// define a typemap to handle IMAGE arrays as lists in Python
// needed to specify names to have multiple argument working
%typemap(in) (IMAGE **imap, int nc) {
  int i,dim;
  int res1;
  void *argp1 = 0 ;
  if (!PySequence_Check($input)) {
    PyErr_SetString(PyExc_ValueError,"Expected a sequence");
    return NULL;
  }
  dim=PySequence_Length($input);
  $2=dim;
  printf("coucou: dim=%d\n", dim);
  $1 = (IMAGE **) malloc(dim*sizeof(IMAGE **));
  for (i = 0; i < dim; i++) {
    PyObject *o = PySequence_GetItem($input,i);
    res1 = SWIG_ConvertPtr(o, &argp1,SWIGTYPE_p_IMAGE, 0 |  0 );
    if (SWIG_IsOK(res1)) {
      $1[i] = (IMAGE *) argp1;
    }
    else {
      PyErr_SetString(PyExc_ValueError,"Sequence elements must be IMAGE pointers");      
      free($1);
      return NULL;
    }
  }
 }

%typemap(out) IMAGE **rotatecoor {
  int i;
  int nc=2;
  IMAGE **imap=(IMAGE **)$1;
  $result = PyList_New(nc);
  PyObject * o = 0 ;
  for (i = 0; i < nc; i++) {
    o = SWIG_NewPointerObj(SWIG_as_voidptr(imap[i]), SWIGTYPE_p_IMAGE, SWIG_POINTER_OWN |  0 );
    PyList_SetItem($result,i,o);
  }
  free(imap);
 }

%typemap(out) IMAGE **imrgb2hsx {
  int i;
  int nc=2;
  IMAGE **imap=(IMAGE **)$1;
  $result = PyList_New(nc);
  PyObject * o = 0 ;
  for (i = 0; i < nc; i++) {
    o = SWIG_NewPointerObj(SWIG_as_voidptr(imap[i]), SWIGTYPE_p_IMAGE, SWIG_POINTER_OWN |  0 );
    PyList_SetItem($result,i,o);
  }
  free(imap);
 }

%typemap(out) IMAGE **PartitionSimilarity {
  int i;
  int nc=4;
  IMAGE **imap=(IMAGE **)$1;
  $result = PyList_New(nc);
  PyObject * o = 0 ;
  for (i = 0; i < nc; i++) {
    o = SWIG_NewPointerObj(SWIG_as_voidptr(imap[i]), SWIGTYPE_p_IMAGE, SWIG_POINTER_OWN |  0 );
    PyList_SetItem($result,i,o);
  }
  free(imap);
 }

%typemap(out) IMAGE **alphatree {
  int i;
  int nc=5;
  IMAGE **imap=(IMAGE **)$1;
  $result = PyList_New(nc);
  PyObject * o = 0 ;
  for (i = 0; i < nc; i++) {
    o = SWIG_NewPointerObj(SWIG_as_voidptr(imap[i]), SWIGTYPE_p_IMAGE, SWIG_POINTER_OWN |  0 );
    PyList_SetItem($result,i,o);
  }
  free(imap);
 }




// These are the headers with the declarations that will be warped
// It needs to be inserted before the extend declaration
%include "mialib_swig.h"
%include "op.h"

 // 20160922
 // Allow for automatic garbage collection (no need to patch)
%extend IMAGE {             // Attach these functions to struct IMAGE
  IMAGE(int type, long int nx, int ny, int nz) {
    return create_image(type, nx,ny,nz);
  }
  ~IMAGE() {
    free_image($self);
  }
  void iminfoMethod() {
    iminfo($self);
  }
};

%typemap(newfree) IMAGE * {
  delete $1;
}

%typemap(newfree) IMAGE ** {
  delete $1;
}


// We still need to deal with IMAGE **






// Addtional code for IMAGE<->NumPy array conversions [20160729]
// adapted from gdal_array.i


%init %{
  print_mia_banner();
  import_array();
%}



#if defined(SWIGPYTHON)
%include "mialib_python.i"
#endif
