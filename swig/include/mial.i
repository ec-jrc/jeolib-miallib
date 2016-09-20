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

%module(docstring=DOCSTRING) mialmodule


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
%rename(imInfo) iminfo;
%rename(imToArray) imtoarray;
%rename(arrayToIm) arraytoim;
%rename(setPixVal) setpixval;
%rename(getPixVal) getpixval;
%rename(createImArray) create_imarray;

// rename the C declarations
// %rename("%(lowercamelcase)s", %$isfunction) ""; // foo_bar -> fooBar; FooBar -> fooBar


%rename("nd_%s", regextarget=1, fullname=1) "IMAGE \*\(.*";




// These are the headers with the declarations that will be warped
%include "mialib_swig.h"
%include "op.h"

// new object with their constructor and destructor
%newobject *IMAGE();
%newobject *G_TYPE();

 

// Addtional code for IMAGE<->NumPy array conversions [20160729]
// adapted from gdal_array.i


%init %{
  print_mia_banner();
  import_array();
%}



#if defined(SWIGPYTHON)
%include "mial_python.i"
#endif
