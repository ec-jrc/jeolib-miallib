/* created by P. Soille, January 1995 (superseeds GRILISP) */

#include <stdlib.h>
#include <string.h>
#include "xlisp.h"

#ifdef MIAL

struct apoint { double u, v; };

#include "xlglue1.h"    /* external declarations of mialib functions */
#ifdef MARCIN
#include "xlglue1_marcin.h"
#endif
#ifdef GRAZZJA
#include "xlglue1_grazzja.h"
#include "mialib_grazzja.h"
#endif 

/****************************************************************/
#ifdef SAVE_LIST_IMAGES  /* Save list of output multispectral images */
#undef SAVE_LIST_IMAGES
#endif
#define SAVE_LIST_IMAGES( imarray, xlim, result, n, m ) \
  xlstkcheck(n+1); /* n? */                             \
  xlsave(result);                                       \
  for( m=n-1; m>=0; m-- ) {                             \
    xlsave(xlim);                                       \
    xlim = cvimage(imarray[m]);                         \
    result = cons (xlim, result);                       \
  }                                                     \
  xlpopn(n+1); /* n? */                                 



#define maxNumberOfBands 255

extern LVAL cvimage _((IMAGE *im));
extern LVAL s_true;	/*	True Lisp	*/
extern long long total;	/*	Memory counter	*/


/* new Xlisp function for getting a G_TYPE value */
G_TYPE getgenericnum(type)
int type;
{
  G_TYPE gval;

  if (type == t_UCHAR){
    gval.uc_val = (UCHAR) getfixnum(xlgafixnum());
    gval.generic_val = gval.uc_val;
  }
  else if  (type == t_USHORT){
    gval.us_val  = (USHORT) getfixnum(xlgafixnum());
  }
  else if (type == t_SHORT){
    gval.s_val = (SHORT) getfixnum(xlgafixnum());
  }
  else if (type == t_INT32){
    gval.i32_val = (INT32) getfixnum(xlgafixnum());
  }
  else if (type == t_UINT32){ /* GLOUP (DNW): UINT32 are BIGNUM and not FIXNUM in xlisp */
    gval.u32_val = (UINT32) getfixnum(xlgafixnum());
  }
  else if (type == t_FLOAT){
    gval.f_val = (MIAFLOAT) getflonum(xlgaflonum());
  }
  else if (type == t_DOUBLE){
    gval.d_val = (DOUBLE) getflonum(xlgaflonum());
  }
  else
    xlabort("invalid type in getgenericnum");
  return(gval);
}
LVAL itermbeep()
{
  printf("\a");
/*
  \lspfunction{}{beep}{}
  \desc{alert the terminal user by sounding an audible alarm on the terminal (or flashing the screen if not possible).}
*/
  return s_true;
}

/* 
\module{Input/Output}\label{m.io} % Mostly in imio.c
\basesection
*/
LVAL ireadall()
{
  char *fn;
  int nx, ny, nz, data_type, header_size, pc;
  if (!moreargs())
    xlabort("(*readall fn nx ny nz data_type header_size &opt (pc 1))\n data_type = t_TIFFONEBITPERPIXEL (line byte padd), t_ONEBITPERPIXEL (line word padd), t_FOURBITPERPIXEL (line byte padd), t_CHAR, t_UCHAR, etc.");

/*
  \lspfunction{*}{readall}{fn nx ny nz data_type header_size &opt (pc 1)}
  \param{fn}{a string for the name of a file (possibly including its path)}
  \param{nx}{integer for number of columns}
  \param{ny}{integer for number of lines}
  \param{nz}{integer for number of x-y planes}
  \param{data_type}{image data type (t_UCHAR, t_USHORT, t_INT32, t_FLOAT, or t_DOUBLE)}
  \param{header_size}{number of bytes at the beginning of the file before image data}
  \param{pc}{optional number for planar configuration (1 or pc_bip, 2 or pc_bsq, and 3 or pc_bil) default equals to 1 (i.e., pc_bip).}
  \return{a new image node containing the read image or NIL in case an error occured}
  \desc{reads nx*ny*nz pixel values stored in the binary file fn assuming that their type matches data_type and that they are stored in lexicographical order after skipping a header of header_size bytes and assuming a planar configuration of type pc.  To extract a specific x-y plane, use the function *getxyplane.}
  \cfunction{\cfreadUDall}
  \cfile{imio.c}
  \example{(*readall "landsat.bil" 512 1024 6 t_UCHAR 0 pc_bil)}{}
*/
  fn = (char *)getstring(xlgetfname());
  nx = (int)getfixnum(xlgafixnum());
  ny = (int)getfixnum(xlgafixnum());
  nz = (int)getfixnum(xlgafixnum());
  data_type   = (int)getfixnum(xlgafixnum());
  header_size = (int)getfixnum(xlgafixnum());
  if (!moreargs())
    pc=1;
  else
    pc = (int)getfixnum(xlgafixnum());
  
  xllastarg();

  return(cvimage(read_all(fn, nx, ny, nz, data_type, header_size, pc)));
}

LVAL ireadimage()
{
  char *fn;
  if (!moreargs())
    xlabort("(*readimage fn)\n fn Must be a Tiff, Visilog, or Kiff image file");

/*
  \lspfunction{*}{readimage}{fn}
  \param{fn}{a string for the name of a TIFF file (possibly including its path)}
  \return{a new image node containing the read image or NIL in case an error occured}
  \desc{reads a \htmladdnormallink{TIFF}{http://www.libtiff.org/} image stored in the filename specified by fn.}
  \cfunction{\cfreadUDimage}
  \cfile{imio.c}
*/

  fn = (char *)getstring(xlgetfname());

  xllastarg();

  return(cvimage(read_image(fn)));
}

LVAL ireadcharimage2ushort()
{
  char *fn;
  int data_type=5;
  if (!moreargs())
    xlabort("(*readcharimage2ushort fn)\n fn Must be a Tiff file with one strip per line");

/*
  \lspfunction{*}{readcharimage2ushort}{fn}
  \param{fn}{a string for the name of a TIFF file (possibly including its path)}
  \return{a new image node containing the read image casted to USHORT or NIL in case an error occured}
  \desc{reads a \htmladdnormallink{TIFF}{http://www.libtiff.org/} image stored in the filename specified by fn.}
  \cfunction{\cfreadUDimageUDtoUDtype}
  \cfile{imio.c}
*/

  fn = (char *)getstring(xlgetfname());

  xllastarg();

  return(cvimage(read_image_to_type(fn, data_type)));
}

LVAL itiffinfo()
{
  float a_float32;
  char *fn, *fieldn;
  if (!moreargs())
    xlabort("(*tiffinfo fn fieldn)\n fn Must be a Tiff image file");

/*
  \lspfunction{*}{tiffinfo}{fn fieldn}
  \param{fn}{a string for the name of a TIFF file (possibly including its path)}
  \param{fieldn}{a string for the TIFF field name}
  \return{float holding the value of the specified field}
  \desc{the following field names are valid: TIFFTAG_IMAGEWIDTH, TIFFTAG_IMAGELENGTH, TIFFTAG_BITSPERSAMPLE, TIFFTAG_SAMPLESPERPIXEL, TIFFTAG_SAMPLEFORMAT (1 for unsigned integer data, 2 for two's complement signed integer data, and 3 for floating point data), GTModelTypeGeoKey (1 for projected, 2 for geographic, and 3 for geocentric), GeographicTypeGeoKey, ProjectedCSTypeGeoKey, and GTRasterTypeGeoKey (1 for RasterPixelIsArea).}
  \cfunction{\cftiffinfo}
  \cfile{imio.c}
*/

  fn = (char *)getstring(xlgetfname());
  fieldn = (char *)getstring(xlgetfname());

  xllastarg();
  if (tiffinfo(fn,fieldn,&a_float32)==ERROR)
    return NIL;

  return(cvfixnum((int)a_float32)); // CHECK was cvflonum
}
LVAL iGetTIFFTagGeo()
{
  char *fn, *tagname;
  if (!moreargs())
    xlabort("(*GetTIFFTagGeo fn tagname)\n fn Must be a TIFF image file holding the required tag");

/*
  \lspfunction{*}{GetTIFFTagGeo}{fn tagname}
  \param{fn}{a string for the name of a TIFF file (possibly including its path)}
  \param{tagname}{a string for the geographic tag name (either "TIFFTAG_GEOTIEPOINTS", "TIFFTAG_GEOPIXELSCALE", or "TIFFTAG_GEOTRANSMATRIX")}
  \return{an image of type t_DOUBLE holding the values of the corresponding geographic tag if available, NIL otherwise}
  \desc{\begin{itemize} \item For TIFFTAG_GEOTIEPOINTS, the pixel values of the returned 1-D image correspond to a series of hexlets of the form I,J,K,X,Y,Z where (I,J,K) is the point at location (I,J) in raster space with pixel value K and (X,Y,Z) is the coordinate of this point in the associated coordinate reference system. \item For TIFFTAG_GEOPIXELSCALE, the 3 pixel values of the returned 1-D image correspond to the scale in x, y, and z respecively. \item For TIFFTAG_GEOTRANSMATRIX, the 16 pixel values of the returned 1-D image correspond to the successive values of the transformation matrix. \end{itemize}}
  \cfunction{\cfGetTIFFTagGeo}
  \cfile{imio.c}
*/
  
  fn = (char *)getstring(xlgetfname());
  tagname = (char *)getstring(xlgetfname());
  xllastarg();
  return(cvimage(GetTIFFTagGeo(fn, tagname)));
}


LVAL iGetGeoKey()
{
  char *fn, *keyname;
  if (!moreargs())
    xlabort("(*GetGeoKey fn keyname)\n fn Must be a GTIFF image file holding the required key");

/*
  \lspfunction{*}{GetGeoKey}{fn keyname}
  \param{fn}{a string for the name of a GTIFF file (possibly including its path)}
  \param{tagname}{a string for the geographic key name "GTModelTypeGeoKey"}
  \return{an image of type t_INT32 holding the values of the corresponding geographic key if available, NIL otherwise}
  \desc{}
  \cfunction{\cfGetGeoKey}
  \cfile{imio.c}
*/
  
  fn = (char *)getstring(xlgetfname());
  keyname = (char *)getstring(xlgetfname());
  xllastarg();
  return(cvimage(GetGeoKey(fn, keyname)));
}

#ifdef TEST2
LVAL ireadimage2()
{
  char *fn;
  int x, y, szx, szy, scale;
  if (!moreargs())
    xlabort("(*readimage2 fn x y szx szy scale)\n fn Must be a Tiff image file");
/*
  \lspfunction{*}{readimage2}{fn x y szx szy scale}
  \param{fn}{a string for the name of a TIFF file (possibly including its path)}
  \param{x}{integer for x-coordinate of upper left pixel}
  \param{y}{integer for y-coordinate of upper left pixel}
  \param{szx}{number of pixels to read along x-axis}
  \param{szy}{number of pixels to read along y-axis}
  \param{scale}{integer to specify that one out of scale pixels are to be read}
  \desc{reads a \htmladdnormallink{TIFF}{http://www.libtiff.org/} image from file fn and from x-y coordinates (x,y) while reading every scale successive pixel until szx times szy pixels are read.  This is of interest for reading portions of a large image on a computer with little random access memory.  Compressed TIFF files cannot be read by this routine.  In addition, the byte order of the stored file must match that of the machine running the programme and all pixel values must be stored one after each other in one block.}
  \cfunction{\cfreadUDimageTWO}
  \cfile{imio2.c}
  \example{(*readimage2 "tm1.tif" 5 5 100 10 2)}{returns a 100 times 10 image by reading image tm1.tif from coordinate (5,5) while skeeping 1 pixel out of two.}
*/
  fn = (char *)getstring(xlgetfname());
  x = (int)getfixnum(xlgafixnum());
  y = (int)getfixnum(xlgafixnum());
  szx = (int)getfixnum(xlgafixnum());
  szy = (int)getfixnum(xlgafixnum());
  scale = (int)getfixnum(xlgafixnum());

  xllastarg();

  return(cvimage(read_image2(fn, x, y, szx, szy, scale)));
}
#endif


LVAL ireadTiffSubset()
{
  char *fn;
  int x, y;
  unsigned szx, szy;
  if (!moreargs())
    xlabort("(*readtiffsubset fn x y szx szy)\n fn Must be a Tiff image file");
/*
  \lspfunction{*}{readtiffsubset}{fn x y szx szy}
  \param{fn}{a string for the name of a TIFF file (possibly including its path)}
  \param{x}{integer for x-coordinate of upper left pixel}
  \param{y}{integer for y-coordinate of upper left pixel}
  \param{szx}{number of pixels to read along x-axis}
  \param{szy}{number of pixels to read along y-axis}
  \desc{reads a szx times szy rectangular area of a \htmladdnormallink{TIFF}{http://www.libtiff.org/} image from file fn and from x-y coordinates (x,y).  This is of interest for reading portions of a large image on a computer with little random access memory.}
  \cfunction{\cfreadTiffSubset}
  \cfile{imio2.c}
  \example{(*readtiffsubset "tm1.tif" 5 5 100 10)}{returns a 100 times 10 image by reading image tm1.tif from coordinate (5,5).}
*/
  fn = (char *)getstring(xlgetfname());
  x = (int)getfixnum(xlgafixnum());
  y = (int)getfixnum(xlgafixnum());
  szx = (unsigned int)getfixnum(xlgafixnum());
  szy = (unsigned int)getfixnum(xlgafixnum());
  xllastarg();
  return(cvimage(readTiffSubset(fn, x, y, szx, szy)));
}

LVAL iGDALInfo()
{
  char *fn;
  if (!moreargs())
    xlabort("(*GDALInfo fn)\n the file format of fn must be recognised by GDAL");

/*
  \lspfunction{*}{GDALInfo}{fn}
  \param{fn}{a string for the name of valid file recognised by GDAL}
  \return{an image of type t_DOUBLE holding 10 values regarding the geolocation and size of the image, NIL otherwise}
  \desc{The values are in the following order: x-coordinate of upper left corner of upper left pixel, W-E pixel resolution, rotation (0 if image is north up), x-coordinate of upper left corner of upper left pixele, rotation (0 if image is north up), N-S pixel resolution, number of rows of raster, number of columns of raster, number of bands of raster, and EPSG code of projection (-1 if it could not be retrieved).  The function also prints in stdout the type of driver used for reading fn and the name of the projection.}
  \seealso{*getgeotiffulc}
  \cfunction{\cfGDALInfoJIP}
  \cfile{imio_gdal.c}
  \creationdate{20130911}
*/
  
  fn = (char *)getstring(xlgetfname());
  xllastarg();
  return(cvimage(GDALInfoJIP(fn)));
}

LVAL iGDALRead()
{
  char *fn;
  LVAL temp;          /* key arguments */
  int band, nXOff=-1, nYOff=-1, nXSize=0, nYSize=0;
  int nBufXSize=0, nBufYSize=0;
  if (!moreargs())
    xlabort("(*GDALRead fn band &opt nXOff nYOff nXSize nYSize &key nBufXSize nBufYSize)); where fn is an image file recognised by GDAL as such\n");

/*
  \lspfunction{*}{GDALRead}{fn band nXOff nYOff nXSize nYSize &key (nBufXSize nXSize) (nBufYSize nYSize)}
  \param{fn}{a string for the name of an image file (possibly including its path)}
  \param{band}{an integer for the band number, 0 for first band}
  \param{nXOff}{integer for the pixel offset to the top left corner of the region of the band to be accessed.  This would be zero to start from the left side (default value).}
  \param{nYOff}{integer for the line offset to the top left corner of the region of the band to be accessed.  This would be zero to start from the top (default value).}
  \param{nXSize}{integer for the width of the region of the band to be accessed in pixels.}
  \param{nYSize}{integer for the height of the region of the band to be accessed in lines.}
  \param{nBufXSize}{integer for the number of columns of output image (default is nXSize).}
  \param{nBufYSize}{integer for the number of lines of output image (default is nYSize).}
  \return{a new image node containing the read image or NIL in case an error occured.}
  \desc{reads a \htmladdnormallink{GDAL}{http://www.gdal.org/} compatible image stored in the filename specified by fn.  This function is a wrapper to the function GDALRasterIO.}
  \cfunction{\cfGDALRead}
  \cfile{imio.c}
*/

  fn = (char *)getstring(xlgetfname());
  band = (int)getfixnum(xlgafixnum());
  
  if (moreargs()){
    if (fixp(*xlargv)){
      nXOff  = (int)getfixnum(xlgafixnum());
      nYOff  = (int)getfixnum(xlgafixnum());
      nXSize = (int)getfixnum(xlgafixnum());
      nYSize = (int)getfixnum(xlgafixnum());
    }
    if (xlgetkeyarg(kl_nBufXSize, &temp) && (temp != NIL)){
      nBufXSize = getfixnum(temp);
    }
    if (xlgetkeyarg(kl_nBufYSize, &temp) && (temp != NIL)){
      nBufYSize = getfixnum(temp);
    }
  }
  xllastarg();

  return(cvimage(GDALRead(fn, band, nXOff, nYOff, nXSize, nYSize, nBufXSize, nBufYSize)));
}

LVAL iwrite_tiff()
{
  LVAL xlim1;
  char *fn;
  if (!moreargs())
    xlabort("(*writetiff im fn)");
/*
  \lspfunction{*}{writetiff}{im fn}
  \param{im}{an image node}
  \param{fn}{a file name (possibly including a path)}
  \return{true if success, NIL otherwise}
  \desc{writes image im on disk as a \htmladdnormallink{TIFF}{http://www.libtiff.org/} file.  Beware that if the files already exists, it will be overwritten.}
  \cfunction{\cfwriteUDtiff}
  \cfile{imio.c}
*/

  xlim1 = xlgaimage();
  fn = (char *)getstring(xlgetfname());
  
  xllastarg();

  if (write_tiff((IMAGE *)getimage(xlim1), fn) == ERROR)
    return(NIL);
  return(s_true);
}

LVAL iwriteTiffOneStripPerLine()
{
  LVAL xlim1;
  char *fn, *desc=NULL;
  if (!moreargs())
    xlabort("(*writetiffospl im fn &opt desc)");
/*
  \lspfunction{*}{writetiffospl}{im fn}
  \param{im}{an image node}
  \param{fn}{a file name (possibly including a path)}
  \param{desc}{optional string for description}
  \return{true if success, NIL otherwise}
  \desc{writes image im on disk as a \htmladdnormallink{TIFF}{http://www.libtiff.org/} file.  The image is stored in strips with a single line per strip, using LZW compression with horizontal differencing.  The number of planes of the input image must be equal to 1.  Beware that if the files already exists, it will be overwritten.}
  \cfunction{\cfwriteTiffOneStripPerLine}
  \cfile{imio2.c}
*/

  xlim1 = xlgaimage();
  fn = (char *)getstring(xlgetfname());
  if (moreargs())
    desc = (char *)getstring(xlgetfname());

  xllastarg();

  if (writeTiffOneStripPerLine((IMAGE *)getimage(xlim1), fn, desc) == ERROR)
    return(NIL);
  return(s_true);
}

LVAL iwriteGeoTiffOneStripPerLine()
{
  LVAL xlim1;
  LVAL temp;          /* key arguments */
  char *fn;
  int PCSCode;
  double ulcx, ulcy, scale;
  unsigned short RasterType=1;
  int nodata_flag=0, nodata_val=0;
  int metadata_flag=0;
  char *metadata_str=NULL;
  if (!moreargs())
    xlabort("(*writegeotiffospl im fn PCSCode ulcx ulcy scale &opt (RasterType 1) &key (nodata 'nil) (metadata 'nil))");
/*
  \lspfunction{*}{writegeotiffospl}{im fn PCSCode ulcx ulcy scale &opt (RasterType 1)}
  \param{im}{an image node}
  \param{fn}{a file name (possibly including a path)}
  \param{PCSCode}{integer for Projection Coordinate System Code (e.g., 3035 for ETRS-LAEA, 326zz (resp. 327zz) where zz is UTM Northern (resp. Southern) zone number), 65535 being reserved for geographic latitude-longitude System in WGS84)}
  \param{ulcx}{float for map x-coordinate of upper left pixel corner (or point).  Units are metres for projected and degrees for geographic data.}
  \param{ulcy}{float for map y-coordinate (or latitude) of upper left pixel corner (or point).  Units are metres for projected and degrees for unprojected data.}
  \param{scale}{float for width of pixel in metres (projected) or degrees (unprojected)}
  \param{RasterType}{1 (default) for PixelIsArea, 2 for PixelIsPoint}
  \param{nodata}{key with integer value for nodata value (same for all bands)}
  \param{metadata}{key with string containing metadata.  Recommended XML format, see \url{http://www.awaresystems.be/imaging/tiff/tifftags/gdal_metadata.html}.}
  \return{true if success, NIL otherwise}
  \desc{writes image im on disk as a \htmladdnormallink{GEOTIFF}{http://www.remotesensing.org/geotiff} file.  The image is stored in strips with a single line per strip, using LZW compression with horizontal differencing.  The number of planes of the input image must be equal to 1.  Beware that if the files already exists, it will be overwritten.}
  \cfunction{\cfwriteGeoTiffOneStripPerLine}
  \cfile{imio2.c}
*/

  xlim1 = xlgaimage();
  
  fn = (char *)getstring(xlgetfname());
  PCSCode = getfixnum(xlgafixnum());
  ulcx = (DOUBLE) getflonum(xlgaflonum());
  ulcy = (DOUBLE) getflonum(xlgaflonum());
  scale = (DOUBLE) getflonum(xlgaflonum());

  if (moreargs()){
    if (fixp(*xlargv)){
      RasterType = getfixnum(xlgafixnum());
      if (RasterType > 2 || RasterType < 1 )
	xlabort("invalid RasterType (1 PixelIsArea or 2 for PixelIsPoint)");
    }
    if (xlgetkeyarg(kl_metadata, &temp) && (temp != NIL)){
      metadata_flag=1;
      metadata_str= (char *)getstring(temp);
    }
    if (xlgetkeyarg(kl_nodata,&temp) && (temp != NIL)){
      nodata_flag=1;
      nodata_val=getfixnum(temp);
    }
  }
    
  xllastarg();   /* Last argument */

  if (writeGeoTiffOneStripPerLine((IMAGE *)getimage(xlim1), fn, PCSCode, ulcx, ulcy, scale, RasterType, nodata_flag, nodata_val, metadata_flag, metadata_str) == ERROR)
    return(NIL);
  return(s_true);
}

LVAL iwriteMBGeoTiffOneStripPerLine()
{
  int n=0;
  char *fn;
  int PCSCode;
  double ulcx, ulcy, scale;
  unsigned short RasterType=1;
  IMAGE *imarray[maxNumberOfBands];
  LVAL arg, arg2;
  LVAL temp;          /* key arguments */
  int nodata_flag=0, nodata_val=0;
  int metadata_flag=0;
  char *metadata_str=NULL;
  if (!moreargs())
    xlabort("(*writembgeotiffospl imlist fn PCSCode ulcx ulcy scale &opt (RasterType 1)) &key (nodata 'nil) (metadata 'nil))");
/*
  \lspfunction{*}{writembgeotiffospl}{imlist fn PCSCode ulcx ulcy scale &opt (RasterType 1)}
  \param{imlist}{a list of 2-D images having all the same size and pixel data type}
  \param{fn}{a file name (possibly including a path)}
  \param{PCSCode}{integer for Projection Coordinate System Code (e.g., 3035 for ETRS-LAEA, 326zz (resp. 327zz) where zz is UTM Northern (resp. Southern) zone number), 65535 being reserved for geographic latitude-longitude System in WGS84}
  \param{ulcx}{float for map x-coordinate of upper left pixel corner (or point).  Units are metres for projected and degrees for geographic data.}
  \param{ulcy}{float for map y-coordinate (or latitude) of upper left pixel corner (or point).  Units are metres for projected and degrees for unprojected data.}
  \param{scale}{float for width of pixel in metres (projected) or degrees (unprojected)}
  \param{RasterType}{1 (default) for PixelIsArea, 2 for PixelIsPoint}
  \param{nodata}{key with integer value for nodata value (same for all bands)}
  \param{metadata}{key with string containing metadata.  Recommended XML format, see \url{http://www.awaresystems.be/imaging/tiff/tifftags/gdal_metadata.html}.}
  \return{true if success, NIL otherwise}
  \desc{consider the successive elements of the image list as the successive bands of a multiband image and writes this multiband image on disk as a \htmladdnormallink{GEOTIFF}{http://www.remotesensing.org/geotiff} file.  The image is stored in strips with a single line per strip, using LZW compression with horizontal differencing.  Beware that if the files already exists, it will be overwritten.}
  \cfunction{\cfwriteGeoTiffOneStripPerLine}
  \cfile{imio2.c}
*/
  arg=xlgetarg();
  if (listp(arg)){
    for (n = 0; consp(arg); arg = cdr(arg)){
      arg2 = car(arg);           
      if (imagep(arg2)){
	imarray[n++]=(IMAGE*)getimage(arg2);
        if (n>1){
	  if (GetImNByte(imarray[n-2])!=GetImNByte(imarray[n-1]))
	    xlabort("(*writeMBGeoTiffOneStripPerLine imlist fn PCSCode ulcx ulcy scale &opt (RasterType 1)): all images in imlist must have the same number of bytes!!!");
	}
      }
      else{                      
	xlbadtype(arg2);          
	break;                    
      }                           
    }
  }
  else
    xlbadtype(arg);
  fn = (char *)getstring(xlgetfname());
  PCSCode = getfixnum(xlgafixnum());
  ulcx = (DOUBLE) getflonum(xlgaflonum());
  ulcy = (DOUBLE) getflonum(xlgaflonum());
  scale = (DOUBLE) getflonum(xlgaflonum());

  if (moreargs()){
    if (fixp(*xlargv)){
      RasterType = getfixnum(xlgafixnum());
      if (RasterType > 2 || RasterType < 1 )
	xlabort("invalid RasterType (1 PixelIsArea or 2 for PixelIsPoint)");
    }
    if (xlgetkeyarg(kl_metadata, &temp) && (temp != NIL)){
      metadata_flag=1;
      metadata_str= (char *)getstring(temp);
    }
    if (xlgetkeyarg(kl_nodata,&temp) && (temp != NIL)){
      nodata_flag=1;
      nodata_val=getfixnum(temp);
    }
  }  
  xllastarg(); /* Last argument */

  if (writeMBGeoTiffOneStripPerLine(imarray, n, fn, PCSCode, ulcx, ulcy, scale, RasterType, nodata_flag, nodata_val, metadata_flag, metadata_str) == ERROR)
    return(NIL);
  return(s_true);
}

LVAL iwrite_image_data()
{
  LVAL xlim1;
  char *fn;
  FILE *fp;
  int pc;
  if (!moreargs())
    xlabort("(*writeimagedata im fn pc)");
/*
  \lspfunction{*}{writeimagedata}{im fn pc}
  \param{im}{an image node}
  \param{fn}{a file name (possibly including a path)}
  \param{pc}{integer for planar configuration}
  \return{true if success, NIL otherwise}
  \desc{writes image date im on disk as binary file with pixels values interleaved by pixel (if pc equals 1), by plane (if pc equals 2), by line (if pc equals 3).  Beware that if the files already exists, it will be overwritten.  This function does not write a header file (see *writeenvi for this purpose).}
  \cfunction{\cfwriteUDimageUDdata}
  \cfile{imio.c}
*/

  xlim1 = xlgaimage();
  fn = (char *)getstring(xlgetfname());
  pc = (int)getfixnum(xlgafixnum());
  
  xllastarg();

  /*  Open output file  */
  fp = fopen(fn, "wb");

  if (write_image_data(fp, (IMAGE *)getimage(xlim1), pc) == ERROR){
    (void)fclose(fp);
    return(NIL);
  }
  (void)fclose(fp);
  return(s_true);
}

LVAL iwrite_ColorMap_tiff()
{
  LVAL xlim1;
  char *fn;
  if (!moreargs())
      xlabort("(*writecmtiff im fn)");  /* OBSOLETE FUNCTION */
/*
%  \lspfunction{*}{writecmtiff}{im fn}
%  \param{im}{an image node holding a colour LUT}
%  \param{fn}{a file name (possibly including a path)}
%  \return{true if success, NIL otherwise}
%  \desc{write image im on disk as a TIFF file.  Beware that if the files already exists, it will be overwritten.}
%  \cfunction{write_ColorMap_tiff(IMAGE *im, char *fn)}
%  \cfile{imio.c}
*/

  xlim1 = xlgaimage();
  fn = (char *)getstring(xlgetfname());
  
  xllastarg();

  if (write_ColorMap_tiff((IMAGE *)getimage(xlim1), fn) == ERROR)
    return(NIL);
  return(s_true);
}


LVAL irotatefast()
{
  IMAGE **im;
  LVAL xlim1, result, u_ptr, v_ptr;
  double theta;
/*
  \lspfunction{*}{rotatefast}{im theta}
  \param{im}{an image node}
  \param{theta}{a floating point value for rotation angle in radian}
  \return{a list of x- and y- coordinates}
  \desc{in development 20110207}
  \cfunction{\cfrotatecoor}
  \cfile{imio.c}
*/
  if (!moreargs())
    xlabort("(*rotatefast im theta)\n");

  xlim1 = xlgaimage();
  theta = (double)getfixnum(xlgaflonum());

  xllastarg();

  im=rotatecoor((IMAGE *)getimage(xlim1), theta);
  if (im==NULL)
    xlabort("rotatecoor() returned NULL");
  xlstkcheck(3);
  xlsave(u_ptr);
  xlsave(v_ptr);
  xlsave(result);
  u_ptr=cvimage(im[0]);
  v_ptr=cvimage(im[1]);
  free(im);
  result = cons(u_ptr, cons (v_ptr, NIL));
  xlpopn(3);
  return result;
}

/*
  \lispsection
  \input{io.tex}
*/



/* 
\module{Image memory and LUT}\label{m.memlut} % Mostly in imem.c
\basesection
*/
LVAL iimcreate()
{
  int nx, ny, nz, data_type;
  if (!moreargs())
    xlabort("(*imcreate data_type nx ny nz)\n data_type = t_TIFFONEBITPERPIXEL (line byte padd), t_ONEBITPERPIXEL (line word padd), t_FOURBITPERPIXEL (line byte padd), t_CHAR, t_UCHAR, etc.");

/*
  \lspfunction{*}{imcreate}{data_type nx ny nz}
  \param{data_type}{an integer value for image data type (t_UCHAR, t_USHORT, t_INT32, t_FLOAT, or t_DOUBLE)}
  \param{nx}{number of columns}
  \param{ny}{number of lines}
  \param{nz}{number of x-y planes (1 for a 2-D image)}
  \return{an image node}
  \desc{creates a new image of specified data type and geometry.}
  \cfunction{\cfcreateUDimage}
  \cfile{imem.c}
  \example{(*imcreate t_UCHAR 32 64 1)}{creates a 2-D image of type unsigned char with 32 columns, 64 lines, and 1 x-y plane}
*/

  data_type = (int)getfixnum(xlgafixnum());
  nx = (int)getfixnum(xlgafixnum());
  ny = (int)getfixnum(xlgafixnum());
  nz = (int)getfixnum(xlgafixnum());

  xllastarg();

  return(cvimage(create_image(data_type, nx, ny, nz)));
}

LVAL ishmatimage()
{
  int shmkey, nx, ny, nz, nbyte, data_type;
  if (!moreargs())
    xlabort("(*shmatimage semkey shmkey nx ny nz nbyte type");

/*
  \lspfunction{*}{shmatimage}{shmkey nx ny nz type}
  \param{shmkey}{integer for shared memory key value}
  \param{nx}{number of columns}
  \param{ny}{number of lines}
  \param{nz}{number of x-y planes (1 for a 2-D image)}
  \param{nbyte}{nymber of bytes of memory buffer used for storing the image data}
  \param{type}{an integer value for image data type (t_UCHAR, t_USHORT, t_INT32, t_FLOAT, or t_DOUBLE)}
  \return{an image node}
  \desc{attach the image corresponding to the shared memor keys shmkey}
  \cfunction{\cfshmatimage}
  \cfile{shm.c}
  \creationdate{20130926}
  \example{}{}
*/
  shmkey = (int)getfixnum(xlgafixnum());
  nx = (int)getfixnum(xlgafixnum());
  ny = (int)getfixnum(xlgafixnum());
  nz = (int)getfixnum(xlgafixnum());
  nbyte = (int)getfixnum(xlgafixnum());
  data_type = (int)getfixnum(xlgafixnum());

  xllastarg();

  return(cvimage(shmatimage(shmkey,  nx, ny, nz, nbyte, data_type)));
}

LVAL ishmdtimage()
{
  int semkey_flag=0, semkey=0;
  LVAL xlim1;
  IMAGE *im;
  if (!moreargs())
    xlabort("(*shmdtimage im)");

/*
  \lspfunction{*}{shmdtimage}{semkey im}
  \param{semkey}{in debug mode}
  \param{im}{image node whose data buffer correspnds to a shared memory buffer}
  \return{true on success, NIL otherwise}
  \desc{detach the shared memory associated with im.  The image im cannot be accessed anymore after this call.  semkey_flag is hard set to 0 (no key) for the time being}
  \cfunction{\cfshmdtimage}
  \cfile{shm.c}
  \creationdate{20130926}
  \example{}{}
*/

  /* semkey_flag is hard set to 0 (no key) for the time being */
  /* semkey = (int)getfixnum(xlgafixnum()); */
  xlim1 = xlgaimage();

  xllastarg();

  im=(IMAGE *)getimage(xlim1);

  if (shmdtimage((void *)GetImPtr(im), semkey_flag, semkey)!=NO_ERROR){
    return(NIL);
  }
  SetImPtr(im,NULL);
  free((char *)im);
  xlim1->n_type = FREE;
  return(s_true);
}

LVAL iimcopy()
{
  LVAL xlim1;
  if (!moreargs())
    xlabort("(*imcopy im)");

/*
  \lspfunction{*}{imcopy}{im}
  \param{im}{an image node}
  \return{an image node}
  \desc{creates a new image holding a copy of the input image im.}
  \cfunction{\cfcopyUDimage}
  \cfile{imem.c}
*/

  xlim1 = xlgaimage();
  
  xllastarg();

  return(cvimage(copy_image((IMAGE *)getimage(xlim1))));
}

LVAL ifree_image()
{
  LVAL arg;
  long nbyte;
  if (!moreargs())
    xlabort("(*imfree im1 im2 ...)");
/*
  \lspfunction{*}{imfree}{im1 &rest imseries}
  \param{im1}{an image node}
  \param{imseries}{a series of further image nodes}
  \return{true if success, NIL otherwise}
  \desc{frees all images given in argument (at least one image).}
  \cfunction{\cffreeUDimage}
  \cfile{imem.c}
  \example{(*imfree i1 i0)}{frees the images associated with i0 and i1 image nodes}
*/
  while(moreargs()){
    arg = xlgaimage();
    nbyte = GetImNByte((IMAGE *)getimage(arg));
    free_image((IMAGE *)getimage(arg));
    total -= nbyte;
    arg->n_type = FREE;
  }
  return(s_true);
}

LVAL iiminfo()
{
  LVAL xlim1;
  if (!moreargs())
    xlabort("(*iminfo im)");
/*
  \lspfunction{*}{iminfo}{im}
  \param{im}{an image node}
  \return{im}
  \desc{prints in stdout information contained in the image structure as well as the minimum and maximum image values.}
  \cfunction{\cfiminfo}
  \cfile{imem.c}
*/
  xlim1 = xlgaimage();
  
  iminfo((IMAGE *)getimage(xlim1));
  return(xlim1);
}

LVAL idumpxyz()
{
  LVAL xlim1;
  int x, y, z, dx, dy;

  if (!moreargs())
    xlabort("(*dumpxyz im x y z dx dy)\n");
/*
  \lspfunction{*}{dumpxyz}{im x y z dx dy}
  \param{im}{an image node}
  \param{x}{x coordinate}
  \param{y}{y coordinate}
  \param{z}{z coordinate}
  \param{dx}{integer for size of window along x-axis}
  \param{dy}{integer for size of window along y-axis}
  \return{true on success, NIL otherwise}
  \desc{dumps on screen a dx$\times$dy window with the image values around coordinates (x,y) and within the plane z.}
  \cfunction{\cfdumpxyz}
  \cfile{geom.c}
*/

  xlim1 = xlgaimage();
  x     = (int)getfixnum(xlgafixnum());
  y     = (int)getfixnum(xlgafixnum());
  z     = (int)getfixnum(xlgafixnum());
  dx    = (int)getfixnum(xlgafixnum());
  dy    = (int)getfixnum(xlgafixnum());

  xllastarg();

  dumpxyz((IMAGE *)getimage(xlim1),x,y,z,dx,dy);
  return s_true;
}

LVAL inx()
{
  LVAL a = NIL;

  if (!moreargs())
    xlabort("(*getnx im)");

/*
  \lspfunction{*}{getnx}{im}
  \param{im}{an image node}
  \return{the number of columns of im}
  \cfunction{macro GetImNx(IMAGE *im)}
  \cfile{mialib.h}
*/
  a = xlgaimage();
  xllastarg();
  
  return(cvfixnum(a->n_info.n_image->nx));
}

LVAL iny()
{
  LVAL a = NIL;

  if (!moreargs())
    xlabort("(*getny im)");
/*
  \lspfunction{*}{getny}{im}
  \param{im}{an image node}
  \return{the number of lines of im}
  \cfunction{macro GetImNy(IMAGE *im)}
  \cfile{mialib.h}
*/
  a = xlgaimage();
  xllastarg();
  return(cvfixnum(a->n_info.n_image->ny));
}

LVAL inz()
{
  LVAL a = NIL;

  if (!moreargs())
    xlabort("(*getnz im)");
/*
  \lspfunction{*}{getnz}{im}
  \param{im}{an image node}
  \return{the number of x-y planes of im}
  \cfunction{macro GetImNz(IMAGE *im)}
  \cfile{mialib.h}
*/
  a = xlgaimage();
  xllastarg();
  return(cvfixnum(a->n_info.n_image->nz));
}

LVAL iimdatatype()
{
  LVAL a = NIL;

  if (!moreargs())
    xlabort("(*getdatatype im)");

/*
  \lspfunction{*}{getdatatype}{im}
  \param{im}{an image node}
  \return{an integer value corresponding to the image data type (e.g., t_UCHAR for unsigned char)}
  \cfunction{macro GetImDataType(IMAGE *im)}
  \cfile{mialib.h}
*/
  a = xlgaimage();
  xllastarg();
  return(cvfixnum(a->n_info.n_image->DataType));
}

LVAL igetmin()
{
  LVAL a = NIL;
  G_TYPE *pg, pgs;

  if (!moreargs())
    xlabort("(*getmin im)");
/*
  \lspfunction{*}{getmin}{im}
  \param{im}{an image node}
  \return{the minimum value of im}
*/
  a = xlgaimage();
  xllastarg();

  /* get min values */
  pg = min_max(a->n_info.n_image);
  if (pg == NULL)
    xlabort("");
  pgs = *pg;
  free((char *)pg);

  switch(a->n_info.n_image->DataType){
  case t_UCHAR:
    return(cvfixnum((int) pgs.uc_val));
  case t_USHORT:
    return(cvfixnum((int) pgs.us_val));
  case t_SHORT:
    return(cvfixnum((int) pgs.s_val));
  case t_INT32:
    return(cvfixnum((int) pgs.i32_val));
  case t_UINT32:
    return(cvfixnum((int) pgs.u32_val));
  case t_INT64:
    return(cvfixnum((int) pgs.i64_val));
  case t_UINT64:
    return(cvfixnum((int) pgs.u64_val));
  case t_FLOAT:
    return(cvflonum((float) pgs.f_val));
  case t_DOUBLE:
    return(cvflonum((float) pgs.d_val));
  default:
    xlabort(" (*getmin) undefined image data type");
  }
  return NIL;
}

LVAL igetmax()
{
  LVAL a = NIL;
  G_TYPE *pg, pgs;

  if (!moreargs())
    xlabort("(*getmax im)");
/*
  \lspfunction{*}{getmax}{im}
  \param{im}{an image node}
  \return{the maximum value of im}
*/
  a = xlgaimage();
  xllastarg();

  /* get min values */
  pg = min_max(a->n_info.n_image);
  if (pg == NULL)
    xlabort("");
  pgs = pg[1];
  free((char *)pg);

  switch(a->n_info.n_image->DataType){
  case t_UCHAR:
    return(cvfixnum((int) pgs.uc_val));
  case t_USHORT:
    return(cvfixnum((int) pgs.us_val));
  case t_SHORT:
    return(cvfixnum((int) pgs.s_val));
  case t_INT32:
    return(cvfixnum((int) pgs.i32_val));
  case t_UINT32:
    return(cvfixnum((int) pgs.u32_val));
  case t_INT64:
    return(cvfixnum((int) pgs.i64_val));
  case t_UINT64:
    return(cvfixnum((int) pgs.u64_val));
  case t_FLOAT:
    return(cvflonum((float) pgs.f_val));
  case t_DOUBLE:
    return(cvflonum((float) pgs.d_val));
  default:
    xlabort(" (*getmax) undefined image data type");
  }
  return NIL;
}
LVAL igetfirstmaxpos()
{
  LVAL xlim1;
  unsigned long int pos;

  if (!moreargs())
    xlabort("(*getfirstmaxpos im)");
/*
  \lspfunction{*}{getfirstmaxpos}{im}
  \param{im}{an image node}
  \return{the offset of the first maximum encountered}
  \cfunction{\cfgetfirstmaxpos}
  \cfile{imstat.c}
*/
  xlim1 = xlgaimage();
  xllastarg();

  if (getfirstmaxpos((IMAGE *)getimage(xlim1), &pos) == ERROR)
   xlabort("*getfirstmaxpos: error");

  return(cvfixnum((int) pos));
}

LVAL igetpix()
{
  LVAL a = NIL;
  int x, y, z;
  unsigned offset;

  if (!moreargs())
    xlabort("(*getpix im x y z) ; x y z values not checked for overflows!");
/*
  \lspfunction{*}{getpix}{im x y z}
  \param{im}{an image node}
  \param{x}{x coordinate in [0,(*getnx im)-1]}
  \param{y}{y coordinate in [0,(*getny im)-1]}
  \param{z}{z coordinate in [0,(*getnz im)-1]}
  \return{the value of im at coordinates (x,y,z)}
*/
  a = xlgaimage();
  x = getfixnum(xlgafixnum());
  y = getfixnum(xlgafixnum());
  z = getfixnum(xlgafixnum());
  xllastarg();
  offset = x+y*a->n_info.n_image->nx+z*a->n_info.n_image->nx*a->n_info.n_image->ny;
  if ((unsigned)GetImNPix(a->n_info.n_image)<=offset)
    xlabort(" (*getpix im x y z) x y z values lead to an out of range offset");
  switch(a->n_info.n_image->DataType){
  case t_UCHAR:
      return(cvfixnum((int) *((UCHAR *) a->n_info.n_image->p_im + offset)));
  case t_USHORT:
      return(cvfixnum((int) *((USHORT *) a->n_info.n_image->p_im + offset)));
  case t_SHORT:
      return(cvfixnum((int) *((SHORT *) a->n_info.n_image->p_im + offset)));
  case t_INT32:
      return(cvfixnum((int) *((INT32 *) a->n_info.n_image->p_im + offset)));
  case t_UINT32:
      return(cvfixnum((int) *((UINT32 *) a->n_info.n_image->p_im + offset)));
  case t_INT64:
      return(cvfixnum((int) *((INT64 *) a->n_info.n_image->p_im + offset)));
  case t_UINT64:
      return(cvfixnum((int) *((UINT64 *) a->n_info.n_image->p_im + offset)));
  case t_FLOAT:
      return(cvflonum((float) *((MIAFLOAT *) a->n_info.n_image->p_im + offset)));
  case t_DOUBLE:
      return(cvflonum((float) *((DOUBLE *) a->n_info.n_image->p_im + offset)));
  default:
    xlabort(" (*getpix im x y z) undefined image data type");
  }
  return NIL;
}

LVAL igetpixi()
{
  LVAL a = NIL;
  unsigned offset;

  if (!moreargs())
    xlabort("(*getpixi im offset) ; offset in [0,npix-1]");

/*
  \lspfunction{*}{getpixi}{im offset}
  \param{im}{an image node}
  \param{offset}{an offset value in [0,npix-1]}
  \return{the value of the image (stored as a 1-D array) at position offset}
*/

  a = xlgaimage();
  offset = (unsigned)getfixnum(xlgafixnum());
  if (offset >= ((long int) a->n_info.n_image->nx * a->n_info.n_image->ny * a->n_info.n_image->nz) ){
    xlabort(" (*getpixi im offset) offset out of range");
  }
  xllastarg();
  switch(a->n_info.n_image->DataType){
  case t_UCHAR:
      return(cvfixnum((int) *((UCHAR *) a->n_info.n_image->p_im + offset)));
  case t_USHORT:
      return(cvfixnum((int) *((USHORT *) a->n_info.n_image->p_im + offset)));
  case t_SHORT:
      return(cvfixnum((int) *((SHORT *) a->n_info.n_image->p_im + offset)));
  case t_INT32:
      return(cvfixnum((int) *((INT32 *) a->n_info.n_image->p_im + offset)));
  case t_FLOAT:
    // printf("val=%20.40g\n", (double) *((MIAFLOAT *) (a->n_info.n_image->p_im) + offset));
    // printf("val=%20.40f\n",  *((MIAFLOAT *) (a->n_info.n_image->p_im) + offset));
    return(cvflonum((FLOTYPE) *((MIAFLOAT *) a->n_info.n_image->p_im + offset)));
  case t_DOUBLE:{
    // printf("val=%20.40g\n", *((DOUBLE *) (a->n_info.n_image->p_im) + offset));
    // printf("val=%20.40f\n", (float) *((DOUBLE *) (a->n_info.n_image->p_im) + offset));
      return(cvflonum((FLOTYPE) *((DOUBLE *) a->n_info.n_image->p_im + offset)));
  }
  default:
    xlabort(" (*getpixi im i) undefined image data type");
  }
  return NIL;
}

LVAL igetpixmin()
{
  LVAL a = NIL;

  if (!moreargs())
    xlabort("(*getpixmin im )");
/*
  \lspfunction{*}{getpixmin}{im}
  \param{im}{an image node}
  \return{the minimum value allowed for the image data type defined by im.}
*/  
  a = xlgaimage();
  xllastarg();
  switch(a->n_info.n_image->DataType){
  case t_UCHAR:
      return(cvfixnum((int) UCHAR_MIN));
  case t_USHORT:
      return(cvfixnum((int) USHORT_MIN));
  case t_SHORT:
      return(cvfixnum((int) SHORT_MIN));
  case t_INT32:
      return(cvfixnum((int) INT32_MIN));
  case t_UINT32:
      return(cvfixnum((int) UINT32_MIN));
  case t_FLOAT:
      return(cvflonum((float) MIAFLOAT_MIN));
  case t_DOUBLE:
      return(cvflonum((float) DOUBLE_MIN));
  default:
    xlabort(" (*getpixmin im) unsupported image data type");
  }
  return NIL;
}


LVAL igetpixmax()
{
  LVAL a = NIL;

  if (!moreargs())
    xlabort("(*getpixmax im)");
/*
  \lspfunction{*}{getpixmax}{im}
  \param{im}{an image node}
  \return{the maximum value allowed for the image data type defined by im.}
*/  
  a = xlgaimage();
  xllastarg();
  switch(a->n_info.n_image->DataType){
  case t_UCHAR:
      return(cvfixnum((int) UCHAR_MAX));
  case t_USHORT:
      return(cvfixnum((int) USHORT_MAX));
  case t_SHORT:
      return(cvfixnum((int) SHORT_MAX));
  case t_INT32:
  case t_UINT32:
      return(cvfixnum((int) INT32_MAX));
  case t_FLOAT:
      return(cvflonum((float) MIAFLOAT_MAX));
  case t_DOUBLE:
      return(cvflonum((float) DOUBLE_MAX));
  default:
    xlabort(" (*getpixmax im) unsupported image data type");
  }
  return NIL;
}

LVAL isetnx()
{
  LVAL a = NIL;
  int nx;

  if (!moreargs())
    xlabort("(*setnx im nx)");
/*
  \lspfunction{*}{setnx}{im nx}
  \param{im}{an image node}
  \param{nx}{desired number of image columns}
  \return{the image im (destructive function) with its number of columns set to nx}
  \example{}{}
*/
  
  a = xlgaimage();
  nx = getfixnum(xlgafixnum());
  a->n_info.n_image->nx=nx;
  return s_true;
}

LVAL isetny()
{
  LVAL a = NIL;
  int ny;

  if (!moreargs())
    xlabort("(*setny im ny)");
/*
  \lspfunction{*}{setny}{im ny}
  \param{im}{an image node}
  \param{nx}{desired number of image lines}
  \return{the image im (destructive function) with its number of lines set to ny}
*/
  
  
  a = xlgaimage();
  ny = getfixnum(xlgafixnum());
  a->n_info.n_image->ny=ny;
  return s_true;
}

LVAL isetnz()
{
  LVAL a = NIL;
  int nz;

  if (!moreargs())
    xlabort("(*setny im nz)");
/*
  \lspfunction{*}{setnz}{im nz}
  \param{im}{an image node}
  \param{nz}{desired number of x-y planes}
  \return{the image im (destructive function) with its number of x-y planes set to nz}
*/
  
  a = xlgaimage();
  nz = getfixnum(xlgafixnum());
  a->n_info.n_image->nz=nz;
  return s_true;
}

LVAL isetdatatype()
{
  LVAL a = NIL;
  int atype;

  if (!moreargs())
    xlabort("(*setdatatype im atype)");
/*
  \lspfunction{*}{setdatatype}{im atype}
  \param{im}{an image node}
  \param{atype}{an image data type (e.g. t_UCHAR for unsigned char)}
  \return{the image im with its datatype set to atype}
  \xlispcfile{xlglue1.c}
*/
  a = xlgaimage();
  atype = getfixnum(xlgafixnum());
  xllastarg();
  
  if (a->n_info.n_image->DataType==atype) /* nothing to do */
    return s_true;
    
  if (atype == t_RGB){
    if ((a->n_info.n_image->ny)%3 != 0)
      xlabort("The number of image lines MUST be a multiple of 3 for t_RGB...");
    a->n_info.n_image->ny/=3;
    a->n_info.n_image->DataType=atype;
    a->n_info.n_image->nz=3;
  }
  else if (atype == t_UCHAR){
    if (a->n_info.n_image->DataType != t_RGB)
      xlabort("Only t_RGB images can be possibly set to t_UCHAR...");
    a->n_info.n_image->ny*=3;
    a->n_info.n_image->DataType=atype;
    a->n_info.n_image->nz=1;
  }
  else if (atype == t_USHORT && a->n_info.n_image->DataType == t_SHORT){
    a->n_info.n_image->DataType=atype;
  }
  else if (atype == t_SHORT && a->n_info.n_image->DataType == t_USHORT){
    a->n_info.n_image->DataType=atype;
  }
  else if (atype == t_UINT32 && a->n_info.n_image->DataType == t_INT32){
    a->n_info.n_image->DataType=atype;
  }
  else if (atype == t_INT32 && a->n_info.n_image->DataType == t_UINT32){
    a->n_info.n_image->DataType=atype;
  }
  else if (atype == t_INT32 && a->n_info.n_image->DataType == t_FLOAT){
    a->n_info.n_image->DataType=atype;
  }
  else if (atype == t_FLOAT && a->n_info.n_image->DataType == t_INT32){
    a->n_info.n_image->DataType=atype;
  }
  else
    xlabort("atype must be t_RGB or correspond to the same number of bytes as original");
  return s_true;
}

LVAL iswapim()
{
  LVAL a = NIL, b = NIL;
  IMAGE *imtmp;

  if (!moreargs())
    xlabort("(@swapim im1 im2)");
/*
  \lspfunction{@}{swapim}{im1 im2}
  \param{im1}{an image node}
  \param{im2}{an image node}
  \return{im1}
  \desc{swaps the image contents referred to by im1 and im2.}
  \cfunction{Not available in C library (just use 'get' and 'set' macros).}
  \cfile{xlglue1.c}
  \example{}{}
*/

  
  a = xlgaimage();
  b = xlgaimage();
  xllastarg();
  imtmp=a->n_info.n_image;
  a->n_info.n_image=b->n_info.n_image;
  b->n_info.n_image=imtmp;
  return a;
}

LVAL isetpix()
{
  LVAL a = NIL;
  unsigned x, y, z, val, nx, ny, offset;
  double valflo;

  if (!moreargs())
    xlabort("(*setpix im x y z val)");
/*
  \lspfunction{*}{setpix}{im x y z val}
  \param{im}{an image node}
  \param{x}{x coordinate (integer value)}
  \param{y}{y coordinate (integer value)}
  \param{z}{z coordinate (integer value)}
  \param{val}{a value in the range defined by the data type of im}
  \return{true if success}
  \desc{set to val the pixel of im at coordinates (x, y, z)}
  \example{(*setpix im 0 0 0 255)}{sets to 255 the very first pixel of im}
*/
  a = xlgaimage();
  x = getfixnum(xlgafixnum());
  y = getfixnum(xlgafixnum());
  z = getfixnum(xlgafixnum());


  nx = a->n_info.n_image->nx;
  ny = a->n_info.n_image->ny;

  offset = x + y*nx + z*nx*ny;
  if ((unsigned)GetImNPix(a->n_info.n_image)<=offset)
    xlabort(" (*setpix im x y z val) x y z values lead to an offset out of range");

  switch(a->n_info.n_image->DataType){ 
  case t_UCHAR:
      val = getfixnum(xlgafixnum());
      xllastarg();
      *((UCHAR *) a->n_info.n_image->p_im + offset) = (UCHAR)val;
      return s_true;
  case t_USHORT:
      val = getfixnum(xlgafixnum());
      xllastarg();
      *((USHORT *) a->n_info.n_image->p_im + offset) = (USHORT)val;
      return s_true;
  case t_SHORT:
      val = getfixnum(xlgafixnum());
      xllastarg();
      *((SHORT *) a->n_info.n_image->p_im + offset) = (SHORT)val;
      return s_true;
  case t_INT32:
      val = getfixnum(xlgafixnum());
      xllastarg();
      *((INT32 *) a->n_info.n_image->p_im + offset) = (INT32)val;
      return s_true;
  case t_UINT32:
      val = getfixnum(xlgafixnum());
      xllastarg();
      *((UINT32 *) a->n_info.n_image->p_im + offset) = (UINT32)val;
      return s_true;
  case t_FLOAT:
      valflo = (DOUBLE) getflonum(xlgaflonum());
      xllastarg();
      *((MIAFLOAT *) a->n_info.n_image->p_im + offset) = (MIAFLOAT)valflo;
      return s_true;
  case t_DOUBLE:
      valflo = (DOUBLE) getflonum(xlgaflonum());
      xllastarg();
      *((DOUBLE *) a->n_info.n_image->p_im + offset) = (DOUBLE)valflo;
      return s_true;
  default:
    xlabort(" (*setpix im x y z val) im data type not allowed");
  }
  return NIL;
}

LVAL isetpixtruncate()
{
  LVAL a = NIL, arg;
  unsigned x, y, z, ival=0, nx, ny, offset;
  double fval=0.0;
  int integerp=0;

  if (!moreargs())
    xlabort("(*setpixtruncate im x y z val)");
/*
  \lspfunction{*}{setpixtruncate}{im x y z val}
  \param{im}{an image node}
  \param{x}{x coordinate (integer value)}
  \param{y}{y coordinate (integer value)}
  \param{z}{z coordinate (integer value)}
  \param{val}{a float or integer number}
  \return{true if success}
  \desc{set to val the pixel of im at coordinates (x, y, z).  If val is outside the range defined by the image data type, val is automatically set to PIX_MIN or PIX_MAX depending on whether val is smaller or greater than these bounds.  In case val is a float and the image data type is integer, val is casted to the integer data type.}
  \example{}{}
*/
  a = xlgaimage();
  x = getfixnum(xlgafixnum());
  y = getfixnum(xlgafixnum());
  z = getfixnum(xlgafixnum());

  nx = a->n_info.n_image->nx;
  ny = a->n_info.n_image->ny;

  offset = x + y*nx + z*nx*ny;
  if ((unsigned)GetImNPix(a->n_info.n_image)<=offset)
    xlabort(" (*setpixtruncate im x y z val) x y z values lead to an offset out of range");

  arg = xlgetarg();
  if (fixp(arg)){
    ival = getfixnum(arg);
    integerp=1;
  }
  else if (floatp(arg)){
    fval = getflonum(arg);
  }
  else
    xlbadtype(arg);

  xllastarg();

  if(integerp)
    fval=ival;
  
  switch(a->n_info.n_image->DataType){
  case t_UCHAR:
    if (fval<UCHAR_MIN)
      fval=UCHAR_MIN;
    else if (fval>UCHAR_MAX)
      fval=UCHAR_MAX;
    *((UCHAR *) a->n_info.n_image->p_im + offset) = (UCHAR)fval;
    return s_true;
  case t_USHORT:
    if (fval<USHORT_MIN)
      fval=USHORT_MIN;
    else if (fval>USHORT_MAX)
      fval=USHORT_MAX;
    *((USHORT *) a->n_info.n_image->p_im + offset) = (USHORT)fval;
    return s_true;
  case t_SHORT:
    if (fval<SHORT_MIN)
      fval=SHORT_MIN;
    else if (fval>SHORT_MAX)
      fval=SHORT_MAX;
    *((SHORT *) a->n_info.n_image->p_im + offset) = (SHORT)fval;
    return s_true;
  case t_INT32:
    if (fval<INT32_MIN)
      fval=INT32_MIN;
    else if (fval>INT32_MAX)
      fval=INT32_MAX;
    *((INT32 *) a->n_info.n_image->p_im + offset) = (INT32)fval;
    return s_true;
  case t_UINT32:
    if (fval<UINT32_MIN)
      fval=UINT32_MIN;
    else if (fval>UINT32_MAX)
      fval=UINT32_MAX;
    *((UINT32 *) a->n_info.n_image->p_im + offset) = (INT32)fval;
    return s_true;
  case t_FLOAT:
      *((MIAFLOAT *) a->n_info.n_image->p_im + offset) = (MIAFLOAT)fval;
      return s_true;
  case t_DOUBLE:
      *((DOUBLE *) a->n_info.n_image->p_im + offset) = (DOUBLE)fval;
      return s_true;
  default:
    xlabort(" (*setpixtruncate im x y z val) im data type not allowed");
  }
  return NIL;
}


LVAL isetpixi()
{
  LVAL a = NIL;
  unsigned val, nx, ny, offset;
  float valflo;
  double valdbl;

  if (!moreargs())
    xlabort("(*setpixi im offset val)");
/*
  \lspfunction{*}{setpixi}{im offset val}
  \param{im}{an image node}
  \param{offset}{an offset value in [0,npix-1]}
  \param{val}{a value in the range defined by the data type of im}
  \return{true if success}
  \desc{set to val the pixel of im at position given by offset}
  \example{(*setpixi im 0 255)}{sets to 255 the very first pixel of im}
*/
  a = xlgaimage();
  offset = (unsigned)getfixnum(xlgafixnum());
  nx = a->n_info.n_image->nx;
  ny = a->n_info.n_image->ny;

  if (nx*ny*a->n_info.n_image->nz<=offset)
    xlabort(" (*setpixi im offset val) offset out of range");
    
  switch(a->n_info.n_image->DataType){
  case t_UCHAR:
      val = getfixnum(xlgafixnum());
      xllastarg();
      *((UCHAR *) a->n_info.n_image->p_im + offset) = (UCHAR)val;
      return s_true;
  case t_USHORT:
      val = getfixnum(xlgafixnum());
      xllastarg();
      *((USHORT *) a->n_info.n_image->p_im + offset) = (USHORT)val;
      return s_true;
  case t_SHORT:
      val = getfixnum(xlgafixnum());
      xllastarg();
      *((SHORT *) a->n_info.n_image->p_im + offset) = (SHORT)val;
      return s_true;
  case t_INT32:
      val = getfixnum(xlgafixnum());
      xllastarg();
      *((INT32 *) a->n_info.n_image->p_im + offset) = (INT32)val;
      return s_true;
  case t_UINT32:
      val = getfixnum(xlgafixnum());
      xllastarg();
      *((INT32 *) a->n_info.n_image->p_im + offset) = (UINT32)val;
      return s_true;
  case t_FLOAT:
      valflo = (MIAFLOAT) getflonum(xlgaflonum());
      xllastarg();
      *((MIAFLOAT *) a->n_info.n_image->p_im + offset) = (MIAFLOAT)valflo;
      return s_true;
  case t_DOUBLE:
      valdbl = (DOUBLE) getflonum(xlgaflonum());
      xllastarg();
      *((DOUBLE *) a->n_info.n_image->p_im + offset) = (DOUBLE)valdbl;
      return s_true;
  default:
    xlabort(" (*setpixi im offset val) im data type not allowed");
  }
  return NIL;
}

LVAL icreate_lut()
{
  LVAL xlim1;
  if (!moreargs())
    xlabort("(*addlut im)");

/*
  \lspfunction{*}{addlut}{im}
  \param{im}{an image node of type t_UCHAR}
  \return{true on success, NIL otherwise}
  \desc{adds a colour LUT to the image im (it is initialised as a linear grey scale LUT)}
  \cfunction{\cfcreateUDlut}
  \cfile{imem.c}
*/

  xlim1 = xlgaimage();
  
  xllastarg();

  if (create_lut((IMAGE *)getimage(xlim1)) == ERROR)
   xlabort("*addlut: couldn't add CorlourMap");
  return(s_true);  
}

LVAL ifree_lut()
{
  LVAL xlim1;
  if (!moreargs())
    xlabort("(*freelut im)");

/*
  \lspfunction{*}{freelut}{im}
  \param{im}{an image node of type t_UCHAR}
  \return{true on success, NIL otherwise}
  \desc{frees the colour LUT associated with the image im}
  \cfunction{\cffreeUDlut}
  \cfile{imem.c}
*/

  xlim1 = xlgaimage();
  
  xllastarg();

  free_lut((IMAGE *)getimage(xlim1));
  return(s_true);  
}

LVAL isetlutval()
{
  LVAL a = NIL;
  unsigned short int r, g, b, index;

  if (!moreargs())
    xlabort("(*setlutval im index r g b)");
/*
  \lspfunction{*}{setlutval}{im index r g b}
  \param{im}{an image node (already having a colour LUT)}
  \param{index}{integer value in [0,255]}
  \param{r}{integer value for red in [0-65535]}
  \param{g}{integer value for green in [0-65535]}
  \param{b}{integer value for blue in [0-65535]}
  \return{true on success NIL otherwise}
  \desc{associates the grey scale value index of im with r,g,b values}
  \cfunction{LVAL isetlutval()}
  \xlispcfile{xlglue1.c}
  \example{(*setlutval im 255 65535 65535 0)}{sets to yellow all pixels of im at value 255}
*/

  a = xlgaimage();
  index = getfixnum(xlgafixnum());
  r = getfixnum(xlgafixnum());
  g = getfixnum(xlgafixnum());
  b = getfixnum(xlgafixnum());

  xllastarg();

  if (a->n_info.n_image->lut==NULL)
    xlabort("(*setlutval im index r g b) Image im has no ColorMap");

  a->n_info.n_image->lut[index]=r;
  a->n_info.n_image->lut[index+256]=g; /* assuming 256 entries in ColorMap! */
  a->n_info.n_image->lut[index+512]=b; /* assuming 256 entries in ColorMap! */

  return s_true;
}

LVAL igetlutval()
{
  LVAL a = NIL;
  unsigned index, fcolour;

  if (!moreargs())
    xlabort("(*getlutval im index fcolour)");
/*
  \lspfunction{*}{getlutval}{im index fcolour}
  \param{im}{an image node (already having a colour LUT)}
  \param{index}{integer value in [0,255]}
  \param{fcolour}{integer value: 0 for red, 1 for green, and 2 for blue}
  \return{short integer value}
  \desc{returns the value associated with the specified index and fundamental colour}
  \cfunction{LVAL igetlutval()}
  \xlispcfile{xlglue1.c}
  \example{(*getlutval im 4 2)}{gets the blue component value associated with the index 4 in the LUT of image im}
*/

  a = xlgaimage();

  if (a->n_info.n_image->lut==NULL)
    xlabort("(*getlutval im index fcolour) Image im has no ColorMap");

  index = getfixnum(xlgafixnum());
  if ( (index<0) || (index>255) )
    xlabort("(*getlutval im index fcolour) LUT index must belong to {0,1,...,255}");
  fcolour = getfixnum(xlgafixnum());
  if ( (fcolour!=0) && (fcolour !=1) && (fcolour!=3) )
    xlabort("(*getlutval im index fcolour) fcolour must equal either 0 (red), 1 (green), or 2 (blue)");

  return(cvfixnum(a->n_info.n_image->lut[index+fcolour*256]));
}

LVAL icopy_lut()
{
  LVAL xlim1, xlim2;
  if (!moreargs())
    xlabort("(*copylut im1 im2)");

/*
  \lspfunction{*}{copylut}{im1 im2}
  \param{im1}{an image node with a color LUT}
  \param{im2}{an image node}
  \return{true on success, NIL otherwise}
  \desc{copy the colour LUT associated with im1 to im2}
  \cfunction{\cfcopyUDlut}
  \cfile{imem.c}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  
  xllastarg();

  if (copy_lut((IMAGE *)getimage(xlim2), (IMAGE *)getimage(xlim1)) == ERROR)
   xlabort("*copylut: couldn't copy CorlorMap");
  return(s_true);  
}

/*
  \lispsection
  \input{lut.tex}
*/


/* 
\module{Type conversions}\label{m.format} % Mostly in format.c
\basesection
*/
LVAL ito_uchar()
{
  LVAL xlim1;
  unsigned long int nbyte;

  if (!moreargs())
    xlabort("(@touchar im)");

/*
  \lspfunction{@}{touchar}{im}
  \param{im}{an image node}
  \return{im}
  \desc{converts im into an unsigned char image (unsigned byte).}
  \cfunction{\cftoUDuchar}
  \cfile{format.c}
*/
  xlim1 = xlgaimage();
  
  xllastarg();

  nbyte=GetImNByte((IMAGE *)getimage(xlim1));
  if ( to_uchar((IMAGE *)getimage(xlim1)) == ERROR)
    return NIL;
  total-=nbyte;
  total+=GetImNByte((IMAGE *)getimage(xlim1));
  return xlim1;
}

LVAL ito_ushort()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(*toushort im)");

/*
  \lspfunction{*}{toushort}{im}
  \param{im}{an image node}
  \return{an image node}
  \desc{converts im into an unsigned short integer image.}
  \cfunction{\cftoUDushort}
  \cfile{format.c}
*/
  xlim1 = xlgaimage();
  
  xllastarg();

  return(cvimage(to_ushort((IMAGE *)getimage(xlim1))));
}

LVAL ito_int32()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(*tolong im)");
/*
  \lspfunction{*}{tolong}{im}
  \param{im}{an image node}
  \return{an image node}
  \desc{converts im into a long integer image.}
  \cfunction{\cftoUDintTHREETWO}
  \cfile{format.c}
*/
  xlim1 = xlgaimage();
  
  xllastarg();

  return(cvimage(to_int32((IMAGE *)getimage(xlim1))));
}

LVAL ito_float()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(*tofloat im)");
/*
  \lspfunction{*}{tofloat}{im}
  \param{im}{an image node}
  \return{an image node}
  \desc{converts im into a float image.}
  \cfunction{\cftoUDfloat}
  \cfile{format.c}
*/
  xlim1 = xlgaimage();
  
  xllastarg();

  return(cvimage(to_float((IMAGE *)getimage(xlim1))));
}


LVAL iuint32_to_float()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(@uint32tofloat im)");
/*
  \lspfunction{@}{uint32tofloat}{im}
  \param{im}{an image node with data type equal to uint32}
  \return{the same image node}
  \desc{converts im into a float image.}
  \cfunction{\cftoUDfloat}
  \cfile{format.c}
*/
  xlim1 = xlgaimage();

  xllastarg();
  if (GetImDataType((IMAGE *)getimage(xlim1))!=t_UINT32)
    xlabort("(@uint32tofloat im): im must be of type UINT32");
  
  if (uint32_to_float((IMAGE *)getimage(xlim1)) != NO_ERROR)
    return NIL;
  return xlim1;
}

LVAL ito_double()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(*todouble im)");
/*
  \lspfunction{*}{todouble}{im}
  \param{im}{an image node}
  \return{an image node}
  \desc{converts im into a float image.}
  \cfunction{\cftoUDfloat}
  \cfile{format.c}
*/
  xlim1 = xlgaimage();
  
  xllastarg();

  return(cvimage(to_double((IMAGE *)getimage(xlim1))));
}

LVAL idbltofloat()
{
  LVAL xlim1;
  unsigned long int nbyte;

  if (!moreargs())
    xlabort("(@dbltofloat im)");
/*
  \lspfunction{@}{dbltofloat}{im}
  \param{im}{an image node}
  \return{im}
  \desc{converts im into a float image (destructive function).}
  \cfunction{\cfdbltofloat}
  \cfile{format.c}
*/
  xlim1 = xlgaimage();
  
  xllastarg();

  nbyte=GetImNByte((IMAGE *)getimage(xlim1));
  if ( dbltofloat((IMAGE *)getimage(xlim1)) == ERROR)
    return NIL;
  total-=nbyte;
  total+=GetImNByte((IMAGE *)getimage(xlim1));

  return(xlim1);
}

LVAL ideinterleave()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(*deinterleave im)");

/*
  \lspfunction{*}{deinterleave}{im nc}
  \param{im}{an image node}
  \return{an image node}
  \desc{rearranges the pixel values of the input image in such a way that all values of the first channel appear first, then those of the second channel, and so forth.  It assumes therefore that the input image holds interleaved pixel values such as RGBRGBRGB... for a colour RGB image (i.e., the number of image planes (nz) equals 3 in this case).}
  \cfunction{\cfdeinterleave}
  \cfile{miscel.c}
*/

  xlim1 = xlgaimage();
  xllastarg();
  return(cvimage(deinterleave((IMAGE *)getimage(xlim1))));
}

/*
% \module{Colour}  % Mostly in colconv.c
*/

LVAL irgb2hsx()
{
  LVAL temp;
  LVAL result;
  LVAL xlim, xlim1, xlim2, xlim3;
  int n=3, m;
  IMAGE **imarray;
  int type=0;
  char *stype=NULL;

  if (!moreargs())
    xlabort("(*rgb2hsx r g b &key (type "V"))");

/*
  \lspfunction{*}{rgb2hsx}{r g b &key (type "V")}
  \param{imh}{an image node for red channel (UCHAR)}
  \param{ims}{an image node for green channel (UCHAR)}
  \param{imi}{an image node for blue channel (UCHAR)}
  \param{type}{string with key ("V" (default) for Value, "L" for Lightness, and "I" for Intensity)}
  \return{an list of three images corresponding to the Hue, Saturation, and V/L/I components of the input RGB image.}
  \desc{returns the hue, saturation, and value, lightness, or intensity channels of an input RGB colour image.  The hue component is identical for all 3 models. The luminance is equal to max(R,G,B) for HSV, (max-min)/2 for HSL and (R+G+B)/3 for HSI.  See specific formulae for the saturation at \url{http://en.wikipedia.org/wiki/HSL_and_HSV}.}
  \cfunction{\cfimrgbTWOhsx}
  \cfile{colconv.c}
  \authors{Pierre Soille}
  \creationdate{20140904: for global landsat mosaic}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();

  if (moreargs()){
    if (xlgetkeyarg(kl_type, &temp) && (temp != NIL)){
      stype = (char *)getstring(temp);
    }
  }
  xllastarg();

  if (stype!=NULL){
    if (strcmp(stype,"V")==0)
      type=0;
    else if (strcmp(stype,"L")==0)
      type=1;
    else  if (strcmp(stype,"I")==0)
      type=2;
  }

  if ( (imarray=imrgb2hsx((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2),(IMAGE *)getimage(xlim3), type)) == NULL)
    xlabort("(*rgb2hsx r g b): returned NULL");
  SAVE_LIST_IMAGES( imarray, xlim, result, n, m );
  free(imarray);
  return result;
}

LVAL ihsi2rgb()
{
  LVAL xlim1, xlim2, xlim3;

  if (!moreargs())
    xlabort("(*hsi2rgb imh ims imi)");

/*
  \lspfunction{*}{hsi2rgb}{imh ims imi}
  \param{imh}{an image node for hue channel (UCHAR)}
  \param{ims}{an image node for saturation channel (UCHAR)}
  \param{imi}{an image node for intensity channel (UCHAR)}
  \return{an image node for colour RGB image}
  \desc{takes the hue, saturation, and intensity channels of a colour image and returns an image node containing a colour RGB image.}
  \cfunction{\cfimhsiTWOrgb}
  \cfile{colconv.c}
  \example{}{}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();

  xllastarg();

  return(cvimage(imhsi2rgb((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2),(IMAGE *)getimage(xlim3))));
}

LVAL ihls2rgb()
{
  LVAL xlim1, xlim2, xlim3;

  if (!moreargs())
    xlabort("(*hls2rgb imh iml ims)");
/*
  \lspfunction{*}{hls2rgb}{imh iml ims}
  \param{imh}{an image node for hue channel (UCHAR)}
  \param{iml}{an image node for lightness channel (UCHAR)}
  \param{ims}{an image node for saturation channel (UCHAR)}
  \return{an image node for colour RGB image}
  \desc{takes the hue, lightness, and saturation channels of a colour image and returns an image node containing a colour RGB image.}
  \cfunction{\cfimhlsTWOrgb}
  \cfile{colconv.c}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();

  xllastarg();
  
  return(cvimage(imhls2rgb((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2),(IMAGE *)getimage(xlim3))));
}

LVAL icrgb2rgb()
{
  LVAL xlim1, xlim2, xlim3;

  if (!moreargs())
    xlabort("(*crgb2rgb imr img imb)");
/*
  \lspfunction{*}{crgb2rgb}{imr img imb}
  \param{imh}{an image node for red channel (UCHAR)}
  \param{iml}{an image node for green channel (UCHAR)}
  \param{ims}{an image node for blue channel (UCHAR)}
  \return{an image node for colour RGB image}
  \desc{takes the red, green, and blue channels of a colour image and returns an image node containing a colour RGB image.}
  \cfunction{\cfcrgbTWOrgb}
  \cfile{colconv.c}
  \example{}{}
*/


  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();

  xllastarg();

  return(cvimage(crgb2rgb((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2),(IMAGE *)getimage(xlim3))));
}

/*
  \lispsection
  \input{format.tex}
*/

/* 
\module{Geometry and frames} % Mostly in geom.c
\basesection
*/
LVAL iimcut()
{
  LVAL xlim1;
  int x1, y1, z1, x2, y2, z2;

  if (!moreargs())
    xlabort("(*imcut im x1 y1 z1 x2 y2 z2)");

/*
  \lspfunction{*}{imcut}{im x1 y1 z1 x2 y2 z2}
  \param{im}{an image node}
  \param{x1}{x coordinate of 1st pixel}
  \param{y1}{y coordinate of 1st pixel}
  \param{z1}{z coordinate of 1st pixel}
  \param{x2}{x coordinate of 2nd pixel}
  \param{y2}{y coordinate of 2nd pixel}
  \param{z2}{z coordinate of 2nd pixel}
  \return{an image node}
  \desc{extracts a subimage from image im by cutting it from coordinates (x1,y1,z1) to (x2,y2,z2)}
  \cfunction{\cfimcut}
  \cfile{geom.c}
  \example{}{}
*/

  xlim1 = xlgaimage();
  x1 = (int)getfixnum(xlgafixnum());
  y1 = (int)getfixnum(xlgafixnum());
  z1 = (int)getfixnum(xlgafixnum());
  x2 = (int)getfixnum(xlgafixnum());
  y2 = (int)getfixnum(xlgafixnum());
  z2 = (int)getfixnum(xlgafixnum());


  xllastarg();

  return(cvimage(imcut((IMAGE *)getimage(xlim1), x1, y1, z1, x2, y2, z2)));
}

LVAL iimputop()
{
  LVAL xlim1, xlim2;
  int x, y, z, op;

  if (!moreargs())
    xlabort("(@imputop imin im x y z op) ; puts im in imin and returns imin (destructive) using op operation type");

/*
  \lspfunction{@}{imputop}{imin im x y z op}
  \param{imin}{an image node}
  \param{im}{an image node}
  \param{x}{x coordinate of 1st pixel}
  \param{y}{y coordinate of 1st pixel}
  \param{z}{z coordinate of 1st pixel}
  \param{op}{integer for operation type}
  \return{the first image node (imin)}
  \desc{imin is modified by merging it with the values of im, the origin of im being set at the coordinates (x,y,z) of imin.  The merging is performed using op operation type such as INF_op, SUP_op, etc. defined for point operators.  In addition, the op value OVW_op can de used to overwrite imin with the values of im.  Note that the definition domain of im must be a subset of that of imin.}
  \cfunction{\cfimputop}
  \cfile{geom.c}
  \example{(@imputop imin im 10 10 0 INF_op)}{}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  x = (int)getfixnum(xlgafixnum());
  y = (int)getfixnum(xlgafixnum());
  z = (int)getfixnum(xlgafixnum());
  op = (int)getfixnum(xlgafixnum());

  xllastarg();

  if (imputop((IMAGE *)getimage(xlim1),(IMAGE *)getimage(xlim2), x, y, z, op) == ERROR)
    return NIL;

  return xlim1;
}

LVAL iimputcompose()
{
  LVAL xlim1, xlim2, xlim3;
  int x, y, z, val;

  if (!moreargs())
    xlabort("(@imputcompose imin imlbl im x y z val) ; puts im in imin and returns imin (destructive) using op operation type");

/*
  \lspfunction{@}{imputcompose}{imin imlbl im x y z val}
  \param{imin}{an image node}
  \param{imlbl}{an image node}
  \param{im}{an image node}
  \param{x}{x coordinate of 1st pixel}
  \param{y}{y coordinate of 1st pixel}
  \param{z}{z coordinate of 1st pixel}
  \param{val}{integer for label value}
  \return{the first image node (imin)}
  \desc{imin is modified by merging it with the values of im, the origin of im being set at the coordinates (x,y,z) of imin.  The image imlbl has the same extent as imin, the values of im are inserted in imin if and only if the value of imlbl at the corresponding position equals val.  Note that the definition domain of im must be a subset of that of imin.}
  \cfunction{\cfimputcompose}
  \cfile{geom.c}
  \example{(@imputcompose imin im 10 10 0 INF_op)}{}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();
  x = (int)getfixnum(xlgafixnum());
  y = (int)getfixnum(xlgafixnum());
  z = (int)getfixnum(xlgafixnum());
  val = (int)getfixnum(xlgafixnum());

  xllastarg();

  if (imputcompose((IMAGE *)getimage(xlim3),(IMAGE *)getimage(xlim2),(IMAGE *)getimage(xlim1), x, y, z, val) == ERROR)
    return NIL;

  return xlim1;
}

LVAL iframebox()
{
  LVAL xlim1;
  G_TYPE gval;
  int i;
  int box[BOXELEM];

  if (!moreargs())
    xlabort("(@framebox im l r t b u d val)");

/*
  \lspfunction{@}{framebox}{im l r t b u d val}
  \param{im}{an image node}
  \param{l}{width of left frame}
  \param{r}{width if right frame}
  \param{t}{width of top frame}
  \param{b}{width if bottom frame}
  \param{u}{width of upper frame}
  \param{d}{width if lower frame}
  \param{val}{value of frame}
  \return{the image node im}
  \desc{sets the values of the image frame to val.  The geometry of the frame is specified by the width parameters.}
  \cfunction{\cfframebox}
  \cfile{geom.c}
*/
  xlim1 = xlgaimage();
  for (i=0; i<BOXELEM; i++)
    box[i]= (unsigned)getfixnum(xlgafixnum());
  gval = getgenericnum(GetImDataType((IMAGE *)getimage(xlim1)));
  
  xllastarg();

  if (framebox((IMAGE *)getimage(xlim1), box, gval) == ERROR)
    return(NIL);

  return(xlim1);
}

LVAL iaddframebox()
{
  LVAL xlim1;
  G_TYPE gval;
  int i;
  int box[BOXELEM];
  unsigned long int nbyte;

  if (!moreargs())
    xlabort("(@addframebox im l r t b u d val)");
/*
  \lspfunction{@}{addframebox}{im l r t b u d val}
  \param{im}{an image node}
  \param{l}{width of left frame}
  \param{r}{width if right frame}
  \param{t}{width of top frame}
  \param{b}{width if bottom frame}
  \param{u}{width of upper frame}
  \param{d}{width if lower frame}
  \param{val}{value of frame}
  \return{the image node im}
  \desc{adds a frame of value val to im (destructive function).  The geometry of the frame is specified by the width parameters.}
  \cfunction{\cfaddframebox}
  \cfile{geom.c}
*/
  xlim1 = xlgaimage();
  for (i=0; i<BOXELEM; i++)
    box[i]= (unsigned)getfixnum(xlgafixnum());
  gval = getgenericnum(GetImDataType((IMAGE *)getimage(xlim1)));
  
  xllastarg();

  nbyte = GetImNByte((IMAGE *)getimage(xlim1));

  if (addframebox((IMAGE *)getimage(xlim1), box, gval) == ERROR)
    return(NIL);

  total += (GetImNByte((IMAGE *)getimage(xlim1))-nbyte);

  return(xlim1);
}

LVAL isubframebox()
{
  LVAL xlim1;
  int i;
  int box[BOXELEM];
  long nbyte;

  if (!moreargs())
    xlabort("(@subframebox im l r t b u d)");
/*
  \lspfunction{@}{subframebox}{im l r t b u d}
  \param{im}{an image node}
  \param{l}{width of left frame}
  \param{r}{width if right frame}
  \param{t}{width of top frame}
  \param{b}{width if bottom frame}
  \param{u}{width of upper frame}
  \param{d}{width if lower frame}
  \param{val}{value of frame}
  \return{the image node im}
  \desc{peels off a frame of im (destructive function).  The geometry of the frame is specified by the width parameters.}
  \cfunction{\cfsubframebox}
  \cfile{geom.c}
*/


  xlim1 = xlgaimage();
  for (i=0; i<BOXELEM; i++)
    box[i]= (unsigned)getfixnum(xlgafixnum());
  
  xllastarg();

  nbyte = GetImNByte((IMAGE *)getimage(xlim1));

  if (subframebox((IMAGE *)getimage(xlim1), box) == ERROR)
    return(NIL);

  total -= (nbyte-GetImNByte((IMAGE *)getimage(xlim1)));

  return(xlim1);
}

LVAL imagnify()
{
  LVAL xlim1;
  int n;

  if (!moreargs())
    xlabort("(*magnify im n)");
/*
  \lspfunction{*}{magnify}{im n}
  \param{im}{an image node}
  \param{n}{a positive integer for magnifying size by pixel replication}
  \return{an image node containing the magnified image}
  \desc{}
  \cfunction{\cfmagnify}
  \cfile{geom.c}
*/
  xlim1 = xlgaimage();
  n = (int)getfixnum(xlgafixnum());

  xllastarg();

  return(cvimage(magnify((IMAGE *)getimage(xlim1), n)));
}

LVAL igetboundingbox()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(*getboundingbox im)");
/*
  \lspfunction{*}{getboundingbox}{im}
  \param{im}{an image node}
  \return{an image node containing the bounding box coordinates of im}
  \desc{the bounding box coordinates are the upper left and lower rigth image coordinates of the smallest rectangle containing all non zero values of the input image im.  The returned image is a 1-D image with nx equal to 4 and coordinates stored in the following order: ulc_x, ulc_y, lrc_x, and lrc_y.}
  \cfunction{\cfgetboundingbox}
  \cfile{geom.c}
*/


  xlim1 = xlgaimage();
  
  xllastarg();

  return(cvimage(getboundingbox((IMAGE *)getimage(xlim1))));
}
#endif

LVAL iplotline()
{
  LVAL xlim1;
  int x1, y1, x2, y2, val;

  if (!moreargs())
    xlabort("(@plotline im x1 y1 x2 y2 val)");
/*
  \lspfunction{@}{plotline}{im x1 y1 x2 y2 val}
  \param{im}{an image node}
  \param{x1}{an integer for x-coordinate of 1st point}
  \param{y1}{an integer for y-coordinate of 1st point}
  \param{x2}{an integer for x-coordinate of 2nd point}
  \param{y2}{an integer for y-coordinate of 2nd point}
  \param{val}{an integer for value of line pixels}
  \return{im}
  \desc{draws a line at value 1 in im from (x1,y1) to (x2,y2) by setting all pixels of the line to the value val.}
  \cfunction{\cfplotline}
  \cfile{bresenham.c}
*/
  xlim1 = xlgaimage();
  x1 = (unsigned int)getfixnum(xlgafixnum());
  y1 = (unsigned int)getfixnum(xlgafixnum());
  x2 = (unsigned int)getfixnum(xlgafixnum());
  y2 = (unsigned int)getfixnum(xlgafixnum());
  val= (int)getfixnum(xlgafixnum());

  xllastarg();

  if (plotline((IMAGE *)getimage(xlim1), x1, y1, x2, y2, val)==ERROR)
    return NIL;

  return(xlim1);
}

LVAL icompose()
{
  LVAL xlim1, xlim2, xlim3, xlim4;
  int graph;

  if (!moreargs())
    xlabort("(@compose marker mask g lbl graph) ;");

/*
  \lspfunction{@}{compose}{marker mask g lbl graph}
  \param{marker}{an image node for marker}
  \param{mask}{an image node for  mask (typically gradient image)}
  \param{g}{an image node indicating degree of overlap (0 for no overlap)}
  \param{lbl}{an image node such that $lbl(x)=\sum_2^i |  f_i(x)$ is in ROI of $f_i$}
  \param{graph}{integer for connectivity}
  \return{the first image node (marker)}
  \desc{The marker image .  The g image is created by the @gorder function that creates pseudo overlap levels: increasing values of g indicates regions created by a given combination of images and with increasing degree of overlap.  see \citep{soille2006pami} for details.}
  \cfunction{\cfcompose}
  \cfile{wsfah.c}
  \example{(@compose marker mask g lbl 8)}{}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();
  xlim4 = xlgaimage();
  graph = (int)getfixnum(xlgafixnum());

  xllastarg();

  if (compose((IMAGE *)getimage(xlim1),(IMAGE *)getimage(xlim2),(IMAGE *)getimage(xlim3),(IMAGE *)getimage(xlim4),graph) == ERROR)
    return NIL;

  return xlim1;
}

LVAL iovlmatrix()
{
  LVAL xlim1, xlim2;
  char *odir;

  if (!moreargs())
    xlabort("(ovlmatrix matrix maxg_arra odir)");
/*
  \lspfunction{}{ovlmatrix}{matrix maxg_array odir}
  \param{matrix}{an image node holding overlap matrix}
  \param{maxg_array}{an image node holding max number of overlap for each image}
  \param{odir}{string referring to output directory}
  \return{t on success, nil otherwise}
  \desc{}
  \cfunction{\cfovlmatrix}
  \cfile{geom.c}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  odir = (char *)getstring(xlgetfname());
  
  xllastarg();

  if (ovlmatrix((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), odir) == ERROR)
    return NIL;

  return s_true;
}

LVAL igrid()
{
  LVAL xlim1, xlim2, xlim3, xlim4;
  float alpha;

  if (!moreargs())
    xlabort("(*grid im roi imx imy alpha)");

/*
  \lspfunction{*}{grid}{im roi imx imy alpha}
  \param{im}{an image node}
  \param{roi}{an image node of type t_UCHAR}
  \param{imx}{an image node}
  \param{imy}{an image node}
  \param{alpha}{a floating point value}
  \return{an image node}
  \desc{given a base image im, computes the bicubic interpolation \cite{keys81} of the points whose x-y coordinates in this base image are given by the images imx and imy.   Recommended values for alpha are -0.5 or -2/3 (rather than -1), see \cite{park-schowengerdt83}.  16 pixels of the input image are needed for interpolating each point.  If one or more points are not falling in the definition domain of im, no interpolation is performed.  Overshoots (i.e., interpolated values not fitting in range of the input image) are set to either the min or max value authorised by the considered data type.\linebreak  A bilinear interpolation is computed when setting alpha to 0.0.  For the nearest neighbour interpolation, alpha should be set to 1.0.  In these two latter cases, only 4 pixels of the input image are needed for interpolating each point.\linebreak  The roi image indicates those image pixels of the input image that represent data points (0 if no data, 1 otherwise).}
  \cfunction{\cfgrid}
  \cfile{grid.c}
  \example{}{}
*/
// the corresponding interpolation function can be thought as a smooth
// approximation of the triangular function associated with linear
// interpolation.

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();
  xlim4 = xlgaimage();
  alpha = (float) getflonum(xlgaflonum());
  xllastarg();

  return (cvimage(grid((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), (IMAGE *)getimage(xlim3), (IMAGE *)getimage(xlim4), alpha)) );
}

#ifdef NNI
LVAL inni()
{
  LVAL xlim1, xlim2, xlim3;
  double startx, starty, incx, incy, wmin;
  int nx, ny;

  if (!moreargs())
    xlabort("(*nni im imx imy startx starty inx incy nx ny wmin)");

/*
  \lspfunction{*}{nni}{im imx imy startx starty inx incy nx ny wmin}
  \param{im}{an image node}
  \param{imx}{an image node of the same size as holding the x coordinates of each pixel}
  \param{imy}{an image node of the same size as holding the y coordinates of each pixel}
  \param{startx}{a floating value with x-coordinate of first point}
  \param{starty}{a floating value with y-coordinate of first point}
  \param{incx}{a floating value with increment in x}
  \param{incy}{a floating value with increment in y}
  \param{nx}{integer with number of points of grid in x}
  \param{ny}{integer with number of points of grid in y}
  \param{wmin}{a floating value with minimum allowed weight (0.0 for interpolation restricted to convex hull of input points)}
  \return{an image node}
  \desc{Calls Pavel Sakov's code for nni (CSIRO Marine Research).  This latter code relies itself on efficient triangle.c (version 1.4) from Jonathan Richard Shewchuk, Berkeley California.  Downloaded from googlecode  on 20140226}
  \cfunction{\cfnni}
  \cfile{natngbint.c}
  \example{}{}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();
  startx = (double) getflonum(xlgaflonum());
  starty = (double) getflonum(xlgaflonum());
  incx = (double) getflonum(xlgaflonum());
  incy = (double) getflonum(xlgaflonum());
  nx = (double) getfixnum(xlgafixnum());
  ny = (double) getfixnum(xlgafixnum());
  wmin = (double) getflonum(xlgaflonum());
  xllastarg();

  return (cvimage(nni((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), (IMAGE *)getimage(xlim3), startx, starty, incx, incy, nx, ny, wmin )) );
}
#endif /* NNI */



/*
  \lispsection
  \input{geometry.tex}
*/


#ifdef ALLFUNCTIONS
/*
\module{Statistics} % Mostly imstat.c
\basesection
*/
LVAL ihisto1d()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(*histo1d im)");

/*
  \lspfunction{*}{histo1d}{im}
  \param{im}{an image node}
  \return{an image node}
  \desc{computes the frequency distribution (histogram) of the grey levels of im}
  \cfunction{\cfhistoONEd}
  \cfile{imstat.c}
*/
  xlim1 = xlgaimage();

  xllastarg();

  return(cvimage(histo1d((IMAGE *)getimage(xlim1))));
}

LVAL ihisto2d()
{
  LVAL xlim1, xlim2;

  if (!moreargs())
    xlabort("(*histo2d im1 im2)");
/*
  \lspfunction{*}{histo2d}{im1 im2}
  \param{im1}{an image node}
  \param{im2}{an image node}
  \return{an image node}
  \desc{computes the frequency distribution (2D histogram) of the grey levels pairs defined by im1 and im2}
  \cfunction{\cfhistoTWOd}
  \cfile{imstat.c}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();

  xllastarg();

  return(cvimage(histo2d((IMAGE *)getimage(xlim1),(IMAGE *)getimage(xlim2))));
}

LVAL ihisto3d()
{
  LVAL xlim1, xlim2, xlim3;

  if (!moreargs())
    xlabort("(*histo3d im1 im2 im3)");
/*
  \lspfunction{*}{histo3d}{im1 im2 im3}
  \param{im1}{an image node}
  \param{im2}{an image node}
  \param{im3}{an image node}
  \return{an image node}
  \desc{computes the frequency distribution (3D histogram) of the grey levels pairs defined by im1, im2, and im3}
  \cfunction{\cfhistoTHREEd}
  \cfile{imstat.c}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();

  xllastarg();

  return(cvimage(histo3d((IMAGE *)getimage(xlim1),(IMAGE *)getimage(xlim2),(IMAGE *)getimage(xlim3))));
}

LVAL irsum()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(*rsum im)");

/*
  \lspfunction{*}{rsum}{im}
  \param{im}{an image node}
  \return{an image node}
  \desc{computes the cumulative frequency distribution of the grey levels of im}
  \cfunction{\cfrsum}
  \cfile{imstat.c}
  \example{}{}
*/


  xlim1 = xlgaimage();

  xllastarg();

  return(cvimage(rsum((IMAGE *)getimage(xlim1))));
}

LVAL ilookup()
{
  LVAL xlim1, xlim2;

  if (!moreargs())
    xlabort("(@lookup im imlut)");
/*
  \lspfunction{@}{lookup}{im imlut}
  \param{im}{an image node}
  \param{imlut}{an image node holding a look-up-table (type float)}
  \return{im}
  \desc{pixels values of im are mapped using the mapping function defined by the look-up-table stored in imlut.  Imlut must be of type t_FLOAT.}
  \cfunction{\cflookup}
  \cfile{imstat.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();

  xllastarg();

  if (lookup((IMAGE *)getimage(xlim1),(IMAGE *)getimage(xlim2))==ERROR)
    return NIL;

  return(xlim1);
}

LVAL ilookuptypematch()
{
  LVAL xlim1, xlim2;

  if (!moreargs())
    xlabort("(@lookuptypematch im imlut)");
/*
  \lspfunction{@}{lookuptypematch}{im imlut}
  \param{im}{an image node}
  \param{imlut}{an image node holding a look-up-table (same type as im)}
  \return{im}
  \desc{pixels values of im are mapped using the mapping function defined by the look-up-table stored in imlut. Should superseeds @lookup to avoid type conversion within the routine (better control).}
  \cfunction{\cflookuptypematch}
  \cfile{imstat.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();

  xllastarg();

  if (lookuptypematch((IMAGE *)getimage(xlim1),(IMAGE *)getimage(xlim2))==ERROR)
    return NIL;

  return(xlim1);
}



LVAL ilookuprgb()
{
  LVAL xlim1, xlim2, xlim3, xlim4;

  if (!moreargs())
    xlabort("(@lookuprgb imr img imb imlut)");
/*
  \lspfunction{*}{lookuprgb}{imr img imb imlut}
  \param{imr}{an image node for first channel}
  \param{img}{an image node for 2nd channel}
  \param{imb}{an image node for 3rd channel}
  \param{imlut}{an image node holding a 3D look-up-table (type UCHAR)}
  \return{im}
  \desc{pixels values of im are mapped using the mapping function defined by the look-up-table stored in imlut.  Imlut must be of type t_FLOAT.}
  \cfunction{\cflookup}
  \cfile{imstat.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();
  xlim4 = xlgaimage();

  xllastarg();

  return(cvimage(lookuprgb((IMAGE *)getimage(xlim1),(IMAGE *)getimage(xlim2),(IMAGE *)getimage(xlim3),(IMAGE *)getimage(xlim4))));
}



LVAL ihistcompress()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(@histcompress im)");
/*
  \lspfunction{@}{histcompress}{im}
  \param{im}{an image node}
  \return{im}
  \desc{redistributes the intensity values of im in such a way that the minimum value is zero and that all subsequent intensity values are used until the maximum value (i.e. no gaps).}
  \cfunction{\cfhistcompress}
  \cfile{imstat.c}
  \example{}{}
*/
  xlim1 = xlgaimage();

  xllastarg();

  if (histcompress((IMAGE *)getimage(xlim1))==ERROR)
    return NIL;

  return(xlim1);
}

LVAL iimequalp()
{
  LVAL xlim1, xlim2;

  if (!moreargs())
    xlabort("(imequalp im1 im2)");

/*
  \lspfunction{}{imequalp}{im1 im2}
  \param{im1}{an image node}
  \param{im2}{an image node}
  \return{true on success, nil otherwise}
  \desc{tests whether the two input images are identical.}
  \cfunction{\cfimequalp}
  \cfile{imstat.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();

  xllastarg();
  
  if (imequalp((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2)) == ERROR)
    return NIL;
  return s_true;
}

LVAL ivolume()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(*volume im)");
/*
  \lspfunction{*}{volume}{im}
  \param{im}{an image node}
  \return{a floating point number}
  \desc{computes the sum of the grey levels of im}
  \cfunction{\cfvolume}
  \cfile{imstat.c}
  \example{}{}
*/
  xlim1 = xlgaimage();

  xllastarg();

  if (volume((IMAGE *)getimage(xlim1))==ERROR)
    return NIL;
  
  return(cvflonum((float) xlim1->n_info.n_image->vol));
}

LVAL iarea()
{
  LVAL xlim1;
  int r, type;

  if (!moreargs())
    xlabort("(*area im r type)");

/*
  \lspfunction{*}{area}{im r type}
  \param{im}{an image node}
  \param{r}{integer for length of the side of each square pixel expressed in units of the pixel intensity values}
  \param{type}{integer in [0,2] for algorithm selection}
  \return{an image node (type DOUBLE)}
  \desc{computes the surface area of a grey tone image by setting the corner of each square cell to the area of its square influence zone.  It follows that the returned image is one pixel less in x and y directions.  The variable type is used for specifying the way the area is calculated:  0 for triangulation at  45 degrees,   1 for triangulation at -45 degrees,  2 for triangular prism method.}
  \cfunction{\cfarea}
  \cfile{imstat.c}
  \example{}{}
*/

  xlim1 = xlgaimage();
  r    = (int)getfixnum(xlgafixnum());
  type = (int)getfixnum(xlgafixnum());

  xllastarg();

  return(cvimage(area((IMAGE *)getimage(xlim1), r, type)));
}

LVAL iclass2d()
{
  LVAL xlim1, xlim2, xlim3;

  if (!moreargs())
    xlabort("(*class2d im1 im2 imlut)");

/*
  \lspfunction{*}{class2d}{im1 im2 imlut}
  \param{im1}{an image node}
  \param{im2}{an image node}
  \param{imlut}{an image node}
  \return{an image node}
  \desc{classifies the pixels of the image pair (im1,im2) using imlut as labelled 2-D histogram.}
  \cfunction{\cfclassTWOd}
  \cfile{imstat.c}
  \example{}{}
*/


  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();

  xllastarg();

  return(cvimage(class2d((IMAGE *)getimage(xlim1),(IMAGE *)getimage(xlim2),(IMAGE *)getimage(xlim3))));
}

#endif

LVAL idirmax()
{
  LVAL xlim1;
  int dir;

/*
  \lspfunction{@}{dirmax}{im dir}
  \param{im}{an image node}
  \param{dir}{integer in {0,...,3} for direction: 0 for bottom to top, 1 for left to right, 2 for top to bottom, 3 for right to left. }
  \return{im}
  \desc{propagates the current maximum value encountered when scanning the image in a given direction.}
  \cfunction{\cfdirmax}
  \cfile{imstat.c}
  \example{}{}
*/

  if (!moreargs())
    xlabort("(@dirmax im dir); dir in {0,1,2,3}");

  xlim1 = xlgaimage();
  dir = (int) getfixnum(xlgafixnum());

  xllastarg();

  if (dirmax((IMAGE *)getimage(xlim1), dir)==ERROR)
    return NIL;

  return(xlim1);
}

LVAL idirsum()
{
  LVAL xlim1;
  int dir;

/*
  \lspfunction{*}{dirsum}{im dir}
  \param{im}{an image node}
  \param{dir}{integer: 0 for horizontal  and 1 for vertical sum}
  \return{an image node holding the sum of the image values in the specified direction (resp. nx columns for vertical and ny lines for horizontal directions).}
  \desc{}
  \cfunction{\cfdirsum}
  \cfile{imstat.c}
  \example{}{}
*/

  if (!moreargs())
    xlabort("(@dirsum im dir); dir=0 for horizontal and 1 for vertical");

  xlim1 = xlgaimage();
  dir = (int) getfixnum(xlgafixnum());

  xllastarg();

  return(cvimage(dirsum((IMAGE *)getimage(xlim1), dir)));
}

LVAL isortindex()
{
  LVAL xlim1;
/*
  \lspfunction{*}{sortindex}{im}
  \param{im}{an image node}
  \return{an image node}
  \desc{returns a 1-D image (INT32 type) with the first pixel giving the offset to the smallest value of im, the second pixel the second smallest value, etc.}
  \cfunction{\cfsortindex}
  \cfile{indexx.c}
  \example{}{}
*/

  if (!moreargs())
    xlabort("(*sortindex im )");
  xlim1 = xlgaimage();
  xllastarg();
  return(cvimage(sortindex((IMAGE *)getimage(xlim1))));
}


LVAL idendro()
{
/*
  \lspfunction{*}{dendro}{imlist fn}
  \param{imlist}{a list of labelled images of a hierarchy}
  \param{fn}{a string for output ascii filename to write the dendrogram description}
  \return{true on success, nil on error}
  \desc{to use in combination with gnuplot to draw a 3D (spatially rooted) dendrogram matching a hierarchy of segmentations of a 2-D image.}
  \cfunction{\cfdendro}
  \cfile{imstat.c}
  \example{}{}
*/
  LVAL arg, arg2;
  IMAGE *imarray[maxNumberOfBands];
  int n=0;
  char *fn;

  //imarray[0]= (IMAGE *)getimage(xlgaimage());
  //imarray[1]= (IMAGE *)getimage(xlgaimage());
  arg=xlgetarg();
  if (listp(arg)){
    for (n = 0; consp(arg); arg = cdr(arg)){
      fprintf(stderr, "n=%d", n);
      arg2 = car(arg);
      if (imagep(arg2)){
	imarray[n++]=(IMAGE*)getimage(arg2);
        iminfo(imarray[n-1]);
      }
      else{
	xlbadtype(arg2);
	break;
      }
    }
  }
  else
    xlbadtype(arg);

  fprintf(stderr, "\n IMARRAY SET, now calling dendro n=%d\n", n);
  fn  = (char *)getstring(xlgetfname());

  iminfo(imarray[0]);
  iminfo(imarray[1]);
  if (dendro((IMAGE **)imarray, n, fn)==ERROR)
    return NIL;
  return(s_true);
}

LVAL ihistrgbmatch()
{
  LVAL result;
  LVAL xlim, xlim1, xlim2, xlim3, xlim4;
  int n=3, m;
  IMAGE **imarray;

  if (!moreargs())
    xlabort("(*histrgbmatch )\n");
/*
  \lspfunction{*}{histrgbmatch}{}
  \param{}{an image for }
  \param{}{an image for }
  \param{}{an image for }
  \param{}{an image for }
  \return{an image list with the three 3-D LUTs (for the output R, G, and B values respectively)}
  \desc{}
  \creationdate{20130430}
  \cfunction{\cfhistrgbmatch}
  \cfile{histo.c}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();
  xlim4 = xlgaimage();
  xllastarg();
 
  if ( (imarray=histrgbmatch((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2),  (IMAGE *)getimage(xlim3),  (IMAGE *)getimage(xlim4))) == NULL)
    xlabort("(*histrgbmatch ): returned NULL");
    
  SAVE_LIST_IMAGES( imarray, xlim, result, n, m );
  free(imarray);
  
  return result;
}

LVAL ihistrgb3dmatch()
{
  LVAL result;
  LVAL xlim, xlim1, xlim2, xlim3, xlim4;
  int n=3, m;
  IMAGE **imarray;

  if (!moreargs())
    xlabort("(*histrgb3dmatch )\n");
/*
  \lspfunction{*}{histrgb3dmatch}{}
  \param{}{an image for }
  \param{}{an image for }
  \param{}{an image for }
  \param{}{an image for }
  \return{an image list with the three 3-D LUTs (for the output R, G, and B values respectively)}
  \desc{}
  \creationdate{20130430}
  \cfunction{\cfhistrgbTHREEdmatch}
  \cfile{histo.c}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();
  xlim4 = xlgaimage();
  xllastarg();
 
  if ( (imarray=histrgb3dmatch((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2),  (IMAGE *)getimage(xlim3),  (IMAGE *)getimage(xlim4))) == NULL)
    xlabort("(*histrgb3dmatch ): returned NULL");
    
  SAVE_LIST_IMAGES( imarray, xlim, result, n, m );
  free(imarray);
  
  return result;
}

LVAL icondmean()
{
  LVAL argi, arg, arg2;
  IMAGE *imarray[maxNumberOfBands];
  int n=0;

  printf("COUCOUC\n");
  printf("COUCOUC\n");
  printf("COUCOUC\n");

  if (!moreargs())
    xlabort("(@condmean imlist)\n");
/*
  \lspfunction{@}{condmean}{imlist}
  \param{imlist}{a list of 6 image nodes holding the channels of a multichannel image}
  \return{the input image list with the 3 first channels normalised according to the conditional mean method described in \citep{carlotto99ijrs}.}
  \desc{For haze removal purposes, the 3 first channels should be in the visible range while the 3 last channels in the non-visible range (typically used for Landsat type sensors).}
  \cfunction{\cfcondmean}
  \cfile{mblincomb.c}
*/
  arg=xlgetarg();
  argi=arg;
  if (listp(arg)){
    for (n = 0; consp(arg); arg = cdr(arg)){
      arg2 = car(arg);           
      if (imagep(arg2))                    
	imarray[n++]=(IMAGE*)getimage(arg2); 
      else{                      
	xlbadtype(arg2);          
	break;                    
      }                           
    }
  }
  else
    xlbadtype(arg);

  printf("CO\n");
  printf("CO\n");
  printf("CO\n");
  if (condmean(imarray, n) == ERROR)
    return NIL;
  return argi;  
}


/************ CLASSIFIERS *************/

LVAL ist()
{
  LVAL xlim1, xlim2;

  if (!moreargs())
    xlabort("(*classstatsinfo imin immos)");

/*
  \lspfunction{*}{classstatsinfo}{imin immos}
  \param{imin}{an image node}
  \param{lmmos}{a mosaic (class) image}
  \return{}
  \desc{Prints class statistics of multiband image 'imin', according to classes defined in 'immos'.}
  \cfunction{\cfclassstatsinfo}
  \cfile{marcin.c}
  \authors{Marcin Iwanowski}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();

  xllastarg();

  if (classstatsinfo((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2))==ERROR)
    return NIL;

  return(xlim1);    
}


LVAL iclmindist()
{
  LVAL xlim1, xlim2;
  int bklab, mode;
  double thr;
  if (!moreargs())
    xlabort("(@cl_mindist immos imin bklabel &opt (thr))");

/*
  \lspfunction{@}{clmindist}{immos imin bklabel &opt (thr)}
  \param{immos}{mosaic image - initial classification and the result}
  \param{imin}{multispectral image}
  \param{bklabel}{label of background class (which is not considered in clasification)}
  \param{thr}{threshold for distance value}
  \return{immos}
  \desc{Minimum distance classifier.}
  \cfunction{\cfclmindist}
  \cfile{marcin.c}
  \authors{Marcin Iwanowski}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  bklab = (int)getfixnum(xlgafixnum());
  if (!moreargs())
    {mode = 0; thr = 1.0; /* thr doesn't matter in this case */}
  else
    { mode  = 1;
      thr  = (double)getflonum(xlgaflonum()); }
 
  xllastarg();

  if (clmindist((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), bklab, mode, thr )==ERROR)
    return NIL;

  return(xlim1);    
}

LVAL iclparpip()
{
  LVAL xlim1, xlim2;
  int bklab, mode;
  double mult=0.0;
  if (!moreargs())
    xlabort("(@clparpip immos imin bklabel &opt (mode mult))");
 
/*
  \lspfunction{@}{clparpip}{immos imin bklabel &opt (mode mult)}
  \param{immos}{mosaic image - initial classification and the result}
  \param{imin}{multispectral image}
  \param{bklabel}{label of background class (which is not considered in clasification)}
  \param{mode}{type of operation}
  \param{mult}{multiplicator of standart deviation (for mode = 1,3,5)}
  \return{immos}
  \desc{Parallelpiped classifier. The variable 'mode' indictaes type of classification: mode = 0,2,4 - min and max values among all the members of pixels which belong to every class ('mult' doesn't play a role in this case), mode = 1,3,5 - in each dimension - mean value +/- the multiplication (by parameter 'mult') of standard deviation, mode = 0,1 - pixels from inseparability regions are classified to the last class matched, mode = 2,3 - pixels from inseparability regions are not classified, mode = 4,5 - returns number of classes to which each image point belong.}
  \cfunction{\cfclparpip}
  \cfile{marcin.c}
  \authors{Marcin Iwanowski}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  bklab = (int)getfixnum(xlgafixnum());
  if (!moreargs())
    {mode = 1; mult = 1.0;}
  else
    {
     mode  = (int)getfixnum(xlgafixnum());
     if ((mode&1) == 1) mult  = (double)getflonum(xlgaflonum());  /* only for +/- stddev i.e. mode == 1,3,5 */
    }
  xllastarg();

  if (clparpip((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), bklab, mode, mult )==ERROR)
    return NIL;

  return(xlim1);    
}

LVAL iclmaha()
{
  LVAL xlim1, xlim2;
  int bklab, mode;
  double thr;
  if (!moreargs())
    xlabort("(@clmaha immos imin bklabel)");

/*
  \lspfunction{@}{clmaha}{immos imin bklabel &opt (thr)}
  \param{immos}{mosaic image - initial classification and the result}
  \param{imin}{multispectral image} 
  \param{bklabel}{label of background class (which is not considered in clasification)}
  \param{thr}{threshold for distance value}
  \return{immos}
  \desc{Mahalonobis distance classifier.}
  \cfunction{\cfclmaha}
  \cfile{marcin.c}
  \authors{Marcin Iwanowski}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  bklab = (int)getfixnum(xlgafixnum());
  if (!moreargs())
    {mode = 0; thr = 1.0; /* thr doesn't matter in this case */ }
  else
    {
      mode  = 1;
      thr  = (double)getflonum(xlgaflonum());
    }
  xllastarg();

  if (clmaha((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), bklab, mode, thr )==ERROR)
    return NIL;

  return(xlim1);    
}


LVAL iclmaxlike()
{
  LVAL xlim1, xlim2;
  int bklab,type;
  double thr=1.0;
  if (!moreargs())
    xlabort("(@cl_maxlike immos imin bklabel &opt (type thr)");

/*
  \lspfunction{@}{clmaxlike}{immos imin bklabel &opt (type thr)}
  \param{immos}{mosaic image - initial classification and the result}
  \param{imin}{multispectral image}
  \param{bklabel}{label of background class (which is not considered in clasification)}
  \param{type}{type of operation}
  \param{thr}{discriminant fuction threshold (for type = 4,5,6)}
  \return{immos}
  \desc{Maximum likelihood classifier. Variable 'mode' - indicates a way of computing the classification: mode  = 0 - no threshold ('th' not used), mode = 1 - distance threshold ('th' indicates threshold = power of 2 of maximum acceptable Mahalonobis distance)}
  \cfunction{\cfclmaxlike}
  \cfile{marcin.c}
  \authors{Marcin Iwanowski}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  bklab = (int)getfixnum(xlgafixnum());
  if (!moreargs())
    {type = 0; thr = 1.0; /* thr doesn't matter in this case */ }
  else
    {
     type  = (int)getfixnum(xlgafixnum());
     if (type >3) thr  = (double)getflonum(xlgaflonum());
    }
  xllastarg();
 
  if (clmaxlike((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), bklab, type, thr )==ERROR)
    return NIL;

  return(xlim1);    
}

/********************** END OF CLASSIFIERS ********************/



/*
  \lispsection
  \input{imstat.tex}
*/

/* 
\module{Point operators} % Mostly in pointop.c
\text{When there are more than one input image, they must all have the same definition domain.  Except for some widely used operations (such as the so-called arithop operations), they must have the same pixel data type.}
\basesection
*/
LVAL ibitwise_op()
{
  LVAL xlim1, xlim2;
  int op;

  if (!moreargs())
    xlabort("(@bitwiseop im1 im2 op)\n op: AND_op (10), OR_op (11), or XOR_op (12");

/*
  \lspfunction{@}{bitwiseop}{im1 im2 op}
  \param{im1}{an image node}
  \param{im2}{an image node}
  \param{op}{integer for type of bitwise operation}
  \return{im1 (destructive function)}
  \desc{bitwise operation bewteen two images.  The following codes are defined: 10 or AND_op, 11 or OR_op, and 12 or XOR_op.}
  \cfunction{\cfbitwiseUDop}
  \cfile{pointop.c}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  op    = (int)getfixnum(xlgafixnum());

  xllastarg();

  if (bitwise_op((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), op)==ERROR)
    return NIL;

  return(xlim1);
}

LVAL inot()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(@not im)");
/*
  \lspfunction{@}{not}{im}
  \param{im}{an image node}
  \return{im (destructive function)}
  \desc{destructive negation of image im.  The image im must be of type unsigned char and pixel values should be restricted to 0 and 1.}
  \myseealso{\htmlref{@complement}{@complement}}
  \cfunction{\cfnegation}
  \cfile{pointop.c}
*/

  xlim1 = xlgaimage();

  xllastarg();

  if (negation((IMAGE *)getimage(xlim1))==ERROR)
    return NIL;

  return(xlim1);
}

LVAL iarith()
{
  LVAL xlim1, xlim2;
  int op;

  if (!moreargs())
    xlabort("(@arithop im1 im2 op)\n op: ADD_op, SUB_op, DIV_op, MULT_op, INF_op, SUP_op");
/*
  \lspfunction{@}{arithop}{im1 im2 op}
  \param{im1}{an image node}
  \param{im2}{an image node}
  \param{op}{integer for type of arithmetical/logical operation}
  \return{im1 (destructive function)}
  \desc{arithmetic operation bewteen two images.  The following codes are defined:
  \begin{itemize}
  \item 0/ADD_op for addition (if overflow, output is set to PIX_MAX);
  \item 1/SUB_op for subtraction (if underflow, output is set to PIX_MIN);
  \item 2/MULT_op (if overflow, output is set to PIX_MAX);
  \item 3/DIV_op (if division by 0, output is set to extremum value);
  \item 4/INF_op for pixelwise minimum, and 5/SUP_op for pixelwise maximum;
  \item 6/MASK_op for a masking operation: if p2[i]!=0 then p1[i]=p2[i] otherwise p1[i] unchanged;
  \item 7/ADD_op_ovfl for addition without check for overflows;
  \item 8/SUB_op_ovfl for subtraction without check for overflows;
  \item 10/AND_op;
  \item 11/OR_op;
  \item 12/XOR_op;
  \item 13/CMP_op for comparison: if p1[i]<p2[i] then p1[i]=1 else if p1[i]>p2[i] then p1[i]=2, else p1[i]=0;
  \item 14/ABSSUB_op: p1[i]=abs(p1[i]-p2[i]);
  \item 15/MASK_op2 for a masking operation: if p1[i]==0 then p1[i]=p2[i] otherwise p1[i] unchanged;
  \item 21/NDI_op: for normalised difference index: *p1=(*p1-*p2)/(*p1+*p2).
  \end{itemize}}
  \cfunction{\cfarith}
  \cfile{pointop.c}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  op    = (int)getfixnum(xlgafixnum());

  xllastarg();

  if (arith((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), op)==ERROR)
    return NIL;

  return(xlim1);
}

LVAL iarithcst()
{
  LVAL xlim1;
  G_TYPE gt;
  int op;

  if (!moreargs())
    xlabort("(@arithopcst im cst op)\n op: ADD_op, SUB_op, DIV_op, MULT_op, INF_op, SUP_op, etc");
/*
  \lspfunction{@}{arithopcst}{im cst op}
  \param{im}{an image node}
  \param{cst}{a constant value (same type as pixels of im)}
  \param{op}{integer for type of arithmetic operation}
  \return{im (destructive function)}
  \desc{arithmetic operation bewteen an image and a constant value.  The following codes are defined: 0/ADD_op for addition (if overflow, output is set to PIX_MAX), 1/SUB_op for subtraction (if underflow, output is set to PIX_MIN), 2/MULT_op (if overflow, output is set to PIX_MAX), 3/DIV_op (if division by 0, output is set to extremum value), 4/INF_op for pixelwise minimum, and 5/SUP_op for pixelwise maximum, 6/MASK_op for a masking operation: if cst!=0 then p1[i]=cst otherwise p1[i] unchanged, 7/ADD_op_ovfl for addition without check for overflows, 8/SUB_op_ovfl for subtraction without check for overflows, 9/MULT_op_ovfl, 10/AND_op, 11/OR_op, 12/XOR_op, 13/CMP_op for comparison: if p1[i]<cst then p1[i]=1 else if p1[i]>cst then p1[i]=2, else p1[i]=0, 14/ABSSUB_op: p1[i]=abs(p1[i]-cst), 15/MASK_op2 for a masking operation: if p1[i]==0 then p1[i]=cst otherwise p1[i] unchanged.}
  \cfunction{\cfarithcst}
  \cfile{pointop.c}
*/

  xlim1 = xlgaimage();
  gt = getgenericnum(GetImDataType((IMAGE *)getimage(xlim1)));
  op    = (int)getfixnum(xlgafixnum());

  xllastarg();

  if (arithcst((IMAGE *)getimage(xlim1), gt, op)==ERROR)
    return NIL;

  return(xlim1);
}

LVAL iimabs()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(@abs im)");

/*
  \lspfunction{@}{abs}{im}
  \param{im}{an image node}
  \return{im}
  \desc{compute absolute value of im}
  \cfunction{\cfimabs}
  \cfile{pointop.c}
*/

  xlim1 = xlgaimage();
  xllastarg();

  if (imabs((IMAGE *)getimage(xlim1))==ERROR)
    return NIL;

  return(xlim1);
}

LVAL iimsqrt()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(@sqrt im)");

/*
  \lspfunction{@}{sqrt}{im}
  \param{im}{an image node}
  \return{im}
  \desc{compute square root of im}
  \cfunction{\cfimsqrt}
  \cfile{pointop.c}
*/

  xlim1 = xlgaimage();
  xllastarg();

  if (imsqrt((IMAGE *)getimage(xlim1))==ERROR)
    return NIL;

  return(xlim1);
}

LVAL iimlog()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(@log im)");

/*
  \lspfunction{@}{log}{im}
  \param{im}{an image node (FLOAT)}
  \return{im}
  \desc{compute natural logarithm of im.}
  \cfunction{\cfimlog}
  \cfile{pointop.c}
*/

  xlim1 = xlgaimage();

  xllastarg();

  if (imlog((IMAGE *)getimage(xlim1))==ERROR)
    return NIL;

  return(xlim1);
}

LVAL iimatan()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(@atan im)");

/*
  \lspfunction{@}{atan}{im}
  \param{im}{an image node (FLOAT)}
  \return{im}
  \desc{compute arc tangent of im values.}
  \cfunction{\cfimatan}
  \cfile{pointop.c}
*/

  xlim1 = xlgaimage();

  xllastarg();

  if (imatan((IMAGE *)getimage(xlim1))==ERROR)
    return NIL;

  return(xlim1);
}

LVAL iimacos()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(@acos im)");

/*
  \lspfunction{@}{acos}{im}
  \param{im}{an image node (FLOAT)}
  \return{im}
  \desc{compute arc tangent of im values.}
  \cfunction{\cfimatan}
  \cfile{pointop.c}
*/

  xlim1 = xlgaimage();

  xllastarg();

  if (imacos((IMAGE *)getimage(xlim1))==ERROR)
    return NIL;

  return(xlim1);
}

LVAL iimasin()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(@asin im)");

/*
  \lspfunction{@}{asin}{im}
  \param{im}{an image node (FLOAT)}
  \return{im}
  \desc{compute arc tangent of im values.}
  \cfunction{\cfimatan}
  \cfile{pointop.c}
*/

  xlim1 = xlgaimage();

  xllastarg();

  if (imasin((IMAGE *)getimage(xlim1))==ERROR)
    return NIL;

  return(xlim1);
}

LVAL iimcos()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(@cos im)");

/*
  \lspfunction{@}{cos}{im}
  \param{im}{an image node (FLOAT)}
  \return{im}
  \desc{compute cosine of im values.}
  \cfunction{\cfimatan}
  \cfile{pointop.c}
*/

  xlim1 = xlgaimage();

  xllastarg();

  if (imcos((IMAGE *)getimage(xlim1))==ERROR)
    return NIL;

  return(xlim1);
}

LVAL iimsin()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(@sin im)");

/*
  \lspfunction{@}{sin}{im}
  \param{im}{an image node (FLOAT)}
  \return{im}
  \desc{compute the sine of im values.}
  \cfunction{\cfimatan}
  \cfile{pointop.c}
*/

  xlim1 = xlgaimage();

  xllastarg();

  if (imsin((IMAGE *)getimage(xlim1))==ERROR)
    return NIL;

  return(xlim1);
}

LVAL ipower2p()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(@power2p im)");

/*
  \lspfunction{@}{power2p}{im}
  \param{im}{an image node (unsigned data type)}
  \return{im}
  \desc{power of 2 predicate function: pixels whose values are a power of 2 are set to 1, all other pixels being set to 0.}
  \cfunction{\cfpowerTWOp}
  \cfile{pointop.c}
*/

  xlim1 = xlgaimage();

  xllastarg();

  if (power2p((IMAGE *)getimage(xlim1))==ERROR)
    return NIL;

  return(xlim1);
}

LVAL ithresh()
{
  LVAL xlim1;
  G_TYPE gt1, gt2, gfg, gbg;

  if (!moreargs())
    xlabort("(@thresh im t1 t2 bg fg)");

/*
  \lspfunction{@}{thresh}{im t1 t2 bg fg}
  \param{im}{an image node}
  \param{t1}{a lower bound in the range of im}
  \param{t2}{an upper bound in the range of im}
  \param{bg}{value of im data type for background}
  \param{fg}{value of im data type for foreground}
  \return{im}
  \desc{performs a threshold of the original image, i.e., all values in [t1,t2] are set to fg and the remaining to bg. @thresh returns the input image node im (destructive modification).}
  \cfunction{\cfthresh}
  \cfile{pointop.c}
  \example{}{}
*/

  xlim1 = xlgaimage();
  gt1 = getgenericnum(GetImDataType((IMAGE *)getimage(xlim1)));
  gt2 = getgenericnum(GetImDataType((IMAGE *)getimage(xlim1)));
  gbg = getgenericnum(GetImDataType((IMAGE *)getimage(xlim1)));
  gfg = getgenericnum(GetImDataType((IMAGE *)getimage(xlim1)));

  xllastarg();

  if (thresh((IMAGE *)getimage(xlim1), gt1, gt2, gbg, gfg)==ERROR)
    return NIL;

  return(xlim1);
}

LVAL isetlevel()
{
  LVAL xlim1;
  G_TYPE gt1, gt2, gval;

  if (!moreargs())
    xlabort("(@setlevel im t1 t2 val)");

/*
  \lspfunction{@}{setlevel}{im t1 t2 val}
  \param{im}{an image node}
  \param{t1}{a lower bound in the range of im}
  \param{t2}{an upper bound in the range of im}
  \param{val}{value of im data type}
  \return{im}
  \desc{sets all image pixels lying in [t1,t2] to val, all other pixels being not modified.  @setlevel returns the input image node im (destructive modification).}
  \cfunction{\cfsetlevel}
  \cfile{pointop.c}
  \example{}{}
*/

  xlim1 = xlgaimage();
  gt1  = getgenericnum(GetImDataType((IMAGE *)getimage(xlim1)));
  gt2  = getgenericnum(GetImDataType((IMAGE *)getimage(xlim1)));
  gval = getgenericnum(GetImDataType((IMAGE *)getimage(xlim1)));

  xllastarg();

  if (setlevel((IMAGE *)getimage(xlim1), gt1, gt2, gval) == ERROR)
    return NIL;

  return(xlim1);
}

LVAL imodulo()
{
  LVAL xlim1;
  int val;

  if (!moreargs())
    xlabort("(@modulo im val)");

/*
  \lspfunction{@}{modulo}{im val}
  \param{im}{an image node}
  \param{val}{a positive integer value}
  \return{im}
  \desc{sets all pixels of im to the remainder of their division by val.}
  \cfunction{\cfmodulo}
  \cfile{pointop.c}
  \example{}{}
*/

  xlim1 = xlgaimage();
  val   = (int) getfixnum(xlgafixnum());

  xllastarg();

  if (modulo((IMAGE *)getimage(xlim1), val) == ERROR)
    return NIL;

  return(xlim1);
}

LVAL iblank()
{
  LVAL xlim1;
  G_TYPE gval;

  if (!moreargs())
    xlabort("(@blank im val)");

/*
  \lspfunction{@}{blank}{im val}
  \param{im}{an image node}
  \param{val}{a value matching the data type of im}
  \return{im}
  \desc{sets all pixels of im to val.}
  \cfunction{\cfblank}
  \cfile{pointop.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  gval = getgenericnum(GetImDataType((IMAGE *)getimage(xlim1)));

  xllastarg();

  if (blank((IMAGE *)getimage(xlim1), gval)==ERROR)
    return NIL;

  return(xlim1);
}

LVAL icomplement()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(@complement im)");
/*
  \lspfunction{@}{complement}{im}
  \param{im}{an image node}
  \return{im}
  \desc{complements (negates) the image im.  @complement returns the input image node im (destructive modification).}
  \myseealso{\htmlref{@not}{@not}}
  \cfunction{\cfcomplement}
  \cfile{pointop.c}
*/
  xlim1 = xlgaimage();

  xllastarg();

  if (complement((IMAGE *)getimage(xlim1))==ERROR)
    return NIL;

  return(xlim1);
}

LVAL ishift()
{
  LVAL xlim1;
  int val;

  if (!moreargs())
    xlabort("(@shift im val)");

/*
  \lspfunction{@}{shift}{im val}
  \param{im}{an image node}
  \param{val}{an integer value}
  \return{im}
  \desc{multiplication (val < 0) or division (val > 0) by val multiples of 2 using bit shifting operations.}
  \cfunction{\cfshift}
  \cfile{pointop.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  val   = (int) getfixnum(xlgafixnum());

  xllastarg();

  if (shift((IMAGE *)getimage(xlim1), val)==ERROR)
    return NIL;

  return(xlim1);
}

LVAL isetrange()
{
  LVAL xlim1;
  G_TYPE gt1, gt2;

  if (!moreargs())
    xlabort("(@setrange im lval uval)");
/*
  \lspfunction{@}{setrange}{im lval uval}
  \param{im}{an image node}
  \param{lval}{lower bound}
  \param{uval}{upper bound}
  \return{im}
  \desc{stretches or shrinks the input image values within the range [uval,rval].  @setrange returns the input image node im (destructive modification).}
  \cfunction{\cfsetrange}
  \cfile{pointop.c}
*/
  xlim1 = xlgaimage();
  gt1  = getgenericnum(GetImDataType((IMAGE *)getimage(xlim1)));
  gt2  = getgenericnum(GetImDataType((IMAGE *)getimage(xlim1)));

  xllastarg();

  if (setrange((IMAGE *)getimage(xlim1), gt1, gt2)==ERROR)
    return NIL;

  return(xlim1);
}

LVAL indi()
{
  LVAL xlim1, xlim2;

  if (!moreargs())
    xlabort("(*ndi im1 im2)");
/*
  \lspfunction{*}{ndi}{im1 im2}
  \param{im1}{an image node}
  \param{im2}{an image node}
  \return{a float image holding the normalised difference index: (im1-im1)/(im1+im2)}
  \desc{Normalised difference index computation.  The output image is of type float.}
  \cfunction{\cfarith}
  \cfile{pointop.c}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();

  xllastarg();

  return(cvimage(ndi((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2))));
}

LVAL iFindPixWithVal()
{
  LVAL xlim1;
  G_TYPE gt;
  unsigned long int ofs;

  if (!moreargs())
    xlabort("(FindPixWithVal im val)");
/*
  \lspfunction{}{FindPixWithVal}{im val}
  \param{im}{an image node}
  \param{val}{a value of type matching im}
  \return{nonnegative integer corresponding to the offset of first pixel of im with value val, nil if no such pixel is found.}
  \desc{}
  \cfunction{\cfFindPixWithVal}
  \cfile{imem.c}
*/
  xlim1 = xlgaimage();
  gt  = getgenericnum(GetImDataType((IMAGE *)getimage(xlim1)));

  xllastarg();

  if (FindPixWithVal((IMAGE *)getimage(xlim1), gt, &ofs)==ERROR)
    return NIL;

  return(cvfixnum(ofs));
}

LVAL iswap()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(@swap im)");
/*
  \lspfunction{@}{swap}{im}
  \param{im}{an image node}
  \return{im}
  \desc{swap the bytes of each pixel (little to big or vice versa conversion)}
  \cfunction{\cfswap}
  \cfile{format.c}
*/

  xlim1 = xlgaimage();

  xllastarg();

  if (swap((IMAGE *)getimage(xlim1))==ERROR)
    return NIL;

  return(xlim1);
}

LVAL imblincomb()
{
  LVAL argi, arg, arg2, xlim1;
  IMAGE *imarray[maxNumberOfBands];
  int n=0;

  if (!moreargs())
    xlabort("(@mblincomb imlist matrix)\n");
/*
  \lspfunction{@}{mblincomb}{imlist matrix}
  \param{imlist}{a list of image nodes holding the channels of a multichannel image}
  \param{matrix}{an image holding the transformation matrix (nb of list elements x nb of list elements) coefficients (double) }
  \return{}
  \desc{}
  \cfunction{\cfmblincomb}
  \cfile{mblincomb.c}
*/
  arg=xlgetarg();
  argi=arg;
  if (listp(arg)){
    for (n = 0; consp(arg); arg = cdr(arg)){
      arg2 = car(arg);           
      if (imagep(arg2))                    
	imarray[n++]=(IMAGE*)getimage(arg2); 
      else{                      
	xlbadtype(arg2);          
	break;                    
      }                           
    }
  }
  else
    xlbadtype(arg);

  xlim1 = xlgaimage();

  if (mblincomb(imarray, n, (IMAGE *)getimage(xlim1)) == ERROR)
    return NIL;
  return argi;  
}

/*
  \lispsection
  \input{pointop.tex}  % Functions defined in lisp
*/

/* end of point operators */


#ifdef ALLFUNCTIONS
/* 
\module{Labelling and associated functions} % Mostly in label.c
\basesection
*/
LVAL ilabelpix()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(@labelpix im)\n");

/*
  \lspfunction{@}{labelpix}{im}
  \param{im}{an image node}
  \return{the image node im}
  \desc{labels each non-zero pixel of im with a unique label unless label overflows occurs.}
  \cfunction{\cflabelpix}
  \cfile{label.c}
*/

  xlim1 = xlgaimage();
  xllastarg();

  if (labelpix((IMAGE *)getimage(xlim1))==ERROR)
    return NIL;

  return(xlim1);
}
LVAL ilabel()
{
  LVAL xlim1, xlim2;
  int ox, oy, oz;

  if (!moreargs())
    xlabort("(@label im1 imngb  ox oy oz)\n");

/*
  \lspfunction{@}{label}{im imngb  ox oy oz}
  \param{im}{an image node to label}
  \param{imngb}{an image node for neighbourhood}
  \param{ox}{x coordinate of origin of imngb}
  \param{oy}{y coordinate of origin of imngb}
  \param{oz}{z coordinate of origin of imngb}
  \return{the image node im}
  \desc{labels the connected components (at value 1) of im using the neighbourhood (connectivity) defined by imngb, the origin of the latter being at coordinates (x,y,z).}
  \cfunction{\cflabel}
  \cfile{label.c}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  ox    = (int)getfixnum(xlgafixnum());
  oy    = (int)getfixnum(xlgafixnum());
  oz    = (int)getfixnum(xlgafixnum());

  xllastarg();

  if (label((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), ox, oy, oz)==ERROR)
    return NIL;

  return(xlim1);
}

LVAL ilabelpixngb()
{
  LVAL xlim1, xlim2;
  int ox, oy, oz;

  if (!moreargs())
    xlabort("(@labelpixngb im1 imngb  ox oy oz)\n");

/*
  \lspfunction{@}{labelpixngb}{im imngb  ox oy oz}
  \param{im}{an image node to label}
  \param{imngb}{an image node for neighbourhood}
  \param{ox}{x coordinate of origin of imngb}
  \param{oy}{y coordinate of origin of imngb}
  \param{oz}{z coordinate of origin of imngb}
  \return{the image node im}
  \desc{labels the pixels of the im1 using a propagation based on the  neighbourhood (connectivity) defined by imngb, the origin of the latter being at coordinates (x,y,z).}
  \cfunction{\cflabelpixngb}
  \cfile{label.c}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  ox    = (int)getfixnum(xlgafixnum());
  oy    = (int)getfixnum(xlgafixnum());
  oz    = (int)getfixnum(xlgafixnum());

  xllastarg();

  if (labelpixngb((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), ox, oy, oz)==ERROR)
    return NIL;

  return(xlim1);
}

LVAL ilabelplat()
{
  LVAL xlim1, xlim2;
  int ox, oy, oz;

  if (!moreargs())
    xlabort("(@labelplat im imngb  ox oy oz)\n");
/*
  \lspfunction{@}{labelplat}{im imngb  ox oy oz}
  \param{im}{an image node to label}
  \param{imngb}{an image node for neighbourhood}
  \param{ox}{x coordinate of origin of imngb}
  \param{oy}{y coordinate of origin of imngb}
  \param{oz}{z coordinate of origin of imngb}
  \return{the image node im}
  \desc{labels the flat regions of im using the neighbourhood (connectivity) defined by imngb, the origin of the latter being at coordinates (x,y,z).}
  \cfunction{\cflabelplat}
  \cfile{label.c}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  ox    = (int)getfixnum(xlgafixnum());
  oy    = (int)getfixnum(xlgafixnum());
  oz    = (int)getfixnum(xlgafixnum());

  xllastarg();

  if (labelplat((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), ox, oy, oz)==ERROR)
    return NIL;

  return(xlim1);
}

LVAL ialphacc()
{
  LVAL xlim1, xlim2;
  int alpha;

  if (!moreargs())
    xlabort("(*alphacc dissimh dissimv alpha)\n");
/*
  \lspfunction{*}{alphacc}{dissimh dissimv alpha}
  \param{dissimh}{an image for horizontal edge dissimilarities}
  \param{dissmv}{an image for vertical edge dissimilarities}
  \param{alpha}{integer for dissimilarity threshold value}
  \return{a new image holding the labelled alpha-connected component.}
  \desc{}
  \creationdate{20120218}
  \cfunction{\cfalphacc}
  \cfile{alphacc.c}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  alpha    = (int)getfixnum(xlgafixnum());

  xllastarg();
  return (cvimage(alphacc((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), alpha)));
}


LVAL ialphatree()
{
  LVAL result;
  LVAL xlim, xlim1, xlim2;
  int alphamax;
  int n=5, m;
  IMAGE **imarray;

  if (!moreargs())
    xlabort("(*alphatree dissimh dissimv alphamax)\n");
/*
  \lspfunction{*}{alphatree}{dissimh dissimv alphamax}
  \param{dissimh}{an image for horizontal edge dissimilarities}
  \param{dissmv}{an image for vertical edge dissimilarities}
  \param{alphamax}{integer for largest dissimilarity threshold value}
  \return{the tree stored in an image list: }
  \desc{}
  \creationdate{20120219}
  \cfunction{\cfalphatree}
  \cfile{alphatree.c}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  alphamax    = (int)getfixnum(xlgafixnum());

  xllastarg();
  // return (cvimage(alphatree((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), alphamax)));
 
  if ( (imarray=alphatree((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), alphamax)) == NULL)
    xlabort("(*alphatree dissimh dissimv alphamax): returned NULL");
    
  SAVE_LIST_IMAGES( imarray, xlim, result, n, m );
  free(imarray);
  
  return result;
}

LVAL ialphatreeincattr()
{
  LVAL arg, arg2;
  IMAGE *atree[5], *attr0cc[255];
  int n=0, type;


  if (!moreargs())
    xlabort("(*alphatreeincattr atree attr0cc type)\n");
/*
  \lspfunction{*}{alphatreeincattr}{atree attr0cc type}
  \param{atree}{a list of images coding the alpha tree}
  \param{attr0cc}{a list of look-up-tables holding the attribute(s) of the 0-CCs}
  \param{type}{integer value for attribute type: 0 for min, 1 for max, and 2 for sum}
  \return{a lut holding the attribute values of the tree nodes}
  \desc{}
  \creationdate{20120229}
  \cfunction{\cfalphatreeincattr}
  \cfile{alphatree.c}
*/

  arg=xlgetarg();
  if (listp(arg)){
    for (n = 0; consp(arg); arg = cdr(arg)){
      arg2 = car(arg);           
      if (imagep(arg2))                    
	atree[n++]=(IMAGE*)getimage(arg2); 
      else{                      
	xlbadtype(arg2);          
	break;                    
      }                           
    }
  }
  else
    xlbadtype(arg);

  arg=xlgetarg();
  if (listp(arg)){
    for (n = 0; consp(arg); arg = cdr(arg)){
      arg2 = car(arg);           
      if (imagep(arg2))                    
	attr0cc[n++]=(IMAGE*)getimage(arg2); 
      else{                      
	xlbadtype(arg2);          
	break;                    
      }                           
    }
  }
  else
    xlbadtype(arg);

  type = (int)getfixnum(xlgafixnum());

  xllastarg();
  return (cvimage(alphatreeincattr(atree,attr0cc,type)));
}


LVAL ialphatreetoCCs()
{
  LVAL arg, arg2, xlim1, xlim2;
  IMAGE *atree[5];
  int n=0, rule;

  if (!moreargs())
    xlabort("(*alphatreetoCCs atree imblbl flaglut rule)\n");
/*
  \lspfunction{*}{alphatreetoCCs}{atree imblbl flaglut rule}
  \param{atree}{a list of images coding the alpha tree}
  \param{imblbl}{an image node with base labels (labelled 0-CCs)}
  \param{flaglut}{an image node holding a lut indicating whether an atree node is flagged or not}
  \param{rule}{integer for type of rule. Rule 0: for keeping top nodes set to 1. Rule 1: for keeping top nodes with all subnodes set to 1.  Rule 2:  each flagged node is set with its label value.  Rule 3: keep only those nodes that have no descendant node(s) set to 1.}
  \return{an image node holding the labelled CCs corresponding to the top nodes of flaglut after considering the rule rule}
  \desc{in development!  Note that the flaglut is modified within the routine!!!}
  \creationdate{20120301}
  \cfunction{\cfalphatree}
  \cfile{alphatree.c}
*/

  arg=xlgetarg();
  if (listp(arg)){
    for (n = 0; consp(arg); arg = cdr(arg)){
      arg2 = car(arg);           
      if (imagep(arg2))                    
	atree[n++]=(IMAGE*)getimage(arg2); 
      else{                      
	xlbadtype(arg2);          
	break;                    
      }                           
    }
  }
  else
    xlbadtype(arg);

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  rule = (int)getfixnum(xlgafixnum());
  xllastarg();
  return (cvimage(alphatreetoCCs(atree,(IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), rule)));
}

LVAL ialphatreenextlevel()
{
  LVAL arg, arg2, xlim1;
  IMAGE *atree[5];
  int n=0, alpha;


  if (!moreargs())
    xlabort("(*alphatreenextlevel atree crtprtlbl alpha)\n");
/*
  \lspfunction{*}{alphatreenextlevel}{atree crtprtlbl alpha}
  \param{atree}{a list of images coding the alpha tree}
  \param{crtprtlbl}{an image node with current parent labels of base nodes}
  \param{alpha}{integer for alpha level (implicitely must be the next level with respect to crtprtlbl)}
  \return{an image node holding a labelling of the base nodes to produce a CC labelling of the CCs appearing at level alpha}
  \desc{note that the image crtprtlbl is updated by this routine so as to be called with the subsequent value of alpha}
  \creationdate{20120315}
  \cfunction{\cfalphatree}
  \cfile{alphatree.c}
*/

  arg=xlgetarg();
  if (listp(arg)){
    for (n = 0; consp(arg); arg = cdr(arg)){
      arg2 = car(arg);           
      if (imagep(arg2))                    
	atree[n++]=(IMAGE*)getimage(arg2); 
      else{                      
	xlbadtype(arg2);          
	break;                    
      }                           
    }
  }
  else
    xlbadtype(arg);


  xlim1 = xlgaimage();
  alpha = (int)getfixnum(xlgafixnum());
  
  xllastarg();
  return (cvimage(alphatreenextlevel(atree,(IMAGE *)getimage(xlim1), alpha)));
}

LVAL ialphatreepersistencelut()
{
  LVAL arg, arg2;
  IMAGE *atree[5];
  int n=0;

  if (!moreargs())
    xlabort("(*alphatreepersistencelut atree)\n");
/*
  \lspfunction{*}{alphatreepersistencelut}{atree}
  \param{atree}{a list of images coding the alpha tree}
  \return{an image node holding the persistence value of each node of the alpha tree}
  \desc{}
  \creationdate{20120324}
  \cfunction{\cfalphatree}
  \cfile{alphatree.c}
*/

  arg=xlgetarg();
  if (listp(arg)){
    for (n = 0; consp(arg); arg = cdr(arg)){
      arg2 = car(arg);           
      if (imagep(arg2))                    
	atree[n++]=(IMAGE*)getimage(arg2); 
      else{                      
	xlbadtype(arg2);          
	break;                    
      }                           
    }
  }
  else
    xlbadtype(arg);
  
  xllastarg();
  return (cvimage(alphatreepersistencelut(atree)));
}


LVAL ilabelcc()
{
  LVAL xlim1, xlim2;
  int ox, oy, oz, rg , rl;

  if (!moreargs())
    xlabort("(*labelcc im imngb ox oy oz rg rl)\n");
/*
  \lspfunction{*}{labelcc}{im imngb  ox oy oz rg rl}
  \param{im}{an image node to label}
  \param{imngb}{an image node for neighbourhood}
  \param{ox}{x coordinate of origin of imngb}
  \param{oy}{y coordinate of origin of imngb}
  \param{oz}{z coordinate of origin of imngb}
  \param{rg}{integer for range parameter $\lambda_g$}
  \param{rl}{integer for range parameter $\lambda_l$}
  \return{a new image holding the labelled connected component}
  \desc{in development!}
  \cfunction{\cflabelcc}
  \cfile{label.c}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  ox    = (int)getfixnum(xlgafixnum());
  oy    = (int)getfixnum(xlgafixnum());
  oz    = (int)getfixnum(xlgafixnum());
  rg    = (int)getfixnum(xlgafixnum());
  rl    = (int)getfixnum(xlgafixnum());

  xllastarg();
  return (cvimage(labelcc((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), ox, oy, oz, rg, rl)));
}

LVAL ilabelccattr()
{
  LVAL xlim1;
  int graph, rg , rl;

  if (!moreargs())
    xlabort("(*labelccattr im graph rg rl)\n");
/*
  \lspfunction{*}{labelccattr}{im graph rg rl}
  \param{im}{an image node to label}
  \param{graph}{integer for connectivity}
  \param{rg}{integer for range parameter $\lambda_g$}
  \param{rl}{integer for range parameter $\lambda_l$}
  \return{a new image holding the labelled connected component}
  \desc{a breadth first computation of the (alpha,omega)-CC of an image suitable for non-decreasing attributes (contrary to the (faster) depth-first computation that is suitable only for decreasing attributes.}
  \cfunction{\cflabelccattr}
  \cfile{newlabel.c}
*/
  xlim1 = xlgaimage();
  graph = (int)getfixnum(xlgafixnum());
  rg    = (int)getfixnum(xlgafixnum());
  rl    = (int)getfixnum(xlgafixnum());

  xllastarg();
  return (cvimage(labelccattr((IMAGE *)getimage(xlim1), graph, rg, rl)));
}

LVAL ilabelccmi()
{
  LVAL xlim1, xlim2, xlim3;
  int ox, oy, oz, rg , rl;

  if (!moreargs())
    xlabort("(*labelccmi im immi imngb ox oy oz rg rl)\n");
/*
  \lspfunction{*}{labelccmi}{im immi imngb  ox oy oz rg rl}
  \param{im}{an image node to label}
  \param{immi}{an image node of type UCHAR}
  \param{imngb}{an image node for neighbourhood}
  \param{ox}{x coordinate of origin of imngb}
  \param{oy}{y coordinate of origin of imngb}
  \param{oz}{z coordinate of origin of imngb}
  \param{rg}{integer for range parameter $\lambda_g$}
  \param{rl}{integer for range parameter $\lambda_l$}
  \return{a new image holding the labelled connected component}
  \desc{in development!}
  \cfunction{\cflabelccmi}
  \cfile{label.c}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();
  ox    = (int)getfixnum(xlgafixnum());
  oy    = (int)getfixnum(xlgafixnum());
  oz    = (int)getfixnum(xlgafixnum());
  rg    = (int)getfixnum(xlgafixnum());
  rl    = (int)getfixnum(xlgafixnum());

  xllastarg();
  return (cvimage(labelccmi((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), (IMAGE *)getimage(xlim3), ox, oy, oz, rg, rl)));
}

LVAL ilabelccdissim()
{
  LVAL xlim1, xlim2, xlim3;
  int rg, rl;

  if (!moreargs())
    xlabort("(*labelccdissim im imh imv rg rl)\n");
/*
  \lspfunction{*}{labelccdissim}{im imh imv rg rl}
  \param{im}{an image node to label}
  \param{imh}{an image for horizontal edge dissimilarities}
  \param{imv}{an image for vertical edge dissimilarities}
  \param{rg}{integer for range parameter $\lambda_g$}
  \param{rl}{integer for range parameter $\lambda_l$}
  \return{a new image holding the labelled connected component.  Initially implemented for experiments detailed in \cite{soille2011ismm,gueguen-soille2011ismm}.}
  \desc{}
  \creationdate{20101018}
  \cfunction{\cflabelccdissim}
  \cfile{label.c}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();
  rg    = (int)getfixnum(xlgafixnum());
  rl    = (int)getfixnum(xlgafixnum());

  xllastarg();
  return (cvimage(labelccdissim((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), (IMAGE *)getimage(xlim3), rg, rl)));
}

LVAL ilabelccmsdissimlist()
{
  LVAL arg, arg2, xlim1, xlim2;
  IMAGE *imarray[maxNumberOfBands];
  int n=0, rg, rl;

  if (!moreargs())
    xlabort("(*labelccmsdissimlist imlist dissh dissv rg rl)\n");
/*
  \lspfunction{*}{labelccmsdissimlist}{imlist dissh dissv rg rl}
  \param{imlist}{a list of image nodes holding the channels of a multichannel image}
  \param{imh}{an image for horizontal edge dissimilarities}
  \param{imv}{an image for vertical edge dissimilarities}
  \param{rg}{integer for global range parameter $\lambda_g$}
  \param{rl}{integer for local range parameter $\lambda_l$}
  \return{a new image holding the labelled alpha-omega connected component of the input multichannel image}
  \desc{see \citep{soille2008pami} for a detailed description.  Initially implemented for experiments detailed in \cite{soille2011ismm,gueguen-soille2011ismm}.}
  \cfunction{\cflabelccmsdissim}
  \cfile{label.c}
*/
  arg=xlgetarg();
  if (listp(arg)){
    for (n = 0; consp(arg); arg = cdr(arg)){
      arg2 = car(arg);           
      if (imagep(arg2))                    
	imarray[n++]=(IMAGE*)getimage(arg2); 
      else{                      
	xlbadtype(arg2);          
	break;                    
      }                           
    }
  }
  else
    xlbadtype(arg);

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  rg = (int)getfixnum(xlgafixnum());
  rl = (int)getfixnum(xlgafixnum());

  xllastarg();
  return (cvimage(labelccmsdissim((IMAGE **)imarray, n, (IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), rg, rl)));
}

LVAL ilabelccvar()
{
  LVAL xlim1, xlim2;
  int ox, oy, oz, rg , rl;
  double varmax;

  if (!moreargs())
    xlabort("(*labelccvar im imngb ox oy oz rg rl varmax)\n");
/*
  \lspfunction{*}{labelccvar}{im imngb  ox oy oz rg rl varmax}
  \param{im}{an image node to label}
  \param{imngb}{an image node for neighbourhood}
  \param{ox}{x coordinate of origin of imngb}
  \param{oy}{y coordinate of origin of imngb}
  \param{oz}{z coordinate of origin of imngb}
  \param{rg}{integer for range parameter $\lambda_g$}
  \param{rl}{integer for range parameter $\lambda_l$}
  \param{varmax}{float for maximum variance of cc}
  \return{a new image holding the labelled connected component}
  \desc{in development!}
  \cfunction{\cflabelccvar}
  \cfile{label.c}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  ox    = (int)getfixnum(xlgafixnum());
  oy    = (int)getfixnum(xlgafixnum());
  oz    = (int)getfixnum(xlgafixnum());
  rg    = (int)getfixnum(xlgafixnum());
  rl    = (int)getfixnum(xlgafixnum());
  varmax    = (double)getflonum(xlgaflonum());

  xllastarg();
  return (cvimage(labelccvar((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), ox, oy, oz, rg, rl, varmax)));
}

LVAL ilabelccmslist()
{
  LVAL arg, arg2, xlim1;
  IMAGE *imarray[maxNumberOfBands];
  int n=0, ox, oy, oz, rg, rl;

  if (!moreargs())
    xlabort("(*labelccmslist imlist imngb ox oy oz rg rl)\n");
/*
  \lspfunction{*}{labelccmslist}{imlist imngb  ox oy oz rg rl}
  \param{imlist}{a list of image nodes holding the channels of a multichannel image}
  \param{imngb}{an image node for neighbourhood}
  \param{ox}{x coordinate of origin of imngb}
  \param{oy}{y coordinate of origin of imngb}
  \param{oz}{z coordinate of origin of imngb}
  \param{rg}{integer for global range parameter $\lambda_g$}
  \param{rl}{integer for local range parameter $\lambda_l$}
  \return{a new image holding the labelled alpha-omega connected component of the input multichannel image}
  \desc{see \citep{soille2008pami} for a detailed description.}
  \cfunction{\cflabelccms}
  \cfile{label.c}
*/
  arg=xlgetarg();
  if (listp(arg)){
    for (n = 0; consp(arg); arg = cdr(arg)){
      arg2 = car(arg);           
      if (imagep(arg2))                    
	imarray[n++]=(IMAGE*)getimage(arg2); 
      else{                      
	xlbadtype(arg2);          
	break;                    
      }                           
    }
  }
  else
    xlbadtype(arg);

  xlim1 = xlgaimage();
  ox = (int)getfixnum(xlgafixnum());
  oy = (int)getfixnum(xlgafixnum());
  oz = (int)getfixnum(xlgafixnum());
  rg = (int)getfixnum(xlgafixnum());
  rl = (int)getfixnum(xlgafixnum());

  xllastarg();
  return (cvimage(labelccms((IMAGE **)imarray, n, (IMAGE *)getimage(xlim1), ox, oy, oz, rg, rl)));
}

LVAL ilabelcimslist()
{
  LVAL arg, arg2, xlim1;
  IMAGE *imarray[maxNumberOfBands];
  int n=0, ox, oy, oz, r;

  if (!moreargs())
    xlabort("(*labelcimslist imlist imngb ox oy oz r)\n");
/*
  \lspfunction{*}{labelcimslist}{imlist imngb  ox oy oz r}
  \param{imlist}{a list of image nodes holding the channels of a multichannel image}
  \param{imngb}{an image node for neighbourhood}
  \param{ox}{x coordinate of origin of imngb}
  \param{oy}{y coordinate of origin of imngb}
  \param{oz}{z coordinate of origin of imngb}
  \param{r}{integer for local range parameter $\lambda_l$}
  \return{a new image holding the labelled strongly connected component of the input multichannel image}
  \desc{see \citep{soille2008pami} for a detailed description.}
  \cfunction{\cflabelcims}
  \cfile{label.c}
*/
  arg=xlgetarg();
  if (listp(arg)){
    for (n = 0; consp(arg); arg = cdr(arg)){
      arg2 = car(arg);           
      if (imagep(arg2))                    
	imarray[n++]=(IMAGE*)getimage(arg2); 
      else{                      
	xlbadtype(arg2);          
	break;                    
      }                           
    }
  }
  else
    xlbadtype(arg);

  xlim1 = xlgaimage();
  ox = (int)getfixnum(xlgafixnum());
  oy = (int)getfixnum(xlgafixnum());
  oz = (int)getfixnum(xlgafixnum());
  r  = (int)getfixnum(xlgafixnum());

  xllastarg();
  return (cvimage(labelcims((IMAGE **)imarray, n, (IMAGE *)getimage(xlim1), ox, oy, oz, r)));
}

LVAL ilabelccms()
{
  LVAL arg;
  IMAGE *imarray[maxNumberOfBands];
  int nc=0, ox=0, oy, oz, rg, rl;

  if (!moreargs())
    xlabort("(*labelccms im1 [im2] [...] [imi] imngb ox oy oz rg rl)\n");
/*
  \lspfunction{*}{labelccms}{im1 [im2] [...] imngb  ox oy oz rg rl}
  \param{im1}{an image node to label (first channel)}
  \param{imi}{an image node for ith channel (maximum of 254 channels)}
  \param{imngb}{an image node for neighbourhood}
  \param{ox}{x coordinate of origin of imngb}
  \param{oy}{y coordinate of origin of imngb}
  \param{oz}{z coordinate of origin of imngb}
  \param{rg}{integer for global range parameter $\lambda_g$}
  \param{rl}{integer for local range parameter $\lambda_l$}
  \return{a new image holding the labelled connected component}
  \desc{in development!}
  \cfunction{\cflabelccms}
  \cfile{label.c}
*/
  while ( (arg=xlgetarg()) != NIL ){
    if (imagep(arg)){
      imarray[nc++]=(IMAGE *)getimage(arg);
    }
    else
      break;
  }
  nc-=1; /* last image is for neighbourhood */
  if (integerp(arg))
    ox = (int)getfixnum(arg);
  else
    xlbadtype(arg);
  oy    = (int)getfixnum(xlgafixnum());
  oz    = (int)getfixnum(xlgafixnum());
  rg    = (int)getfixnum(xlgafixnum());
  rl    = (int)getfixnum(xlgafixnum());

  xllastarg();
  return (cvimage(labelccms((IMAGE **)imarray, nc, imarray[nc], ox, oy, oz, rg, rl)));
}

LVAL ilabelci()
{
  LVAL xlim1, xlim2;
  int ox, oy, oz, rl;

  if (!moreargs())
    xlabort("(*labelci im imngb ox oy oz rl)\n");
/*
  \lspfunction{*}{labelci}{im imngb  ox oy oz rl}
  \param{im}{an image node to label}
  \param{imngb}{an image node for neighbourhood}
  \param{ox}{x coordinate of origin of imngb}
  \param{oy}{y coordinate of origin of imngb}
  \param{oz}{z coordinate of origin of imngb}
  \param{rl}{integer for range parameter $\lambda_l$ under the strongly connected assumption}
  \return{a new image holding the labelled connected component}
  \desc{in development!}
  \cfunction{\cflabelci}
  \cfile{labelci.c}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  ox    = (int)getfixnum(xlgafixnum());
  oy    = (int)getfixnum(xlgafixnum());
  oz    = (int)getfixnum(xlgafixnum());
  rl    = (int)getfixnum(xlgafixnum());

  xllastarg();
  return (cvimage(labelci((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), ox, oy, oz, rl)));
}

LVAL ilabelcims()
{
  LVAL arg;
  IMAGE *imarray[maxNumberOfBands];
  int nc=0, ox=0, oy, oz, rl;

  if (!moreargs())
    xlabort("(*labelcims im1 [im2] [...] [imi] imngb ox oy oz rl)\n");
/*
  \lspfunction{*}{labelcims}{im1 [im2] [...] imngb  ox oy oz rl}
  \param{im1}{an image node to label (first channel)}
  \param{imi}{an image node for ith channel (maximum of 254 channels)}
  \param{imngb}{an image node for neighbourhood}
  \param{ox}{x coordinate of origin of imngb}
  \param{oy}{y coordinate of origin of imngb}
  \param{oz}{z coordinate of origin of imngb}
  \param{rl}{integer for local range parameter $\lambda_l$}
  \return{a new image holding the labelled connected component}
  \desc{in development!}
  \cfunction{\cflabelcims}
  \cfile{label.c}
*/
  while ( (arg=xlgetarg()) != NIL ){
    if (imagep(arg)){
      imarray[nc++]=(IMAGE *)getimage(arg);
    }
    else
      break;
  }
  nc-=1; /* last image is for neighbourhood */
  if (integerp(arg))
    ox = (int)getfixnum(arg);
  else
    xlbadtype(arg);
  oy    = (int)getfixnum(xlgafixnum());
  oz    = (int)getfixnum(xlgafixnum());
  rl    = (int)getfixnum(xlgafixnum());

  xllastarg();
  return (cvimage(labelcims((IMAGE **)imarray, nc, imarray[nc], ox, oy, oz, rl)));
}

#ifdef MCISRG
LVAL imslabel()
{
  LVAL arg;
  IMAGE *imarray[maxNumberOfBands], *imout=NULL;
  int n=0;
  int graph=4;
  long int delta;

  if (!moreargs())
    xlabort("(*mslabel im1 [im2] ... [im255] graph lambda)");

/*
  \lspfunction{*}{mslabel}{im1 [im2] ... [im255] graph lambda}
  \param{im1}{an image node for 1st channel}
  \param{imi}{an image node for ith channel (maximum of 255 channels)}
  \param{graph}{integer for connectivity (either 4 or 8)}
  \param{lambda}{integer for maximum deviation between 2 neighbour pixels in each channel (lambda value of lambda-flat zones)}
  \return{an image node}
  \desc{multispectral labelling of the quasi-flat zones using graph connectivity.  If delta equals to 0, actual flat zones are computed.}
  \cfunction{\cflabelImage}
  \cfile{mslabel.c}
  \example{}{}
*/
  while ( (arg=xlgetarg()) != NIL ){
    if (imagep(arg)){
      imarray[n++]=(IMAGE *)getimage(arg);
    }
    else
      break;
  }
  if (integerp(arg))
    graph = (int)getfixnum(arg);
  else
    xlbadtype(arg);
  delta    = (int)getfixnum(xlgafixnum());

  xllastarg();
 
  imout = (IMAGE *)labelImage(imarray, n, NULL, graph, delta);
  if (imout!=NULL)
    return (cvimage(imout));
  else
    return NIL;
}
#endif

LVAL iresolveLabels()
{
  LVAL xlim1, xlim2, xlim3;
  int graph;

  if (!moreargs())
    xlabort("(@resolvelabels imlbl imlut imlutback graph)\n");
/*
  \lspfunction{@}{resolvelabels}{imlbl imlut graph}
  \param{imlbl}{an image node with labels}
  \param{imlut}{an image holding a LUT (histogram)}
  \param{imlutback}{an image holding a LUT (histogram)}
  \param{graph}{integer for connectivity}
  \return{the image node im}
  \desc{composite labels of imlbl are set to plain labels using a majority rule: the most frequent plain label encountered along the boundary of the region with composite label is retained (provided that this plain label is included in the composite label).  In case, no such label was found (i.e., all neighbour regions with composite labels themselves or plain labels not part of the composite label) the first plain label of the composite label is retained.}
  \cfunction{\cfresolveLabels}
  \cfile{label.c}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();
  graph = (int)getfixnum(xlgafixnum());

  xllastarg();

  if (resolveLabels((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), (IMAGE *)getimage(xlim3), graph)==ERROR)
    return NIL;

  return(xlim1);
}

LVAL igorder()
{
  LVAL xlim1, xlim2;
  int n;
  if (!moreargs())
    xlabort("(@gorder imlbl g n)\n");
/*
  \lspfunction{@}{gorder}{imlbl g &opt n}
  \param{imlbl}{an image node with labels}
  \param{g}{an image node with degree of overlap}
  \param{n}{integer for number of input images}
  \return{the image node imlbl}
  \desc{rearrange the labels in such a way that labels occur in increasing order of g values}
  \cfunction{\cfgorder}
  \cfile{label.c}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  if (moreargs())
    n        = (int)getfixnum(xlgafixnum());
  else
    n=0;
  xllastarg();

  if (gorder((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), n)==ERROR)
    return NIL;
  return(xlim1);
}

LVAL iseededplat()
{
  LVAL xlim1, xlim2, xlim3;
  int ox, oy, oz;

  if (!moreargs())
    xlabort("(@seededplat im imngb imseeds ox oy oz)\n");

/*
  \lspfunction{@}{seededplat}{im imngb imseeds ox oy oz}
  \param{im}{an image node}
  \param{imngb}{an image node for neighbourhood}
  \param{imseeds}{an image node for seeds (must be of type UCHAR)}
  \param{ox}{x coordinate of origin of imngb}
  \param{oy}{y coordinate of origin of imngb}
  \param{oz}{z coordinate of origin of imngb}
  \return{the image node im}
  \desc{sets all flats regions of im which contain a seed to 1.  The connectivity rule is defined by the image imngb and its associated origin (all pixels in imngb with a non-zero value are neighbour pixels of the origin).}
  \cfunction{\cfseededplat}
  \cfile{label.c}
  \example{}{}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();
  ox    = (int)getfixnum(xlgafixnum());
  oy    = (int)getfixnum(xlgafixnum());
  oz    = (int)getfixnum(xlgafixnum());

  xllastarg();

  if (seededplat((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2),(IMAGE *)getimage(xlim3), ox, oy, oz)==ERROR)
    return NIL;

  return(xlim1);
}

LVAL iseededlabelplat()
{
  LVAL xlim1, xlim2, xlim3;
  int ox, oy, oz;

  if (!moreargs())
    xlabort("(@seededlabelplat im imngb imseeds ox oy oz)\n");

/*
  \lspfunction{@}{seededlabelplat}{im imngb imseeds ox oy oz}
  \param{im}{an image node to label}
  \param{imngb}{an image node for neighbourhood}
  \param{imseeds}{an image node for seeds (must be of type UCHAR)}
  \param{ox}{x coordinate of origin of imngb}
  \param{oy}{y coordinate of origin of imngb}
  \param{oz}{z coordinate of origin of imngb}
  \return{the image node im}
  \desc{labels the flat regions of im using the neighbourhood (connectivity) defined by imngb, the origin of the latter being at coordinates (x,y,z).  Only those flat regions are marked by imseeds are labelled.  A flat region is marked if it contains at least one pixel in imseeds having a non-zero value.}
  \cfunction{\cfseededlabelplat}
  \cfile{label.c}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();
  ox    = (int)getfixnum(xlgafixnum());
  oy    = (int)getfixnum(xlgafixnum());
  oz    = (int)getfixnum(xlgafixnum());

  xllastarg();

  if (seededlabelplat((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2),(IMAGE *)getimage(xlim3), ox, oy, oz)==ERROR)
    return NIL;

  return(xlim1);
}

#ifdef ALLFUNCTIONS
/* setregions.c */
LVAL iset_regions()
{
  LVAL xlim1, xlim2;
  int index;

  if (!moreargs())
    xlabort("(@setregions ilbl ival index)");
/*
  \lspfunction{@}{setregions}{ilbl ival index}
  \param{ilbl}{image node for labelled image}
  \param{ival}{image node}
  \param{index}{integer for type of operation}
  \return{}
  \desc{for each labelled region of ibl, look its values in ival, perform some operation defined by type, and finally set all pixels of the labelled region to the output of this operation.  Authorized operations are:  1 for mean value, 2 for standard deviation, 3 for maximum value, 4 for minimum value,  8 for sum of values, 9 for orientation of labelled region, 12 for most frequent value occurring along the external boundary of the labelled region, 13 for length of minor axis of ellipsis of identical inertia, 14 for length of major axis of ellipsis of identical inertia, 15 for irradiance of ellipsis of identical inertia, 20 for range of the values with the labelled region.}
  \cfunction{\cfsetUDregions}
  \cfile{setreg.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  index = (int) getfixnum(xlgafixnum());

  xllastarg();

  if (set_regions((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), index)==ERROR)
    xlabort("set_regions()");

  return(xlim1);
}

LVAL isetregionsgraph()
{
  LVAL xlim1, xlim2;
  int index, graph;

  if (!moreargs())
    xlabort("(@setregionsgraph ilbl ival index graph)");
/*
  \lspfunction{@}{setregionsgraph}{ilbl ival index}
  \param{ilbl}{image node for labelled image}
  \param{ival}{image node}
  \param{index}{integer for type of operation: 0 for min of adjancent neighbours, 1 for max, and 2 for max-min}
  \param{graph}{integer for connectivity}
  \return{}
  \desc{}
  \cfunction{\cfsetregionsgraph}
  \cfile{setreg.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  index = (int) getfixnum(xlgafixnum());
  graph = (int) getfixnum(xlgafixnum());

  xllastarg();

  if (setregionsgraph((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), index, graph)==ERROR)
    xlabort("setregionsgraph()");

  return(xlim1);
}

LVAL iregion_lut()
{
  LVAL xlim1;
  int graph, type, param1=0, param2=0;
  IMAGE *imout;

  if (!moreargs())
    xlabort("(*regionlut ilbl graph type &opt param1 param2)");
/*
  \lspfunction{*}{regionlut}{ilbl graph type}
  \param{ilbl}{image node for labelled image}
  \param{graph}{integer for connectivity used to create labelled image}
  \param{type}{type of operation to perform: 0 for length of diagonal of bounding box, 1 for area, 2 for inertia, 3 for perimeter as number of boundary pixels, 4 for perimeter as number of edgels, 5 for majority filtering with minimum area given by param1, 6 for moment of order param1 and param2, 7 for sumx, sumy, sumx2, sumy2, and sumxy, 8 for semiminor axis, semimajor axis, and tilt angle in degrees, 10 for bounding box computations, 99 for offset of first pixel of each labelled region.}
  \param{param1}{integer for additional parameter}
  \param{param2}{integer for additional parameter}
  \return{a look-up-table holding the desired measurements for each label value of ilbl}
  \desc{For type 5, minimum area is mandatory and should match param1.  For type 6, param1 and param2 are mandatory and express the order of the moment.  An elongation measure is the first moment invariant of Hu~\cite{hu62} as pointed out in \citep{urbach-roerdink-wilkinson2007}: $\eta_{20}+\eta_{02}$.}

}
  \cfunction{\cfregionUDlut}
  \cfile{setreglut.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  graph = (int) getfixnum(xlgafixnum());
  type = (int) getfixnum(xlgafixnum());
  if (type==5 || type==6)
    param1 = (int) getfixnum(xlgafixnum());
  if (type==6)
    param2 = (int) getfixnum(xlgafixnum());

  xllastarg();

  imout = (IMAGE *)region_lut((IMAGE *)getimage(xlim1), graph, type, param1, param2);
  if (imout!=NULL)
    return (cvimage(imout));
  else
    return NIL;
}

LVAL iregion_lut_seq()
{
  LVAL xlim1;
  int graph, type;
  IMAGE *imout;

  if (!moreargs())
    xlabort("(*regionlutseq ilbl graph type)");
/*
  \lspfunction{*}{regionlutseq}{ilbl graph type}
  \param{ilbl}{image node for labelled image}
  \param{graph}{integer for connectivity used to create labelled image}
  \param{type}{type of operation to perform.  0 for NW neighbour statistics}
  \return{a look-up-table}
  \desc{}
  \cfunction{\cfregionUDlut}
  \cfile{setreglut.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  graph = (int) getfixnum(xlgafixnum());
  type = (int) getfixnum(xlgafixnum());

  xllastarg();

  imout = (IMAGE *)region_lut_seq((IMAGE *)getimage(xlim1), graph, type);
  if (imout!=NULL)
    return (cvimage(imout));
  else
    return NIL;
}

LVAL iregion_im_lut()
{
  LVAL xlim1, xlim2;
  int graph, type;
  IMAGE *imout;
  float aval=0.0;

  if (!moreargs())
    xlabort("(*regionimlut ilbl im graph type  &opt aval)");
/*
  \lspfunction{*}{regionimlut}{ilbl im graph type &opt aval}
  \param{ilbl}{image node for labelled image}
  \param{im}{image node for intensity values}
  \param{graph}{integer for connectivity used to create labelled image}
  \param{type}{type of operation to perform (0 for parameters of best plane fitting)}
  \param{aval}{optional floating point number}
  \return{a look-up-table with desired parameters, one per plane}
  \desc{Type 0: least square 3D plane fitting http://www.geometrictools.com/Documentation/LeastSquaresFitting.pdf where the 3 parameters are the slope in x, the slope in y and the offest at the origin of the best fitting plane.  If there are less than 3 points for the region or if the matrix is singular, all three parameters are set to 0. Type 1: mean value. Type 2: standard deviation. Type 3: maximum value.  Type 4: minimum value.  Type 5: percentile with percentile given by aval (e.g., 0.5 for median).  Type 6: sum of values.  Type 20: range of values.}
  \cfunction{\cfregionUDlut}
  \cfile{setreglut.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  graph = (int) getfixnum(xlgafixnum());
  type = (int) getfixnum(xlgafixnum());

  if (moreargs())
    aval = (float) getflonum(xlgaflonum());

  xllastarg();

  imout = (IMAGE *)region_im_lut((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2),graph, type, aval);
  if (imout!=NULL)
    return (cvimage(imout));
  else
    return NIL;
}

LVAL icontortion_lut()
{
  LVAL xlim1;
  int graph;
  IMAGE *imout;

  if (!moreargs())
    xlabort("(*contortionlut ilbl graph)");
/*
  \lspfunction{*}{contortionlut}{ilbl graph}
  \param{ilbl}{image node for labelled image}
  \param{graph}{integer for connectivity used to create labelled image}
  \return{a look-up-table with the contortion measure}
  \desc{returns a LUT indicating for each label its contortion number, that is, the number of times a given direction change its sign.  This can be used for shape description and in particular to distinguish random shapes from regular shapes.  Idea came on the evening of the 18th of May 2011.  First programmed on the morning of the 19th of May 2011.}
  \cfunction{\cfcontortionUDlut}
  \cfile{setreglut.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  graph = (int) getfixnum(xlgafixnum());

  xllastarg();

  imout = (IMAGE *)contortion_lut((IMAGE *)getimage(xlim1), graph);
  if (imout!=NULL)
    return (cvimage(imout));
  else
    return NIL;
}

LVAL imoments2ellipse()
{
  LVAL arg, arg2;
  IMAGE *imarray[maxNumberOfBands];
  int n=0;

  if (!moreargs())
    xlabort("(*moments2ellipse lut_list)\n");
  /*
  \lspfunction{*}{moments2ellipse}{lut_list}
  \param{lut_list}{a list with five luts in the form of 1D images with the moments m00, m10, m01, m11, m20, m02}
  \return{an image with 3 lines, containing the length of the semi-major, semi-minor, and tilt angle (in degrees) of the ellipse with the given moments.}
  \cfunction{\cflabelccmsdissim}
  \cfile{setreglut.c}
  */
  arg=xlgetarg();
  if (listp(arg)){
    for (n = 0; consp(arg); arg = cdr(arg)){
      arg2 = car(arg);           
      if (imagep(arg2))                    
	imarray[n++]=(IMAGE*)getimage(arg2); 
      else{                      
	xlbadtype(arg2);          
	break;                    
      }                           
    }
  }
  else
    xlbadtype(arg);

  if(n!=6){
    xlabort("(*moments2ellipse lut_list): the list must contains five luts with moments");
  }
  xllastarg();
  return (cvimage(moments_lut_to_ellipse_lut((IMAGE **)imarray)));
}

LVAL isurface()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(@surface ilbl)");

/*
  \lspfunction{@}{surface}{ibl}
  \param{ilbl}{an image node}
  \return{ilbl}
  \desc{sets all pixels of ilbl having a given value to the number of pixels having this value.}
  \cfunction{\cftesselUDsurface}
  \cfile{setreg.c}
  \example{}{}
*/


  xlim1 = xlgaimage();

  xllastarg();

  if (tessel_surface((IMAGE *)getimage(xlim1))==ERROR)
    xlabort("tessel_surface");

  return(xlim1);
}
#endif

LVAL irelabel()
{
  LVAL xlim1, xlim2, xlim3;
  if (!moreargs())
    xlabort("(@relabel ilbl1 ilbl2 ival2)");
/*
  \lspfunction{@}{relabel}{ilbl1 ilbl2 ival2}
  \param{ilbl1}{image node for first labelled image}
  \param{ilbl2}{image node for second labelled image}
  \param{ival2}{image node for valued regions of ilbl2}
  \return{}
  \desc{sets the labels of the image ilbl1 to the label value in image ilbl2 that satisfies the following condition: it must match the pixel(s) with the highest value in the image ival2.}
  \cfunction{\cfrelabel}
  \cfile{setreg.c}
  \example{}{}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();

  xllastarg();

  if (relabel((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), (IMAGE *)getimage(xlim3))==ERROR)
    xlabort("(@lbl2lbl)");

  return(xlim1);
}

LVAL ipropagate()
{
  LVAL xlim1, xlim2, arg, arg2;
  IMAGE *imarray[maxNumberOfBands];
  int n=0, graph=0;
  if (!moreargs())
    xlabort("(@propagate lbl dst im1 ... imn graph)");
/*
  \lspfunction{@}{propagate}{lbl dst im1 ... imn graph}
  \param{lbl}{image node for a labelled image}
  \param{dst}{image node for distance from labels into background (UCHAR type)}
  \param{im1 ... imn}{image nodes for the bands of a multiband image}
  \param{graph}{integer for connectivity}
  \return{the label image label where the point of the dst image that were not set to zero are set to the value of the label that reached this point.}
  \desc{}
  \cfunction{\cfpropagate}
  \cfile{propagate.c}
  \example{}{}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  arg=xlgetarg();
  if (listp(arg)){
    for (n = 0; consp(arg); arg = cdr(arg)){
      arg2 = car(arg);           
      if (imagep(arg2))                    
	imarray[n++]=(IMAGE*)getimage(arg2); 
      else{                      
	xlbadtype(arg2);          
	break;                    
      }                           
    }
    arg=xlgetarg();
  }
  else{
    while ( arg != NIL ){
      if (imagep(arg)){
	imarray[n++]=(IMAGE *)getimage(arg);
	arg=xlgetarg();
      }
      else
	break;
    }
  }

  if (integerp(arg))
    graph = (int)getfixnum(arg);
  else
    xlbadtype(arg);

  if (propagate(getimage(xlim1), getimage(xlim2), imarray, n, graph)==ERROR)
    xlabort("(@propagate lbl dst im1 ... imn graph)");

  return xlim1;
}

LVAL ilabelvertex()
{
  LVAL xlim;
  IMAGE *imout;
  xlim = xlgaimage();
  int graph, alpha;

  if (!moreargs())
    xlabort("(@labelvertex im graph alpha)");
/*
  \lspfunction{@}{labelvertex}{im graph alpha}
  \param{im}{image node for labelled image}
  \param{graph}{integer for connectivity}
  \param{alpha}{integer for maximum difference between adjacent pixels}
  \return{an image of type UCHAR indicating the degree of each node of the corresponding graph}
  \desc{}
  \cfile{labelvertex.c}
  \example{}{}
*/

  graph    = (int)getfixnum(xlgafixnum());
  alpha    = (int)getfixnum(xlgafixnum());

  xllastarg();
 
  imout = (IMAGE *)labelvertex((IMAGE *)getimage(xlim), graph, alpha);
  if (imout!=NULL)
    return (cvimage(imout));
  else
    return NIL;
}

LVAL ivertexseparation()
{
  LVAL xlim;
  IMAGE *imout;
  xlim = xlgaimage();
  int graph, type;

  if (!moreargs())
    xlabort("(@vertexseparation im graph type)");
/*
  \lspfunction{@}{vertexseparation}{im graph type}
  \param{im}{image node}
  \param{graph}{integer for connectivity}
  \param{type}{0 for lower values, 1 for higher, and 2 for MIN(0,1)}
  \return{an image node containing the minimum separation value of each pixel in graph-connectivity}
  \desc{}
  \cfile{labelvertex.c}
  \example{}{}
*/

  graph    = (int)getfixnum(xlgafixnum());
  type     = (int)getfixnum(xlgafixnum());

  xllastarg();
 
  imout = (IMAGE *)vertexseparation((IMAGE *)getimage(xlim), graph, type);
  if (imout!=NULL)
    return (cvimage(imout));
  else
    return NIL;
}

LVAL ilabelvertexconnectedness()
{
  LVAL xlim;
  IMAGE *imout;
  xlim = xlgaimage();
  int graph, alpha, deg;

  if (!moreargs())
    xlabort("(@labelvertexconnectedness im graph alpha deg)");
/*
  \lspfunction{@}{labelvertex}{im graph alpha}
  \param{im}{image node for labelled image}
  \param{graph}{integer for connectivity}
  \param{alpha}{integer for maximum difference between adjacent pixels}
  \param{deg}{integer for connectivity}
  \return{an image of labels of the flat zones of a given degree and whose pixels are alpha-tight}
  \desc{}
  \cfile{labelvertex.c}
  \example{}{}
*/
  graph    = (int)getfixnum(xlgafixnum());
  alpha    = (int)getfixnum(xlgafixnum());
  deg      = (int)getfixnum(xlgafixnum());

  xllastarg();
 
  imout = (IMAGE *)labelvertexconnectedness((IMAGE *)getimage(xlim), graph, alpha, deg);
  if (imout!=NULL)
    return (cvimage(imout));
  else
    return NIL;
}

LVAL iouteredgelut()
{
  LVAL xlim1, xlim2;
  IMAGE *imout;
  if (!moreargs())
    xlabort("(*outeredgelut ilbl iedgelbl)");
/*
  \lspfunction{*}{outeredgelut}{ilbl iedgelbl}
  \param{ilbl}{image node for labelled image}
  \param{iedgelbl}{image node for labelled internal edges of ilbl}
  \return{an image holding a LUT which preserves labelled of outer boundaries and set to 0 the others}
  \desc{the outer edge of a CC is defined here as the connected component of internal edge pixels that is connected to the infinite connected component of the background the CC.  The related function outeredge should be used to extract only those pixels that are adjacent to the infinite connected component of the background the CC.}
  \myseealso{\htmlref{*outeredge}{*outeredge}}
  \cfunction{\cfouteredgelut}
  \cfile{outeredge.c}
  \example{}{}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();

  xllastarg();

  imout=(IMAGE *)outeredgelut((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2));
  if (imout!=NULL)
    return (cvimage(imout));
  else
    return NIL;
}

LVAL iouteredge()
{
  LVAL xlim1;
  int graph;
  IMAGE *imout;
  if (!moreargs())
    xlabort("(*outeredge ilbl graph)");
/*
  \lspfunction{*}{outeredge}{ilbl graph}
  \param{ilbl}{image node for a labelled image}
  \param{graph}{integer for connectivity (4 or 8)}
  \return{a binary image holding the outer edge of each connected component}
  \desc{Assumes that the border of ilbl is equal to zero to avoid border overflow.  Implementation based on Moore's contour tracing algorithm \citep{sobel78}, optimised and extended from \htmladdnormallink{http://www.thebigblob.com/moore-neighbor-contour-tracing-algorithm-in-c/}{http://www.thebigblob.com/moore-neighbor-contour-tracing-algorithm-in-c/}.  See further interesting information on chain codes at \url{http://poseidon.csd.auth.gr/LAB_PUBLICATIONS/Books/dip_material/chapter_7/chap7en.pdf}.}
  \myseealso{\htmlref{*outeredgelut}{*outeredgelut}}
  \creationdate{20100918}
  \cfunction{\cfouteredge}
  \cfile{outeredge.c}
  \example{}{}
*/

  xlim1 = xlgaimage();
  graph    = (int)getfixnum(xlgafixnum());

  xllastarg();

  imout=(IMAGE *)outeredge((IMAGE *)getimage(xlim1), graph);
  if (imout!=NULL)
    return (cvimage(imout));
  else
    return NIL;
}

LVAL ioutercontour()
{
  LVAL xlim1;
  int graph;
  IMAGE *imout;
  if (!moreargs())
    xlabort("(*outercontour ilbl graph)");
/*
  \lspfunction{*}{outercontour}{ilbl graph}
  \param{ilbl}{image node for a labelled image}
  \param{graph}{integer for connectivity (4 or 8)}
  \return{a uchar image with outer edge pixels set to 9 if change of direction, the other values (i.e., 1 to 8) being used for coding the direction in case it does not change.  Zero is used for the non outer edge pixels.}
  \desc{Assumes that the border of ibl is equal to zero to avoid border overflow.  Implementation based on Moore's contour tracing algorithm \citep{sobel78}, optimised and extended from \htmladdnormallink{http://www.thebigblob.com/moore-neighbor-contour-tracing-algorithm-in-c/}{http://www.thebigblob.com/moore-neighbor-contour-tracing-algorithm-in-c/}.  See further interesting information on chain codes at \url{http://poseidon.csd.auth.gr/LAB_PUBLICATIONS/Books/dip_material/chapter_7/chap7en.pdf}.}
  \myseealso{\htmlref{*outeredgelut}{*outeredgelut}}
  \creationdate{20100918}
  \cfunction{\cfoutercontour}
  \cfile{outeredge.c}
  \example{}{}
*/

  xlim1 = xlgaimage();
  graph    = (int)getfixnum(xlgafixnum());

  xllastarg();

  imout=(IMAGE *)outercontour((IMAGE *)getimage(xlim1), graph);
  if (imout!=NULL)
    return (cvimage(imout));
  else
    return NIL;
}


LVAL ichull()
{
  LVAL xlim1;
  int graph;
  IMAGE *imout;
  if (!moreargs())
    xlabort("(*chull ilbl graph)");
/*
  \lspfunction{*}{chull}{ilbl graph}
  \param{ilbl}{image node for a labelled image}
  \param{graph}{integer for connectivity (4 or 8)}
  \return{}
  \desc{}
  \myseealso{\htmlref{*outeredgelut}{*outeredgelut}}
  \creationdate{20100918}
  \cfunction{\cfoutercontour}
  \cfile{outeredge.c}
  \example{}{}
*/

  xlim1 = xlgaimage();
  graph    = (int)getfixnum(xlgafixnum());

  xllastarg();

  imout=(IMAGE *)chull((IMAGE *)getimage(xlim1), graph);
  if (imout!=NULL)
    return (cvimage(imout));
  else
    return NIL;
}

LVAL iedgeweight()
{
  LVAL xlim1;
  int dir, type;
  IMAGE *imout;
  if (!moreargs())
    xlabort("(*edgeweight im dir type)");
/*
  \lspfunction{*}{edgeweight}{im dir type}
  \param{im}{an image node}
  \param{dir}{integer for coding direction (horizontal if 0, vertical otherwise)}
  \param{type}{integer value (0 for absolute value of difference, 1 for maximum value, 2 for minimum value)}
  \return{an image node with horizontal edge weights: $e(p,q)=|f(p)-f(q)|$ for type equal to 0, $e(p,q)=MAX(f(p)-f(q))$ for type equal to 1}
  \desc{the size of the returned image is nx-1 times ny}
  \myseealso{\htmlref{*edgeweight}{*edgeweight}}
  \creationdate{20101018}
  \cfunction{\cfedgeweight}
  \cfile{edgeweight.c}
  \example{}{}
*/

  xlim1 = xlgaimage();
  dir    = (int)getfixnum(xlgafixnum());
  type   = (int)getfixnum(xlgafixnum());

  xllastarg();

  imout=(IMAGE *)edgeweight((IMAGE *)getimage(xlim1), dir, type);
  if (imout!=NULL)
    return (cvimage(imout));
  else
    return NIL;
}

LVAL idissim()
{
  LVAL arg, arg2, xlim1;
  IMAGE *imarray[maxNumberOfBands];
  int type, n=0;
  IMAGE *imout;
  if (!moreargs())
    xlabort("(*dissim imlist mask type)");
/*
  \lspfunction{*}{dissim}{im mask type}
  \param{imlist}{a list of image nodes}
  \param{mask}{an image node for mask}
  \param{type}{integer value for type of distance calculation (0 for Euclidean)}
  \return{an image node holding similarity matrix (with value -1.0 for non-mask pixels)}
  \desc{used for clustering purposes}
  \myseealso{}
  \creationdate{20120130}
  \cfunction{\cfdissim}
  \cfile{dbscan.c}
  \example{}{}
*/
  arg=xlgetarg();
  if (listp(arg)){
    for (n = 0; consp(arg); arg = cdr(arg)){
      arg2 = car(arg);           
      if (imagep(arg2))                    
	imarray[n++]=(IMAGE*)getimage(arg2); 
      else{                      
	xlbadtype(arg2);          
	break;                    
      }                           
    }
  }
  else
    xlbadtype(arg);

  xlim1 = xlgaimage();
  type   = (int)getfixnum(xlgafixnum());

  xllastarg();

  imout=(IMAGE *)dissim(imarray, n, (IMAGE *)getimage(xlim1), type);
  if (imout!=NULL)
    return (cvimage(imout));
  else
    return NIL;
}


LVAL idbscan()
{
  LVAL xlim1;
  double eps;
  int minpts;
  IMAGE *imout;
  if (!moreargs())
    xlabort("(*dbscan dissim eps minpts)");
/*
  \lspfunction{*}{dbscan}{dissim eps minpts}
  \param{dissim}{an image node holding dissimilarity matrix}
  \param{eps}{floating point value for maximum dissimilarity threshold}
  \param{minpts}{integer value for minimum number of points in eps neighbourhood}
  \return{an image node with labelled clusters}
  \desc{DBSCAN}
  \myseealso{\href{*dissim}{*dissim}}
  \creationdate{20120131}
  \cfunction{\cfdbscan}
  \cfile{dbscan.c}
  \example{}{}
*/

  xlim1 = xlgaimage();
  eps    = (double)getflonum(xlgaflonum());
  minpts = (int)getfixnum(xlgafixnum());

  xllastarg();

  imout=(IMAGE *)dbscan((IMAGE *)getimage(xlim1), eps, minpts);
  if (imout!=NULL)
    return (cvimage(imout));
  else
    return NIL;
}

/*
  \lispsection
  \input{label.tex}  % Functions defined in lisp
*/


#ifdef ALLFUNCTIONS
/*
\module{Distance transforms} % dist.c
\basesection
*/
LVAL idst2d4()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(@dst2d4 im)");
/*
  \lspfunction{@}{dst2d4}{im}
  \param{im}{an image node}
  \return{the image node im}
  \desc{computes the 2-dimensional 4-connected distance function of an image im.}
  \cfunction{\cfdstTWOdFOUR}
  \cfile{dist.c}
  \example{}{}
*/
  xlim1 = xlgaimage();

  xllastarg();

  if (dst2d4((IMAGE *)getimage(xlim1))==ERROR)
    xlabort("dst2d4");

  return(xlim1);
}

LVAL idst2dchamfer()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(@dst2dchamfer im)");

/*
  \lspfunction{@}{dst2dchamfer}{im}
  \param{im}{an image node}
  \return{the image node im}
  \desc{computes the 5-7 chamfer distance of im.}
  \cfunction{\cfdstTWOdchamfer}
  \cfile{dist.c}
  \example{}{}
*/

  xlim1 = xlgaimage();

  xllastarg();

  if (dst2dchamfer((IMAGE *)getimage(xlim1))==ERROR)
    return NIL;

  return(xlim1);
}

LVAL ichamfer2d()
{
  LVAL xlim1;
  int type;

  if (!moreargs())
    xlabort("(@chamfer2d im type); type in {1,11,34,57,5711}");

/*
  \lspfunction{@}{chamfer2d}{im type}
  \param{im}{an image node}
  \param{type}{integer for type of chamfer distance in \{1, 11, 34, 57, or 5711\} }
  \return{the image node im}
  \desc{computes the chamfer distance function of im.  Note that the type of im must be large enough to hold the largest distance otherwise overflow may occur and generate artefacts in the resulting distance function.}
  \cfunction{\cfchamferTWOd}
  \cfile{dist.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  type = (int) getfixnum(xlgafixnum());

  xllastarg();

  if (chamfer2d((IMAGE *)getimage(xlim1), type)==ERROR)
    return NIL;

  return(xlim1);
}

LVAL iedistfifo()
{
  int graph;
  LVAL xlim1;

  if (!moreargs())
    xlabort("(*edstfifo im graph)");
/*
  \lspfunction{*}{edstfifo}{im graph}
  \param{im}{an image node}
  \param{graph}{integer for connectivity (either 4 or 8)}
  \return{an image node of type USHORT}
  \desc{outputs the Euclidean distance function of im using the algorithm described in~\citep{soille92phd,soille-gratin94}.  im must be a 2-D binary image.}
  \cfunction{\cfedistfifoTWOd}
  \cfile{dist.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  graph = (int)getfixnum(xlgafixnum());

  xllastarg();

  return(cvimage(edistfifo2d((IMAGE *)getimage(xlim1), graph)));
}

LVAL isqedt()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(*sqedt im)");
/*
  \lspfunction{*}{sqedt}{im}
  \param{im}{an UCHAR image node}
  \return{an image node of type INT32}
  \desc{outputs the squared Euclidean distance transform of im.  im must be a 2-D binary image.  Original algorihtm proposed by \citet{saito-toriwaki94} and then optimised independently by \citep{hirata96} and \citep{meijster-roerdink-hesselink2000}.  See also *edt for the actual Euclidean distance transform.  Note that a temporary buffer of type UINT16 is used for sums along/lines and columns so that uncontrolled results will occur if an object shows more than $16^2/2$ foreground pixels along a given line or column.}
  \cfunction{\cfsqedt}
  \cfile{efedt.c}
  \example{}{}
  \changedate{20131002}
*/
  xlim1 = xlgaimage();

  xllastarg();

  return(cvimage(sqedt((IMAGE *)getimage(xlim1))));
}

LVAL iiz()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(*iz im)");
/*
  \lspfunction{*}{iz}{im}
  \param{im}{an image node}
  \return{an image node of the same type}
  \desc{outputs the error-free influence zones of the labelled regions in im.  im must be a 2-D image, its last bit being reserved (e.g., for a UCHAR images, authorized label values range from 1 to 127).  Algorithm based on *edt.}
  \cfunction{\cfiz}
  \cfile{efedt.c}
  \example{}{}
*/
  xlim1 = xlgaimage();

  xllastarg();

  return(cvimage(iz((IMAGE *)getimage(xlim1))));
}


/*
  \lispsection
  \input{distance.tex}  % Functions defined in lisp
*/



#endif



#endif



/*
\module{Erosion and dilation}
\basesection
*/

LVAL ierode()
{
  LVAL xlim1, xlim2;
  int ox, oy, oz, trflag=0;

  if (!moreargs())
    xlabort("(*erode im imse &optional ox oy oz trflag)\n");

/*
  \lspfunction{*}{erode}{im imse &optional ox oy oz trflag}
  \param{im}{an image node}
  \param{imse}{an image node for SE (UCHAR type)}
  \param{ox}{x coordinate (default equals integer part of half the width of imse)}
  \param{oy}{y coordinate (default equals integer part of half the height of imse)}
  \param{oz}{z coordinate (default equals integer part of half the number of x-y planes of imse)}
  \param{trflag}{optional parameter: 0(default) or 1}
  \return{an image node}
  \desc{outputs the erosion of im using the SE defined by imse, its origin being set at coordinates (x,y,z).  The reflection of the SE is considered if trflag equals 1 (no reflection by default).  Points of the SE are points with a non zero value in imse.}
  \cfunction{\cferode}
  \cfile{erodil.c}
  \example{}{}
*/
  xlim1     = xlgaimage();
  xlim2     = xlgaimage();
  if (moreargs()){
    ox        = (int)getfixnum(xlgafixnum());
    oy        = (int)getfixnum(xlgafixnum());
    oz        = (int)getfixnum(xlgafixnum());
    trflag    = (int)getfixnum(xlgafixnum());
  }
  else{
    ox = GetImNx((IMAGE *)getimage(xlim2))/2;
    oy = GetImNy((IMAGE *)getimage(xlim2))/2;
    oz = GetImNz((IMAGE *)getimage(xlim2))/2;
  }

  xllastarg();

  return(cvimage(erode((IMAGE *)getimage(xlim1),(IMAGE *)getimage(xlim2),ox,oy,oz,trflag)));
}

LVAL idilate()
{
  LVAL xlim1, xlim2;
  int ox, oy, oz, trflag=0;


  if (!moreargs())
    xlabort("(*dilate im imse &optional ox oy oz trflag)\n");
/*
  \lspfunction{*}{dilate}{im imse &optional ox oy oz trflag}
  \param{im}{an image node}
  \param{imse}{an image node for SE (UCHAR type)}
  \param{ox}{x coordinate (default equals integer part of half the width of imse)}
  \param{oy}{y coordinate (default equals integer part of half the height of imse)}
  \param{oz}{z coordinate (default equals integer part of half the number of x-y planes of imse)}
  \param{trflag}{optional parameter: 0(default) or 1}
  \return{an image node}
  \desc{outputs the dilation of im using the SE defined by imse, its origin being set at coordinates (x,y,z).  The reflection of the SE is considered if trflag equals 1 (no reflection by default).  Points of the SE are points with a non zero value in imse.}
  \cfunction{\cfdilate}
  \cfile{erodil.c}
  \example{}{}
*/
  xlim1     = xlgaimage();
  xlim2     = xlgaimage();
  if (moreargs()){
    ox        = (int)getfixnum(xlgafixnum());
    oy        = (int)getfixnum(xlgafixnum());
    oz        = (int)getfixnum(xlgafixnum());
    trflag    = (int)getfixnum(xlgafixnum());
  }
  else{
    ox = GetImNx((IMAGE *)getimage(xlim2))/2;
    oy = GetImNy((IMAGE *)getimage(xlim2))/2;
    oz = GetImNz((IMAGE *)getimage(xlim2))/2;
  }
  xllastarg();

  return(cvimage(dilate((IMAGE *)getimage(xlim1),(IMAGE *)getimage(xlim2),ox,oy,oz,trflag)));
}

LVAL irank()
{
  LVAL xlim1, xlim2;
  int r, ox, oy, oz, trflag=0;

  if (!moreargs())
    xlabort("(*rank im imse rank &optional ox oy oz trflag)\n");
/*
  \lspfunction{*}{rank}{im imse rank &optional ox oy oz trflag}
  \param{im}{an image node}
  \param{imse}{an image node for SE (UCHAR type)}
  \param{rank}{integer value in [1,card(SE)]}
  \param{ox}{x coordinate (default equals integer part of half the width of imse)}
  \param{oy}{y coordinate (default equals integer part of half the height of imse)}
  \param{oz}{z coordinate (default equals integer part of half the number of x-y planes of imse)}
  \param{trflag}{optional parameter: 0(default) or 1}
  \return{an image node}
  \desc{outputs the rank filter of im using the SE defined by imse, its origin being set at coordinates (x,y,z).  The reflection of the SE is considered if trflag equals 1 (no reflection by default).  Points of the SE are points with a non zero value in imse.  The rank must belong to [1,card(SE)].}
  \cfunction{\cfrank}
  \cfile{erodil.c}
  \example{}{}
*/
  xlim1     = xlgaimage();
  xlim2     = xlgaimage();
  r         = (int)getfixnum(xlgafixnum()); /* the rank */
  if (moreargs()){
    ox        = (int)getfixnum(xlgafixnum());
    oy        = (int)getfixnum(xlgafixnum());
    oz        = (int)getfixnum(xlgafixnum());
    trflag    = (int)getfixnum(xlgafixnum());
  }
  else{
    ox = GetImNx((IMAGE *)getimage(xlim2))/2;
    oy = GetImNy((IMAGE *)getimage(xlim2))/2;
    oz = GetImNz((IMAGE *)getimage(xlim2))/2;
  }
  xllastarg();

  return(cvimage(rank((IMAGE *)getimage(xlim1),(IMAGE *)getimage(xlim2),r,ox,oy,oz, trflag)));
}

LVAL ilineplero()
{
  LVAL xlim1;
  int dx, dy, k, o, t=1;

  if (!moreargs())
    xlabort("(@lerode im dx dy k &optional o p)\n");
/*
  \lspfunction{@}{lerode}{im dx dy k &optional o p}
  \param{im}{an image node}
  \param{dx}{x coordinate for line segment slope}
  \param{dy}{y coordinate for line segment slope}
  \param{k}{number of pixels along line segment}
  \param{o}{integer in [0,k-1] for origin of SE along the line segment (default value equals the integer part of half of k)}
  \param{p}{positive integer for periodicity (default value equals 1)}
  \return{im}
  \desc{erosion by a line segment with arbitrary length, origin, and periodicity \citep{soille-breen-jones96}.}
  \coding{Version with openMP since 20120420.}
  \cfunction{\cfherkplero}
  \cfile{herk.c}
*/

  xlim1     = xlgaimage();
  dx        = (int)getfixnum(xlgafixnum());
  dy        = (int)getfixnum(xlgafixnum());
  k         = (int)getfixnum(xlgafixnum());

  if(moreargs()){
    o = (int)getfixnum(xlgafixnum());
    t = (int)getfixnum(xlgafixnum());
  }
  else
    o = k/2;

  xllastarg();

  if (herkplero((IMAGE *)getimage(xlim1),dx,dy,k,o,t)==ERROR)
    return NIL;

  return(xlim1);
}

LVAL ilinepldil()
{
  LVAL xlim1;
  int dx, dy, k, o, t=1;
  
  if (!moreargs())
    xlabort("(@ldilate im dx dy k &optional o p)\n");
/*
  \lspfunction{@}{ldilate}{im dx dy k &optional o p}
  \param{im}{an image node}
  \param{dx}{x coordinate for line segment slope}
  \param{dy}{y coordinate for line segment slope}
  \param{k}{number of pixels along line segment}
  \param{o}{integer in [0,k-1] for origin of SE along the line segment (default value equals the integer part of half of k)}
  \param{p}{positive integer (default value equals 1)}
  \return{im}
  \desc{dilation by a line segment with arbitrary length, origin, and periodicity \citep{soille-breen-jones96}.}
  \cfunction{\cfherkpldil}
  \cfile{herk.c}
*/
  xlim1     = xlgaimage();
  dx        = (int)getfixnum(xlgafixnum());
  dy        = (int)getfixnum(xlgafixnum());
  k         = (int)getfixnum(xlgafixnum());

  if(moreargs()){
    o = (int)getfixnum(xlgafixnum());
    t = (int)getfixnum(xlgafixnum());
  }
  else
    o=k/2;

  xllastarg();

  if (herkpldil((IMAGE *)getimage(xlim1),dx,dy,k,o,t)==ERROR)
    return NIL;

  return(xlim1);
}

LVAL ilinerank()
{
  LVAL xlim1;
  int dx, dy, k, rank, o;


  if (!moreargs())
    xlabort("(@lrank im dx dy k rank &optional o)\n");
/*
  \lspfunction{@}{lrank}{im dx dy k rank &optional o}
  \param{im}{an image node}
  \param{dx}{x coordinate for line segment slope}
  \param{dy}{y coordinate for line segment slope}
  \param{k}{number of pixels along line segment}
  \param{rank}{integer value in [1,k] for rank}
  \param{o}{integer in [0,k-1] for origin of SE along the line segment (default value equals the integer part of half of k)}
  \return{the image node im}
  \desc{translation variant implementation of a rank filter by a line segment of slope dy/dx, length k pixels, and rank rank.}
  \cfunction{\cflinerank}
  \cfile{rank.c}
*/
  
  xlim1     = xlgaimage();
  dx        = (int)getfixnum(xlgafixnum());
  dy        = (int)getfixnum(xlgafixnum());
  k         = (int)getfixnum(xlgafixnum());
  rank = (int)getfixnum(xlgafixnum());
  if (moreargs())
    o = (int)getfixnum(xlgafixnum());
  else
    o = k/2;

  xllastarg();

  if (linerank((IMAGE *)getimage(xlim1),dx,dy,k,rank,o)==ERROR)
    return NIL;

  return(xlim1);
}

LVAL ilrankti()
{
  LVAL xlim1;
  int dx, dy, k, o, i, rank, trflag=1;

  if (!moreargs())
    xlabort("(*lrankti im dx dy k rank &optional o i trflag)\n");
/*
  \lspfunction{*}{lrankti}{im dx dy k rank &optional o i trflag}
  \param{im}{an image node}
  \param{dx}{x coordinate for line segment slope}
  \param{dy}{y coordinate for line segment slope}
  \param{k}{number of pixels along line segment}
  \param{rank}{integer value in [1,k] for rank}
  \param{o}{integer in [0,k-1] for origin of SE along the line segment (default value equals the integer part of half of k)}
  \param{i}{among all possible SEs along the Bresenham line with slope dy/dx, select the one with index i (default is 0)}
  \param{trflag}{integer flag for reflection: 1(default for no reflection) or -1 otherwise}
  \return{a new image node}
  \desc{translation invariant implementation of rank filter by a line segment \citep{soille-talbot2001}.}
  \cfunction{\cflrankti}
  \cfile{herkbl.c}
  \feature{it seems that results are incorrect if the origin does not equal the default value (0).}
*/
  xlim1     = xlgaimage();
  dx        = (int)getfixnum(xlgafixnum());
  dy        = (int)getfixnum(xlgafixnum());
  k         = (int)getfixnum(xlgafixnum());
  rank      = (int)getfixnum(xlgafixnum());
  if (moreargs()){
    o = (int)getfixnum(xlgafixnum());
    i = (int)getfixnum(xlgafixnum());
    trflag        = (int)getfixnum(xlgafixnum());
  }
  else{
    o = k/2;
    i = 0;
    trflag=1;
    }
  xllastarg();

  return(cvimage(lrankti((IMAGE *)getimage(xlim1),dx,dy,k,rank,o,i,trflag)));
}

#ifdef ALLFUNCTIONS
/* lerodil.c */
LVAL ilinero()
{
  LVAL xlim1;
  int dx, dy, k, line_type=0;

  if (!moreargs())
    xlabort("(@linero im dx dy k line_type)\nline_type: 0, for connected and 1 for periodic lines");

/*
  \lspfunction{@}{linero}{im dx dy k &optional line_type}
  \param{im}{an image node}
  \param{dx}{x coordinate for line segment slope}
  \param{dy}{y coordinate for line segment slope}
  \param{k}{number of pixels along line segment}
  \param{line_type}{0 for plain (default), 1 for periodic line segment}
  \return{the image node im}
  \desc{erosion by a line segment of length k and slope dy/dx.  The length of the line segment must be odd (if not, it is extended by one pixel).  Translation variant implementation.  This function is obsolete since it is superseeded by @lerode.}
  \myseealso{\htmlref{@lerode}{@lerode}}
  \cfunction{\cflinero}
  \cfile{lerodil.c}
  \example{(@linero im 1 1 11 0)}{performs the erosion of im with a 11 pixel line segment at -45 degrees.}
*/


  xlim1     = xlgaimage();
  dx        = (int)getfixnum(xlgafixnum());
  dy        = (int)getfixnum(xlgafixnum());
  k         = (int)getfixnum(xlgafixnum());
  if (moreargs())
    line_type = (int)getfixnum(xlgafixnum());

  xllastarg();

  if (linero((IMAGE *)getimage(xlim1),dx,dy,k,line_type)==ERROR)
    return NIL;

  return(xlim1);
}

LVAL ilindil()
{
  LVAL xlim1;
  int dx, dy, k, line_type=0;

  if (!moreargs())
    xlabort("(@lindil im dx dy k line_type)\nline_type: 0, for connected and 1 for periodic lines");
/*
  \lspfunction{@}{lindil}{im dx dy k &optional line_type}
  \param{im}{an image node}
  \param{dx}{x coordinate for line segment slope}
  \param{dy}{y coordinate for line segment slope}
  \param{k}{number of pixels along line segment}
  \param{line_type}{0 for plain (default), 1 for periodic line segment}
  \return{the image node im}
  \desc{dilation by a line segment of length k and slope dy/dx.  The length of the line segment must be odd (if not, it is extended by one pixel).  Translation variant implementation.  This function is obsolete since it is superseeded by @ldilate.}
  \myseealso{\htmlref{@ldilate}{@ldilate}}
  \cfunction{\cflindil}
  \cfile{lerodil.c}
  \example{(@lindil im 1 1 11 0)}{performs the dilation of im with a 11 pixel line segment at -45 degrees.}
*/


  xlim1     = xlgaimage();
  dx        = (int)getfixnum(xlgafixnum());
  dy        = (int)getfixnum(xlgafixnum());
  k         = (int)getfixnum(xlgafixnum());
  if (moreargs())
    line_type = (int)getfixnum(xlgafixnum());

  xllastarg();

  if (lindil((IMAGE *)getimage(xlim1), dx,dy,k,line_type)==ERROR)
    return NIL;

  return(xlim1);
}


LVAL ierode4()
{
  LVAL xlim1;
  int ox=1, oy=1;

  if (!moreargs())
    xlabort("(@erode4 im &optional ox oy) elementary 4-connected erosion of im");
/*
  \lspfunction{@}{erode4}{im &optional ox oy}
  \param{im}{an image node}
  \param{ox}{integer x coordinate of origin of SE (0,1, or 2), default equals 1}
  \param{oy}{integer y coordinate of origin of SE (0,1, or 2), default equals 1}
  \return{the image node im}
  \desc{performs the elementary 4-connected erosion of im.  By default, a centred SE is used, otherwise the x and y coordinates of origin must be specified.}
  \cfunction{\cferodeFOUR}
  \cfile{erodil.c}
*/
  xlim1     = xlgaimage();
  if (moreargs()){
    ox = (int)getfixnum(xlgafixnum());
    oy = (int)getfixnum(xlgafixnum());
  }
    
  xllastarg();
  if (erode4((IMAGE *)getimage(xlim1),ox,oy)==ERROR)
    return NIL;
  return(xlim1);
}
LVAL idilate4()
{
  LVAL xlim1;
  int ox=1, oy=1;

  if (!moreargs())
    xlabort("(@dilate4 im &optional ox oy) elementary 4-connected dilation of im");
/*
  \lspfunction{@}{dilate4}{im &optional ox oy}
  \param{im}{an image node}
  \param{ox}{integer x coordinate of origin of SE (0,1, or 2), default equals 1}
  \param{oy}{integer y coordinate of origin of SE (0,1, or 2), default equals 1}
  \return{the image node im}
  \desc{performs the elementary 4-connected dilation of im.  By default, a centred SE is used, otherwise the x and y coordinates of origin must be specified.}
  \cfunction{\cfdilateFOUR}
  \cfile{erodil.c}
*/
  xlim1     = xlgaimage();
  if (moreargs()){
    ox = (int)getfixnum(xlgafixnum());
    oy = (int)getfixnum(xlgafixnum());
  }
    
  xllastarg();
  if (dilate4((IMAGE *)getimage(xlim1),ox,oy)==ERROR)
    return NIL;
  return(xlim1);
}
#endif

LVAL isquarerank()
{
  LVAL xlim1;
  int k, rank, ox, oy;

  if (!moreargs())
    xlabort("(*8rank im k rank &optional ox oy)\n");
/*
  \lspfunction{*}{8rank}{im k rank &optional ox oy}
  \param{im}{an image node}
  \param{k}{integer width of square SE}
  \param{rank}{integer for rank value in [1,k*k]}
  \param{ox}{\descoxdef}
  \param{oy}{\descoxdef}
  \return{an image node}
  \desc{rank filters using a square SE of width k and origin at (ox,oy).  Border pixels where the SE is not included in the image definition domain are not treated (the output value of these pixels is set to 0).}
  \myseealso{for function `avoiding' border effects.}
  \cfunction{\cfsquarerank}
  \cfile{rank.c}
  \example{(*8rank i0 3 9 1 1)}{rank filter of i0 by a centered 3x3 square with rank 9 (identical to a dilation).}
*/  
  xlim1     = xlgaimage();
  k         = (int)getfixnum(xlgafixnum());
  rank = (int)getfixnum(xlgafixnum());
  if (moreargs()){
    ox = (int)getfixnum(xlgafixnum());
    oy = (int)getfixnum(xlgafixnum());
  }
  else{
    ox = k/2;
    oy = k/2;
  }

  xllastarg();

  return(cvimage(squarerank((IMAGE *)getimage(xlim1), k, rank, ox, oy)));
}

LVAL imsgradlinf()
{
  LVAL xlim1, xlim2, xlim3, xlim4, xlim5, xlim6, xlim7, xlim8; /* maximum of 8 bands */
  IMAGE *imarray[8];
  int n=0, graph;

  if (!moreargs())
    xlabort("(*msgradlinf graph im1 [im2] ... [im255])");
/*
  \lspfunction{*}{msgradlinf}{graph im1 [im2] ... [im255]}
  \param{graph}{integer for connectivity}
  \param{im1}{an image node for 1st channel}
  \param{imi}{an image node for ith channel (maximum of 255 channels)}
  \return{an image node}
  \desc{graph-connected multispectral gradient of a multi-channel image (up to 255 channels). }
  \cfunction{\cfmsgradlinf}
  \cfile{test.c}
  \example{}{}
*/

  graph = (int) getfixnum(xlgafixnum());
  xlim1 = xlgaimage();
  imarray[0]=(IMAGE *)getimage(xlim1);
  n++;
  if (moreargs()){
    xlim2 = xlgaimage();
    imarray[1]=(IMAGE *)getimage(xlim2);
    n++;
    if (moreargs()){
      xlim3 = xlgaimage();
      imarray[2]=(IMAGE *)getimage(xlim3);
      n++;
      if (moreargs()){
        xlim4 = xlgaimage();
        imarray[3]=(IMAGE *)getimage(xlim4);
	n++;
        if (moreargs()){
          xlim5 = xlgaimage();
          imarray[4]=(IMAGE *)getimage(xlim5);
	  n++;
          if (moreargs()){
            xlim6 = xlgaimage();
            imarray[5]=(IMAGE *)getimage(xlim6);
	    n++;
            if (moreargs()){
	      xlim7 = xlgaimage();
	      imarray[6]=(IMAGE *)getimage(xlim7);
	      n++;
	      if (moreargs()){
		xlim8 = xlgaimage();
		imarray[7]=(IMAGE *)getimage(xlim8);
		n++;
	      }
	    }
	  }
	}
      }
    }
  }

  xllastarg();

  return(cvimage(msgradlinf(imarray, n, graph)));
}


LVAL imsgradlinfngb()
{
  LVAL xlim1, xlim2, xlim3, xlim4, xlim5, xlim6, xlim7, xlim8, xlim9, xlim10; /* maximum of 10 bands */
  LVAL xlimngb;
  int ox, oy, oz;
  IMAGE *imarray[maxNumberOfBands];
  int n=0;

  if (!moreargs())
    xlabort("(*msgradlinfngb imngb ox oy oz im1 [im2] ... [im255])");

/*
  \lspfunction{*}{msgradlinfngb}{imngb ox oy oz im1 [im2] ... [im255]}
  \param{imngb}{an image node for neighbouring pixels}
  \param{ox}{\descox}
  \param{oy}{\descoy}
  \param{oz}{\descoz}
  \param{im1}{an image node for 1st channel}
  \param{imi}{an image node for ith channel (maximum of 255 channels)}
  \return{an image node}
  \desc{multispectral gradient of a multi-channel image (up to 255 channels) using a neighbourhood defined by imngb with origin at (ox,oy,oz).}
  \cfunction{\cfmsgradlinfngb}
  \cfile{msmm.c}
  \example{}{}
*/
  xlimngb    = xlgaimage();
  ox    = (int)getfixnum(xlgafixnum());
  oy    = (int)getfixnum(xlgafixnum());
  oz    = (int)getfixnum(xlgafixnum());

  xlim1 = xlgaimage();
  imarray[0]=(IMAGE *)getimage(xlim1);
  n++;
  if (moreargs()){
    xlim2 = xlgaimage();
    imarray[1]=(IMAGE *)getimage(xlim2);
    n++;
    if (moreargs()){
      xlim3 = xlgaimage();
      imarray[2]=(IMAGE *)getimage(xlim3);
      n++;
      if (moreargs()){
        xlim4 = xlgaimage();
        imarray[3]=(IMAGE *)getimage(xlim4);
	n++;
        if (moreargs()){
          xlim5 = xlgaimage();
          imarray[4]=(IMAGE *)getimage(xlim5);
	  n++;
          if (moreargs()){
            xlim6 = xlgaimage();
            imarray[5]=(IMAGE *)getimage(xlim6);
	    n++;
            if (moreargs()){
	      xlim7 = xlgaimage();
	      imarray[6]=(IMAGE *)getimage(xlim7);
	      n++;
	      if (moreargs()){
		xlim8 = xlgaimage();
		imarray[7]=(IMAGE *)getimage(xlim8);
		n++;
		if (moreargs()){
		  xlim9 = xlgaimage();
		  imarray[8]=(IMAGE *)getimage(xlim9);
		  n++;
		  if (moreargs()){
		    xlim10 = xlgaimage();
		    imarray[9]=(IMAGE *)getimage(xlim10);
		    n++;
		  }
		}
	      }
	    }
	  }
	}
      }
    }
  }

  xllastarg();

  return(cvimage(msgradlinfngb(imarray, n, (IMAGE *)getimage(xlimngb), ox, oy, oz)));
}


LVAL ivolerode()
{
  LVAL xlim1, xlim2, xlim3;
  int ox, oy, oz;

  if (!moreargs())
    xlabort("(*volerode im imse imweight &optional ox oy oz)");
/*
  \lspfunction{*}{volerode}{im imse imweight &optional ox oy oz}
  \param{im}{an image node to erode}
  \param{imse}{an image node for defining the support of the SE (type UCHAR)}
  \param{imweigth}{an image node for defining the heights of SE point (any type)}
  \param{ox}{integer for x-coordinate of origin of kernel (default equals integer part of half the number of columns of imse)}
  \param{oy}{integer for y-coordinate of origin of kernel (default equals integer part of half the number of lines of imse)}
  \param{oz}{integer for z-coordinate of origin of kernel(default equals integer part of half the number of x-y planes imse)}
  \return{the volumic erosion of im with the selected SE (FLOAT image)}
  \desc{performs the volumic erosion of im using the convolution kernel defined as follows: points of imse set to 1 define the definition domain of the kernel while the corresponding weigth is defined by the value in imweight at the same position.  Note that the kernel is not reflected so that *volerode actually returns the cross-correlation of im with the image defined by imse and imweight.}
  \cfunction{\cfvolerode}
  \cfile{erodil.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();
  if (moreargs()){
    ox = (int) getfixnum(xlgafixnum());
    oy = (int) getfixnum(xlgafixnum());
    oz = (int) getfixnum(xlgafixnum());
  }
  else{
    ox = GetImNx((IMAGE *)getimage(xlim2))/2;
    oy = GetImNy((IMAGE *)getimage(xlim2))/2;
    oz = GetImNz((IMAGE *)getimage(xlim2))/2;
  }
  xllastarg();

  return (cvimage(volerode((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2),  (IMAGE *)getimage(xlim3), ox, oy, oz)));
}

LVAL ierodelabel()
{
  LVAL xlim1;
  int graph;

  if (!moreargs())
    xlabort("(*erodelabel im graph)");
/*
  \lspfunction{*}{erodelabel}{im graph}
  \param{im}{an image node}
  \param{graph}{integer for connectivity}
  \return{an image node containing the union of the erosions of the iso-intensity connected components of im}
  \desc{p}
  \cfunction{\cferodelabel}
  \cfile{mmlabel.c}
*/
  xlim1     = xlgaimage();
  graph = (int)getfixnum(xlgafixnum());
    
  xllastarg();
  return(cvimage(erodelabel((IMAGE *)getimage(xlim1), graph)));
}


/*
  \lispsection
  \input{erodil.tex}
*/


/*
\module{Opening and closing}
\basesection
*/

LVAL iareaopen()
{
  LVAL xlim1;
  int lambda, graph;
  IMAGE *i0, *i1;
  
  if (!moreargs())
    xlabort("(*areaopen im lambda graph)");
/*
  \lspfunction{*}{areaopen}{im lambda graph}
  \param{im}{an image node}
  \param{lambda}{integer for number of pixels}
  \param{graph}{integer for connectivity (either 4 or 8)}
  \return{an image node}
  \desc{graph-connected area opening of im: all maxima of the input image are levelled until they contain at least lambda pixels.  Implementation based on algorithm described by \citet{meijster-wilkinson2002}.}
  \cfunction{\cfGreyAreaOpening}
  \cfile{uswilk.c}
*/
  xlim1 = xlgaimage();
  lambda = (int)getfixnum(xlgafixnum());
  if (!moreargs())
    graph =4;
  else
    graph=(int)getfixnum(xlgafixnum());

  xllastarg();

  if ((graph!=4) && (graph!=8))
     xlabort("(*areaopen im area graph) must be equal to 4 or 8");

  /* feature: only runs for unsigned short */
  if (GetImDataType((IMAGE *)getimage(xlim1)) == 3){
    i0 = (IMAGE *)to_ushort((IMAGE *)getimage(xlim1));
    if (i0 == NULL)
      xlabort("to_ushort in *areaclose returned NULL image pointer");
    i1 = (IMAGE *)GreyAreaOpening(i0, lambda, graph);
    free_image(i0);
    to_uchar(i1);
    return(cvimage(i1));
  }
  else if (GetImDataType((IMAGE *)getimage(xlim1)) == 5)
    return(cvimage(GreyAreaOpening((IMAGE *)getimage(xlim1), lambda, graph)));
  else
     xlabort("(*areaopen im lambda graph): im must be either UCHAR or USHORT");
  return NIL;
}

LVAL iareaclose()
{
  LVAL xlim1;
  int lambda, graph;
  IMAGE *i0, *i1;
  
  if (!moreargs())
    xlabort("(*areaclose im lambda graph)");
/*
  \lspfunction{*}{areaclose}{im lambda graph}
  \param{im}{an image node}
  \param{lambda}{integer for number of pixels}
  \param{graph}{integer for connectivity (either 4 or 8)}
  \return{an image node}
  \desc{graph-connected area closing of im: all image minima are filled in until they contain at least lambda pixels.  Implementation based on algorithm described by \citet{meijster-wilkinson2002}.}
  \cfunction{\cfGreyAreaClosing}
  \cfile{uswilk.c}
  \example{}{}
*/

  xlim1 = xlgaimage();
  lambda = (int)getfixnum(xlgafixnum());
  if (!moreargs())
    graph =4;
  else
    graph=(int)getfixnum(xlgafixnum());

  xllastarg();

  if ((graph!=4) && (graph!=8))
     xlabort("(*areaclose im area graph) must be equal to 4 or 8");

  /* feature: only runs for unsigned short */
  if (GetImDataType((IMAGE *)getimage(xlim1)) == 3){
    i0 = (IMAGE *)to_ushort((IMAGE *)getimage(xlim1));
    if (i0 == NULL)
      xlabort("to_ushort in *areaclose returned NULL image pointer");
    i1 = (IMAGE *)GreyAreaClosing(i0, lambda, graph);
    free_image(i0);
    to_uchar(i1);
    return(cvimage(i1));
  }
  else if (GetImDataType((IMAGE *)getimage(xlim1)) == 5)
    return(cvimage(GreyAreaClosing((IMAGE *)getimage(xlim1), lambda, graph)));
  else
    xlabort("(*areaclose im lambda graph): im must be either UCHAR or USHORT");
  return NIL;
}


LVAL iareaopenroi()
{
  LVAL xlim1;
  int lambda, graph;
  
  if (!moreargs())
    xlabort("(*areaopenroi im lambda graph)");
/*
  \lspfunction{*}{areaopenroi}{im lambda graph}
  \param{im}{an image node}
  \param{lambda}{integer for number of pixels}
  \param{graph}{integer for connectivity (either 4 or 8)}
  \return{an image node}
  \desc{BETA FUNCTION}
  \cfunction{\cfGreyAreaOpeningROI}
  \cfile{uswilk.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  lambda = (int)getfixnum(xlgafixnum());
  if (!moreargs())
    graph =4;
  else
    graph=(int)getfixnum(xlgafixnum());

  xllastarg();

  return(cvimage(GreyAreaOpeningROI((IMAGE *)getimage(xlim1), lambda, graph)));
}

LVAL iareacloseroi()
{
  LVAL xlim1;
  int lambda, graph;
  
  if (!moreargs())
    xlabort("(*areacloseroi im lambda graph)");
/*
  \lspfunction{*}{areacloseroi}{im lambda graph}
  \param{im}{an image node}
  \param{lambda}{integer for number of pixels}
  \param{graph}{integer for connectivity (either 4 or 8)}
  \return{an image node}
  \desc{BETA FUNCTION}
  \cfunction{\cfGreyAreaClosingROI}
  \cfile{uswilk.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  lambda = (int)getfixnum(xlgafixnum());
  if (!moreargs())
    graph =4;
  else
    graph=(int)getfixnum(xlgafixnum());

  xllastarg();

  return(cvimage(GreyAreaClosingROI((IMAGE *)getimage(xlim1), lambda, graph)));
}

LVAL iattribute()
{
  LVAL xlim1;
  int oporclo, type, graph;
  double lambda;
  
  if (!moreargs())
    xlabort("(*attribute im type 0(for opening)|1(for closing)  lamdba graph(4(default)|8)) type=0 for surface, 1 for inertia, 2 for area of enclosed rectange, 3 for diagonal of enclosed rectangle");

/*
  \lspfunction{*}{attribute}{im type lamdba graph}
  \param{}{}
  \return{}
  \desc{BETA FUNCTION}
  \cfunction{\cfattribute}
  \cfile{uswilk.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  type = (int)getfixnum(xlgafixnum());
  oporclo = (int)getfixnum(xlgafixnum());
  lambda = (DOUBLE) getflonum(xlgaflonum());
  if (!moreargs())
    graph =4;
  else
    graph=(int)getfixnum(xlgafixnum());

  xllastarg();

  return(cvimage(attribute((IMAGE *)getimage(xlim1), type, oporclo, lambda, graph)));
}

/* hpclose.c */
LVAL ihpclose()
{
  LVAL xlim1;
  int dx, dy;

  if (!moreargs())
    xlabort("(*hpclose im dx dy)");
/*
  \lspfunction{*}{hpclose}{im dx dy}
  \param{im}{an image node}
  \param{dx}{integer for defining slope}
  \param{dy}{integer for defining slope}
  \return{an image node}
  \desc{computes the half-plane closing of im with a plane of slope dy/dx.  Translation variant implementation.}
  \cfunction{\cfhpclose}
  \cfile{hull.c}
  \example{}{}
*/

  xlim1 = xlgaimage();
  dx    = (int) getfixnum(xlgafixnum());
  dy    = (int) getfixnum(xlgafixnum());

  xllastarg();

  return(cvimage(hpclose((IMAGE *)getimage(xlim1), dx, dy)));
}

LVAL ihpcloseti()
{
  LVAL xlim1;
  int dx, dy;

  if (!moreargs())
    xlabort("(*hpcloseti im dx dy)");

/*
  \lspfunction{*}{hpcloseti}{im dx dy}
  \param{im}{an image node}
  \param{dx}{integer for defining slope}
  \param{dy}{integer for defining slope}
  \return{an image node}
  \desc{computes the half-plane closing of im with a plane of slope dy/dx.  Translation invariant implementation.}
  \cfunction{\cfhpcloseti}
  \cfile{hullti.c}
  \example{}{}
*/

  xlim1 = xlgaimage();
  dx    = (int) getfixnum(xlgafixnum());
  dy    = (int) getfixnum(xlgafixnum());

  xllastarg();

  return(cvimage(hpcloseti((IMAGE *)getimage(xlim1), dx, dy)));
}

/*
  \lispsection
  \input{opclo.tex}
*/


/*
\module{Hit-or-miss and skeletons}
\basesection
*/

LVAL iepc()
{
  LVAL xlim1, xlim2;

  if (!moreargs())
    xlabort("(*epc im lutim)");
/*
  \lspfunction{*}{epc}{im lutim}
  \param{im}{an image node (binary image)}
  \param{lutim}{an image node for a 1-D binary image coding HMT configurations in a 3 times 3 neighbourhood}
  \return{an image node}
  \desc{extract all pixels matching the neighbourhood configurations coded by lutim. The lut image must given on/off codes using the following configuration coding scheme: \begin{tabular}{c}064 004 032\\ 008 001 002\\ 128 016 256\end{tabular}.  For example the code of an 8-isolated foreground pixel is 1 while that of an 8-isolated background point is 511.  That is, for extracting all 8-isolated pixels, lutim must have 0 values everywhere except at indices 1 and 511 where it should have a value of 1.}
  \cfunction{\cfepc}
  \cfile{test.c}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  
  xllastarg();

  return (cvimage(epc((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2))));
}

LVAL iskeleton()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(@skeleton im)");

/*
  \lspfunction{@}{skeleton}{im}
  \param{im}{an image node}
  \return{im}
  \desc{4-connected skeletonisation by order dependent sequential thinning.}
  \cfunction{\cfskeleton}
  \cfile{skelodthin.c}
  \example{}{}
*/
  xlim1 = xlgaimage();

  xllastarg();

  if (skeleton((IMAGE *)getimage(xlim1))==ERROR)
    return NIL;

  return(xlim1);
}

LVAL iprune()
{
  LVAL xlim1;
  int n, graph;

  if (!moreargs())
    xlabort("(@prune im n graph)");
/*
  \lspfunction{@}{prune}{im n graph}
  \param{im}{an image node (UCHAR)}
  \param{n}{integer for size of pruning}
  \param{graph}{integer (4 or 8) for connectivity}
  \return{im}
  \desc{prunes the image im by n pixels using graph-connectivity.}
  \cfunction{\cfbprune}
  \cfile{skelodthin.c}
  \example{}{}
*/


  xlim1 = xlgaimage();
  n = (int) getfixnum(xlgafixnum());
  graph = (int) getfixnum(xlgafixnum());

  xllastarg();

  if (GetImDataType( (IMAGE *)getimage(xlim1) ) != t_UCHAR ){
    xlabort("(@prune im n graph): im must be of type UCHAR");
  }

  if (bprune((IMAGE *)getimage(xlim1), n, graph)==ERROR)
    return NIL;

  return(xlim1);
}

LVAL ioiskeleton()
{
  LVAL xlim1, xlim2;
  
  if (!moreargs())
    xlabort("(@oiskeleton imin imanchor)");

/*
  \lspfunction{@}{oiskeleton}{imin imanchor}
  \param{imin}{an image node to skeletonise}
  \param{imanchor}{an image node with anchor points}
  \return{imin}
  \desc{binary order independent anchor skeleton \citep{ranwez-soille2002}.}
  \cfunction{\cfoiskeleton}
  \cfile{oiht.c}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();

  xllastarg();

  if (oiskeleton((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2))==ERROR)
    return NIL;
  return(xlim1);
  
}

LVAL ioiask()
{
  LVAL xlim1, xlim2;

  if (!moreargs())
    xlabort("(@oiask imin imanchor)");

/*
  \lspfunction{@}{oiask}{imin imanchor}
  \param{imin}{an image node to skeletonise}
  \param{imanchor}{an image node with anchor points}
  \return{imin}
  \desc{grey tone order independent skeleton following~\cite{ranwez-soille2002}.}
  \cfunction{\cfoiask}
  \cfile{oiht.c}
  \example{}{}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();

  xllastarg();

  if (oiask((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2))==ERROR)
    return NIL;
  return(xlim1);
}


/********************** BINARY SKELETONISATION ***************/

LVAL ibinskelOD ()
{
  LVAL xlim1; 
  int type, alg;
  if (!moreargs())  xlabort("(@binskelOD imin type &opt (alg))");
/*
  \lspfunction{@}{binskelOD}{imin type &opt (alg)}
  \param{imin}{input (binary shape) and output (skeleton) image}
  \param{type}{type of skeletonisation}
  \param{alg}{type of algorithm}
  \return{imin}
  \desc{Computes order dependent skeleton of imin. The image 'imin' is a binary image (with 0 and 1 values). The variable 'type' controls the type of pixels which are removed during the thinning process: 0 - 8-simple; 1 - 4-simple; 2 - (4,8)-simple; 3 - 8-b-simple; 4 - 4-b-simple; 5 - (4,8)-b-simple. The varaible 'arg' stands for the type of algorithm used: 0 - fast queue based (default); 1 - classical (slow, multiple image scannings).}
  \cfunction{\cfbinODthinUDFIFO}
  \cfile{skel.c}
  \authors{Marcin Iwanowski}
*/

  xlim1 = xlgaimage();
  type = (int)getfixnum(xlgafixnum());
  if (type < 0) type = 0;
  if (type > 5) type = 5;
  if (!moreargs()) alg = 0;  
  else alg = (int)getfixnum(xlgafixnum());
  xllastarg();
  if (alg == 0) 
   {if (binODthin_FIFO((IMAGE *)getimage(xlim1), type, 0, NULL) == ERROR) return NIL;}
  else 
   {if (binODthin_noqueue((IMAGE *)getimage(xlim1), type, 0, NULL) == ERROR) return NIL;}  
  return(xlim1); 
}

LVAL ibinskelOI ()
{
  LVAL xlim1; 
  int type, alg;
  if (!moreargs())  xlabort("(@binskelOI imin type &opt (alg))");
/*
  \lspfunction{@}{binskelOI}{imin type &opt (alg)}
  \param{imin}{input (binary shape) and output (skeleton) image}
  \param{type}{type of skeletonisation}
  \param{alg}{type of algorithm}
  \return{imin}
  \desc{Computes order independent skeleton of 'imin'. The image 'imin' is a binary image (with 0 and 1 values). The variable 'type' controls the type of pixels which are removed during the thinning process: 0 - 8-simple; 1 - 4-simple; 2 - (4,8)-simple; 3 - 8-b-simple; 4 - 4-b-simple; 5 - (4,8)-b-simple. The variable 'arg' stands for the type of algorithm used: 0 - fast queue based (default); 1 - classical (slow, multiple image scannings).}
  \cfunction{\cfbinOIthinUDFIFO(binOIthin_noqueue)}
  \cfile{skel.c}
  \authors{Marcin Iwanowski}
*/
  xlim1 = xlgaimage();
  type = (int)getfixnum(xlgafixnum());
  if (type < 0) type = 0;
  if (type > 5) type = 5;
  if (!moreargs()) alg = 0; 
  else alg = (int)getfixnum(xlgafixnum());
  xllastarg();
  if (alg == 0) 
   {if (binOIthin_FIFO( (IMAGE *)getimage(xlim1), type, 0, NULL) == ERROR) return NIL;}
  else 
   {if (binOIthin_noqueue( (IMAGE *)getimage(xlim1), type, 0, NULL) == ERROR) return NIL;}  
  return(xlim1); 
}

LVAL ibinanchorskelOD ()
{
  LVAL xlim1,xlim2; 
  int type, alg;
  if (!moreargs())  xlabort("(@binanchorskelOD imin imanchor type &opt (alg))");
/*
  \lspfunction{@}{binanchorskelOD}{imin imanchor type &opt (alg)}
  \param{imin}{input (binary shape) and output (skeleton) image}
  \param{imanchor}{input anchor image (1 - anchor pixel, 0 - otherwise)}
  \param{type}{type of skeletonisation}
  \param{alg}{type of algorithm}
  \return{imin}
  \desc{Computes order dependent anchored skeleton of 'imin'. The image 'imin'is a binary image (with 0 and 1 values). Variable 'type' controls the type of pixels which are removed during the thinning process: 0 - 8-simple; 1 - 4-simple; 2 - (4,8)-simple; 3 - 8-b-simple; 4 - 4-b-simple; 5 - (4,8)-b-simple. The variable 'arg' stands for the type of algorithm used: 0 - fast queue based (default); 1 - classical (slow, multiple image scannings).}
  \cfunction{\cfbinODthinUDFIFO(binODthin_noqueue)}
  \cfile{skel.c}
  \authors{Marcin Iwanowski}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  type = (int)getfixnum(xlgafixnum());
  if (type < 0) type = 0;
  if (type > 5) type = 5;
  if (!moreargs()) alg = 0; 
   else alg = (int)getfixnum(xlgafixnum());
  xllastarg();
  if (alg == 0) 
   {if (binODthin_FIFO( (IMAGE *)getimage(xlim1), type, 1, (IMAGE *)getimage(xlim2)) == ERROR) return NIL;}
  else 
   {if (binODthin_noqueue( (IMAGE *)getimage(xlim1), type, 1, (IMAGE *)getimage(xlim2)) == ERROR) return NIL;}  
  return(xlim1); 
}

LVAL ibinanchorskelOI ()
{
  LVAL xlim1,xlim2; 
  int type, alg;
  if (!moreargs())  xlabort("(@binanchorskelOI imin imanchor type &opt (alg))");
/*
  \lspfunction{@}{binanchorskelOI}{imin imanchor type &opt (alg)}
  \param{imin}{input (binary shape) and output (skeleton) image}
  \param{imanchor}{input anchor image (1 - anchor pixel, 0 - otherwise)}
  \param{type}{type of skeletonisation}
  \param{alg}{type of algorithm}
  \return{imin}
  \desc{Computes order independent anchored skeleton of imin. Image 'imin' is a binary image (with 0 and 1 values). The variable 'type' controls the type of pixels which are removed during the thinning process: 0 - 8-simple; 1 - 4-simple; 2 - (4,8)-simple; 3 - 8-b-simple; 4 - 4-b-simple; 5 - (4,8)-b-simple. The variable 'arg' stands for the type of algorithm used: 0 - fast queue based (default); 1 - classical (slow, multiple image scannings).}
  \cfunction{\cfbinOIthinUDFIFO(binOIthin_noqueue)}
  \cfile{skel.c}
  \authors{Marcin Iwanowski}
*/
  xlim1 = xlgaimage();  
  xlim2 = xlgaimage();
  type = (int)getfixnum(xlgafixnum());
  if (type < 0) type = 0;
  if (type > 5) type = 5;
  if (!moreargs()) alg = 0; 
   else alg = (int)getfixnum(xlgafixnum());
  xllastarg();
  if (alg == 0) 
   {if (binOIthin_FIFO( (IMAGE *)getimage(xlim1), type, 1, (IMAGE *)getimage(xlim2)) == ERROR) return NIL;}
  else 
   {if (binOIthin_noqueue( (IMAGE *)getimage(xlim1), type, 1, (IMAGE *)getimage(xlim2)) == ERROR) return NIL;}  
  return(xlim1); 
}

/********************* END OF BINARY SKELETONISATION  **********************/


/*
  \lispsection
  \input{hmt.tex}
*/



/*
\module{Geodesic transformations}
\basesection
*/

#ifdef ALLFUNCTIONS
/* recons.c */
LVAL irero()
{
  LVAL xlim1, xlim2;
  int graph, flag=1;

  if (!moreargs())
    xlabort("(@rero mark mask graph &opt (flag 1))");
/*
  \lspfunction{@}{rero}{mark mask graph &opt (flag 1)}
  \param{mark}{image node for marker}
  \param{mask}{image node for mask}
  \param{graph}{integer for connectivity}
  \param{flag}{integer for flag (border values are set to PIX_MAX in BOTH images if flag equals 0, otherwise the image are internally processed by adding a border which is then removed at the end of the processing).  Default value is 1.}
  \return{mark}
  \desc{graph-connected reconstruction by erosion of mask from marker mark.}
  \cfunction{\cfrero}
  \cfile{recons.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  graph = (int) getfixnum(xlgafixnum());
  if (moreargs())
    flag = (int) getfixnum(xlgafixnum());

  xllastarg();

  if (rero((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), graph, flag)==ERROR)
    return NIL;

  return(xlim1);
}
LVAL irdil()
{
  LVAL xlim1, xlim2;
  int graph, flag=1;

  if (!moreargs())
    xlabort("(@rdil mark mask graph &opt (flag 1))");
/*
  \lspfunction{@}{rdil}{mark mask graph &opt (flag 1)}
  \param{mark}{image node for marker}
  \param{mask}{image node for mask}
  \param{graph}{integer for connectivity}
  \param{flag}{integer for flag (border values are set to PIX_MIN in BOTH images if flag equals 0, otherwise the image are internally processed by adding a border which is then removed at the end of the processing).  Default value is 1.}
  \return{mark}
  \desc{graph-connected reconstruction by dilation of mask from marker mark.}
  \cfunction{\cfrdil)}
  \cfile{recons.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  graph = (int) getfixnum(xlgafixnum());
  if (moreargs())
    flag = (int) getfixnum(xlgafixnum());

  xllastarg();

  if (rdil((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), graph, flag)==ERROR)
    return NIL;

  return(xlim1);
}
LVAL irerodilp()
{
  LVAL xlim1, xlim2;
  int graph, flag=1;

  if (!moreargs())
    xlabort("(@rerodilp mark mask graph &opt (flag 1))");
/*
  \lspfunction{@}{rerodilp}{mark mask graph &opt (flag 1) (version 1)}
  \param{mark}{image node for marker}
  \param{mask}{image node for mask}
  \param{graph}{integer for connectivity}
  \param{flag}{integer for flag (border values are set to PIX_MIN in BOTH images if flag equals 0, otherwise the image are internally processed by adding a border which is then removed at the end of the processing).  Default value is 1.}
  \param{flag}{integer for version number: 1 for using a single queue, 2 for using two queues, 3 for using 2 priority queues, and 4 for a version based on a version nased on an extension of the downhill version (default is 1).}
  \return{mark}
  \desc{graph-connected self-dual reconstruction mask from marker mark using alternative definition (no jump over from adjacent markers).}
  \cfunction{\cfrdil)}
  \cfile{recons.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  graph = (int) getfixnum(xlgafixnum());
  if (moreargs())
    flag = (int) getfixnum(xlgafixnum());

  xllastarg();

  if (rerodilp((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), graph, 1, flag)==ERROR)
    return NIL;

  return(xlim1);
}
#endif


LVAL iminima()
{
  LVAL xlim1;
  int graph;

  if (!moreargs())
    xlabort("(*minima im graph)");
/*
  \lspfunction{*}{minima}{im graph}
  \param{im}{an image node}
  \param{graph}{integer for connectivity}
  \return{an image node of type t_UCHAR and regional minima set to 1}
  \desc{ouputs a binary image with regional minima of im set to 1, all other pixels being set to 0.  Contrary to *rmin, *minima is based on a fast dedicated algorithm due to \citet{breen-jones96a} and also described on page 203 of \citep{soille2003sv}.}
  \myseealso{\htmlref{*rmin}{*rmin}}
  \cfunction{\cfminima}
  \cfile{rminmax.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  graph = (int) getfixnum(xlgafixnum());

  xllastarg();

  return (cvimage(minima((IMAGE *)getimage(xlim1), graph)));
}

LVAL iimtoarray()
{
  LVAL xlim1, xlim2;
  if (!moreargs())
    xlabort("(*imtoarray im imroi)\n");
/*
  \lspfunction{*}{imtoarray}{im imroi}
  \param{im}{an image node}
  \param{imroi}{an image node of type UCHAR and same size as im1 indicating the region of interest (values > 0)}
  \return{a new image corresponding to a 1D array containing the data values of im1 that falls in imroi.}
  \desc{}
  \cfunction{\cfimtoarray}
  \cfile{gsl.c}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xllastarg();
  return(cvimage(imtoarray((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2))));
}

LVAL iarraytoim()
{
  LVAL xlim1, xlim2;
  if (!moreargs())
    xlabort("(*arraytoim im imroi)\n");
/*
  \lspfunction{*}{arraytoim}{im imroi}
  \param{im}{an image nodee}
  \param{imroi}{an image node of type UCHAR indicating the region of interest (values > 0) and used intially when calling *imtoarray}
  \return{a new image corresponding to the inverse transformation of *imtoarray.}
  \desc{}
  \cfunction{\cfarraytoim}
  \cfile{gsl.c}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xllastarg();
  return(cvimage(arraytoim((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2))));
}

LVAL igeodist()
{
  LVAL xlim1, xlim2;
  int graph;

  if (!moreargs())
    xlabort("(@geodist mask marker graph)");
/*
  \lspfunction{@}{geodist}{mask marker graph}
  \param{mask}{an image node for geodesic mask}
  \param{marker}{an image node for marker image}
  \param{graph}{integer for connectivity}
  \return{mask}
  \desc{geodesic distance function from marker within mask using graph connectivity.}
  \cfunction{\cfgeodist}
  \cfile{geodist.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  graph = (int) getfixnum(xlgafixnum());

  xllastarg();

  if (geodist((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2),graph)==ERROR)
    return NIL;
  return(xlim1);
}

LVAL iced()
{
  LVAL xlim1, xlim2;

  if (!moreargs())
    xlabort("(*ced marker mask)");
/*
  \lspfunction{*}{ced}{marker mask}
  \param{marker}{binary UCHAR image with reference (i.e., marker) pixels set to 1}
  \param{mask}{binary UCHAR image with geodesic mask pixels set to 1}
  \return{an image node of type t_FLOAT}
  \desc{computes the  Euclidean geodesic distance from the marker set defined by the image marker and within the geodesic mask defined by the image mask.  The algorithm is described in \citep{soille91}.}
  \cfunction{\cfced}
  \cfile{geodist.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  /*graph = (int) getfixnum(xlgafixnum());*/

  xllastarg();

  return(cvimage(ced((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2))));
}

#ifdef ALLFUNCTIONS
/* complete.c */
LVAL icomplete()
{
  LVAL xlim1, xlim2;
  int graph;

  if (!moreargs())
    xlabort("(@complete imi (type ushort or long) im_rmin (type uchar) graph");
/*
  \lspfunction{@}{complete}{imi im_rmin graph}
  \param{imi}{image to complete (ushort or long)}
  \param{im_rmin}{UCHAR image with regional minima at value 1, all other pixels at 0}
  \param{graph}{integer for connectivity (either 4 or 8)}
  \return{imi}
  \desc{performs the lower complete transformation of imi using graph-connectivity for geodesic distance calculations on plateaus.}
  \cfunction{\cfcomplete}
  \cfile{complete.c}
  \example{}{}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  graph = (int)getfixnum(xlgafixnum());

  xllastarg();

  /* if (GetImDataType( (IMAGE *)getimage(xlim1) ) != t_INT32 ){
    xlabort("(@complete imi im_rmin graph: imi must be of type INT32");
    } */
  if (GetImDataType( (IMAGE *)getimage(xlim2) ) != t_UCHAR ){
    xlabort("(@complete imi im_rmin graph: im_rmin must be of type UCHAR");
  }

  if (complete((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), graph)==ERROR)
    return NIL;

  return(xlim1);
}
#endif
LVAL isqtg()
{
  LVAL xlim1, xlim2;
  int graph;

  if (!moreargs())
    xlabort("(@sqtg mask mark graph)");
/*
  \lspfunction{@}{sqtg}{mask mark graph}
  \param{mask}{image node for geodesic mask}
  \param{marker}{image node for marker (UCHAR)}
  \return{mask}
  \desc{computes the graph-connected geodesic time function from binary markers mark within the grey tone geodesic mask mask (marker pixels should be set to 1, ROI pixels to 0, all other pixels to 3).  Due to the integration nature of this function, overflows may occur very quickly so that it is usually necessary to consider a mask image of type USHORT or even INT32.  Pixels which are not reached are set to 0.}
  \feature{Border pixels of the marker image mark are set to zero by this procedure.}
  \cfunction{\cfsqtg}
  \cfile{ggeo.c}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  graph = (int) getfixnum(xlgafixnum());

  xllastarg();

  if (sqtg((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), graph)==ERROR)
    return NIL;

  return(xlim1);
}

LVAL isqtgsym()
{
  LVAL xlim1, xlim2;
  int graph;

  if (!moreargs())
    xlabort("(*sqtgsym mask mark graph)");
/*
  \lspfunction{*}{sqtgsym}{mask mark graph}
  \param{mask}{image node for geodesic mask}
  \param{marker}{image node for marker (UCHAR)}
  \return{a new image with computed time function using sum of node values for each travelled edge}
  \desc{computes the graph-connected geodesic time function from binary markers mark within the grey tone geodesic mask mask (marker pixels should be set to 1, ROI pixels to 0, all other pixels to 3).  Due to the integration nature of this function, overflows may occur very quickly so that it is usually necessary to consider a mask image of type USHORT or even INT32.  Pixels which are not reached are set to 0.}
  \feature{Border pixels of the marker image mark are set to zero by this procedure.}
  \cfunction{\cfsqtg}
  \cfile{ggeo.c}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  graph = (int) getfixnum(xlgafixnum());

  xllastarg();

  return(cvimage(sqtgsym((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), graph)));
}

LVAL isqtgpla()
{
  LVAL xlim1, xlim2;
  int graph;

  if (!moreargs())
    xlabort("(@sqtgpla mask mark graph)");
/*
  \lspfunction{@}{sqtgpla}{mask mark graph}
  \param{mask}{image node for geodesic mask}
  \param{marker}{image node for marker (UCHAR)}
  \return{mask}
  \return{}
  \desc{}
  \cfunction{\cfsqtgpla}
  \cfile{ggeo.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  graph = (int) getfixnum(xlgafixnum());

  xllastarg();

  if (sqtgpla((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), graph)==ERROR)
    return NIL;

  return(xlim1);
}

#ifdef ALLFUNCTIONS
/* wsfah.c */
LVAL iskelfah()
{
  LVAL xlim1, xlim2, xlim3;
  int graph, maxfl;

  if (!moreargs())
    xlabort("(@skelfah imlabel imref imdir graph maxfl)");

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();
  graph = (int) getfixnum(xlgafixnum());
  maxfl = (int) getfixnum(xlgafixnum());

  xllastarg();

  if(skelfah((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), (IMAGE *)getimage(xlim3), graph, maxfl)==ERROR)
    return NIL;
  return (xlim1);
}

LVAL iskelfah2()
{
  LVAL xlim1, xlim2;
  int graph, n;

  if (!moreargs())
    xlabort("(@skelfah2 impskp imcomplete  n graph)");

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  n     = (int) getfixnum(xlgafixnum());
  graph = (int) getfixnum(xlgafixnum());

  xllastarg();

  if(skelfah2((IMAGE *)getimage(xlim2), (IMAGE *)getimage(xlim1), n, graph)==ERROR)
    return NIL;
  return (xlim1);
}

#ifdef COLLET
LVAL inrms()
{
  LVAL xlim1;
  int graph;

  if (!moreargs())
    xlabort("(*nmrs im graph)");

  xlim1 = xlgaimage();
  graph = (int) getfixnum(xlgafixnum());
  
  xllastarg();

  return(cvimage(nrms((IMAGE *)getimage(xlim1), graph)));
}
#endif


#ifdef TESTING
#ifdef ALLFUNCTIONS
LVAL iswitchop()
{
  LVAL xlim1, xlim2;
  int ox, oy, oz;
  
  if (!moreargs())
    xlabort("(*switchop im imse ox oy oz");

/*
  \lspfunction{*}{switchop}{im imse ox oy oz}
  \param{im}{an image node}
  \param{imse}{an image node for the SE}
  \param{ox}{\descox}
  \param{oy}{\descoy}
  \param{oz}{\descoz}
  \return{an image node}
  \desc{switch operator.}
  \cfunction{\cfswitchop}
  \cfile{switch.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  ox = (int) getfixnum(xlgafixnum());
  oy = (int) getfixnum(xlgafixnum());
  if (!moreargs())
    oz = 0;
  else
    oz = (int) getfixnum(xlgafixnum());

  xllastarg();

  return(cvimage(switchop((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), ox, oy, oz)));
  
}

LVAL ioiiz()
{
  LVAL xlim1;
  
  if (!moreargs())
    xlabort("(@oiiz imin)");
/*
  \lspfunction{@}{oiiz}{im}
  \param{im}{an image node (must be UCHAR)}
  \return{im}
  \desc{performs the order independent influence zone of im, using 8-connectivity for the background and 4-connectivity for the foreground.}
  \cfunction{\cfoiiz}
  \cfile{oiiz.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xllastarg();
  if (oiiz((IMAGE *)getimage(xlim1))==ERROR)
    return NIL;
  return(xlim1);
}

LVAL ioiws()
{
  LVAL xlim1;
  
  if (!moreargs())
    xlabort("(@oiws imin)");

/*
  \lspfunction{@}{oiws}{im}
  \param{im}{an image node (must be UCHAR)}
  \return{im}
  \desc{This is an experimental function (not functional).  Shall perform the order independent watershed of im, using 8-connectivity for the background and 4-connectivity for the foreground.}
  \cfunction{\cfoiws}
  \cfile{oiiz.c}
  \example{}{}
*/

  xlim1 = xlgaimage();
  xllastarg();
  if (oiws((IMAGE *)getimage(xlim1))==ERROR)
    return NIL;
  return(xlim1);
}

LVAL imapori()
{
  LVAL xlim1;
  int ox, oy;

  if (!moreargs())
    xlabort("(@mapori im ox oy)");

/*
  \lspfunction{@}{mapori}{im ox oy}
  \param{im}{an image node}
  \param{ox}{integer for x-coordinate}
  \param{oy}{integer for y-coordinate}
  \return{im}
  \desc{Sets each pixel of the input image i0 to its orientation with respect to an origin with pixel coordinates (ox,oy).  The values are given in degrees rescaled according to the data type of the input image.}
  \cfunction{\cfoiws}
  \cfile{oiiz.c}
  \example{}{}
*/

  xlim1 = xlgaimage();
  ox = (int) getfixnum(xlgafixnum());
  oy = (int) getfixnum(xlgafixnum());

  xllastarg();

  if (mapori((IMAGE *)getimage(xlim1), ox, oy)==ERROR)
    return NIL;

  return(xlim1);
}

/* recons.c */

LVAL iFlatIGeodAFAB()
{
  LVAL xlim1, xlim2;
  int graph;

  if (!moreargs())
    xlabort("(@FlatIGeodAFAB flat im graph)");
/*
  \lspfunction{@}{FlatIGeodAFAB}{flat im graph}
  \param{flat}{}
  \param{im}{}
  \param{graph}{integer for connectivity}
  \return{flat}
  \desc{Inverse geodesic distance Away From Ascending Border}
  \cfunction{\cfFlatIGeodAFAB}
  \cfile{flatdir.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  graph = (int) getfixnum(xlgafixnum());

  xllastarg();

  if (FlatIGeodAFAB((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), graph)==ERROR)
    return NIL;

  return(xlim1);
}
#endif




/*
  \lispsection
  \input{geodesy.tex}
*/


/*
\module{Filtering}
%\basesection
*/


/*
  \lispsection
  \input{filter.tex}
*/




/*
\module{Segmentation}
\basesection
*/


/* wshed.c */
LVAL iws()
{
  LVAL xlim1;
  int graph;

  if (!moreargs())
    xlabort("(*ws im graph)");

/*
  \lspfunction{*}{ws}{im graph}
  \param{im}{an image node}
  \param{graph}{integer for connectivity in \{4,8\} for 2-D images and \{6,18,26\} for 3-D images}
  \return{an image node}
  \desc{watershed segmentation of im using graph connectivity.  Returns a t_UCHAR image with watershed pixels set to 1 and catchment basin pixels set to 0.  Programme based on pseudo-code described in \citep{soille-vincent90} and relying on a distributive sorting followed by an ordered immersion simulation enabled by a FIFO queue data structure.}
  \cfunction{\cfws}
  \cfile{wshed.c}
  \example{}{}
*/

  xlim1 = xlgaimage();
  graph = (int) getfixnum(xlgafixnum());

  xllastarg();

  return(cvimage(ws((IMAGE *)getimage(xlim1), graph)));
}
#endif

LVAL iwsfah()
{
  LVAL xlim1, xlim2;
  int graph, maxfl;

  if (!moreargs())
    xlabort("(@wsfah imlabel imref graph maxfl)");
/*
  \lspfunction{@}{wsfah}{imlabel imref graph maxfl}
  \param{imlabel}{image node for labelled regions to initiate the flooding simulation}
  \param{imref}{image node to be flooded}
  \param{graph}{integer for connectivity}
  \param{maxfl}{integer for highest flooding level}
  \return{imlabel}
  \desc{watersheds using a propagation from a set of labelled markers, the lower the grey values of the mask are, the faster the propagation is (implementation through a priority queue).  Note that the border of the labelled image is set to zero by this function so that labelled markers lying along the image border are not taken into account.}
  \cfunction{\cfwsfah}
  \cfile{wsfah.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  graph = (int) getfixnum(xlgafixnum());
  maxfl = (int) getfixnum(xlgafixnum());

  xllastarg();

  if(wsfah((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), graph, maxfl)==ERROR)
    return NIL;
  return (xlim1);
}

LVAL isrg()
{
  LVAL xlim1, xlim2, xlim3;
  int ox, oy, oz;

  if (!moreargs())
    xlabort("(@srg im1 imseeds imse ox oy oz)\n");
/*
  \lspfunction{@}{srg}{im1 imseeds imse &optional ox oy oz}
  \param{im1}{an image node}
  \param{imseeds}{an t_UCHAR image node with seeds at value 1, all other pixels with value 0}
  \param{imse}{an image node for defining neighbours}
  \param{ox}{x coordinate (default equals integer part of half the width of imse)}
  \param{oy}{y coordinate (default equals integer part of half the height of imse)}
  \param{oz}{z coordinate (default equals integer part of half the number of x-y planes of imse)}
  \return{im1}
  \desc{performs a seeded region growing of im1 using the seeds stored in imseeds and the connectivity defined by imse and the coordinates of its origin.  Both im1 and imseeds are modified by this function.  At the end of the procedure, im1 is simplified in such a way that the grey values that occurred within the regions defined by the seeds are expanded until the whole image definition domain is covered.  Simultaneously, the image of seeds is modified by expanding the seeds until they cover the whole image definition domain.  The growth process is modulated by the grey level differences occurring in im1 along the boundary of the grown seeds.}
  \cfunction{\cfsrg}
  \cfile{srg.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();
  if (moreargs()){
    ox        = (int)getfixnum(xlgafixnum());
    oy        = (int)getfixnum(xlgafixnum());
    oz        = (int)getfixnum(xlgafixnum());
  }
  else{
    ox = GetImNx((IMAGE *)getimage(xlim3))/2;
    oy = GetImNy((IMAGE *)getimage(xlim3))/2;
    oz = GetImNz((IMAGE *)getimage(xlim3))/2;
  }

  xllastarg();

  if (srg((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2),(IMAGE *)getimage(xlim3), ox, oy, oz)==ERROR)
    return NIL;

  return(xlim1);
}

LVAL imssrg()
{
  LVAL xlim1, xlim2, xlim3, xlim4, xlim5, xlim6, xlim7, xlim8, xlim9, xlim10; /* maximum of 10 bands */
  LVAL xlimseeds, xlimse;
  int ox, oy, oz;
  IMAGE *imarray[maxNumberOfBands];
  int n=0;

  if (!moreargs())
    xlabort("(@mssrg imseeds imse ox oy oz im1 [im2] ... [im255])");

/*
  \lspfunction{@}{mssrg}{imseeds imse ox oy oz im1 [im2] ... [im255]}
  \param{imseeds}{an image node for labelled seeds (labels must be larger or equal to 4, 0 for all other pixels) (type UCHAR)}
  \param{imse}{an image node for neighbouring pixels}
  \param{ox}{\descox}
  \param{oy}{\descoy}
  \param{oz}{\descoz}
  \param{im1}{an image node for 1st channel}
  \param{imi}{an image node for ith channel (up to 255 channels)}
  \return{im1}
  \desc{a seeded region algorithm whereby labelled seeds (imseeds) are grown in a multi-channel image (up to channels) using a neighbourhood defined by imse with origin at (ox,oy,oz).  Distances are calculated between pixels along the external boundary of the already grown regions and the corresponding pixels along the internal boundary.  Both im1 and imseeds are modified by this function.  The image of seeds is modified by expanding the corresponding initial values of the seeds.}
  \cfunction{\cfmssrg}
  \cfile{srg.c}
  \example{}{}
*/
  xlimseeds = xlgaimage();
  xlimse    = xlgaimage();
  ox    = (int)getfixnum(xlgafixnum());
  oy    = (int)getfixnum(xlgafixnum());
  oz    = (int)getfixnum(xlgafixnum());

  xlim1 = xlgaimage();
  imarray[0]=(IMAGE *)getimage(xlim1);
  n++;
  if (moreargs()){
    xlim2 = xlgaimage();
    imarray[1]=(IMAGE *)getimage(xlim2);
    n++;
    if (moreargs()){
      xlim3 = xlgaimage();
      imarray[2]=(IMAGE *)getimage(xlim3);
      n++;
      if (moreargs()){
        xlim4 = xlgaimage();
        imarray[3]=(IMAGE *)getimage(xlim4);
	n++;
        if (moreargs()){
          xlim5 = xlgaimage();
          imarray[4]=(IMAGE *)getimage(xlim5);
	  n++;
          if (moreargs()){
            xlim6 = xlgaimage();
            imarray[5]=(IMAGE *)getimage(xlim6);
	    n++;
            if (moreargs()){
	      xlim7 = xlgaimage();
	      imarray[6]=(IMAGE *)getimage(xlim7);
	      n++;
	      if (moreargs()){
		xlim8 = xlgaimage();
		imarray[7]=(IMAGE *)getimage(xlim8);
		n++;
		if (moreargs()){
		  xlim9 = xlgaimage();
		  imarray[8]=(IMAGE *)getimage(xlim9);
		  n++;
		  if (moreargs()){
		    xlim10 = xlgaimage();
		    imarray[9]=(IMAGE *)getimage(xlim10);
		    n++;
		  }
		}
	      }
	    }
	  }
	}
      }
    }
  }

  xllastarg();

  if (mssrg(imarray, n, (IMAGE *)getimage(xlimseeds), (IMAGE *)getimage(xlimse), ox, oy, oz)==ERROR)
    return NIL;

  return(xlim1);
}

LVAL imssrgcore()
{
  LVAL xlim1, xlim2, xlim3, xlim4, xlim5, xlim6, xlim7, xlim8, xlim9, xlim10; /* maximum of 10 bands */
  LVAL xlimseeds, xlimse;
  int ox, oy, oz;
  IMAGE *imarray[maxNumberOfBands];
  int n=0;

  if (!moreargs())
    xlabort("(@mssrgcore imseeds imse ox oy oz im1 [im2] ... [im10])");
/*
  \lspfunction{@}{mssrgcore}{imseeds imse ox oy oz im1 [im2] ... [im10]}
  \param{imseeds}{an image node for labelled seeds (type UCHAR)}
  \param{imse}{an image node for neighbouring pixels}
  \param{ox}{\descox}
  \param{oy}{\descoy}
  \param{oz}{\descoz}
  \param{im1}{an image node for 1st channel}
  \param{imi}{an image node for ith channel (maximum of 255 channels)}
  \return{im1}
  \desc{a seeded region algorithm whereby labelled seeds (imseeds) are grown in a multi-channel image (up to 255 channels) using a neighbourhood defined by imse with origin at (ox,oy,oz).  Distances are calculated between pixels along the external boundary of the already grown regions and the pixels of the original seeds which grew up to the internal boundary of these already grown regions.  Both im1 and imseeds are modified by this function.  The image of seeds is modified by expanding the corresponding initial values of the seeds.}
  \cfunction{\cfmssrgcore}
  \cfile{srg.c}
  \example{}{}
*/
  xlimseeds = xlgaimage();
  xlimse    = xlgaimage();
  ox    = (int)getfixnum(xlgafixnum());
  oy    = (int)getfixnum(xlgafixnum());
  oz    = (int)getfixnum(xlgafixnum());

  xlim1 = xlgaimage();
  imarray[0]=(IMAGE *)getimage(xlim1);
  n++;
  if (moreargs()){
    xlim2 = xlgaimage();
    imarray[1]=(IMAGE *)getimage(xlim2);
    n++;
    if (moreargs()){
      xlim3 = xlgaimage();
      imarray[2]=(IMAGE *)getimage(xlim3);
      n++;
      if (moreargs()){
        xlim4 = xlgaimage();
        imarray[3]=(IMAGE *)getimage(xlim4);
	n++;
        if (moreargs()){
          xlim5 = xlgaimage();
          imarray[4]=(IMAGE *)getimage(xlim5);
	  n++;
          if (moreargs()){
            xlim6 = xlgaimage();
            imarray[5]=(IMAGE *)getimage(xlim6);
	    n++;
            if (moreargs()){
	      xlim7 = xlgaimage();
	      imarray[6]=(IMAGE *)getimage(xlim7);
	      n++;
	      if (moreargs()){
		xlim8 = xlgaimage();
		imarray[7]=(IMAGE *)getimage(xlim8);
		n++;
		if (moreargs()){
		  xlim9 = xlgaimage();
		  imarray[8]=(IMAGE *)getimage(xlim9);
		  n++;
		  if (moreargs()){
		    xlim10 = xlgaimage();
		    imarray[9]=(IMAGE *)getimage(xlim10);
		    n++;
		  }
		}
	      }
	    }
	  }
	}
      }
    }
  }

  xllastarg();

  if (mssrgcore(imarray, n, (IMAGE *)getimage(xlimseeds), (IMAGE *)getimage(xlimse), ox, oy, oz)==ERROR)
    return NIL;

  return(xlim1);
}

#ifdef MCISRG
LVAL ithresholdRegion_Size()
{
  LVAL arg;
  IMAGE *im=NULL;
  long int size;

  if (!moreargs())
    xlabort("(@thresholdregionsize im threshold)");

/*
  \lspfunction{@}{thresholdregionsize}{im size}
  \param{im}{an image node}
  \param{size}{region size must be bigger than this threshold value. Otherwise the values of the pixels of this region are set to 0}
  \return{im1}
  \desc{Algorithm to determine the size of the regions. If the thresholdvalue is 0, then the values of the pixels in the image are replaced by the corresponding size values of the region the pixel belongs to.  Otherwise the region gets an increasing region number if the size is bigger then the threshold.}
  %\cfunction{\cfthresholdRegionUDSize}
  \cfile{dominik/determineSize.c}
  \example{}{}
*/
  arg=xlgetarg();
  if (imagep(arg)){
    im=(IMAGE *)getimage(arg);
  }
  else
    xlabort("(@thresholdregionsize im threshold)");
  size = (int)getfixnum(xlgafixnum());
  xllastarg();
  if (thresholdRegion_Size(im, size)==ERROR)
    return NIL;

  return(arg);
}

LVAL imciasrg()
{
  LVAL arg;
  int graph=4, variance, contrast, version;
  long int area;
  IMAGE *imarray[maxNumberOfBands];
  int n=0;
  char *datfn=NULL;

  if (!moreargs())
    xlabort("(@mciasrg im1 [im2] ... [im255] graph variance area contrast version &opt datfn)");

/*
  \lspfunction{@}{mciasrg}{im1 [im2] ... [im255] graph variance area contrast version &opt datfn}
  \param{im1}{an image node for 1st channel}
  \param{imi}{an image node for ith channel (up to 255 channels)}
  \param{graph}{integer for connectivity (4 or 8)}
  \param{variance}{integer for lambda value of lambda flat zones}
  \param{area}{integer for area in number of pixels}
  \param{contrast}{integer for contrast value}
  \param{version}{nonnegative integer for algorithm version}
  \param{datfn}{optional string for filename (without extension) to store statistics}
  \return{im1}
  \desc{The contrast threshold value is used for merging the regions with similar contrast as follows: < 0 (don't make merging of regions), 0 (determine best contrast value automatically), and > 0 (use this value as threshold value).  Authorised version values are: 0  (compare to whole region), 1 (compare to original seeds), and 2  (compare to pixel neighbours).  If the optional string datfn is given, data files to use with gnuplot are stored in datfn.dat, otherwise no dat files are generates (default).  Details can be found in \cite{brunner-soille2007}.}
  \cfunction{\cfsegmentImage}
  \cfile{\cfsegmentImage}
  \example{}{}
*/
  while ( (arg=xlgetarg()) != NIL ){
    if (imagep(arg)){
      imarray[n++]=(IMAGE *)getimage(arg);
    }
    else
      break;
  }
  if (integerp(arg))
    graph = (int)getfixnum(arg);
  else
    xlbadtype(arg);
  variance  = (int)getfixnum(xlgafixnum());
  area      = (long int)getfixnum(xlgafixnum());
  contrast  = (int)getfixnum(xlgafixnum());
  version   = (int)getfixnum(xlgafixnum());
  if (moreargs())
    datfn = (char *)getstring(xlgetfname());

  xllastarg();
  
  return(cvimage(segmentImage(imarray, n, graph, variance, area, contrast, version, datfn)));

}

LVAL imcisrg()
{
  LVAL firstarg=NIL, arg;
  int graph=4, version;
  IMAGE *imarray[maxNumberOfBands], *imSeeds;
  int n=0;
  UINT32 maxlbl;
  G_TYPE *pg;

  if (!moreargs())
    xlabort("(@mcisrg im1 [im2] ... [im255] imSeeds graph version)");

/*
  \lspfunction{@}{mcisrg}{im1 [im2] ... [im255] imSeeds graph version}
  \param{im1}{an image node for 1st channel}
  \param{imi}{an image node for ith channel (up to 255 channels)}
  \param{imSeeds}{an image node for labelled seeds (UINT32 type)}
  \param{graph}{integer for connectivity (4 or 8)}
  \param{version}{nonnegative integer for algorithm version}
  \return{im1}
  \desc{Authorised version values are: 0  (compare to whole region), 1 (compare to original seeds), and 2  (compare to pixel neighbours). }
  \cfunction{\cfmcisrg}
  \cfile{dominik/mcisrg.c}
  \example{}{}
*/
  while ( (arg=xlgetarg()) != NIL ){
    if (n==0)
      firstarg=arg;
    if (imagep(arg)){
      imarray[n++]=(IMAGE *)getimage(arg);
    }
    else
      break;
  }
  n--;
  imSeeds=imarray[n];
  if (integerp(arg))
    graph = (int)getfixnum(arg);
  else
    xlbadtype(arg);
  version = (int)getfixnum(xlgafixnum());

  xllastarg();

  if(GetImDataType(imSeeds)!=t_LBL_TYPE){
      xlabort("(@mcisrg ...) the image of seeds must be of type LBL_TYPE (see global variable t_LBL_TYPE)");
  }
  pg = min_max(imSeeds);
  if (pg == NULL)
    xlabort("(@mcisrg ...) could not compute min/max value of image of labelled seeds");
  maxlbl=pg[1].u32_val;
  free((char *)pg);
  
  if (mcisrg(imarray, n, imSeeds , graph, maxlbl, version)==ERROR)
    return NIL;

  return(firstarg);
}


LVAL imcisrglist()
{
  LVAL arg, xlist;
  LVAL xlim1;
  int graph=4, version;
  IMAGE *imarray[maxNumberOfBands];
  int nc=0;
  UINT32 maxlbl;
  G_TYPE *pg;

  if (!moreargs())
    xlabort("(@mcisrglist imlist imSeeds graph version)");

/*
  \lspfunction{@}{mcisrglist}{imlist imSeeds graph version}
  \param{imlist}{a list of images for the successive channels of a multichannel image}
  \param{imSeeds}{an image node for labelled seeds (UINT32 type)}
  \param{graph}{integer for connectivity (4 or 8)}
  \param{version}{nonnegative integer for algorithm version}
  \return{im1}
  \desc{Authorised version values are: 0  (compare to whole region), 1 (compare to original seeds), and 2  (compare to pixel neighbours). }
  \cfunction{}
  \cfile{}
  \example{}{}
*/

  xlist = xlgalist();

  for (nc = 0; consp(xlist); xlist = cdr(xlist)){
    arg = car(xlist);
    if (imagep(arg))
      imarray[nc++]=(IMAGE *)getimage(arg);
    else{
      xlbadtype(arg);
      break;
    }
  }

  xlim1=xlgaimage();
  graph = (int)getfixnum(xlgafixnum());
  version = (int)getfixnum(xlgafixnum());

  xllastarg();

  if(GetImDataType((IMAGE*)getimage(xlim1))!=t_LBL_TYPE){
      xlabort("(@mcisrg ...) the image of seeds must be of type LBL_TYPE (see global variable t_LBL_TYPE)");
  }
  pg = min_max((IMAGE*)getimage(xlim1));
  if (pg == NULL)
    xlabort("(@mcisrg ...) could not compute min/max value of image of labelled seeds");
  maxlbl=pg[1].u32_val;
  free((char *)pg);
  
  if (mcisrg(imarray, nc, (IMAGE*)getimage(xlim1) , graph, maxlbl, version)==ERROR)
    return NIL;

  return(xlist);
}



LVAL iwriteGnuPlot3D()
{
  LVAL firstarg=NIL, arg;
  int graph=4, variance;
  long int area;
  IMAGE *imarray[maxNumberOfBands];
  int n=0;
  char *fn;

  if (!moreargs())
    xlabort("(@writegnuplot3d im1 [im2] ... [im10] graph variance area datfn)");

/*
  \lspfunction{@}{writegnuplot3d}{im1 [im2] ... [im255] graph variance area}
  \param{im1}{an image node for 1st channel}
  \param{imi}{an image node for ith channel (maximum of 255 channels)}
  \param{graph}{integer for connectivity (4 or 8)}
  \param{variance}{integer for lambda value of lambda flat zones}
  \param{area}{integer for area in number of pixels}
  \param{datfn}{string for filename (without extension) to store measurements}
  \return{im1}
  \desc{}
  \cfunction{\cfwriteGnuPlot3D}
  \cfile{dominik/segmentation.c}
  \example{}{}
*/

  while ( (arg=xlgetarg()) != NIL ){
    if (n==0)
      firstarg=arg;
    if (imagep(arg)){
      imarray[n++]=(IMAGE *)getimage(arg);
    }
    else
      break;
  }
  if (integerp(arg))
    graph = (int)getfixnum(arg);
  else
    xlbadtype(arg);
 
  variance = (int)getfixnum(xlgafixnum());
  area     = (long int)getfixnum(xlgafixnum());
  fn       = (char *)getstring(xlgetfname());
  xllastarg();
  if (writeGnuPlot3D(imarray, n, graph, area, variance, fn)==ERROR)
    return NIL;

  return(firstarg);
}

LVAL ivectorise()
{
  LVAL arg;
  char *fn=NULL;
  int format;
  double simplify;
  IMAGE *imarray[maxNumberOfBands];
  int n=0;

  if (!moreargs())
    xlabort("(*vectorise im1 [im2] ... [im255] fn format simplify)");

/*
  \lspfunction{*}{vectorise}{im1 [im2] ... [im255] fn format simplify}
  \param{im1}{an image node for 1st channel}
  \param{imi}{an image node for ith channel (maximum of 255 channels)}
  \param{fn}{a string indicating a GeoTIFF containing the geoinformation matching im1}
  \param{format}{integer for vector format (0 for do not vectorise, 1 for SVG, 2 for ESRI shape file, and 3 for both)}
  \param{simplify}{nonnegative float number for simplification degree (0.0 means no simplification)}
  \return{true on success, nil otherwise}
  \desc{writes vector file(s) corresponding to the input imagery at the location of fn and using the same prefix as fn.  The polygons of the vector image are those defined by the flat zones of the input image.}
  \cfunction{\cfvectorizeImage}
  \cfile{dominik/vectorize.c}
  \example{}{}
*/
  while ( (arg=xlgetarg()) != NIL ){
    if (imagep(arg)){
      imarray[n++]=(IMAGE *)getimage(arg);
    }
    else
      break;
  }
  if (stringp(arg))
    fn= (char *)getstring(arg);
  else
    xlbadtype(arg);

  format   = (int)getfixnum(xlgafixnum());
  simplify = (DOUBLE) getflonum(xlgaflonum());

  xllastarg();
  if (vectorizeImage(imarray, n, fn, format, simplify)==ERROR)
    return NIL;
  return(s_true);
}

#endif /* MCISRG */

LVAL iIsPartitionEqual()
{
  LVAL xlim1, xlim2;
  int result=-1;
/*
  \lspfunction{}{IsPartitionEqual}{im1 im2}
  \param{im1}{an image node}
  \param{im2}{an image node}
  \return{-1 if the flat zone partitions of the images im1 and im2 are identical (as well as the domain of definition of the 0 value), the offset of the first non matching pixel otherwise.}
  \desc{}
  \cfunction{\cfIsPartitionEqual}
  \cfile{pointop.c}
*/
  if (!moreargs())
    xlabort("(IsPartitionEqual im1 im2)");

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();

  if (IsPartitionEqual((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), &result)==ERROR)
    return NIL;

  return(cvfixnum(result));
}

LVAL iIsPartitionFiner()
{
  LVAL xlim1, xlim2;
  int graph;
  unsigned long int ofs;

  if (!moreargs())
    xlabort("(IsPartitionFiner im1 im2 graph)");
/*
  \lspfunction{}{IsPartitionFiner}{im1 im2 graph}
  \param{im1}{an image node}
  \param{im2}{an image node with same type as im1}
  \param{graph}{an integer for connectivity}
  \return{-1 if the partition of im1 is equal or finer than that of im2, otherwise nonnegative integer corresponding to the offset of first pixel violating this ordering.}
  \desc{}
  \cfunction{\cfIsPartitionFiner}
  \cfile{partorp.c}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  graph =(int)getfixnum(xlgafixnum());
  xllastarg();

  if (IsPartitionFiner((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), graph, &ofs)==ERROR)
    return NIL;

  return(cvfixnum(ofs));
}

LVAL iPartitionSimilarity()
{
  LVAL result=NIL; 
  LVAL xlim;
  LVAL xlim1, xlim2;
  int graph;
  IMAGE **imarray;
  int n=4, m;

  if (!moreargs())
    xlabort("(*PartitionSimilarity im1 im2 graph)");
/*
  \lspfunction{*}{PartitionSimilarity}{im1 im2 graph}
  \param{im1}{an image node}
  \param{im2}{an image node with same type as im1}
  \param{graph}{an integer for connectivity}
  \return{a list of images}
  \desc{returns a list of 4 1-D images containing the following information: correspondence table between the labels of im1 and im2, similarity measure between these labels, correspondence table between the labels of im2 and im1, similarity measure between these labels.}
  \cfunction{\cfPartitionSimilarity}
  \cfile{partition.c}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  graph =(int)getfixnum(xlgafixnum());
  xllastarg();
  if ( (imarray=PartitionSimilarity((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), graph)) == NULL)
    xlabort("(*PartitionSimilarity im1 im2 graph): returned NULL");
    
  SAVE_LIST_IMAGES( imarray, xlim, result, n, m );
  free(imarray);
  
  return result;
}

/*
  \lispsection
  \input{segment.tex}
*/


/*
\module{DEM processing}
\basesection
*/

LVAL idir()
{
  LVAL xlim1;
  int graph;

  if (!moreargs())
    xlabort("(@dir im graph)");
/*
  \lspfunction{@}{dir}{im graph}
  \param{im}{an image node}
  \param{graph}{integer for number of nearest neighbours to consider (either 4 or 8)}
  \return{dir}
  \desc{computes the local flow directions as the inverse of the flood wave direction occurring during an immersion simulation (i.e., flooding starting from the lowest elevations).  The codes for each direction are as follows: NW=5, N=3, NE=7, W=1, E=2, SW=6, S=4, SE=8.  When a pixel has no lower neighbour, it is set to 0.}
  \cfunction{\cfdir}
  \cfile{flow.c}
*/

  xlim1 = xlgaimage();
  graph = (int) getfixnum(xlgafixnum());

  xllastarg();

  if (dir((IMAGE *)getimage(xlim1), graph)==ERROR)
    return NIL;
  return(xlim1);
}

LVAL id8()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(*d8 im) ; direction of 8-neighbour with greatest slope");

/*
  \lspfunction{*}{d8}{im}
  \param{im}{an image node}
  \return{an image node}
  \desc{computes the D8 steepest slope direction of each pixel.  The codes for each direction are as follows: NW=5, N=3, NE=7, W=1, E=2, SW=6, S=4, SE=8.  When a pixel has no lower neighbour, it is set to 0.}
  \myseealso{*cda, *d8inf}
  \cfunction{\cfdEIGHT}
  \cfile{flow.c}
*/
  xlim1 = xlgaimage();

  xllastarg();

  return cvimage(d8((IMAGE *)getimage(xlim1)));
}

LVAL idinf()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(*dinf im) ; returns dinf slope direction image (t_FLOAT)");

/*
  \lspfunction{*}{dinf}{im}
  \param{im}{an image node}
  \return{an image node (FLOAT type)}
  \desc{computes the dinf steepest slope direction of each pixel according to \citep{tarboton97}.  Slope directions are measured counter-clockwise from east, i.e., range equals (0,2pi(values are in the range (0,2pi(, while pixels having no dowslope (plateaus and pits) are set to -1.}
  \myseealso{\htmlref{*cdainf}{*cdainf}, \htmlref{*d8}{*d8}}
  \cfunction{\cfdinf}
  \cfile{flow.c}
*/
  xlim1 = xlgaimage();

  xllastarg();

  return cvimage(dinf((IMAGE *)getimage(xlim1)));
}

LVAL icboutlet()
{
  LVAL xlim1, xlim2;

  if (!moreargs())
    xlabort("(@cboutlet lbl d8) ; propagate labels of labelled outlets (stored in lbl) according to the image d8 of d8 flow directions");

/*
  \lspfunction{@}{cboutlet}{lbl d8}
  \param{lbl}{an image node holding labelled outlets}
  \param{d8}{an image node holding d8 flow directions}
  \return{lbl (destructive function)}
  \desc{}
  \myseealso{}
  \cfunction{\cfcboutlet}
  \cfile{flow.c}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();

  xllastarg();

  if (cboutlet((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2))==ERROR)
    return NIL;
  return(xlim1);
}

LVAL icbconfluence()
{
  LVAL xlim1, xlim2;

  if (!moreargs())
    xlabort("(@cbconfluence im d8) ; propagate labels of labelled outlets (stored in im) according to the image d8 of d8 flow directions");

/*
  \lspfunction{@}{cbconfluence}{im d8}
  \param{im}{an image node holding labelled outlet pixels with value 1 and river pixels with value 2}
  \param{d8}{an image node holding d8 flow directions}
  \return{im (destructive function)}
  \desc{}
  \myseealso{}
  \cfunction{\cfcboutlet}
  \cfile{flow.c}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();

  xllastarg();

  if (cbconfluence((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2))==ERROR)
    return NIL;
  return(xlim1);
}

LVAL istrahler()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(@strahler d8) ; image with d8 directions on river networks, 0 elsewhere");

/*
  \lspfunction{@}{strahler}{d8}
  \param{d8}{an image node holding d8 directions on river networks, 0 elsewhere}
  \return{d8 (destructive function)}
  \desc{}
  \myseealso{}
  \cfunction{\cfstrahler}
  \cfile{flow.c}
*/
  xlim1 = xlgaimage();

  xllastarg();

  if (strahler((IMAGE *)getimage(xlim1))==ERROR)
    return NIL;
  return(xlim1);
}

LVAL islope8()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(*slope8 im) ; slope along direction of 8-neighbour with greatest slope");

/*
  \lspfunction{*}{slope8}{im}
  \param{im}{an image node}
  \return{an image node}
  \desc{computes the steepest slope within a 3x3 neighbourhood for each pixel.  It corresponds to the slope along the D8 direction.}
  \myseealso{\htmlref{*slopeinf}{*slopeinf}, \htmlref{*d8}{*d8}}
  \cfunction{\cfslopeEIGHT}
  \cfile{flow.c}
*/
  xlim1 = xlgaimage();

  xllastarg();

  return cvimage(slope8((IMAGE *)getimage(xlim1)));
}

LVAL islopeinf()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(*slopeinf im) ; slope along dinf direction");

/*
  \lspfunction{*}{slopeinf}{im}
  \param{im}{an image node}
  \return{an image node}
  \desc{outputs the slope along the dinf drainage directions.}
  \myseealso{\htmlref{*dinf}{*dinf}, \htmlref{*slope8}{*slope8}}
  \cfunction{\cfslopeinf}
  \cfile{flow.c}
*/
  xlim1 = xlgaimage();

  xllastarg();

  return cvimage(slopeinf((IMAGE *)getimage(xlim1)));
}

LVAL iFlatDir()
{
  LVAL xlim1, xlim2;
  int graph;

  if (!moreargs())
    xlabort("(*flatdir flat im graph) ; direction of 8-neighbour with greatest slope on flat regions");

/*
  \lspfunction{*}{flatdir}{flat im graph}
  \param{flat}{an image node for flat regions (USHORT or INT32)}
  \param{im}{an image node for corresponding DEM (USHORT)}
  \return{an image with D8 directions for all pixels in flat regions}
  \desc{see publication \citep{soille2002dgci}.  Flat regions (i.e., no flow direction) must be of type USHORT (with flat regions set to 65533) or INT32 (with flat regions set to INT32_MAX-2).}
  \cfunction{\cfFlatDir}
  \cfile{flatdir.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  graph = (int) getfixnum(xlgafixnum());

  xllastarg();

  return cvimage(FlatDir((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), graph));
}

LVAL iflow()
{
  LVAL xlim1;
  int graph;

  if (!moreargs())
    xlabort("(*flow im graph)");
/*
  \lspfunction{*}{flow}{im graph}
  \param{im}{an image node}
  \param{graph}{integer for connectivity (either 4 or 8)}
  \return{an image node for contributing drainage area}
  \desc{computes the contributing drainage areas of im using D8 drainage directions.} % as flow direction the inverse of the flood wave direction occurring during an immersion simulation (i.e., flooding starting from the lowest elevations).
  \myseealso{*flownew and *cda}
  \cfunction{\cfflow}
  \cfile{flow.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  graph = (int) getfixnum(xlgafixnum());

  xllastarg();

  return (cvimage(flow((IMAGE *)getimage(xlim1), graph)));
}

LVAL iflownew()
{
  LVAL xlim1, xlim2;
  int graph;

  if (!moreargs())
    xlabort("(*flownew im imdir graph)");
/*
  \lspfunction{*}{flownew}{im imdir graph}
  \param{im}{an image node}
  \param{imdir}{the d8 drainage directions for each pixel of im}
  \param{graph}{integer for connectivity (must be 8)}
  \return{an image node}
  \desc{computes the contributing drainage area of each pixel of im given the graph-connected drainage directions stored in imdir.}
  \myseealso{*cda for a version requiring only image of drainage directions}
  \cfunction{\cfflownew}
  \cfile{flow.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  graph = (int) getfixnum(xlgafixnum());

  xllastarg();

  return (cvimage(flownew((IMAGE *)getimage(xlim1),(IMAGE *)getimage(xlim2), graph)));
}

LVAL icdainf()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(*cdainf imdinf)");
/*
  \lspfunction{*}{cdainf}{imdinf}
  \param{imdinf}{an image node with Dinf drainage directions (t_FLOAT, -1.0 for undefined direction)}
  \return{an image node (t_FLOAT) with contributing drainage area}
  \desc{outputs the contributing drainage areas of a DEM given its dinf drainage directions.}
  \myseealso{\htmlref{*dinf}{*dinf}, \htmlref{*cda}{*cda}}
  \cfunction{\cfcdainf}
  \cfile{flow.c}
  \example{(setq cda (*cdainf (*dinf dem)))}{}
*/
  xlim1 = xlgaimage();

  xllastarg();

  return (cvimage(cdainf((IMAGE *)getimage(xlim1))));
}
LVAL icda()
{
  LVAL xlim1;
  int graph;

  if (!moreargs())
    xlabort("(*cda imdir graph)");
/*
  \lspfunction{*}{cda}{imdir graph}
  \param{imdir}{an image node with D8 drainage directions (UCHAR)}
  \param{graph}{integer for number of possible flow directions (either 4 or 8)}
  \return{an image node (type INT32) with contributing drainage area}
  \desc{outputs the contributing drainage areas of a DEM given its graph-connected drainage directions coded as follows: NW=5, N=3, NE=7, W=1, E=2, SW=6, S=4, SE=8.}
  \cfunction{\cfcda}
  \cfile{flow.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  graph = (int) getfixnum(xlgafixnum());

  xllastarg();

  return (cvimage(cda((IMAGE *)getimage(xlim1), graph)));
}


#ifdef ALLFUNCTIONS
/* aflood.c */
LVAL iaflood()
{
  LVAL xlim1, xlim2;
  int graph, maxfl;

  if (!moreargs())
    xlabort("(*@aflood iml imr graph maxfl)");
/*
  \lspfunction{*}{@aflood}{iml imr graph maxfl}
  \param{iml}{an image node with labelled relevant minima}
  \param{imr}{an image node with grey tone image}
  \param{graph}{an integer for connectivity}
  \param{maxfl}{an integer for highest flooding level}
  \return{}
  \desc{used for carving, see algorithm description in \citep{soille-vogt-colombo2003wrr}.}
  \cfunction{\cfaflood}
  \cfile{aflood.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  graph = (int) getfixnum(xlgafixnum());
  maxfl = (int) getfixnum(xlgafixnum());

  xllastarg();

  return(cvimage(aflood((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), graph, maxfl)));
}

/* fillocarve.c */
LVAL ifillocarve()
{
  LVAL xlim1, xlim2;
  int graph, maxfl, flag=0;

  if (!moreargs())
    xlabort("(*@fillocarve im1 im2 graph maxfl &opt (flag 0))");
/*
  \lspfunction{*}{@fillocarve}{iml imr graph maxfl &opt (flag 0)}
  \param{iml}{an image node with labelled relevant minima}
  \param{imr}{an image node with grey tone image}
  \param{graph}{an integer for connectivity}
  \param{maxfl}{an integer for highest flooding level}
  \param{flag}{0 (default) for energy based, area based otherwise}
  \return{an image node with propagation directions}
  \desc{Optimal removal of spurious pits in grid digital elevation models, in the sense of the theory described in \citep{soille2004wrr}.  Note that irrelevant minima must have all an intensity greater than that of the lowest minimum!  The actual carved image is stored in imr.}
  \myseealso{\htmlref{@demfillocarve}{@demfillocarve}}
  \cfunction{\cffillocarve}
  \cfile{fillocarve.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  graph = (int) getfixnum(xlgafixnum());
  maxfl = (int) getfixnum(xlgafixnum());
  if (moreargs())
    flag = (int) getfixnum(xlgafixnum());

  xllastarg();
  return(cvimage(fillocarve((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), graph, maxfl, flag)));
}
#endif

LVAL istratify()
{
  LVAL xlim1, xlim2, xlim3;

  if (!moreargs())
    xlabort("(*stratify cda thresh dir)");

/*
  \lspfunction{*}{stratify}{cda thresh dir}
  \param{cda}{an image node (INT32) for contributing drainage area}
  \param{thresh}{an image node (USHORT) for cda threshold levels}
  \param{dir}{an image node (UCHAR) for flow directions}
  \return{an image node (UCHAR) with drainage networks}
  \desc{extracts river networks by flagging the downstreams of all points whose contributing drainage areas exceed those given by the threshold image.  The dowstreams are detected by following the drainage directions stored in the image dir.}
  \cfunction{\cfstratify}
  \cfile{flow.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();

  xllastarg();

  return(cvimage(stratify((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2),(IMAGE *)getimage(xlim3))));
}

LVAL ihtop()
{
  LVAL xlim1, xlim2;

  if (!moreargs())
    xlabort("(*htop dem imdir)");
/*
  \lspfunction{*}{flownew}{im imdir graph}
  \param{im}{an image node}
  \param{imdir}{the d8 drainage directions for each pixel of im}
  \return{an image node of the same data type as im and indicating for each pixel the maximum height of its upstream}
  \desc{Beware that this function requires imdir to be consistent with DEM.  This function was created following a discussion with Segion Rosim and Joao Oliveira from INPE (visit to JRC on 14--15/11/2013).}
  \myseealso{}
  \cfunction{\cfhtop}
  \cfile{htop.c}
  \example{}{}
  \creationdate{first: 20131118 firt working: 20131126 (2 full days of work in total).}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();

  xllastarg();

  return (cvimage(htop((IMAGE *)getimage(xlim1),(IMAGE *)getimage(xlim2))));
}


/*
  \lispsection
  \input{dem.tex}
*/


/*
\module{Miscellaneous}
*/

LVAL isquarevol()
{
  LVAL xlim1;
  int k, ox, oy;

  if (!moreargs())
    xlabort("(*8vol im k ox oy)\n");
/*
  \lspfunction{*}{8vol}{im k ox oy}
  \param{im}{an image node}
  \param{k}{integer width of square SE}
  \param{ox}{integer for x coordinate of origin in square SE}
  \param{oy}{integer for y coordinate of origin in square SE}
  \return{an image node}
  \desc{outputs for each pixel the sum of the grey levels of pixels falling within a square SE of width k and with origin at (ox,oy).}
  \cfunction{\cfsquarevol}
  \cfile{rank.c}
  \example{}{}
*/  
  
  xlim1     = xlgaimage();
  k         = (int)getfixnum(xlgafixnum());
  ox = (int)getfixnum(xlgafixnum());
  oy = (int)getfixnum(xlgafixnum());

  xllastarg();

  return(cvimage(squarevol((IMAGE *)getimage(xlim1), k, ox, oy)));
}

LVAL ishade()
{
  LVAL xlim1;
  int dir;

  if (!moreargs())
    xlabort("(*shade im dir)");

/*
  \lspfunction{*}{shade}{im dir}
  \param{im}{an image node}
  \param{dir}{a code for direction of illumination (NW=0, N=1, NE=2, W=7, E=3, SW=6, S=5, SE=4).}
  \return{an image node (UCHAR)}
  \desc{computes a shaded view of the input image.}
  \cfunction{\cfshade}
  \cfile{shade.c}
  \example{}{}
*/

  xlim1 = xlgaimage();
  dir = (int) getfixnum(xlgafixnum());

  xllastarg();

  return(cvimage(shade((IMAGE *)getimage(xlim1), dir)));
}

LVAL iLineDilate3D()
{
  LVAL xlim1;
  float dh;

  if (!moreargs())
    xlabort("(*LineDilate3D im dh)");

/*
  \lspfunction{*}{LineDilate3D}{im dh}
  \param{im}{an image node}
  \param{dh}{float for delta h by pixel}
  \return{an image node (FLOAT)}
  \desc{compute the dilation of im by a 3D line segment.}
  \cfunction{\cfLineDilateTHREED)}
  \cfile{shade.c}
  \example{}{}
  \authors{Pierre Soille}
  \creationdate{20141203}
*/

  xlim1 = xlgaimage();
  dh = (float) getflonum(xlgaflonum());

  xllastarg();

  return(cvimage(LineDilate3D((IMAGE *)getimage(xlim1), dh)));
}

LVAL iconvolve()
{
  LVAL xlim1, xlim2, xlim3;
  int ox, oy, oz;

  if (!moreargs())
    xlabort("(*convolve im imse imweight &optional ox oy oz)");
/*
  \lspfunction{*}{convolve}{im imse imweight &optional ox oy oz}
  \param{im}{an image node to convolve}
  \param{imse}{an image node for defining points of convolution kernel (type UCHAR)}
  \param{imweigth}{an image node for defining the weights of each kernel point (any type)}
  \param{ox}{integer for x-coordinate of origin of kernel (default equals integer part of half the number of columns of imse)}
  \param{oy}{integer for y-coordinate of origin of kernel (default equals integer part of half the number of lines of imse)}
  \param{oz}{integer for z-coordinate of origin of kernel(default equals integer part of half the number of x-y planes imse)}
  \return{the convolution of im with the selected kernel (FLOAT image)}
  \desc{performs the convolution of im using the convolution kernel defined as follows: points of imse set to 1 define the definition domain of the kernel while the corresponding weigth is defined by the value in imweight at the same position.  Note that the kernel is not reflected so that *convolve actually returns the cross-correlation of im with the image defined by imse and imweight.}
  \cfunction{\cfconvolve}
  \cfile{convolve.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();
  if (moreargs()){
    ox = (int) getfixnum(xlgafixnum());
    oy = (int) getfixnum(xlgafixnum());
    oz = (int) getfixnum(xlgafixnum());
  }
  else{
    ox = GetImNx((IMAGE *)getimage(xlim2))/2;
    oy = GetImNy((IMAGE *)getimage(xlim2))/2;
    oz = GetImNz((IMAGE *)getimage(xlim2))/2;
  }
  xllastarg();

  return (cvimage(convolve((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), (IMAGE *)getimage(xlim3), ox, oy, oz)));
}

LVAL iconvolvedownsample()
{
  LVAL xlim1, xlim2, xlim3;
  int w=1, ox, oy, oz;

  if (!moreargs())
    xlabort("(*convolvedownsample im imse imweight &optional w ox oy oz)");
/*
  \lspfunction{*}{convolvedownsample}{im imse imweight w &optional ox oy oz}
  \param{im}{an image node to convolve}
  \param{imse}{an image node for defining points of convolution kernel (type UCHAR)}
  \param{imweigth}{an image node for defining the weights of each kernel point (any type)}
  \param{w}{integer for distance in x and y between points at which the convolution is caculated (default is 1, i.e., calculations are performed for all points such that the convolution kernel fully fits within the image frame)}
  \param{ox}{integer for x-coordinate of origin of kernel (default equals integer part of half the number of columns of imse)}
  \param{oy}{integer for y-coordinate of origin of kernel (default equals integer part of half the number of lines of imse)}
  \param{oz}{integer for z-coordinate of origin of kernel(default equals integer part of half the number of x-y planes imse)}
  \return{the convolution of im with the selected kernel (FLOAT image) at the grid nodes of a grid of width w}
  \desc{performs the convolution of im using the convolution kernel defined as follows: points of imse set to 1 define the definition domain of the kernel while the corresponding weigth is defined by the value in imweight at the same position.  Note that the kernel is not reflected so that *convolve actually returns the cross-correlation of im with the image defined by imse and imweight.  The kernel is assumed to be square and convolution is computed for points w apart in both x and y directions.  Computations are only performed at the grid nodes for which the kernel fully fits the image domain.}
  \history{20121128: parameter w added (before it was hardcoded as the width of imse)}
  \history{20121130: openmp added.}
  \feature{20121128: the output image data type is always MIAFLOAT.  It would make sense to have the possibility to have the same data type as input for the output so as to save memory and do calculations at the level of each pixel in double.}
  \cfunction{\cfconvolvedownsample}
  \cfile{convolve.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();
  if (moreargs()){
    w  = (int) getfixnum(xlgafixnum());
    ox = (int) getfixnum(xlgafixnum());
    oy = (int) getfixnum(xlgafixnum());
    oz = (int) getfixnum(xlgafixnum());
  }
  else{
    ox = GetImNx((IMAGE *)getimage(xlim2))/2;
    oy = GetImNy((IMAGE *)getimage(xlim2))/2;
    oz = GetImNz((IMAGE *)getimage(xlim2))/2;
  }
  xllastarg();

  return (cvimage(convolvedownsample((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), (IMAGE *)getimage(xlim3), w, ox, oy, oz)));
}


LVAL irsum2d()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(*rsum2d im)");
/*
  \lspfunction{*}{rsum2d}{im}
  \param{im}{an image node}
  \return{an image holding the running sum of im}
  \desc{This sum table is useful for fast normalised cross-correlation calculations, see \citep{lewis95}.}
  \cfunction{\cfrsumTWOd}
  \cfile{convolve.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xllastarg();

  return (cvimage(rsum2d((IMAGE *)getimage(xlim1))));
}

LVAL irsum3d()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(*rsum3d im)");
/*
  \lspfunction{*}{rsum3d}{im}
  \param{im}{an image node}
  \return{an image holding the running sum of im}
  \desc{This function is used for RGB histogram matching based on cumulative distribution functions, see *hstrgbmatch.}
  \cfunction{\cfrsumTHREEd}
  \cfile{convolve.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xllastarg();

  return (cvimage(rsum3d((IMAGE *)getimage(xlim1))));
}

LVAL irsumsq2d()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(*rsumsq2d im)");
/*
  \lspfunction{*}{rsumsq2d}{im}
  \param{im}{an image node}
  \return{an image holding the running sum of square values of im}
  \desc{This sum table is useful for fast normalised cross-correlation calculations, see \citep{lewis95}.}
  \cfunction{\cfrsumTWOd}
  \cfile{convolve.c}
  \creationdate{20120111}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xllastarg();

  return (cvimage(rsumsq2d((IMAGE *)getimage(xlim1))));
}

LVAL imean2d()
{
  LVAL xlim1;
  int width;

  if (!moreargs())
    xlabort("(*mean2d im width)");
/*
  \lspfunction{*}{mean2d}{im}
  \param{im}{an image node}
  \param{width}{an odd integer for window width}
  \return{an image holding the moving average of im with square window of width width}
  \desc{Uses running sum table as described in  \citep{lewis95}.  Overflow will occur for any running sum exceeding 2 to the power of 32 (this can easily happen for large images). The function imean2dse does not.}
  \cfunction{\cfmeanTWOd}
  \cfile{convolve.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  width = (int) getfixnum(xlgafixnum());
  xllastarg();

  return (cvimage(mean2d((IMAGE *)getimage(xlim1), width)));
}

LVAL imean2dse()
{
  LVAL xlim1, xlim2;
  int ox, oy;

  if (!moreargs())
    xlabort("(*mean2dse im imse ox oy)");
/*
  \lspfunction{*}{mean2dse}{im imse ox oy}
  \param{im}{an image node}
  \param{imse}{an image node for SE (binary UCHAR image)}
  \param{ox}{integer for x-coordinate of centre of SE}
  \param{oy}{integer for y-coordinate of centre of SE}
  \return{an image holding the moving average of im with ith window defined by the image imse centred at coordinates ox oy (only points of imse set to 1 are considered in the calculations)}
  \desc{fast implementation using overlap in x-direction}
  \cfunction{\cfmeanTWOdse}
  \cfile{convolve.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  ox = (int) getfixnum(xlgafixnum());
  oy = (int) getfixnum(xlgafixnum());
  xllastarg();

  return (cvimage(mean2dse((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), ox, oy)));
}

LVAL ivariance2dse()
{
  LVAL xlim1, xlim2;
  int ox, oy;

  if (!moreargs())
    xlabort("(*variance2dse im imse ox oy)");
/*
  \lspfunction{*}{variance2dse}{im}
  \param{im}{an image node}
  \param{imse}{an image node for SE (binary UCHAR image)}
  \param{ox}{integer for x-coordinate of centre of SE}
  \param{oy}{integer for y-coordinate of centre of SE}
  \return{an image holding the moving variance measure of im with window defined by the image imse centred at coordinates ox oy (only points of imse set to 1 are considered in the calculations).  The biased estimator is used in the calculations (i.e., division by the number of points set to 1 in imse).}
  \desc{fast implementation using overlap in x-direction.}
  \cfunction{\cfvarianceTWOdse}
  \cfile{convolve.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  ox = (int) getfixnum(xlgafixnum());
  oy = (int) getfixnum(xlgafixnum());
  xllastarg();

  return (cvimage(variance2dse((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), ox, oy)));
}


LVAL iazimuth()
{
  LVAL xlim1, xlim2;

  if (!moreargs())
    xlabort("(@azimuth imdx imdy)");
/*
  \lspfunction{@}{azimuth}{imdx imdy}
  \param{imdx}{an image node the x-derivative of an image}
  \param{imdy}{an image node the y-derivative of an image}
  \return{imdx}
  \desc{typically, the x- and y-derivatives are obtained by a convolution with Sobel kernels.  Warning, until 20130626, this function was for uint32 and returned degree values rescaled in [0,255] to fit a byte.}
  \cfunction{\cfazimuth}
  \cfile{convolve.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();

  xllastarg();

  if (azimuth((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2))==ERROR)
    return NIL;

  return(xlim1);
}

/* dirmean.c */
LVAL idirmean()
{
  LVAL xlim1, xlim2, xlim3;
  int ox, oy, oz;

  if (!moreargs())
    xlabort("(*dirmean imdx imdy imse &optional ox oy oz)");
/*
  \lspfunction{*}{dirmean}{imdx imdy imse &optional ox oy oz}
  \param{imdx}{an image node the x-derivative of an image}
  \param{imdy}{an image node the y-derivative of an image}
  \param{imse}{an image node for kernel}
  \param{ox}{integer for x-coordinate of origin of kernel (default equals integer part of half the number of columns of imse)}
  \param{oy}{integer for y-coordinate of origin of kernel (default equals integer part of half the number of lines of imse)}
  \param{oz}{integer for z-coordinate of origin of kernel(default equals integer part of half the number of x-y planes imse)}
  \return{an image node}
  \desc{}
  \cfunction{\cfdirmean}
  \cfile{dirmean.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();
  if (moreargs()){
    ox = (int) getfixnum(xlgafixnum());
    oy = (int) getfixnum(xlgafixnum());
    oz = (int) getfixnum(xlgafixnum());
  }
  else{
    ox = GetImNx((IMAGE *)getimage(xlim3))/2;
    oy = GetImNy((IMAGE *)getimage(xlim3))/2;
    oz = GetImNz((IMAGE *)getimage(xlim3))/2;
  }

  xllastarg();

  return ( cvimage(dirmean((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), (IMAGE *)getimage(xlim3), ox, oy, oz)) );
}

LVAL icoherence()
{
  LVAL xlim1, xlim2, xlim3;
  int ox, oy, oz;

  if (!moreargs())
    xlabort("(*coherence imdx imdy imse &optional ox oy oz)");

/*
  \lspfunction{*}{coherence}{imdx imdy imse &optional ox oy oz}
  \param{imdx}{an image node the x-derivative of an image}
  \param{imdy}{an image node the y-derivative of an image}
  \param{imse}{an image node for kernel}
  \param{ox}{integer for x-coordinate of origin of kernel (default equals integer part of half the number of columns of imse)}
  \param{oy}{integer for y-coordinate of origin of kernel (default equals integer part of half the number of lines of imse)}
  \param{oz}{integer for z-coordinate of origin of kernel(default equals integer part of half the number of x-y planes imse)}
  \return{an image node}
  \desc{}
  \cfunction{\cfcoherence}
  \cfile{dirmean.c}
  \example{}{}
*/


  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();
  if (moreargs()){
    ox = (int) getfixnum(xlgafixnum());
    oy = (int) getfixnum(xlgafixnum());
    oz = (int) getfixnum(xlgafixnum());
  }
  else{
    ox = GetImNx((IMAGE *)getimage(xlim3))/2;
    oy = GetImNy((IMAGE *)getimage(xlim3))/2;
    oz = GetImNz((IMAGE *)getimage(xlim3))/2;
  }

  xllastarg();

  return ( cvimage(coherence((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), (IMAGE *)getimage(xlim3), ox, oy, oz)) );
}

LVAL incc()
{
  LVAL xlim1, xlim2;
  int xi, yi, w;
  if (!moreargs())
    xlabort("(*ncc im template xi yi w)");

/*
  \lspfunction{*}{ncc}{im template xi yi w}
  \param{im}{an image node}
  \param{imtemplate}{an image node holding template (odd width and length)}
  \param{xi}{integer for x-coordinate}
  \param{yi}{integer for y-coordinate}
  \param{w}{odd integer for width of search window centred on (xi,yi)}
  \return{an image node holding normalised cross-correlation function, see details in \citep{barnea-silverman72}.}
  \desc{}
  \cfunction{\cfncc}
  \cfile{registration.c}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xi= (int) getfixnum(xlgafixnum());
  yi= (int) getfixnum(xlgafixnum());
  w = (int) getfixnum(xlgafixnum());
  
  xllastarg();

  return(cvimage(ncc((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), xi,yi,w)));
}

LVAL incclewis()
{
  LVAL xlim1, xlim2, xlim3, xlim4;
  int xi, yi, w;
  if (!moreargs())
    xlabort("(*ncclewis im template sumim sqsumim xi yi w)");

/*
  \lspfunction{*}{ncc}{im template xi yi w}
  \param{im}{an image node}
  \param{imtemplate}{an image node holding template (odd width and length)}
  \param{sumim}{the running sum of im}
  \param{sumsqim}{the running square sum of im}
  \param{xi}{integer for x-coordinate}
  \param{yi}{integer for y-coordinate}
  \param{w}{odd integer for width of search window centred on (xi,yi)}
  \return{an image node holding normalised cross-correlation function, see details in \citep{barnea-silverman72}.}
  \desc{based on the use of sum-tables as suggested in \cite{lewis95}.}
  \cfunction{\cfncc}
  \cfile{registration.c}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xlim3 = xlgaimage();
  xlim4 = xlgaimage();
  xi= (int) getfixnum(xlgafixnum());
  yi= (int) getfixnum(xlgafixnum());
  w = (int) getfixnum(xlgafixnum());
  
  xllastarg();

  return(cvimage(ncclewis((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), (IMAGE *)getimage(xlim3), (IMAGE *)getimage(xlim4),xi,yi,w)));
}

LVAL issda()
{
  LVAL xlim1, xlim2;
  int xi, yi, w;
  if (!moreargs())
    xlabort("(*ssda im imtemplate xi yi w)");

/*
  \lspfunction{*}{ssda}{im imtemplate xi yi w}
  \param{im}{an image node}
  \param{imtemplate}{an image node holding template (odd width and length)}
  \param{xi}{integer for x-coordinate}
  \param{yi}{integer for y-coordinate}
  \param{w}{odd integer for width of search window centred on (xi,yi)}
  \return{an image node}
  \desc{}
  \cfunction{\cfssda}
  \cfile{registration.c}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xi= (int) getfixnum(xlgafixnum());
  yi= (int) getfixnum(xlgafixnum());
  w = (int) getfixnum(xlgafixnum());
  
  xllastarg();

  return(cvimage(ssda((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), xi,yi,w)));
}

LVAL iphase_correlation()
{
  LVAL xlim1, xlim2;
  if (!moreargs())
    xlabort("(*phase_correlation im template)");

/*
  \lspfunction{*}{phase_correlation}{im template}
  \param{im}{an image node}
  \param{imtemplate}{an image node holding template (must have the same size and type as im)}
  \return{an image node holding the phase correlation}
  \desc{Based on method originally described in \cite{kuglin-hines75}.  Uses FFTW3 library to compute the inverse FT of the cross-product of the FT of the input images.}
  \cfunction{\cfphaseUDcorrelation}
  \cfile{registration.c}
  \authors{Pierre Soille}
  \creationdate{20120808}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  
  xllastarg();

  return(cvimage(phase_correlation((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2))));
}


LVAL itransgrad()
{
  LVAL xlim1;
  int graph;
  if (!moreargs())
    xlabort("(*transgrad im graph)");

/*
  \lspfunction{*}{transgrad}{im graph}
  \param{im}{an image node}
  \param{graph}{integer for connectivity (either 4 or 8)}
  \return{an image node where the value of each pixel is equal to the difference between the least upper adjacent pixel and the greatest lowest adjacent pixel (if one of these values is not defined, the output pixel value is set to the dynamic range of the considered data type).}
  \desc{}
  \cfunction{\cftransgrad}
  \cfile{transition.c}
*/

  xlim1 = xlgaimage();
  graph = (int) getfixnum(xlgafixnum());
  
  xllastarg();

  return(cvimage(transgrad((IMAGE *)getimage(xlim1), graph)));
}

LVAL icoor_extrema_paraboloid()
{
  LVAL xlim1;
  if (!moreargs())
    xlabort("(*coor_extrema_paraboloid b)\n");
/*
  \lspfunction{*}{solve}{b}
  \param{im1}{an image node with 1 column of 5 double values matching the sample values of the paraboloid at coordinates (0,-1), (-1,0), (0,0), (1,0), (0,1)}
  \return{a new image with one column containing the 5 double values of the solution X: A X = B}
  \desc{}
  \cfunction{\cfcoorUDextremaUDparaboloid}
  \cfile{gsl.c}
*/

  xlim1 = xlgaimage();
  xllastarg();
  return(cvimage(coor_extrema_paraboloid((IMAGE *)getimage(xlim1))));
}

LVAL ifitlinear()
{
  LVAL xlim1, xlim2;
  if (!moreargs())
    xlabort("(*fitlinear im1 im2)\n");
/*
  \lspfunction{*}{fitlinear}{im1 im2}
  \param{im1}{an image node of type double}
  \param{im2}{an image node of type double and same size as im1}
  \return{a new image with one line containing the 6 double values of the result of the linear regression: }
  \desc{}
  \cfunction{\cffitlinear}
  \cfile{gsl.c}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  xllastarg();
  return(cvimage(fitlinear((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2))));
}

/*
\module{Contrast enhancement}
*/


/*
  \lispsection
  \input{contrast.tex}  % Functions defined in lisp
*/



/*
\module{Generic ideas}
*/
/*
  \lispsection
  \input{idea.tex}  % Functions defined in lisp
*/
/*
\module{Remote sensing}\label{m.rs}
\basesection
*/

#define NPARMS 32
LVAL iproj()
{
  LVAL arg, result, u_ptr, v_ptr;
  struct apoint idata, odata;
  char *parms[NPARMS];
  int n=0, flag=1;
/*
  \lspfunction{*}{proj}{u v &opt proj4args (flag 1)}
  \param{u}{floating point value:  x-coordinate in m for inverse projection or longitude in radian for forward projection (positive eastward)}
  \param{v}{floating point value:  y-coordinate in m for inverse projection or latitude in radian for forward projection (positive northward)}
  \param{proj4args}{series of strings, one per proj4 parameter}
  \param{flag}{optional integer flag (default (1) for forward projection, inverse projection otherwise)}
  \return{a list with two elements representing the value of u (1st element, i.e. car) and v (2nd element, i.e., cadr) according to the specified forward or inverse projection.}
  \desc{wrapper function to \htmladdnormallink{proj4}{http://trac.osgeo.org/proj} cartographic projection procedures \cite{evenden2003}.}
  \cfunction{see proj API}
  \cfile{grid.c}
  \example{(*proj4 634500.0 2376650.0 "proj=lcc" "lat_0=46.8" "lon_0=2.337229"  "x_0=600000."  "y_0=2200000."  "ellps=clrk80"  "lat_1=45898919"  "lat_2=47.6960140" 0)}{; inverse projection of Loing/Seine river confluence from France zone II \'etendu map coordinates (i.e., Lambert conic conformal projection)}
*/
  if (!moreargs())
    xlabort("(*proj u v &opt proj4args (flag 1)");

  idata.u = (double) getflonum(xlgaflonum());
  idata.v = (double) getflonum(xlgaflonum());

  if ( moreargs() ){
    while ( (arg=xlgetarg()) != NIL){
      if ( stringp(arg) )
 	parms[n++]=(char *)getstring(arg);
      else if ( fixp(arg) ){
	flag= getfixnum(arg);
      }
      else{
	printf("error: invalid argument");
	return NIL;
      }
      if (n==NPARMS){
	printf("error: too many projection arguments");
	return NIL;
      }
      if ( ! moreargs() )
	break;
    }
  }

  odata = proj(idata, parms, n, flag);
  xlstkcheck(3);
  xlsave(u_ptr);
  xlsave(v_ptr);
  xlsave(result);
  u_ptr=cvflonum( (float)odata.u);
  v_ptr=cvflonum( (float)odata.v);
  result = cons(u_ptr, cons (v_ptr, NIL));
  xlpopn(3);
  return result;
}
LVAL ics2cs()
{
  LVAL arg, result, u_ptr, v_ptr;
  double res, ulc_e, ulc_n;
  int nx, ny;
  char *parmsi[NPARMS], *parmso[NPARMS], to[3]="to", emptystr[1]="";
  IMAGE *imx=NULL, *imy=NULL;
  int ni=0, no=0, flag=1;
  IMAGE **imarray=NULL;
  
/*
  \lspfunction{*}{cs2cs}{ulc_e ulc_n nx ny res &opt proj4args (flag 1)}
  \param{ulc_e}{floating point value for upper left corner x-coordinate (in m or rad) of target image}
  \param{ulc_n}{floating point value for upper left corner y-coordinate (in m or rad) of target image}
  \param{nx}{integer value for number of columns of target image}
  \param{ny}{integer value for number of lines of target image}
  \param{res}{floating point value for resolution of target image}
  \param{proj4args}{series of strings, one per proj4 parameter}
  \return{a list containing two images of float data type, the first for the x-ccordinates of the target image, the second for the y coodinates.}
  \desc{wrapper function to \htmladdnormallink{proj4}{http://trac.osgeo.org/proj/} cartographic projection procedures \cite{evenden2003}.}
  \cfunction{\cfcsTWOcs}
  \cfile{projection.c}
  \example{}{}
*/
  if (!moreargs())
    xlabort("(*cs2cs ulc_e ulc_n nx ny res &proj4args");
  ulc_e = (double) getflonum(xlgaflonum());
  ulc_n = (double) getflonum(xlgaflonum());
  nx= (int) getfixnum(xlgafixnum());
  ny= (int) getfixnum(xlgafixnum());
  res= (double) getflonum(xlgaflonum());

  if ( moreargs() ){
    flag=1;
    while ( (arg=xlgetarg()) != NIL){
      if ( stringp(arg) && flag ){
 	parmsi[ni++]=(char *)getstring(arg);
	if (strcmp(parmsi[ni-1],emptystr)==0)
	  ni-=1;
	/* else
	   printf("%s\n", parmsi[ni-1]); */
      }
      else if ( stringp(arg) ){
	parmso[no++]=(char *)getstring(arg);
	if (strcmp(parmso[no-1],emptystr)==0)
	  no-=1;
	/* else
	   printf("%s\n", parmso[no-1]); */
      }
      else{
	printf("error: invalid argument");
	return NIL;
      }
      if ( (ni==NPARMS) || (no ==NPARMS) ){
	printf("error: too many projection arguments");
	return NIL;
      }
      if ( (flag==1) && (strcmp(parmsi[ni-1],to)==0) ){
	flag=0;
	ni-=1;
	/* printf("ni=%d\n", ni);*/
      }
      if ( ! moreargs() )
	break;
    }
  }

  imarray=cs2cs(ulc_e, ulc_n, nx, ny, res, parmsi, ni, parmso, no);
  if (imarray == NULL){
      sprintf(buf,"error: returned by cs2cs()\n"); errputstr(buf);
      return NIL;
  }
  imx=imarray[0];
  imy=imarray[1];

  xlstkcheck(3);
  xlsave(u_ptr);
  xlsave(v_ptr);
  xlsave(result);
  u_ptr=cvimage(imx);
  v_ptr=cvimage(imy);
  result = cons(u_ptr, cons (v_ptr, NIL));
  xlpopn(3);
  free(imarray);
  return result;
}
LVAL ijulian_date()
{
  short int year, month, day;
  double hour, jld;
  
  if (!moreargs())
    xlabort("(*juliandate year month day hour)");
/*
  \lspfunction{*}{juliandate}{year month day hour}
  \param{year}{integer for Gregorian year}
  \param{year}{integer for month in the year}
  \param{year}{integer for day in the month}
  \param{hour}{float for hour in the day}
  \return{Julian Date (float number)}
  \desc{This function returns the Julian date of a given Gregorian calendar date (year, month, day, hour) using the compact algorithm proposed by \citet{fliegel-flandern68}.  This function is useful for calculating the number of days separating two given dates.  The reference date for the Julian calendar equals to the 24th of November 4714 BC in the proleptic Gregorian calendar (the proleptic Gregorian calendar is produced by extending the Gregorian calendar to dates preceding its official introduction on 24 February 1582 by a decree of Pope Gregory XIII).}
  %\cfunction{\cfjulianUDdate}
  \cfile{remsens.c}
  \example{(*juliandate 2000 31 3 10.0)}{; returns Simon's birth date in Julian calendar}
*/
  year= (short int) getfixnum(xlgafixnum());
  month= (short int) getfixnum(xlgafixnum());
  day= (short int) getfixnum(xlgafixnum());
  hour= (double) getflonum(xlgaflonum());
  
  xllastarg();

  jld=julian_date(year, month, day, hour);

  return cvflonum((float)jld);
  
}

#undef NPARMS
/*
  \lispsection
  \input{remsens.tex}  % Functions defined in lisp
*/


/*
  % Now inserted in imstat and hmtskel
  %\module{Marcin's functions}\label{m.marcin} % Mostly in imio.c
  %\basesection
*/

/*
  %\lispsection
  % \input{marcin.tex}  % Functions defined in lisp
*/
/* #ifdef MARCIN */
/* #include "xlglue1_marcin.c" */
/* #endif  */


#ifdef LEFTOVER
#include "xlglue1_leftover.c"
#endif


#ifdef GRAZZJA
#include "xlglue1_grazzja.c"
#endif 



#endif /* ifdef MIAL */
