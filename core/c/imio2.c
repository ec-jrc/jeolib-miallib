/***********************************************************************
Author(s): Pierre Soille
Copyright (C) 2000-2020 European Union (Joint Research Centre)

This file is part of miallib.

miallib is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

miallib is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with miallib.  If not, see <https://www.gnu.org/licenses/>.
***********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>  /* for gethostname */
#ifndef UNIX
// #include "bytesex.h" /* <endian.h> only for linux ... */
#include  <endian.h>
#endif
#include "miallib.h"
#include "imio.h"
#include <geotiff/xtiffio.h>
#include <tiffio.h>
#include <geotiff/geotiffio.h> /* 2005-12-20 */
/* #include "banner.h"  /\* 2007-12-10 *\/ */

#if (!defined(SEEK_CUR))
#define SEEK_CUR 1
#endif

extern char *getCrtTimeString(char *timeString);
extern int gethostname(char *name, size_t len);  /* fix to prevent warning, include unistd.h not sufficient */


/** \addtogroup group_io
 *  @{
 */

/***********************************************************************/
/* start of patch to accomodate unofficial extended TIFFTAGS
   with initial suggestion from http://even.rouault.free.fr/services.html
   see also code of http://www.remotesensing.org/libtiff/addingtags.html
   First: 20130928
*/

#define TIFFTAG_GDAL_METADATA  42112
#define TIFFTAG_GDAL_NODATA    42113
#define TIFFTAG_RPCCOEFFICIENT 50844


/************************************************************************/
/*                          GTiffTagExtender()                          */
/*                                                                      */
/*      Install tags specially known to GDAL.                           */
/************************************************************************/
static int GdalTiffTags_flag=0;

static TIFFExtendProc _ParentExtender = NULL;
static void GTiffTagExtender(TIFF *tif){
    static const TIFFFieldInfo xtiffFieldInfo[] = {
        { TIFFTAG_GDAL_METADATA,    -1,-1, TIFF_ASCII,	FIELD_CUSTOM,
          TRUE,	FALSE,	(char*) "GDALMetadata" },
        { TIFFTAG_GDAL_NODATA,	    -1,-1, TIFF_ASCII,	FIELD_CUSTOM,
          TRUE,	FALSE,	(char*) "GDALNoDataValue" },
        { TIFFTAG_RPCCOEFFICIENT,   -1,-1, TIFF_DOUBLE,	FIELD_CUSTOM,
          TRUE,	TRUE,	(char*) "RPCCoefficient" }
    };

    if (_ParentExtender)
        (*_ParentExtender)(tif);

    TIFFMergeFieldInfo( tif, xtiffFieldInfo,
		        sizeof(xtiffFieldInfo) / sizeof(xtiffFieldInfo[0]) );
}

/* call it only once in the lifetime of the program before opening/creating a
TIFF */
void InstallGDALTiffTags()
{
    _ParentExtender = TIFFSetTagExtender(GTiffTagExtender);
}

/* end of patch to accomodate unofficial extended TIFFTAGS             */
/***********************************************************************/


void print_mia_banner()
{
  /* char banner[512] = MIA_BANNER; */
  char banner[512] = { [0 ... 511] = ' ' };
  printf("%s\n", banner);
}


/**
 * @synopsis tiffinfo fn field val
 *
 * @param fn a string for the name of a TIFF file (possibly including its path)
 *
 * @param field a string for the TIFF field name
 *
 * @param val holding the value of the specified field
 *
 * @desc the following field names are valid: TIFFTAG_IMAGEWIDTH, TIFFTAG_IMAGELENGTH, TIFFTAG_BITSPERSAMPLE, TIFFTAG_SAMPLESPERPIXEL, TIFFTAG_SAMPLEFORMAT (1 for unsigned integer data, 2 for two's complement signed integer data, and 3 for floating point data), GTModelTypeGeoKey (1 for projected, 2 for geographic, and 3 for geocentric), GeographicTypeGeoKey, ProjectedCSTypeGeoKey, and GTRasterTypeGeoKey (1 for RasterPixelIsArea).
 */
ERROR_TYPE tiffinfo(char *fn, char *field, float *val)
{
  TIFF *tiffp;
  GTIF *gtif=NULL;
  uint32 a_uint32;
  uint16  a_uint16;
  geocode_t gkey_val;
/*   if ((tiffp = TIFFOpen(fn, "rc")) == NULL){ */
/*     (void)sprintf(buf,"ERROR in tiffinfo(): invalid TIFF file name \"%s\" \n", fn); errputstr(buf); */
/*     return ERROR; */
/*   } */

  /* Open TIFF descriptor to read GeoTIFF tags */
  tiffp=XTIFFOpen(fn,"r");
  if (!tiffp){
    (void)sprintf(buf,"ERROR in tiffinfo(): invalid TIFF file name \"%s\" \n", fn); errputstr(buf);
    return ERROR;
  }

  gtif = GTIFNew(tiffp);

  if (strcmp(field,"TIFFTAG_IMAGEWIDTH")==0){
    TIFFGetField(tiffp, TIFFTAG_IMAGEWIDTH, &a_uint32);
    *val=(float)a_uint32;
  }
  else if (strcmp(field,"TIFFTAG_IMAGELENGTH")==0){
    TIFFGetField(tiffp, TIFFTAG_IMAGELENGTH, &a_uint32);
    *val=(float)a_uint32;
  }
  else if (strcmp(field,"TIFFTAG_BITSPERSAMPLE")==0){
    TIFFGetField(tiffp, TIFFTAG_BITSPERSAMPLE, &a_uint16);
    *val=(float)a_uint16;
  }
  else if (strcmp(field,"TIFFTAG_SAMPLEFORMAT")==0){
     if (TIFFGetField(tiffp, TIFFTAG_SAMPLEFORMAT, &a_uint16) == 1)
       *val=(float)a_uint16;
    else{
      (void)sprintf(buf,"ERROR in tiffinfo(): no \"%s\" \n", field); errputstr(buf);
      return ERROR;
    }
  }
  else if (strcmp(field,"TIFFTAG_SAMPLESPERPIXEL")==0){
     if (TIFFGetField(tiffp, TIFFTAG_SAMPLESPERPIXEL, &a_uint16) == 1)
       *val=(float)a_uint16;
    else{
      (void)sprintf(buf,"ERROR in tiffinfo(): no \"%s\" \n", field); errputstr(buf);
      return ERROR;
    }
  }
  else if (strcmp(field,"GeographicTypeGeoKey")==0){
    if (GTIFKeyGet(gtif, GeographicTypeGeoKey, &gkey_val, 0, 1) == 1)
      *val=(float)gkey_val;
    else{
      (void)sprintf(buf,"ERROR in tiffinfo(): no \"%s\" \n", field); errputstr(buf);
      return ERROR;
    }
  }
  else if (strcmp(field,"ProjectedCSTypeGeoKey")==0){
    if (GTIFKeyGet(gtif, ProjectedCSTypeGeoKey, &gkey_val, 0, 1) == 1)
      *val=(float)gkey_val;
    else{
      (void)sprintf(buf,"ERROR in tiffinfo(): no \"%s\" \n", field); errputstr(buf);
      return ERROR;
    }
  }
  else if (strcmp(field,"GTRasterTypeGeoKey")==0){
    if (GTIFKeyGet(gtif, GTRasterTypeGeoKey, &gkey_val, 0, 1) == 1)
      *val=(float)gkey_val;
    else{
      (void)sprintf(buf,"WARNING in tiffinfo(): no \"%s\", assuming default is RasterPixelIsArea (1)\n", field); errputstr(buf);
      *val=1;
    }
  }
  else if (strcmp(field,"GTModelTypeGeoKey")==0){
    if (GTIFKeyGet(gtif, GTModelTypeGeoKey, &gkey_val, 0, 1) == 1)
      *val=(float)gkey_val;
    else{
      (void)sprintf(buf,"ERROR in tiffinfo(): no \"%s\" \n", field); errputstr(buf);
      return ERROR;
    }
  }
  else{
    (void)sprintf(buf,"ERROR in tiffinfo(): invalid TIFF TAG NAME \"%s\" \n", field); errputstr(buf);
    return ERROR;
  }

  if (gtif!=NULL)
    GTIFFree(gtif);
  TIFFClose(tiffp);
  return NO_ERROR;
}

/**
 * @synopsis tiffinfoJIP fn field val
 *
 * @param fn a string for the name of a TIFF file (possibly including its path)
 * *
 * @return an image holding the values of the fields values in the indicated order
 *
 * @desc the following sequence of values are inserted in the image: TIFFTAG_IMAGEWIDTH, TIFFTAG_IMAGELENGTH, TIFFTAG_BITSPERSAMPLE, TIFFTAG_SAMPLESPERPIXEL, TIFFTAG_SAMPLEFORMAT (1 for unsigned integer data, 2 for two's complement signed integer data, and 3 for floating point data), GTModelTypeGeoKey (1 for projected, 2 for geographic, and 3 for geocentric), GeographicTypeGeoKey, ProjectedCSTypeGeoKey, and GTRasterTypeGeoKey (1 for RasterPixelIsArea).
 */
IMAGE *tiffinfoJIP(char *fn)
{
  TIFF *tiffp;
  GTIF *gtif=NULL;
  IMAGE *im=NULL;
  UINT32 *pim;
  uint32 a_uint32;
  uint16  a_uint16;
  geocode_t gkey_val;

  /* Open TIFF descriptor to read GeoTIFF tags */
  tiffp=XTIFFOpen(fn,"r");
  if (!tiffp){
    (void)sprintf(buf,"ERROR in tiffinfoJIP(): invalid TIFF file name \"%s\" \n", fn); errputstr(buf);
    return NULL;
  }

  gtif = GTIFNew(tiffp);

  im=create_image(t_UINT32, 9, 1, 1);
  if (im==NULL){
    if (gtif!=NULL)
      GTIFFree(gtif);
    TIFFClose(tiffp);
    return NULL;
  }
  pim=(UINT32 *)GetImPtr(im);

  TIFFGetField(tiffp, TIFFTAG_IMAGEWIDTH, &a_uint32);
  pim[0]=(UINT32)a_uint32;
  TIFFGetField(tiffp, TIFFTAG_IMAGELENGTH, &a_uint32);
  pim[1]=(UINT32)a_uint32;
  TIFFGetField(tiffp, TIFFTAG_BITSPERSAMPLE, &a_uint16);
  pim[2]=(UINT32)a_uint16;

  if (TIFFGetField(tiffp, TIFFTAG_SAMPLEFORMAT, &a_uint16) == 1)
    pim[3]=(UINT32)a_uint16;
  else{
    (void)sprintf(buf,"warning in tiffinfoJIP(): no \"TIFFTAG_SAMPLEFORMAT\" \n" ); errputstr(buf);
    pim[3]=(UINT32)UINT32_MAX;
  }

  if (TIFFGetField(tiffp, TIFFTAG_SAMPLESPERPIXEL, &a_uint16) == 1)
    pim[4]=(UINT32)a_uint16;
  else{
    (void)sprintf(buf,"warning in tiffinfoJIP(): no \"TIFFTAG_SAMPLESPERPIXEL\" (assuming 1)\n"); errputstr(buf);
    pim[4]=(UINT32)1;
  }

  if (GTIFKeyGet(gtif, GeographicTypeGeoKey, &gkey_val, 0, 1) == 1)
    pim[5]=(UINT32)gkey_val;
  else{
    (void)sprintf(buf,"warning in tiffinfoJIP(): no \"GeographicTypeGeoKey\" \n"); errputstr(buf);
    pim[5]=(UINT32)UINT32_MAX;
  }

  if (GTIFKeyGet(gtif, ProjectedCSTypeGeoKey, &gkey_val, 0, 1) == 1)
    pim[6]=(UINT32)gkey_val;
  else{
    (void)sprintf(buf,"warning in tiffinfoJIP(): no \"ProjectedCSTypeGeoKey\" \n"); errputstr(buf);
    pim[6]=(UINT32)UINT32_MAX;
  }

  if (GTIFKeyGet(gtif, GTRasterTypeGeoKey, &gkey_val, 0, 1) == 1)
    pim[7]=(UINT32)gkey_val;
  else{
    (void)sprintf(buf,"warning in tiffinfoJIP(): no \"GTRasterTypeGeoKey\", assuming default is RasterPixelIsArea (1)\n"); errputstr(buf);
    pim[7]=(UINT32)UINT32_MAX;
  }

  if (GTIFKeyGet(gtif, GTModelTypeGeoKey, &gkey_val, 0, 1) == 1)
    pim[8]=(UINT32)gkey_val;
  else{
    (void)sprintf(buf,"warning in tiffinfoJIP(): no \"GTModelTypeGeoKey\" \n"); errputstr(buf);
    pim[8]=(UINT32)UINT32_MAX;
  }

  if (gtif!=NULL)
    GTIFFree(gtif);
  TIFFClose(tiffp);
  return im;
}

IMAGE *GetGeoKey(char *fname, char *keyname)
{
  IMAGE *im;
  INT32 *ptr;
  TIFF *tif=(TIFF*)0;  /* TIFF-level descriptor */
  GTIF *gtif=(GTIF*)0; /* GeoKey-level descriptor */
  geocode_t modelTypeCode; // , rasterModelType;
  int count=1;

  /* Open TIFF descriptor to read GeoTIFF tags */
  tif=TIFFOpen(fname,"r");
  if (!tif)
    return NULL;

  /* Open GTIF Key parser; keys will be read at this time. */
  gtif = GTIFNew(tif);
  if (!gtif)
    return NULL;

  if (strcmp(keyname,"GTModelTypeGeoKey")==0){
    GTIFKeyGet(gtif, GTModelTypeGeoKey, &modelTypeCode, 0, 1);
  }
  else{
    printf("GeoKey '%s' not handled\n", keyname);
    return NULL;
  }

 /* get rid of the key parser */
 GTIFFree(gtif);

 /* close the TIFF file descriptor */
 TIFFClose(tif);

 im=create_image(t_INT32, count, 1, 1);
 ptr=(INT32 *)GetImPtr(im);
 *ptr=(INT32)modelTypeCode;

 return im;
}


IMAGE *GetTIFFTagGeo(char *fn, char *tagname)
{
  TIFF *tiffp=NULL;
  IMAGE *im=NULL;
  int i, erflag=0;
  uint16 count=0;
  double *data=NULL;
  double *ptr=NULL;
  GTIF *gtif=(GTIF*)NULL; /* GeoKey-level descriptor */

  /* DOC: */
  /* TIFFTAG_GEOTIEPOINTS, &count, &values); */
  /* TIFFGetField(tiff, TIFFTAG_GEOPIXELSCALE, &count, &values); */
  /* TIFFGetField(tiff, TIFFTAG_GEOTRANSMATRIX, &count, &values); */

  /* TIFFGetField returns 0 on failure (as opposed to the more  */
  /* common convention of 0 on success). */

  /* count is the number of double precision values.  For tiepoints,  */
  /* count should always be an even multiple of 6.  For pixel scale,  */
  /* count should always be 3.  For the transformation matrix, count  */
  /* should always be 16. */

  /* For tiepoints, 'values' is ordered as  (i, j, k, x, y, z) for  */
  /* each tiepoint.  For pixel scale, values is ordered x, y, z.   */
  /* For the matrix, values provides a 4x4 matrix order as 4 left-to-right  */
  /* rows top-to-bottom.  libtiff allocates and cleans up the memory  */
  /* holding this array.  The caller is NOT responsible for freeing this */
  /* array. */

  if ((tiffp = TIFFOpen(fn, "rc")) == NULL){
    (void)sprintf(buf,"ERROR in GetTIFFTagGeo(): invalid TIFF file name \"%s\" \n", fn); errputstr(buf);
    return NULL;
  }
  //not used: gtif = GTIFNew(tiffp);

  if (strcmp(tagname,"TIFFTAG_GEOTIEPOINTS")==0){
    if (TIFFGetField(tiffp, TIFFTAG_GEOTIEPOINTS, &count, &data)==0){
      (void)sprintf(buf,"ERROR in *GetTIFFTagGeo(%s, %s): no tie points found\n", fn, tagname); errputstr(buf);
      erflag=1;
    }
#ifdef DEBUG
    else{
      printf("data[3]=%20.40g\n", data[3]);
      printf("data[4]=%20.40g\n", data[4]);
    }
#endif
  }
  else if (strcmp(tagname,"TIFFTAG_GEOPIXELSCALE")==0){
    if (TIFFGetField(tiffp, TIFFTAG_GEOPIXELSCALE, &count, &data)==0){
      (void)sprintf(buf,"ERROR in *GetTIFFTagGeo(%s, %s): no pixel scale found\n", fn, tagname); errputstr(buf);
      erflag=1;
    }
  }
  else if (strcmp(tagname,"TIFFTAG_GEOTRANSMATRIX")==0){
    if (TIFFGetField(tiffp, TIFFTAG_GEOTRANSMATRIX, &count, &data)==0){
      (void)sprintf(buf,"ERROR in *GetTIFFTagGeo(%s, %s): no transformation matrix found\n", fn, tagname); errputstr(buf);
      erflag=1;
    }
  }
  else{
    (void)sprintf(buf,"ERROR in *GetTIFFTagGeo(%s, %s): unrecognised tag\n", fn, tagname); errputstr(buf);
    erflag=1;
    }
  if (erflag){
    if (gtif!=NULL)
      GTIFFree(gtif);
    TIFFClose(tiffp);
    return NULL;
  }

  im=create_image(t_DOUBLE, count, 1, 1);
  if (im==NULL){
    if (gtif!=NULL)
      GTIFFree(gtif);
    TIFFClose(tiffp);
    return NULL;
  }

  ptr=(double *)GetImPtr(im);
  for (i=0; i<count; i++)
    ptr[i]=data[i];

  if (gtif!=NULL)
    GTIFFree(gtif);
  TIFFClose(tiffp);
  return im;
}


/*************************************************************************/
/*                                                                       */
ERROR_TYPE read_image_data2(FILE *fp, IMAGE *im, int x, int y, int inx, int scale)
{
  int nx;
  int bpp=0;
  int i, j, k, ny;
  UCHAR *p;

  p    = (UCHAR *)GetImPtr(im);
  nx   = GetImNx(im);
  ny   = GetImNy(im);

  switch(GetImDataType(im)){
  case t_UCHAR:
    bpp=1;
  case t_USHORT:
  case t_SHORT:
    if (bpp==0)
      bpp=2;
  case t_UINT32:
  case t_INT32:
  case t_FLOAT:
    if (bpp==0)
      bpp=4;
  case t_DOUBLE:
    if (bpp==0)
      bpp=8;
    /* printf("bpp=%d\n", bpp); */
    fseek(fp,(y*inx+x)*bpp,SEEK_CUR);
    if (scale==1){
      for (i=0; i<ny; i++){
	if ((fread((char*)GetImPtr(im)+i*nx*bpp, (int)bpp, nx, fp)) != nx){
	  (void)sprintf(buf,"ERROR in read_image_data2(): \
                      unable to read image data block\n"); errputstr(buf);
          return(ERROR);
	}
	fseek(fp,(inx-nx)*bpp,SEEK_CUR);
      }
    }
    else{ /* preview with 1 pixel out of scale pixels */
      p=(UCHAR *)GetImPtr(im);
      for (i=0; i<ny; i++){
	for (j=0; j<nx; j++){
	  for (k=0; k<bpp; k++)
	    *p++=getc(fp);
	  fseek(fp,(scale-1)*bpp,SEEK_CUR);
	}
	fseek(fp,-bpp*(nx*scale),SEEK_CUR);
	fseek(fp,bpp*(scale*inx),SEEK_CUR);
      }
    }
    break;
  default:
    (void)sprintf(buf,"ERROR in read_image_data2(): \
                  invalid data type=%d\n", GetImDataType(im)); errputstr(buf);
    return(ERROR);
    break;
  }
  return(NO_ERROR);
}

/*
**  Function to read a raw TIFF file (no compression etc. supported)
*/
IMAGE *read_tiff_file2(FILE *fp, struct mytiff tiff_head, int x, int y, int szx, int szy, int scale)
{
  short int test = 0, nbr_tags;
  long int s_o_t=0;     /*  s_o_t: StripOffsetTag  */
  long int cm = 0;    /* offset for ColorMap if any */
  unsigned short int *plut;
  int i, nx=0, ny=0, nz=0, bitpp=0, data_type=t_UNSUPPORTED, pmi=0;
  struct tag *tag_info;
  int inx;
  IMAGE *im=NULL;

  (void)fseek(fp, tiff_head.ptr_dir1, 0);
  if (fread((char *)&nbr_tags, 2, 1, fp) != 1)
    return(NULL);
  if ((tag_info = (struct tag*) malloc(sizeof(struct tag)* nbr_tags)) == NULL)
    return(NULL);
  if (fread((char *)tag_info, 12, nbr_tags, fp) !=  nbr_tags)
    return(NULL);

#if BYTE_ORDER==LITTLE_ENDIAN
  /*  Read indispensable tags  */
  for (i = 0; i < nbr_tags; i++){
    switch (tag_info[i].type){
      case 256:      /*  ImageWidth tag    */
        nx = tag_info[i].ValOrPoint;
        test++;
        break;
      case 257:      /*  ImageLength tag   */
        ny = tag_info[i].ValOrPoint;
        test++;
        break;
      case 258:      /*  BitPerSample tag  */
        bitpp = tag_info[i].ValOrPoint;
        test++;
        break;
      case 273:      /*  StripOffset tag   */
        s_o_t = tag_info[i].ValOrPoint;
        test++;
        break;
    }
  }

  /*  Reading facultatif tags  */
  nz = 1;    /*  2D image by default  */
  for (i = 0; i < nbr_tags; i++){
    switch (tag_info[i].type){
      case 262: /*  PhotometricInterpretation */
	pmi = tag_info[i].ValOrPoint;
	break;
      case 320: /* ColorMap */
	cm = tag_info[i].ValOrPoint;
        break;
      case 1000:
      case 32768:
        nz = tag_info[i].ValOrPoint;
	break;
    }
  }
#else /* big-endian */
  /*  Read indispensable tags  */
  for (i = 0; i < nbr_tags; i++){
    switch (tag_info[i].type){
      case 256:      /*  ImageWidth tag     */
        nx = tag_info[i].ValOrPoint>>16;
        test++;
        break;
      case 257:      /*  ImageLength tag    */
        ny = tag_info[i].ValOrPoint>>16;
        test++;
        break;
      case 258:      /*  BitPerSample tag   */
        bitpp = tag_info[i].ValOrPoint>>16;
        test++;
        break;
      case 273:      /*  StripOffset tag    */
        s_o_t = tag_info[i].ValOrPoint;
        test++;
        break;
    }
  }

  /*  Reading facultatif tags  */
  nz = 1;    /*  2D image by default  */
  for (i = 0; i < nbr_tags; i++){
    switch (tag_info[i].type){
      case 262: /*  PhotometricInterpretation */
	pmi = tag_info[i].ValOrPoint>>16;
	break;
      case 320: /* ColorMap */
	cm = tag_info[i].ValOrPoint;
        break;
      case 1000:
      case 32768:
        nz = tag_info[i].ValOrPoint>>16;
	break;
    }
  }
#endif /* big-endian */
  free((char *)tag_info);

  if (test != 4){
    (void)sprintf(buf, "read_tiff_file(): Tags missing !!!  Unable to read file on disk.\n"); errputstr(buf);
    return(NULL);
  }
  if (bitpp == 1)
    data_type = t_TIFFONEBITPERPIXEL;
  else if (bitpp == 4)
    data_type = t_FOURBITPERPIXEL;
  else if (bitpp == BITPERCHAR)
    data_type = t_UCHAR;
  else if (bitpp == BITPERSHORT)
    data_type = t_USHORT;
  else if (bitpp == BITPERINT32)
    data_type = t_INT32;
  else if (bitpp == BITPERDOUBLE)
    data_type = t_DOUBLE;

  /* Allow for color images  */
  if (pmi == 2)
    data_type = t_RGB;

  /* check block to read */
  if ( (x+szx > nx) || (y+szy > ny) || (nz != 1) ){ /* invalid parameters */
    (void)sprintf(buf,"read_tiff_file(): invalid block parameters x=%d y=%d szx=%d szy=%d nx=%d ny=%d nz=%d!\n", x, y, szx, szy, nx, ny, nz); errputstr(buf);
    return NULL;
  }
  else if (szx==0 || szy==0){ /* default: read all image removed on 8 Apr 2003  || data_type != t_UCHAR */
    x=0; y=0;
    inx=nx;
    nx=inx/scale; ny/=scale;
  }
  else{ /* block size */
    inx=nx;
    nx=szx; ny=szy;
  }

  /* create output image */
  im = create_image(data_type, nx, ny, nz);
  if (im == NULL){
    (void)sprintf(buf,"read_tiff_file2(): not enough memory!\n"); errputstr(buf);
    return(NULL);
  }

  /*  Read image data  */
  (void)fseek(fp, s_o_t, 0);  /*  position file pointer to pixel map  */

  /* read image data */
  if (read_image_data2(fp, im, x, y, inx, scale) != NO_ERROR){
    free_image(im);
    return(NULL);
  }

  /* read Color Map if any */
  if (cm!=0){
    plut = (unsigned short int *)malloc(3*(2<<bitpp)*sizeof(short));
    if (plut!=NULL){
      SetImLut(im,plut);
      (void)fseek(fp, cm, 0);
      if (fread((char *)plut, sizeof(short), 3*(2<<bitpp), fp) != 3*(2<<bitpp)){
	(void)sprintf(buf,"read_tiff_file2(): could not read color map!\n"); errputstr(buf);
	free_lut(im);
      }
    }
  }
  else
    SetImLut(im,NULL);
  return(im);
}

/*
**  Function to read TIFF, VISILOG, or KIFF image files.
*/
IMAGE *read_image2(char *fn, int x, int y, int szx, int szy, int scale)
{
  FILE *fp;
  IMAGE *im=NULL;

  struct mytiff tiff_head;

  /* Open file and try */
  if ((fp = fopen(fn, "rb")) == NULL){
    (void)sprintf(buf,"ERROR in read_image(): \
                   unable to read open file\n"); errputstr(buf);
    return(NULL);
  }


  /*  Check whether it is a TIFF file  */
  (void)fseek(fp, 0L, 0);
  if (fread((char *)&tiff_head, sizeof(tiff_head), 1, fp) != 1){
    (void) fclose(fp);
    return(NULL);
  }
  if (tiff_head.byte_order == 0x4d4d){
#if BYTE_ORDER==BIG_ENDIAN
    im = read_tiff_file2(fp, tiff_head, x, y, szx, szy, scale);
#else
    (void)sprintf(buf,"read_file(): cannot read big-endian TIFF files on little-endian machines\n"); errputstr(buf);
    im = NULL;
#endif
  }
  else if (tiff_head.byte_order == 0x4949){
#if BYTE_ORDER==LITTLE_ENDIAN
    im = read_tiff_file2(fp, tiff_head, x, y, szx, szy, scale);
#else
    (void)sprintf(buf,"read_file(): cannot read little-endian TIFF files on big-endian machines\n"); errputstr(buf);
    im = NULL;
#endif
  }
  else{
    (void)sprintf(buf,"read_file(): unable to read %s on disk\n", fn); errputstr(buf);
  }

  (void) fclose(fp);
  return(im);
}


IMAGE *readTiffSubset(char *fn, int x, int y, unsigned szx, unsigned szy)
{
    TIFF *tif;
    UCHAR *pim = NULL, *ptmp = NULL;
    unsigned long stripCount;
    uint16 bps, spp, rps, comp, sf, bpp;
    uint32 nx, ny;
    int data_type;
    IMAGE *im=NULL;

    tif = TIFFOpen(fn, "r");
    if (tif==NULL){
      (void)sprintf(buf,"readTiffSubset(%s,...): unable to open file\n", fn);
      errputstr(buf);
      return NULL;
    }

    TIFFGetField(tif, TIFFTAG_SAMPLESPERPIXEL, &spp);
    if (spp!=1){
      TIFFClose(tif);
      (void)sprintf(buf,"readTiffSubset(%s,...): SamplePerPixel must be equal to 1\n", fn);
      errputstr(buf);
      return NULL;
    }

    TIFFGetField(tif, TIFFTAG_ROWSPERSTRIP, &rps);
    if (rps!=1){
      TIFFClose(tif);
      (void)sprintf(buf,"readTiffSubset(%s,...): RowsPerStrip must be equal to 1\n", fn);
      errputstr(buf);
      return NULL;
    }

    TIFFGetField(tif, TIFFTAG_BITSPERSAMPLE, &bps);
    if (bps<8){
      TIFFClose(tif);
      (void)sprintf(buf,"readTiffSubset(%s,...): BitPerSample must be greater or equal to 8\n", fn);
      errputstr(buf);
      return NULL;
    }

    TIFFGetField(tif, TIFFTAG_COMPRESSION, &comp);
    TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &nx);
    TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &ny);

    if ( (szx>nx) || (szy>ny) ){
      TIFFClose(tif);
      (void)sprintf(buf,"readTiffSubset(%s,...): subset not included in input image\n", fn);
      errputstr(buf);
      return NULL;
    }

    if (TIFFGetField(tif, TIFFTAG_SAMPLEFORMAT, &sf) != 1)
      sf=1; /* default: unsigned integer data */
    if (bps == BITPERCHAR)
      data_type = t_UCHAR;
    else if ( (bps == BITPERSHORT) && (sf==1) )
      data_type = t_USHORT;
    else if ( (bps == BITPERSHORT) && (sf==2) )
      data_type = t_SHORT;
    else if ( (bps == BITPERINT32)  && (sf==1) )
      data_type = t_UINT32;
    else if ( (bps == BITPERINT32)  && (sf==2) )
      data_type = t_INT32;
    else if ( (bps == BITPERINT32)  && (sf==3) )
      data_type = t_FLOAT;
    else if ( (bps == BITPERDOUBLE) && (sf==3) )
      data_type = t_DOUBLE;
    else{
      TIFFClose(tif);
      (void)sprintf(buf,"readTiffSubset(%s,...): Unsupported pixel type \
                         (BitsPerSample=%d\tSampleFormat=%d)\n",fn,bps,sf);
      errputstr(buf);
      return NULL;
    }

    im=create_image(data_type, szx, szy, 1);
    if (im == NULL){
      TIFFClose(tif);
      (void)sprintf(buf,"readTiffSubset(%s,...): not enough memory\n", fn); errputstr(buf);
      return(NULL);
    }
    bpp=GetImBitPerPixel(im)/8; /* byte(s) per pixel */
    pim=(UCHAR *)GetImPtr(im);

    ptmp=(UCHAR *)malloc(nx*bpp);
    if (ptmp == NULL){
      TIFFClose(tif);
      free_image(im);
      (void)sprintf(buf,"readTiffSubset(%s,...): not enough memory\n", fn); errputstr(buf);
      return(NULL);
    }

    for(stripCount=0; stripCount<szy; stripCount++){
      if (TIFFReadEncodedStrip(tif, stripCount+y, ptmp, nx*bpp) == -1){
	TIFFClose(tif);
	(void)sprintf(buf,"readTiffSubset(%s,...): unable to write %ld th strip on disk, \
                      writing aborted\n", fn, stripCount);
        errputstr(buf);
	return NULL;
      }
      memcpy((void*)pim+(szx*bpp)*stripCount,(void*)ptmp+x*bpp,szx*bpp);
    }
    TIFFClose(tif);
    free(ptmp);
    return im;
}

ERROR_TYPE writeGeoTiffOneStripPerLine(IMAGE *im, char *fn, int PCSCode, double xoff, double yoff, double scale, unsigned short RasterType, int nodata_flag, int nodata_val, int metadata_flag, char *metadata_str)
{
  /* see also geotifcp.c, libgeotiff */
    TIFF *tif=(TIFF*)0;
    GTIF *gtif=(GTIF*)0;
    double pixsize[3], tiepoint[6];
    void *lbuf;
    /* char imdesc[512] = MYBANNER; */
    char imdesc[512] = { [0 ... 511] = ' ' };
    char mode[10];
    char *mp=mode;

    char *user_str=getenv("USER");
    char *localhost_str;
    char nodata_str[11]; /* maximum size is number of digits of 2^32 */
    if (GdalTiffTags_flag==0){
      InstallGDALTiffTags();
      GdalTiffTags_flag=1;
    }

    *mp++ = 'w';
    *mp='\0';

    pixsize[0]  = scale;
    pixsize[1]  = scale;
    pixsize[2]  = 0.0;

    if ( (RasterType!=1) && (RasterType!=2) ){
      (void)sprintf(buf,"writetiffospl(): invalid raster type (%d)\n", RasterType);
      errputstr(buf);
      return ERROR;
    }
    tiepoint[0] = 0.0;  /* would be 0.5 if xoff represents centre of pixel??? */
    tiepoint[1] = 0.0;  /* would be 0.5 if xoff represents centre of pixel??? */

    tiepoint[2] = 0.0;
    tiepoint[3] = xoff;
    tiepoint[4] = yoff;
    tiepoint[5] = 0.0;

    int nx  = GetImNx(im);
    int ny  = GetImNy(im);
    int nz  = GetImNz(im);
    char *p = (char *)GetImPtr(im);
    int bpp = GetImBitPerPixel(im)/8;
    int stripCount, sf;
    char timeString[20];

    /* if size of image data is larger than 4GB than assume BigTIFF even
       though the compressed data may be less than 4GB */
    if ( ((long int) nx * (long int) ny * (long int) nz * bpp) > UINT32_MAX ){
      (void)printf("writetiffospl() message: output file %s will be a bigtiff file\n", fn);
      *mp++ = '8'; *mp = '\0';
    }

    tif = XTIFFOpen(fn, mode); /* X necessary for geotags */
    if (tif==NULL){
      (void)sprintf(buf,"writetiffospl(): unable to write %s on disk\n", fn);
      errputstr(buf);
      return ERROR;
    }
    gtif = GTIFNew(tif);
    if (!gtif){
      TIFFClose(tif);
      (void)sprintf(buf,"writeGeoTiffOneStripPerLine(): error when calling GTIFNew()\n");
      errputstr(buf);
      return ERROR;
    }

    if (PCSCode==4326){  /* ugly since it is NOT projected, but refers to  GeographicTypeGeoKey */
      GTIFKeySet(gtif, GTModelTypeGeoKey, TYPE_SHORT, 1, 2); /* 2 for Geographic latitude-longitude System */
      GTIFKeySet(gtif, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterType); /* 1 for RasterPixelIsArea */
      GTIFKeySet(gtif, GeogAngularUnitsGeoKey, TYPE_SHORT,  1, 9102); // 9102 for Angular_Arc_Degree
      GTIFKeySet(gtif, GeographicTypeGeoKey, TYPE_SHORT, 1, PCSCode); /* 32767 for user-defined */
    }
    else if (PCSCode==65535){  /* assumes wgs84 */
      GTIFKeySet(gtif, GTModelTypeGeoKey, TYPE_SHORT, 1, 2); /* 2 for Geographic latitude-longitude System */
      GTIFKeySet(gtif, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterType); /* 1 for RasterPixelIsArea */
      // GTIFKeySet(gtif, GeographicTypeGeoKey, TYPE_SHORT, 1, 32767); /* 32767 for user-defined */
      GTIFKeySet(gtif, GeographicTypeGeoKey, TYPE_SHORT, 1, 4258); /* 32767 for user-defined */
      GTIFKeySet(gtif, GeogCitationGeoKey, TYPE_ASCII, 0, "wgs84");
      GTIFKeySet(gtif, GeogGeodeticDatumGeoKey,  TYPE_SHORT, 1, 32767); /* 32767 for user-defined */
      GTIFKeySet(gtif, GeogLinearUnitsGeoKey, TYPE_SHORT,  1, 9001);
      GTIFKeySet(gtif, GeogAngularUnitsGeoKey, TYPE_SHORT,  1, 9102); // 9102 for Angular_Arc_Degree
      GTIFKeySet(gtif, GeogEllipsoidGeoKey, TYPE_SHORT, 1, 32767); /* 32767 for user-defined */
      GTIFKeySet(gtif, GeogSemiMajorAxisGeoKey, TYPE_DOUBLE, 1, 6378137.0);
      GTIFKeySet(gtif, GeogInvFlatteningGeoKey, TYPE_DOUBLE, 1, 298.257222101);
      GTIFKeySet(gtif, GeogPrimeMeridianLongGeoKey, TYPE_DOUBLE, 1, 0.0);

      //GTIFKeySet(gtif, GeogAngularUnitsGeoKey, TYPE_SHORT,  1, 9104); // 9104 for Angular_Arc_Second
      //GTIFKeySet(gtif, GeogAngularUnitsGeoKey, TYPE_SHORT,  1, 9102); // 9102 for Angular_Arc_Degree
      //GTIFKeySet(gtif, GeogLinearUnitsGeoKey, TYPE_SHORT,  1, 9001);
    }
    else if ( (PCSCode!=3035) &&  (PCSCode!=65535) ) {
      GTIFKeySet(gtif, GTModelTypeGeoKey, TYPE_SHORT, 1, 1); /* 1 for projected */
      GTIFKeySet(gtif, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterType); /* 1 for RasterPixelIsArea */
      GTIFKeySet(gtif, GeogLinearUnitsGeoKey, TYPE_SHORT,  1, 9001);
      GTIFKeySet(gtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1, PCSCode);
    }
    else{
      GTIFKeySet(gtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1, PCSCode);
      /* 3035 (ETRS-LAEA) not yet recognised by ArcGis: let insert also all parameters without codes */

      GTIFKeySet(gtif, GTCitationGeoKey, TYPE_ASCII, 1, "JRC-EGCS"); /* name of the GeoTIFF scheme */
      GTIFKeySet(gtif, GTModelTypeGeoKey, TYPE_SHORT, 1, 1); /* 1 for projected */
      GTIFKeySet(gtif, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterType); /* 1 for RasterPixelIsArea */
      // GTIFKeySet(gtif, GeographicTypeGeoKey, TYPE_SHORT, 1, 32767); /* 32767 for user-defined */
      GTIFKeySet(gtif, GeographicTypeGeoKey, TYPE_SHORT, 1, 4258); /* ETRS 1989 */
      GTIFKeySet(gtif, GeogCitationGeoKey, TYPE_ASCII, 0, "ETRS89");
      // GTIFKeySet(gtif, GeogGeodeticDatumGeoKey,  TYPE_SHORT, 1, 32767); /* 32767 for user-defined */
      GTIFKeySet(gtif, GeogGeodeticDatumGeoKey,  TYPE_SHORT, 1, 6258); /* Euro. Terr. Ref. Sys. 1989 */
      // GTIFKeySet(gtif, GeogPrimeMeridianGeoKey, TYPE_DOUBLE, 1, 32767); /* 32767 for user-defined */
      GTIFKeySet(gtif, GeogPrimeMeridianGeoKey, TYPE_DOUBLE, 1, 8901); /* Greenwich */
      GTIFKeySet(gtif, GeogLinearUnitsGeoKey, TYPE_SHORT,  1, 9001);
      GTIFKeySet(gtif, GeogAngularUnitsGeoKey, TYPE_SHORT,  1, 9102); // 9102 for Angular_Arc_Degree
      // GTIFKeySet(gtif, GeogEllipsoidGeoKey, TYPE_SHORT, 1, 32767); /* 32767 for user-defined */
      GTIFKeySet(gtif, GeogEllipsoidGeoKey, TYPE_SHORT, 1, 7019); /* GRS 1980 */
      GTIFKeySet(gtif, GeogSemiMajorAxisGeoKey, TYPE_DOUBLE, 1, 6378137.0);
      GTIFKeySet(gtif, GeogInvFlatteningGeoKey, TYPE_DOUBLE, 1, 298.257222101);
      GTIFKeySet(gtif, GeogPrimeMeridianLongGeoKey, TYPE_DOUBLE, 1, 0.0);

      // GTIFKeySet(gtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1, 32767); /* 32767 for user-defined */

      // GTIFKeySet(gtif, PCSCitationGeoKey, TYPE_ASCII, 1, "ETRS-LAEA");
      GTIFKeySet(gtif, PCSCitationGeoKey, TYPE_ASCII, 1, "ETRS89 / ETRS-LAEA");

      /* 4326 for WGS_84 4258 for GCS_EUREF89 */
      /* GTIFKeySet(gtif, GeographicTypeGeoKey, TYPE_SHORT, 1, 4258); */

      GTIFKeySet(gtif, ProjectionGeoKey, TYPE_SHORT, 1, 32767);
      GTIFKeySet(gtif, ProjCoordTransGeoKey, TYPE_SHORT, 1, 10); /* 10 for CT_LambertAzimEqualArea */
      GTIFKeySet(gtif, ProjLinearUnitsGeoKey, TYPE_SHORT, 1, 9001); /* Linear_Metre=9001 */
      GTIFKeySet(gtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1, 4321000.0);
      GTIFKeySet(gtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1, 3210000.0);
      GTIFKeySet(gtif, ProjCenterLongGeoKey, TYPE_DOUBLE, 1, 10.0);
      GTIFKeySet(gtif, ProjCenterLatGeoKey, TYPE_DOUBLE, 1, 52.0);
    }

    GTIFWriteKeys(gtif);
    GTIFFree(gtif);

    TIFFSetField(tif, TIFFTAG_GEOPIXELSCALE, 3, pixsize);
    TIFFSetField(tif, TIFFTAG_GEOTIEPOINTS, 6, tiepoint);

    TIFFSetField(tif, TIFFTAG_IMAGEWIDTH, nx);
    TIFFSetField(tif, TIFFTAG_IMAGELENGTH, ny);
    TIFFSetField(tif, TIFFTAG_BITSPERSAMPLE, GetImBitPerPixel(im));
    TIFFSetField(tif, TIFFTAG_SAMPLESPERPIXEL, nz);
    if (nz > 1)
      TIFFSetField(tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_SEPARATE);
    else
      TIFFSetField(tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);

    TIFFSetField(tif, TIFFTAG_ROWSPERSTRIP, 1);
    TIFFSetField(tif, TIFFTAG_COMPRESSION, COMPRESSION_LZW);
    if ( GetImBitPerPixel(im) <= 16 )
      TIFFSetField(tif, TIFFTAG_PREDICTOR, 2); /* PREDICTOR_HORIZONTAL = 2 */
    if ( (GetImDataType(im)==t_RGB) || (GetImNz(im)==3) )
      TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
    else
      TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);

    switch (GetImDataType(im)){ /* set SampleFormat field value */
    case t_UCHAR:
    case t_USHORT:
    case t_UINT32:
    case t_RGB:
      sf=1; /* unsigned integer data */
      break;
    case t_SHORT:
    case t_INT32:
      sf=2; /* two's complement signed integer data */
      break;
    case t_FLOAT:
    case t_DOUBLE:
      sf=3; /* IEEE floating point data */
      break;
    default:/* undefined data format */
      TIFFClose(tif);
      (void)sprintf(buf,"writetiffospl(%s): undefined data format\n", fn);
      errputstr(buf);
      return ERROR;
    }
    TIFFSetField(tif, TIFFTAG_SAMPLEFORMAT, sf);
    TIFFSetField(tif, TIFFTAG_DATETIME, getCrtTimeString(timeString));
    TIFFSetField(tif, TIFFTAG_DOCUMENTNAME, fn);
#if (defined(XLISP))
    TIFFSetField(tif, TIFFTAG_SOFTWARE, "jipl-xlisp [mialisp]");
#elif (defined(PYTHON))
    TIFFSetField(tif, TIFFTAG_SOFTWARE, "jipl-python");
#else
    TIFFSetField(tif, TIFFTAG_SOFTWARE, "jipl");
#endif
    localhost_str = (char *)malloc(1024);
    if (localhost_str != NULL){
      if (gethostname(localhost_str, 1024)==0){
	TIFFSetField(tif, TIFFTAG_HOSTCOMPUTER, localhost_str);
      }
      free(localhost_str);
    }
    TIFFSetField(tif, TIFFTAG_IMAGEDESCRIPTION, imdesc);
    TIFFSetField(tif, TIFFTAG_ARTIST, user_str);

    if (nodata_flag==1){
      if (sf>2){
	printf("warning, nodata flag ignored: only allowed for integer data type\n");
      }
      else{
	sprintf(nodata_str, "%d", nodata_val);
	TIFFSetField(tif, TIFFTAG_GDAL_NODATA, &nodata_str);
      }
    }
    if (metadata_flag==1){
	TIFFSetField(tif, TIFFTAG_GDAL_METADATA, metadata_str);
    }

    lbuf=(void *)malloc(nx*bpp);
    if (lbuf==NULL){
      TIFFClose(tif);
      (void)sprintf(buf,"writetiffospl(%s): not enough memory for line buffer\n", fn);
      errputstr(buf);
      return ERROR;
    }
    for(stripCount=0; stripCount<ny*nz; stripCount++){
      memcpy(lbuf, p, nx*bpp); /* because TIFFWriteEncodedStrip actually modifies the data ! */
      if (TIFFWriteEncodedStrip(tif, stripCount, lbuf, nx*bpp) == -1){
	TIFFClose(tif);
	(void)sprintf(buf,"writetiffospl(): unable to write %d th strip on disk, writing aborted\n", stripCount);
        errputstr(buf);
	return ERROR;
      }
      p+=nx*bpp;
    }
    free(lbuf);
    TIFFClose(tif);
    return NO_ERROR;
}

ERROR_TYPE writeMBGeoTiffOneStripPerLine(IMAGE **imap, int nc, char *fn, int PCSCode, double xoff, double yoff, double scale, unsigned short RasterType, int nodata_flag, int nodata_val, int metadata_flag, char *metadata_str)
{
  /* see also geotifcp.c, libgeotiff */
    TIFF *tif=(TIFF*)0;
    GTIF *gtif=(GTIF*)0;
    double pixsize[3], tiepoint[6];
    void *lbuf;
    /* char imdesc[512] = MYBANNER; */
    char imdesc[512] = { [0 ... 511] = ' ' };
    char mode[10];
    char *mp=mode;

    IMAGE *im;

    char *user_str=getenv("USER");
    char *localhost_str;
    char nodata_str[11]; /* maximum size is number of digits of 2^32 */
    if (GdalTiffTags_flag==0){
      InstallGDALTiffTags();
      GdalTiffTags_flag=1;
    }

    im=imap[0];

    *mp++ = 'w';
    *mp='\0';

    pixsize[0]  = scale;
    pixsize[1]  = scale;
    pixsize[2]  = 0.0;


    if ( (RasterType!=1) && (RasterType!=2) ){
      (void)sprintf(buf,"writetiffospl(): invalid raster type (%d)\n", RasterType);
      errputstr(buf);
      return ERROR;
    }
    tiepoint[0] = 0.0;  /* would be 0.5 if xoff represents centre of pixel??? */
    tiepoint[1] = 0.0;  /* would be 0.5 if xoff represents centre of pixel??? */
    tiepoint[2] = 0.0;
    tiepoint[3] = xoff;
    tiepoint[4] = yoff;
    tiepoint[5] = 0.0;

    int nx  = GetImNx(im);
    int ny  = GetImNy(im);
    int z, nz  = nc;
    char *p = (char *)GetImPtr(im);
    int bpp = GetImBitPerPixel(im)/8;
    int stripCount, sf;
    char timeString[20];

    /* if size of image data is larger than 4GB than assume BigTIFF even
       though the compressed data may be less than 4GB */
    if ( ((long int) nx * (long int) ny * (long int) nz * bpp) > UINT32_MAX ){
      (void)printf("writetiffospl() message: output file %s will be a bigtiff file\n", fn);
      *mp++ = '8'; *mp = '\0';
    }

    tif = XTIFFOpen(fn, mode); /* X necessary for geotags */
    if (tif==NULL){
      (void)sprintf(buf,"writetiffospl(): unable to write %s on disk\n", fn);
      errputstr(buf);
      return ERROR;
    }
    gtif = GTIFNew(tif);
    if (!gtif){
      TIFFClose(tif);
      (void)sprintf(buf,"writeGeoTiffOneStripPerLine(): error when calling GTIFNew()\n");
      errputstr(buf);
      return ERROR;
    }

    if (PCSCode==4326){  /* ugly since it is NOT projected, but refers to  GeographicTypeGeoKey */
      GTIFKeySet(gtif, GTModelTypeGeoKey, TYPE_SHORT, 1, 2); /* 2 for Geographic latitude-longitude System */
      GTIFKeySet(gtif, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterType); /* 1 for RasterPixelIsArea */
      GTIFKeySet(gtif, GeogAngularUnitsGeoKey, TYPE_SHORT,  1, 9102); // 9102 for Angular_Arc_Degree
      GTIFKeySet(gtif, GeographicTypeGeoKey, TYPE_SHORT, 1, PCSCode); /* 32767 for user-defined */
    }
    else if (PCSCode==65535){  /* assumes wgs84 */
      GTIFKeySet(gtif, GTModelTypeGeoKey, TYPE_SHORT, 1, 2); /* 2 for Geographic latitude-longitude System */
      GTIFKeySet(gtif, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterType); /* 1 for RasterPixelIsArea */
      // GTIFKeySet(gtif, GeographicTypeGeoKey, TYPE_SHORT, 1, 32767); /* 32767 for user-defined */
      GTIFKeySet(gtif, GeographicTypeGeoKey, TYPE_SHORT, 1, 4258); /* 32767 for user-defined */
      GTIFKeySet(gtif, GeogCitationGeoKey, TYPE_ASCII, 0, "wgs84");
      GTIFKeySet(gtif, GeogGeodeticDatumGeoKey,  TYPE_SHORT, 1, 32767); /* 32767 for user-defined */
      GTIFKeySet(gtif, GeogLinearUnitsGeoKey, TYPE_SHORT,  1, 9001);
      GTIFKeySet(gtif, GeogAngularUnitsGeoKey, TYPE_SHORT,  1, 9102); // 9102 for Angular_Arc_Degree
      GTIFKeySet(gtif, GeogEllipsoidGeoKey, TYPE_SHORT, 1, 32767); /* 32767 for user-defined */
      GTIFKeySet(gtif, GeogSemiMajorAxisGeoKey, TYPE_DOUBLE, 1, 6378137.0);
      GTIFKeySet(gtif, GeogInvFlatteningGeoKey, TYPE_DOUBLE, 1, 298.257222101);
      GTIFKeySet(gtif, GeogPrimeMeridianLongGeoKey, TYPE_DOUBLE, 1, 0.0);

      //GTIFKeySet(gtif, GeogAngularUnitsGeoKey, TYPE_SHORT,  1, 9104); // 9104 for Angular_Arc_Second
      //GTIFKeySet(gtif, GeogAngularUnitsGeoKey, TYPE_SHORT,  1, 9102); // 9102 for Angular_Arc_Degree
      //GTIFKeySet(gtif, GeogLinearUnitsGeoKey, TYPE_SHORT,  1, 9001);
    }
    else if ( (PCSCode!=3035) &&  (PCSCode!=65535) ) {
      GTIFKeySet(gtif, GTModelTypeGeoKey, TYPE_SHORT, 1, 1); /* 1 for projected */
      GTIFKeySet(gtif, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterType); /* 1 for RasterPixelIsArea */
      GTIFKeySet(gtif, GeogLinearUnitsGeoKey, TYPE_SHORT,  1, 9001);
      GTIFKeySet(gtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1, PCSCode);
    }
    else{
      GTIFKeySet(gtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1, PCSCode);
      /* 3035 (ETRS-LAEA) not yet recognised by ArcGis: let insert also all parameters without codes */

      GTIFKeySet(gtif, GTCitationGeoKey, TYPE_ASCII, 1, "JRC-EGCS"); /* name of the GeoTIFF scheme */
      GTIFKeySet(gtif, GTModelTypeGeoKey, TYPE_SHORT, 1, 1); /* 1 for projected */
      GTIFKeySet(gtif, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterType); /* 1 for RasterPixelIsArea */
      // GTIFKeySet(gtif, GeographicTypeGeoKey, TYPE_SHORT, 1, 32767); /* 32767 for user-defined */
      GTIFKeySet(gtif, GeographicTypeGeoKey, TYPE_SHORT, 1, 4258); /* ETRS 1989 */
      GTIFKeySet(gtif, GeogCitationGeoKey, TYPE_ASCII, 0, "ETRS89");
      // GTIFKeySet(gtif, GeogGeodeticDatumGeoKey,  TYPE_SHORT, 1, 32767); /* 32767 for user-defined */
      GTIFKeySet(gtif, GeogGeodeticDatumGeoKey,  TYPE_SHORT, 1, 6258); /* Euro. Terr. Ref. Sys. 1989 */
      // GTIFKeySet(gtif, GeogPrimeMeridianGeoKey, TYPE_DOUBLE, 1, 32767); /* 32767 for user-defined */
      GTIFKeySet(gtif, GeogPrimeMeridianGeoKey, TYPE_DOUBLE, 1, 8901); /* Greenwich */
      GTIFKeySet(gtif, GeogLinearUnitsGeoKey, TYPE_SHORT,  1, 9001);
      GTIFKeySet(gtif, GeogAngularUnitsGeoKey, TYPE_SHORT,  1, 9102); // 9102 for Angular_Arc_Degree
      // GTIFKeySet(gtif, GeogEllipsoidGeoKey, TYPE_SHORT, 1, 32767); /* 32767 for user-defined */
      GTIFKeySet(gtif, GeogEllipsoidGeoKey, TYPE_SHORT, 1, 7019); /* GRS 1980 */
      GTIFKeySet(gtif, GeogSemiMajorAxisGeoKey, TYPE_DOUBLE, 1, 6378137.0);
      GTIFKeySet(gtif, GeogInvFlatteningGeoKey, TYPE_DOUBLE, 1, 298.257222101);
      GTIFKeySet(gtif, GeogPrimeMeridianLongGeoKey, TYPE_DOUBLE, 1, 0.0);

      // GTIFKeySet(gtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1, 32767); /* 32767 for user-defined */

      // GTIFKeySet(gtif, PCSCitationGeoKey, TYPE_ASCII, 1, "ETRS-LAEA");
      GTIFKeySet(gtif, PCSCitationGeoKey, TYPE_ASCII, 1, "ETRS89 / ETRS-LAEA");

      /* 4326 for WGS_84 4258 for GCS_EUREF89 */
      /* GTIFKeySet(gtif, GeographicTypeGeoKey, TYPE_SHORT, 1, 4258); */

      GTIFKeySet(gtif, ProjectionGeoKey, TYPE_SHORT, 1, 32767);
      GTIFKeySet(gtif, ProjCoordTransGeoKey, TYPE_SHORT, 1, 10); /* 10 for CT_LambertAzimEqualArea */
      GTIFKeySet(gtif, ProjLinearUnitsGeoKey, TYPE_SHORT, 1, 9001); /* Linear_Metre=9001 */
      GTIFKeySet(gtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1, 4321000.0);
      GTIFKeySet(gtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1, 3210000.0);
      GTIFKeySet(gtif, ProjCenterLongGeoKey, TYPE_DOUBLE, 1, 10.0);
      GTIFKeySet(gtif, ProjCenterLatGeoKey, TYPE_DOUBLE, 1, 52.0);
    }
    GTIFWriteKeys(gtif);
    GTIFFree(gtif);

    TIFFSetField(tif, TIFFTAG_GEOPIXELSCALE, 3, pixsize);
    TIFFSetField(tif, TIFFTAG_GEOTIEPOINTS, 6, tiepoint);

    TIFFSetField(tif, TIFFTAG_IMAGEWIDTH, nx);
    TIFFSetField(tif, TIFFTAG_IMAGELENGTH, ny);
    TIFFSetField(tif, TIFFTAG_BITSPERSAMPLE, GetImBitPerPixel(im));
    TIFFSetField(tif, TIFFTAG_SAMPLESPERPIXEL, nz);
    if (nz > 1)
      TIFFSetField(tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_SEPARATE);
    else
      TIFFSetField(tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);

    TIFFSetField(tif, TIFFTAG_ROWSPERSTRIP, 1);
    TIFFSetField(tif, TIFFTAG_COMPRESSION, COMPRESSION_LZW);
    if ( GetImBitPerPixel(im) <= 16 )
      TIFFSetField(tif, TIFFTAG_PREDICTOR, 2); /* PREDICTOR_HORIZONTAL = 2 */
    if ( (GetImDataType(im)==t_RGB) || (GetImNz(im)==3) )
      TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
    else
      TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);

    switch (GetImDataType(im)){ /* set SampleFormat field value */
    case t_UCHAR:
    case t_USHORT:
    case t_UINT32:
    case t_RGB:
      sf=1; /* unsigned integer data */
      break;
    case t_SHORT:
    case t_INT32:
      sf=2; /* two's complement signed integer data */
      break;
    case t_FLOAT:
    case t_DOUBLE:
      sf=3; /* IEEE floating point data */
      break;
    default:/* undefined data format */
      TIFFClose(tif);
      (void)sprintf(buf,"writetiffospl(%s): undefined data format\n", fn);
      errputstr(buf);
      return ERROR;
    }
    TIFFSetField(tif, TIFFTAG_SAMPLEFORMAT, sf);
    TIFFSetField(tif, TIFFTAG_DATETIME, getCrtTimeString(timeString));
    TIFFSetField(tif, TIFFTAG_DOCUMENTNAME, fn);
#if (defined(XLISP))
    TIFFSetField(tif, TIFFTAG_SOFTWARE, "jipl-xlisp [mialisp]");
#elif (defined(PYTHON))
    TIFFSetField(tif, TIFFTAG_SOFTWARE, "jipl-python");
#else
    TIFFSetField(tif, TIFFTAG_SOFTWARE, "jipl");
#endif
    TIFFSetField(tif, TIFFTAG_IMAGEDESCRIPTION, imdesc);
    localhost_str = (char *)malloc(1024);
    if (localhost_str != NULL){
      if (gethostname(localhost_str, 1024)==0){
	TIFFSetField(tif, TIFFTAG_HOSTCOMPUTER, localhost_str);
      }
      free(localhost_str);
    }
    TIFFSetField(tif, TIFFTAG_ARTIST, user_str);

    if (nodata_flag==1){
      if (sf>2){
	printf("warning, nodata flag ignored: only allowed for integer data type\n");
      }
      else{
	sprintf(nodata_str, "%d", nodata_val);
	TIFFSetField(tif, TIFFTAG_GDAL_NODATA, &nodata_str);
      }
    }
    if (metadata_flag==1){
	TIFFSetField(tif, TIFFTAG_GDAL_METADATA, metadata_str);
    }

    lbuf=(void *)malloc(nx*bpp);
    if (lbuf==NULL){
      TIFFClose(tif);
      (void)sprintf(buf,"writetiffospl(%s): not enough memory for line buffer\n", fn);
      errputstr(buf);
      return ERROR;
    }
    stripCount=0;
    for (z=0; z<nc; z++){
      p=(char *)GetImPtr(imap[z]);
      for(; stripCount<ny*(z+1); stripCount++){
	memcpy(lbuf, p, nx*bpp); /* because TIFFWriteEncodedStrip actually modifies the data ! */
	if (TIFFWriteEncodedStrip(tif, stripCount, lbuf, nx*bpp) == -1){
	  TIFFClose(tif);
	  (void)sprintf(buf,"writetiffospl(): unable to write %d th strip on disk, writing aborted\n", stripCount);
	  errputstr(buf);
	  return ERROR;
	}
	p+=nx*bpp;
      }
    }
    free(lbuf);
    TIFFClose(tif);
    return NO_ERROR;
}

/** @}*/
