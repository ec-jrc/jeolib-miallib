/***********************************************************************
Author(s): Pierre Soille
Copyright (C) 2012-2026 European Union (Joint Research Centre)

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
#include <math.h>
#include <gdal.h>
#include  <ogr_srs_api.h>

/* #include <cpl_conv.h>  for CPLMalloc() but I use standard malloc */

#include "miallib.h"


/** \addtogroup group_io
 *  @{
 */

/**
 * @synopsis Data type conversion from gdal to mial
 *
 * @param aGDALDataType integer for gdal data type
 *
 * @return integer with mial data type matching the input gdal data type
 *
 * @creationdate 20130911
 */
int GDAL2MIALDataType(int aGDALDataType)
{
  switch (aGDALDataType){
  case GDT_Byte:
    return t_UCHAR;
  case GDT_UInt16:
    return t_USHORT;
  case GDT_Int16:
    return t_SHORT;
  case GDT_UInt32:
    return t_UINT32;
  case GDT_Int32:
    return t_INT32;
  case GDT_Float32:
    return t_FLOAT;
  case GDT_UInt64:
    return t_UINT64;
  case GDT_Int64:
    return t_INT64;
  case GDT_Unknown:
    return t_UNSUPPORTED;
  default:
    return t_UNSUPPORTED;
  }
}


/**
 * @synopsis read a single band of a raster image using drivers installed in gdal library
 *
 * @param fn a string for the name of an image file (possibly including its path)
 * @param band an integer for the band number, 0 for first band
 * @param nXOff:  The pixel offset to the top left corner of the region of the band to be accessed.  This would be zero to start from the left side (default value).
 * @param nYOff: The line offset to the top left corner of the region of the band to be accessed.  This would be zero to start from the top (default value).
 * @param nXSize: The width of the region of the band to be accessed in pixels.
 * @param nYSize: The height of the region of the band to be accessed in lines.
 * @param nBufXSize: integer for number of columns of output image (default is nXSize).
 * @param nBufYSize: integer for number of lines of output image (default is nYSize)
 * @desc read a given band of a raster image using gdal with the specified options.  Any invalid geometry parameter will result in the whole image domain to be read!
 */
IMAGE *GDALRead(char *imfn, int band, int nXOff, int nYOff, int nXSize, int nYSize, int nBufXSize, int nBufYSize)
{
  /*
  ** Input Parameters:
  **   char *imfn: file name possibly including path
  **   int band: index of band to read (0 for first band).
  **   int nXOff:  The pixel offset to the top left corner of the region of the band to be accessed.
  **               This would be zero to start from the left side.
  **   int nYOff: The line offset to the top left corner of the region of the band to be accessed.
  **                This would be zero to start from the top.
  **   int nXSize: The width of the region of the band to be accessed in pixels.
  **   int nYSize: The height of the region of the band to be accessed in lines.
  **   int nBufXSize: the width of the buffer image into which the desired region is to be read,
                      or from which it is to be written.
  **   int nBufYSize: the height of the buffer image into which the desired region is to be read,
                      or from which it is to be written.
  **
  **
  ** Return: an image on success, NULL otherwise
  **
  ** Comment: any invalid geometry parameter will result in the whole image domain to be read!
  **
  ** Authors: Pierre Soille
  */
  GDALDatasetH  hDataset;
  GDALRasterBandH hBand;
#if DEBUG
  GDALDriverH   hDriver;
  double        adfGeoTransform[6];
  int           bGotMin, bGotMax;
  double        adfMinMax[2];
  int           nBlockXSize, nBlockYSize;
#endif

  void *imptr;
  IMAGE *im;

  GDALAllRegister();

  /* opening the image file */
  hDataset = GDALOpenEx( imfn, GDAL_OF_RASTER | \
  			 GDAL_OF_READONLY | \
  			 GDAL_OF_VERBOSE_ERROR, \
  			 NULL, NULL, NULL );
  if( hDataset == NULL )
      return NULL;

#if DEBUG
  printf("IMAGE FILE OPENED SUCCESSFULLY by GDALOpen\n");

  /* printing some information */
  hDriver = GDALGetDatasetDriver( hDataset );
  printf( "Driver: %s/%s\n",
	  GDALGetDriverShortName( hDriver ),
	  GDALGetDriverLongName( hDriver ) );

  printf( "Size is %dx%dx%d\n",
	  GDALGetRasterXSize( hDataset ),
	  GDALGetRasterYSize( hDataset ),
	  GDALGetRasterCount( hDataset ) );

  if( GDALGetProjectionRef( hDataset ) != NULL )
    printf( "Projection is `%s'\n", GDALGetProjectionRef( hDataset ) );

  if( GDALGetGeoTransform( hDataset, adfGeoTransform ) == CE_None ){
      printf( "Origin = (%.6f,%.6f)\n",
	      adfGeoTransform[0], adfGeoTransform[3] );

      printf( "Pixel Size = (%.6f,%.6f)\n",
	      adfGeoTransform[1], adfGeoTransform[5] );
    }
  printf("nXOff=%d nYOff=%d nXSize=%d nYSize=%d\n", nXOff, nYOff, nXSize, nYSize);
#endif

  band+=1;

  if ((band <1) || (band > GDALGetRasterCount( hDataset ))){
    (void)sprintf(buf, "GDALRead(): invalid band number\n");
    errputstr(buf);
    GDALClose(hDataset);
    return NULL;
  }

  /* if no patch, == should be replaced by < */
  if ( (nXOff<0) || (nYOff<0) || (nXSize==0) || (nYSize==0) ){
     nXOff=nYOff=0;
     nXSize= GDALGetRasterXSize( hDataset );
     nYSize= GDALGetRasterYSize( hDataset );
  }
  if ( (nBufXSize<=0) ||  (nBufYSize<=0) ){
    nBufXSize=nXSize;
    nBufYSize=nYSize;
  }

  if ( ((nXOff+nXSize)>GDALGetRasterXSize( hDataset ))	\
       || ((nYOff+nYSize)>GDALGetRasterYSize( hDataset ))){
    (void)sprintf(buf, "GDALRead(): nXOff or nXSize (resp. Y.) parameters out of permissible range\n");
    errputstr(buf);
    (void)sprintf(buf, "nXOff=%d nYOff=%d nXSize=%d nYSize=%d\n", nXOff, nYOff, nXSize, nYSize);
    errputstr(buf);
    GDALClose(hDataset);
    return NULL;
  }

  /* Fetching a Raster Band */
  hBand = GDALGetRasterBand( hDataset, band );

  if (GDAL2MIALDataType(GDALGetRasterDataType(hBand)) == t_UNSUPPORTED){
    (void)sprintf(buf, "GDALRead(): invalid type(=%d), detaching shared memory ...\n", GDAL2MIALDataType(GDALGetRasterDataType(hBand)));
    errputstr(buf);
    GDALClose(hDataset);
    return NULL;
  }

#if DEBUG
  printf("gdalread(): nXSize=%d, nYSize=%d, nXOff=%d nYOff=%d band=%d type=%d\n  nBufXSize=%d, nBufYSize=%d\n", nXSize, nYSize, nXOff, nYOff, band,GDAL2MIALDataType(GDALGetRasterDataType(hBand)), nBufXSize, nBufYSize);
  GDALGetBlockSize( hBand, &nBlockXSize, &nBlockYSize );
  (void)sprintf(buf, "Block=%dx%d Type=%s, ColorInterp=%s\n",
	  nBlockXSize, nBlockYSize,
	  GDALGetDataTypeName(GDALGetRasterDataType(hBand)),
	  GDALGetColorInterpretationName(GDALGetRasterColorInterpretation(hBand)));
  stdputstr(buf);
#endif

  /* note: even if a subset is read, is is inserted in the full raster
     (limitation of GDALRasterIO) */
  im = create_image(GDAL2MIALDataType(GDALGetRasterDataType(hBand)), \
		    nBufXSize,		      \
		    nBufYSize,		      \
		    1);
  if (im == NULL){
    (void)sprintf(buf,"GDALRead(): not enough memory!\n"); errputstr(buf);
    GDALClose(hDataset);
    return NULL;
  }

#ifdef DEBUG
  adfMinMax[0] = GDALGetRasterMinimum( hBand, &bGotMin );
  adfMinMax[1] = GDALGetRasterMaximum( hBand, &bGotMax );
  if( ! (bGotMin && bGotMax) )
    GDALComputeRasterMinMax( hBand, TRUE, adfMinMax );
  printf( "Min=%.3fd, Max=%.3f\n", adfMinMax[0], adfMinMax[1] );

  if( GDALGetOverviewCount(hBand) > 0 )
    printf( "Band has %d overviews.\n", GDALGetOverviewCount(hBand));

  if( GDALGetRasterColorTable( hBand ) != NULL )
    printf( "Band has a color table with %d entries.\n",
	    GDALGetColorEntryCount( GDALGetRasterColorTable( hBand ) ) );
#endif

  /* Reading Raster Data */
  imptr = GetImPtr(im);
  if (GDALRasterIO( hBand, GF_Read, nXOff, nYOff, nXSize, nYSize,    \
		    imptr, nBufXSize, nBufYSize,		     \
		    GDALGetRasterDataType(hBand), 0, 0) != CPLE_None){
    (void)sprintf(buf,"GDALRead(): call to GDALRasterIO() returns error!\n"); errputstr(buf);
    GDALClose(hDataset);
    free_image(im);
    return NULL;
  }

  GDALClose(hDataset);
  return im;
}

/**
 * The values are in the following order: x-coordinate of upper left corner of upper left pixel, W-E pixel resolution, rotation (0 if image is north up), x-coordinate of upper left corner of upper left pixel, rotation (0 if image is north up), N-S pixel resolution, number of rows of raster, number of columns of raster, number of bands of raster, and EPSG code of projection (-1 if it could not be retrieved).  The function also prints in stdout the type of driver used for reading fn and the name of the projection.
 *
 * @param imfn string for image file name
 *
 * @return an image of type t_DOUBLE holding 10 values regarding the geolocation and size of the image, NIL otherwise
 */
IMAGE *GDALInfoJIP(char *imfn)
{
  GDALDatasetH  hDataset;
  GDALDriverH   hDriver;
  double        adfGeoTransform[6];

  IMAGE *im=NULL;
  double *ptr=NULL;

  OGRSpatialReferenceH hSRS=NULL;

  GDALAllRegister();
  hDataset = GDALOpen( imfn, GA_ReadOnly );
  if( hDataset == NULL )
    return NULL;

  hDriver = GDALGetDatasetDriver( hDataset );
  (void)sprintf(buf, "GDALInfoJIP(): Driver: %s/%s\n",
		GDALGetDriverShortName( hDriver ),
		GDALGetDriverLongName( hDriver ) );
  stdputstr(buf);

  (void)sprintf(buf, "GDALInfoJIP(): Size is %dx%dx%d\n",
		GDALGetRasterXSize( hDataset ),
		GDALGetRasterYSize( hDataset ),
		GDALGetRasterCount( hDataset ) );
  stdputstr(buf);

  if( GDALGetProjectionRef( hDataset ) != NULL ){
    (void)sprintf(buf, "Projection is `%s'\n", GDALGetProjectionRef( hDataset ) );
    stdputstr(buf);
  }

  if( GDALGetGeoTransform( hDataset, adfGeoTransform ) == CE_None ){
    (void)sprintf(buf, "GDALInfoJIP(): Origin = (%.6f,%.6f)\n",
		  adfGeoTransform[0], adfGeoTransform[3] );
    stdputstr(buf);
    (void)sprintf(buf, "GDALInfoJIP(): Pixel Size = (%.6f,%.6f)\n",
		  adfGeoTransform[1], adfGeoTransform[5] );
    stdputstr(buf);
  }

  im=create_image(t_DOUBLE, 10, 1, 1);
  if (im==NULL){
    GDALClose(hDataset);
    return NULL;
  }

  ptr=(double *)GetImPtr(im);

  ptr[0]=adfGeoTransform[0]; /* top left x */
  ptr[1]=adfGeoTransform[1]; /* w-e pixel resolution */
  ptr[2]=adfGeoTransform[2]; /* rotation, 0 if image is "north up" */
  ptr[3]=adfGeoTransform[3]; /* top left y */
  ptr[4]=adfGeoTransform[4]; /* rotation, 0 if image is "north up" */
  ptr[5]=fabs(adfGeoTransform[5]); /* n-s pixel resolution */
  ptr[6]=GDALGetRasterXSize( hDataset ); /* image width in number of pixels */
  ptr[7]=GDALGetRasterYSize( hDataset ); /* image length in number of pixels */
  ptr[8]=GDALGetRasterCount( hDataset ); /* number of image bands */
  ptr[9]=-1;
  hSRS= OSRNewSpatialReference(GDALGetProjectionRef( hDataset ));
  if (hSRS!=NULL){
    if(OSRGetAttrValue(hSRS, "AUTHORITY", 1)!=NULL){
      (void)sprintf(buf, "GDALInfoJIP(): EPSG=%s\n", OSRGetAttrValue(hSRS, "AUTHORITY", 1));
      stdputstr(buf);
      ptr[9]=(double)atoi(OSRGetAttrValue(hSRS, "AUTHORITY", 1));
    }
    else {
      (void)sprintf(buf, "GDALInfoJIP(): warning: getting GEOGCS EPSG code failed\n");
      stdputstr(buf);
    }
  }
  else{
    (void)sprintf(buf, "GDALInfoJIP(): warning: getting GEOGCS EPSG code failed\n");
    stdputstr(buf);
  }

    GDALClose(hDataset);

    return im;
}

/** @}*/
