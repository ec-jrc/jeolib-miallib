/***************************************************************************
                          vectorize.c  -  description
                             -------------------
  Vectorize multispectral high resolution satellite images.
  Output file formats are Scalabale Vector Graphics (SVG) and/or ESRI Shapefiles
                             
    begin                : Fri May 14 2004
    authors              : Dominik Brunner and Pierre.Soille@jrc.ec.europa.eu
    copyright            : (C) 2004 JRC
    email                : dominik.brunner@jrc.it and Pierre.Soille@jrc.ec.europa.eu
***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "borderdetection.h"
#include "writeSVG.h"
#include "writeShapeFile.h"
#include "base.h"
#include "mslabel.h"
#include "determineSize.h"
#include "simplifyLine.h"


/*
 *  vectorizeImage: Vectorize multispectral satellite images. Output file format is SVG and/or
 *  ESRI Shapefiles
 *
 *  Parameters:
 *
 *    inputImArray  array of pointer to the channels of an input images
 *
 *    nc            size of imArray => number of channels
 *
 *    filename      filename to the input image (is needed in order to read the geo informations, if available)
 *
 *    format        Output vector file format
 *                  0  (don't vectorize)
 *                  1  (SVG)
 *                  2  (ESRI Shapefile)
 *                  3  (SVG and ESRI Shapefile)
 *
 *    simplifyBorderLines
 *                  the bigger the value, the more simplified are the border lines.
 *                  A value of 0 means no simplification
 *
 *  Return values:
 *
 *   NO_ERROR       if everything is ok
 *
 *   ERROR          if error occured
 */
#include "uc_def.h"
ERROR_TYPE vectorizeImage(IMAGE **inputImArray, int nc, char *filename, int format, double simplifyBorderLines){
  int i;
  long int regionNumber, r, crtLabel;
  IMAGE * labelIm;
  struct REGION ** regions;
  struct LINEPOOL linepool;
  int box[6];
  LBL_TYPE * pLabelIm;
  PIX_TYPE * pInputIm;
  // initilize box for frame which shall be added in order to solve
  // problem with neighbours
  box[0] = 1;
  box[1] = 1;
  box[2] = 1;
  box[3] = 1;
  box[4] = 0;
  box[5] = 0;

  printf("vectorize() - start vectorisation \n");
  for (i = 0; i < nc; i++){
    if ((GetImNx(inputImArray[0]) != GetImNx(inputImArray[i])) \
	|| (GetImNy(inputImArray[0]) != GetImNy(inputImArray[i])) \
	|| (GetImDataType(inputImArray[i]) != t_UCHAR)){
      printf("Datatype not provided, or dimensions of image different!");
      return ERROR;
    }
  }
  labelIm = (IMAGE *) create_image(t_LBL_TYPE, GetImNx(inputImArray[0]), GetImNy(inputImArray[0]), 1);
  if (labelIm==NULL){
    return ERROR;
  }
  //labelImage
  if(labelImage(inputImArray, nc, labelIm, 4, 0)==NULL){
    free_image(labelIm);
    return ERROR;
  }
  pLabelIm = (LBL_TYPE *) GetImPtr(labelIm);
  //determineSize
  if((regionNumber = thresholdRegion_Size(labelIm, 1))==ERROR){
    free_image(labelIm);
    return ERROR;
  }
  //initialize Regions
  if((regions=calloc(regionNumber+1, sizeof(struct REGION *)))==NULL){
    printf("Not enough memory!!! - detectBorders()\n");
    return ERROR;
  }
  for(r=0; r<regionNumber+1; r++){
    if((regions[r]=malloc(sizeof(struct REGION)))==NULL){
      for(; r>=0; --r){
        printf("Not enough memory!!! - detectBorders()\n");
        freeRegion(regions[r]);
        free(regions[r]);
      }
      return ERROR;
    }
    if(!(initRegion(regions[r],10, 100, nc))){
      printf("Not enough memory!!! - detectBorders()\n");
      for(; r>=0; --r){
        printf("Not enough memory!!! - detectBorders()\n");
        freeRegion(regions[r]);
        free(regions[r]);
      }
      return ERROR;
    }
  }
  if(!(initLinePool(&linepool, 500))){
    for(r=0; r<regionNumber; r++){
      freeRegion(regions[r]);
      free(regions[r]);
    }
    return ERROR;
  }
  for(r=0; r<GetImNx(inputImArray[0]) * GetImNy(inputImArray[0]); r++){
    for(i=0; i<nc; i++){
      crtLabel = pLabelIm[r];
      pInputIm = (PIX_TYPE *) GetImPtr(inputImArray[i]);
      regions[crtLabel]->colorValues[i]=pInputIm[r];
    }
  }
  //make border detection
  if(detectBorders(labelIm, regions, regionNumber, &linepool)==ERROR){
    for(r=0; r<regionNumber; r++){
      freeRegion(regions[r]);
      free(regions[r]);
    }
    return ERROR;
  }       \
	    //use all points of lines
	    setUseFlags(&linepool, 1);
  //reduce points if necessary
  if(simplifyBorderLines>0){
    simplifyLine(&linepool, simplifyBorderLines, GetImNx(inputImArray[0]), GetImNy(inputImArray[0]));
  }
  if(format==1 || format==3){
    if(writeSVG(regions, regionNumber, filename, GetImNx(inputImArray[0]), GetImNy(inputImArray[0]))==ERROR){
      for(r=0; r<regionNumber; r++){
        freeRegion(regions[r]);
        free(regions[r]);
      }
      return ERROR;  
    }
  }
  if(format==2 || format==3){
    if(writeShapeFile(regions, regionNumber, filename)==ERROR){
      for(r=0; r<regionNumber; r++){
        freeRegion(regions[r]);
        free(regions[r]);
      }
      return ERROR;
    }
  } 
  for(r=0; r<regionNumber+1; r++){
    free(regions[r]);
  }
  free(regions);
  freeLinePool(&linepool);
  return NO_ERROR;
}
#include "uc_undef.h"
