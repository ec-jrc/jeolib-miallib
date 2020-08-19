/***********************************************************************
Author(s): Dominik Brunner and Pierre Soille
Copyright (C) 2004-2020 European Union (Joint Research Centre)

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

/***************************************************************************
                          writeSVG.c  -  description
                             -------------------

 Vectorizes the boundary lines from the region. The output file format is SVG.
 The geoinformations will be considered for the output (if available)

    begin                : Fri May 14 2004
***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <zlib.h>

#define NO_BUF
#include "borderdetection.h"
#include "writeSVG.h"
#include "miallib.h"
#include "base.h"
#include <geotiff/xtiffio.h>
#include <tiffio.h>

/*
 *  writeSVG: write the boundary lines o fregions as SVG
 *
 *  Parameters:
 *
 *    regions       REGIONS with their exact boundary line segments
 *
 *    regionNumber  size of regions array => number of regions
 *
 *    filename      filename to the input image (is needed in order to read the geo informations, if available)
 *
 *    width         width of image (needed to make e.g. viewport settings in SVG)
 *
 *    height        height of image (needed to make e.g. viewport settings in SVG)
 *
 *  Return values:
 *
 *   NO_ERROR       if everything is ok
 *
 *   ERROR          if error occured
 */
ERROR_TYPE writeSVG(struct REGION ** regions, int regionNumber, char * fileName, int width, int height)
{
  char buffer[200], outputFName[1000];
  long int crtPos;
  int i;
  gzFile gzFhd;
  TIFF *tif=(TIFF*)0;
  GTIF *gtif=(GTIF*)0; /* GeoKey-level descriptor */
  double x=0, y=0;
  double widthPCS, heightPCS, widthPCSZero, heightPCSZero;
  GDALDatasetH hDataset;
  char *pszProjection;
  strncpy( outputFName, fileName, sizeof(outputFName)-4 );
  for( i = strlen(outputFName)-1; i > 0; i-- )
    {
      if( outputFName[i] == '.' )
	{
	  strcpy( outputFName + i, ".svgz" );
	  break;
	}
    }
  if( i <= 0 ) strcat( outputFName, ".svgz" );
  printf("write SVG file: %s", outputFName);

  //open geotiff => use for vectorization geo information
  tif=XTIFFOpen(fileName,"r");
  gtif = GTIFNew(tif);
  if(!gtif) gtif=NULL;
  if(!GTIFImageToPCS(gtif, &x, &y)){
    gtif=NULL;
  }
  crtPos=SEED;
  gzFhd=gzopen(outputFName, "w");
  sprintf(buffer, "<?xml version=\"1.0\" standalone=\"no\"?>\n");
  gzputs(gzFhd, buffer);
  sprintf(buffer, "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.0//EN\" \"http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg.dtd\">\n");
  gzputs(gzFhd, buffer);

  if(gtif!=NULL){
    widthPCSZero = 0;
    heightPCSZero = 0;

    GTIFImageToPCS(gtif, & widthPCSZero, & heightPCSZero);
    sprintf(buffer, "<svg viewBox=\"%11.3f %11.3f ", widthPCSZero, -heightPCSZero);
    gzputs(gzFhd, buffer);
    widthPCS = (double) width;
    heightPCS = (double) height;

    GTIFImageToPCS(gtif, & widthPCS, & heightPCS);
    sprintf(buffer, "%11.3f %11.3f\">\n", fabs(widthPCS-widthPCSZero), fabs(heightPCS-heightPCSZero));
    gzputs(gzFhd, buffer);
    sprintf(buffer, "<g transform=\"scale(1 -1)\" shape-rendering=\"optimizeSpeed\">\n");
    gzputs(gzFhd, buffer);
  }else{
    sprintf(buffer, "<svg viewBox=\"0 0 %i %i\">\n", width, height);
    gzputs(gzFhd, buffer);
    sprintf(buffer, "<g shape-rendering=\"optimizeSpeed\">\n");
    gzputs(gzFhd, buffer);
  }

  //write projection information if available
  GDALAllRegister();
  hDataset = GDALOpen( fileName, GA_ReadOnly );
  if( hDataset != NULL ){
    if( GDALGetProjectionRef( hDataset ) != NULL ){
      pszProjection = (char *) GDALGetProjectionRef( hDataset );
      if(pszProjection[0]!=0){
        sprintf(buffer, "<metadata>\n<projection><![CDATA[");
        gzputs(gzFhd, buffer);
        gzputs(gzFhd, pszProjection);
        sprintf(buffer, "]]></projection>\n</metadata>\n");
        gzputs(gzFhd, buffer);
      }
    }
    GDALClose( hDataset );
  }

  for(; crtPos<regionNumber+1; crtPos++){
    if(crtPos==423){
      printf("wait\n");
    }
    if(writeSVGPolygon(regions[crtPos],gzFhd, gtif)==ERROR){
      sprintf(buffer, "ERROR");
      gzputs(gzFhd, buffer);
      gzclose(gzFhd);
      return ERROR;
    }

  }
  sprintf(buffer, "</g>\n</svg>\n");
  gzputs(gzFhd, buffer);
  gzclose(gzFhd);
  if(gtif) GTIFFree(gtif);
  if(tif) XTIFFClose(tif);

  return NO_ERROR;
}

/*
 *  writeSVGPolygon: output function for vectorizing one region
 *
 *  Parameters:
 *
 *    region       REGION with its exact boundary line segments
 *
 *    gzFhd        file handler to gZip file (for outputting the SVG)
 *
 *    gtif         file handler to GeoTiff file. If it is NULL,
 *                 the output will be made without geoinformations
 *                 (without georeferenzing image coordiantes)
 *
 *  Return values:
 *
 *   NO_ERROR       if everything is ok
 *
 *   ERROR          if error occured
 */
ERROR_TYPE writeSVGPolygon(struct REGION * region, gzFile gzFhd, GTIF *gtif)
{
  //go on here
  char buffer[200];
  int i,bool=1, * trace;
  struct LINE * line, ** lineArray;
  struct POINT * firstPoint, * crtPoint, * newPoint;
  int * colorValues;
  colorValues=region->colorValues;
  double x,y;
  if(region->channelSize==3){
    /*ORIGINAL sprintf(buffer, "<polygon fill=\"rgb(%i,%i,%i)\" stroke=\"none\" points=\"", colorValues[0], colorValues[1], colorValues[2]); */
    /* polygon with black border */
    sprintf(buffer, "<polygon fill=\"rgb(%i,%i,%i)\" stroke=\"black\" points=\"", colorValues[0], colorValues[1], colorValues[2]);
  }else if(region->channelSize==1){
    sprintf(buffer, "<polygon fill=\"rgb(%i,%i,%i)\" stroke=\"none\" points=\"", colorValues[0], colorValues[0], colorValues[0]);
  }else{
    sprintf(buffer, "<polygon stroke=\"none\" points=\"");
  }
  gzputs(gzFhd, buffer);

  trace=(int *) calloc(sizeof(int), region->crtPos);
  for(i=0; i<region->crtPos;i++){
    trace[i]=0;
  }
  trace[0]=1;
  line = region->lines[0];
  firstPoint=line->points[0];
  for(i=0; i<line->crtPos; i++){
    if(line->points[i]->useFlag){
      if(gtif!=NULL){
        x=(double) line->points[i]->x;
        y=(double) line->points[i]->y;
        if(GTIFImageToPCS(gtif, &x, &y)){
          sprintf(buffer, "%11.3f,%11.3f ", x ,y);
        }
      }else{
        sprintf(buffer, "%i,%i ", (line->points[i]->x) ,(line->points[i]->y));
      }
      gzputs(gzFhd, buffer);
    }
  }
  crtPoint=line->points[line->crtPos-1];
  while(bool){
    newPoint=getNextPoint(region, crtPoint, trace);
    if(newPoint==NULL){
      break;
    }
    //line = getLineSegment(region, crtPoint, newPoint);
    lineArray = getLineSegment(region, crtPoint, newPoint);
    if(lineArray[1]!=NULL){
      line = lineArray[1];
    }else{
      line = lineArray[0];
    }
    free(lineArray);
    //check in which direction the values must be read!
    if(arePointsEqual(crtPoint, line->points[0])){
      for(i=1; i<line->crtPos-1; i++){
        if(line->points[i]->useFlag){
          if(gtif!=NULL){
            x=(double) line->points[i]->x;
            y=(double) line->points[i]->y;
            if(GTIFImageToPCS(gtif, &x, &y)){
              sprintf(buffer, "%11.3f,%11.3f ", x ,y);
            }
          }else{
            sprintf(buffer, "%i,%i ", (line->points[i]->x) ,(line->points[i]->y));
          }
          gzputs(gzFhd, buffer);
        }
      }
    }else{
      for(i=line->crtPos-2; i>0; i--){
        if(line->points[i]->useFlag){
          if(gtif!=NULL){
            x=(double) line->points[i]->x;
            y=(double) line->points[i]->y;
            if(GTIFImageToPCS(gtif, &x, &y)){
              sprintf(buffer, "%11.3f,%11.3f ", x ,y);
            }
          }else{
            sprintf(buffer, "%i,%i ", (line->points[i]->x) ,(line->points[i]->y));
          }
          gzputs(gzFhd, buffer);
        }
      }
    }
    if(arePointsEqual(newPoint, firstPoint)){
      bool=0;
    }else{
      if(line->points[i]->useFlag){
        if(gtif!=NULL){
          x=(double) newPoint->x;
          y=(double) newPoint->y;
          if(GTIFImageToPCS(gtif, &x, &y)){
            sprintf(buffer, "%11.3f,%11.3f ", x ,y);
          }
        }else{
          sprintf(buffer, "%i,%i ", (newPoint->x) ,(newPoint->y));
        }
        gzputs(gzFhd, buffer);
      }
      crtPoint=newPoint;
    }
    //sprintf(buffer, "%i,%i ", (crtPoint->x)-1 ,(crtPoint->y)-1);
  }
  sprintf(buffer, "\">\n");
  gzputs(gzFhd, buffer);
  if(!(region->channelSize==1 || region->channelSize==3)){
    sprintf(buffer, "<metadata>\n");
    gzputs(gzFhd, buffer);
    for(i=0; i<region->channelSize;i++){
      sprintf(buffer, "<channel id=\"%i\" value=\"%i\"/>\n", i+1, colorValues[i]);
      gzputs(gzFhd, buffer);
    }
    sprintf(buffer, "</metadata>\n");
    gzputs(gzFhd, buffer);
  }
  sprintf(buffer,"</polygon>\n");
  gzputs(gzFhd, buffer);

  free(trace);
  return NO_ERROR;
}
