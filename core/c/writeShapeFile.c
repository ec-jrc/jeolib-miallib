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
                          writeShapeFile.c  -  description
                             -------------------
 Vectorizes the boundary lines from the region. The output file format is ESRI Shapefile.
 The geoinformations will be considered for the output (if available)

    begin                : Mon May 24 2004
***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "borderdetection.h"
#include "writeShapeFile.h"
#include "miallib.h"
#include "base.h"
#include <geotiff/xtiffio.h>
#include <tiffio.h>

/*
 *  writeSVG:      Write the defining matrix for this file to a .tfw (Tiff world) file with the same
 *                 basename.
 *
 *  Parameters:
 *
 *
 *    gtif         file handler to GeoTiff file.
 *
 *    filename     filename to the input image
 *
 *  Return values:
 *
 *  none
 */
static void WriteTFWFile( GTIF * gtif, const char * tif_filename )
{
  char	tfw_filename[1024];
  int		i;
  double	adfCoeff[6], x, y;
  FILE	*fp;

  if (gtif==NULL){
    return;
  }

  /*
   * form .tfw filename
   */
  strncpy( tfw_filename, tif_filename, sizeof(tfw_filename)-4 );
  for( i = strlen(tfw_filename)-1; i > 0; i-- )
    {
      if( tfw_filename[i] == '.' )
        {
	  strcpy( tfw_filename + i, ".tfw" );
	  break;
        }
    }

  if( i <= 0 )
    strcat( tfw_filename, ".tfw" );

  printf("write TFW file: %s", tfw_filename);

  /*
   * Compute the coefficients.
   */
  x = 0.5;
  y = 0.5;
  if( !GTIFImageToPCS( gtif, &x, &y ) )
    return;
  adfCoeff[4] = x;
  adfCoeff[5] = y;

  x = 1.5;
  y = 0.5;
  if( !GTIFImageToPCS( gtif, &x, &y ) )
    return;
  adfCoeff[0] = x - adfCoeff[4];
  adfCoeff[1] = y - adfCoeff[5];

  x = 0.5;
  y = 1.5;
  if( !GTIFImageToPCS( gtif, &x, &y ) )
    return;
  adfCoeff[2] = x - adfCoeff[4];
  adfCoeff[3] = y - adfCoeff[5];

  /*
   * Write out the coefficients.
   */

  fp = fopen( tfw_filename, "wt" );
  if( fp == NULL )
    {
      perror( "fopen" );
      fprintf( stderr, "Failed to open TFW file `%s'\n", tfw_filename );
      return;
    }

  for( i = 0; i < 6; i++ )
    fprintf( fp, "%24.10f\n", adfCoeff[i] );
  fclose( fp );
}

/*
 *  writeShapeFile: write the boundary lines of regions as ESRI Shapefile
 *
 *  Parameters:
 *
 *    regions       REGIONS with their exact boundary line segments
 *
 *    regionNumber  size of regions array => number of regions
 *
 *    filename      filename to the input image (is needed in order to read the geo informations, if available)
 *
 *  Return values:
 *
 *   NO_ERROR       if everything is ok
 *
 *   ERROR          if error occured
 */
ERROR_TYPE writeShapeFile(struct REGION ** regions, int regionNumber, char * fileName)
{
  long int crtPos;
  double x=0, y=0;
  int i, * channels, channelSize, shapeNumber;
  char buffer[50], prjFilename[1024];
  TIFF *tif=(TIFF*)0;
  GTIF *gtif=(GTIF*)0; /* GeoKey-level descriptor */
  GDALDatasetH hDataset;
  char *pszProjection;
  FILE * prjFhd;
  SHPHandle shpHdl;
  DBFHandle dbfHdl;
  SHPObject * shpObject;

  printf("write Shapefiles\n");

  shpHdl = SHPCreate(fileName, SHPT_POLYGON);
  dbfHdl = DBFCreate(fileName);

  //open geotiff
  tif=XTIFFOpen(fileName,"r");
  if (tif==NULL){
    (void)sprintf(buf,"ERROR in writeShapeFile(): invalid GeoTIFF file name \"%s\" \n", fileName); errputstr(buf);
    return ERROR;
  }
  gtif = GTIFNew(tif);
  if(!gtif) gtif=NULL;
  if(!GTIFImageToPCS(gtif, &x, &y)){
    gtif=NULL;
  }
  if(gtif!=NULL) WriteTFWFile(gtif, fileName);
  crtPos=SEED;
  channelSize = regions[crtPos]->channelSize;
  channels = calloc(channelSize, sizeof(int));

  for(i=0; i<channelSize; i++){
    sprintf(buffer, "Channel_%i", i);
    if((channels[i]=DBFAddField(dbfHdl, buffer, FTInteger,5,0))==-1){
      printf("Error during generating fields for xBase file!\n");
      SHPClose(shpHdl);
      DBFClose(dbfHdl);
    }
  }

  for(; crtPos<regionNumber+1; crtPos++){
    if((shpObject=writeSHPPolygon(regions[crtPos],crtPos, gtif))==NULL){
      SHPClose(shpHdl);
      return ERROR;
    }
    shapeNumber = SHPWriteObject(shpHdl, -1, shpObject);
    for(i=0; i<channelSize; i++){
      DBFWriteIntegerAttribute(dbfHdl, shapeNumber, channels[i], regions[crtPos]->colorValues[i]);
    }
  }
  SHPClose(shpHdl);
  DBFClose(dbfHdl);
  if(gtif) GTIFFree(gtif);
  if(tif) XTIFFClose(tif);

  //write prj file
  GDALAllRegister();
  hDataset = GDALOpen( fileName, GA_ReadOnly );
  if( hDataset != NULL ){
    if( GDALGetProjectionRef( hDataset ) != NULL ){
      pszProjection = (char *) GDALGetProjectionRef( hDataset );
      if(pszProjection[0]!=0){
        //form prj filename
        strncpy( prjFilename, fileName, sizeof(prjFilename)-4 );
        for( i = strlen(prjFilename)-1; i > 0; i-- )
	  {
	    if( prjFilename[i] == '.' )
	      {
		strcpy( prjFilename + i, ".prj" );
		break;
	      }
	  }
        if( i <= 0 ) strcat( prjFilename, ".prj" );
        printf("write PRJ file: %s", prjFilename);
        prjFhd=fopen(prjFilename, "w");
        fputs(pszProjection, prjFhd);
	fclose(prjFhd);
      }
    }
    GDALClose( hDataset );
  }
  return NO_ERROR;
}

/*
 *  writeSHPPolygon: output function for vectorizing one region
 *
 *  Parameters:
 *
 *    region       REGION with its exact boundary line segments
 *
 *    id           unique id with which shape object will be generated
 *
 *    gtif         file handler for GeoTiff file. If it is NULL,
 *                 the output will be made without geoinformations
 *                 (without georeferenzing image coordiantes)
 *
 *  Return values:
 *
 *   Shape object  if everything is ok
 *
 *   NULL          if error occured
 */
SHPObject * writeSHPPolygon(struct REGION * region, int id, GTIF * gtif){
  //go on here
  int i, abool=1, * trace;
  struct LINE * line, ** lineArray,* hLine;
  struct POINT * firstPoint, * crtPoint, * newPoint;
  double * xVertices, * yVertices;
  hLine = (struct LINE *) malloc(sizeof(struct LINE));
  initLine(hLine, 100);
  SHPObject * shpObject = NULL;
  trace=(int *) calloc(sizeof(int), region->crtPos);
  for(i=0; i<region->crtPos;i++){
    trace[i]=0;
  }
  trace[0]=1;

  line = region->lines[0];
  firstPoint=line->points[0];
  for(i=0; i<line->crtPos; i++){
    if(line->points[i]->useFlag){
      if(addPointToLine(hLine, line->points[i])==NULL){
        freeLine(hLine);
        return NULL;
      }
    }
  }
  crtPoint=line->points[line->crtPos-1];
  while(abool){
    newPoint=getNextPoint(region, crtPoint, trace);
    if(newPoint==NULL){
      abool=0;
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
          if(addPointToLine(hLine, line->points[i])==NULL){
            freeLine(hLine);
            return NULL;
          }
        }
      }
    }else{
      for(i=line->crtPos-2; i>0; i--){
        if(line->points[i]->useFlag){
          if(addPointToLine(hLine, line->points[i])==NULL){
            freeLine(hLine);
            return NULL;
          }
        }
      }
    }
    if(arePointsEqual(newPoint, firstPoint)){
      abool=0;
      if(line->points[i]->useFlag){
        addPointToLine(hLine, newPoint);
      }
    }else{
      if(line->points[i]->useFlag){
        if(addPointToLine(hLine, line->points[i])==NULL){
          freeLine(hLine);
          return NULL;
        }
      }
      crtPoint=newPoint;
    }
  }
  //now all points for the polygon are collected
  xVertices = calloc (hLine->crtPos, sizeof(double));
  yVertices = calloc (hLine->crtPos, sizeof(double));
  if(!xVertices || ! yVertices){
    printf("not enough memory!!!");
    return NULL;
  }
  for(i=0;i<hLine->crtPos;i++){
    crtPoint=hLine->points[i];
    xVertices[i]=crtPoint->x;
    yVertices[i]=crtPoint->y;
    if(gtif!=NULL){
      GTIFImageToPCS(gtif, & xVertices[i], &yVertices[i]);
    }
  }

  shpObject = SHPCreateObject(SHPT_POLYGON, id, 0, NULL,NULL, i, xVertices, yVertices, NULL, NULL);
  free(trace);
  free(xVertices);
  free(yVertices);
  free(hLine->points);
  free(hLine);
  return shpObject;
}

