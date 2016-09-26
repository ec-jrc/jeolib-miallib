/***************************************************************************
                          segmentation.c  -  description
                             -------------------
 Segmentation algorithm for multispectral high resolution satellite images

    begin                : Tue May 11 2004
    authors              : by Dominik Brunner and Pierre.Soille@jrc.ec.europa.eu
    copyright            : (C) 2004 JRC
    email                : dominik.brunner@jrc.it and Pierre.Soille@jrc.ec.europa.eu
***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "mslabel.h"
#include "mialib.h"
#include "OpenClose.h"
#include "fifo.h"
#include "pqueue.h"
#include "determineSize.h"
#include "mcisrg.h"
#include "string.h"
#include "base.h"
#include "time.h"


/** \addtogroup group_seg
 *  @{
 */


/*
  writeGnuPlotXXX: write data files (.dat) for generating diagrams with
  gnuplot
*/

void writeGnuPlot(FILE * fhd, int contrast, long int regionNumber)
{
  char buffer[50];
  sprintf(buffer, "%i %li\n",contrast, regionNumber);
  fputs(buffer, fhd);
}

void writeGnuPlotFloat(FILE * fhd, int contrast, float value)
{
  char buffer[50];
  sprintf(buffer, "%i %f\n",contrast, value);
  fputs(buffer, fhd);
}

ERROR_TYPE writeGnuPlotFiles(IMAGE **imap, int nc, IMAGE *labelIm, int graph, int varianz, char * filename, long  int regionSize)
{
  long int regionNumber;
  double value1, value2=0.0, value3=0.0, newValue=0.0, oldValue=0.0;
  FILE * fhd, * maxFhd;
  FIFO4 * values;
  char newFileName[300];
  char buffer[100];
  int i=1;
  /* start calculating contrast values */
  strcpy(newFileName, filename);
  sprintf(buffer, "_RS%li",regionSize);
  strcat(newFileName, buffer);
  strcat(newFileName, ".dat");
  printf("Writing file (255 steps): %s\n", newFileName);
  values = create_fifo4((long int)255);
  fhd=fopen(newFileName, "w");
  if(!fhd){
    printf("File could not be generated!\n");
  }
  else{
    for(i=1; i<255;i++){
      if(labelImage(imap, nc, labelIm, graph, varianz)==NULL){
	free_image(labelIm);
	return ERROR;
      }
      regionNumber = thresholdRegion_Contrast(imap, nc, labelIm, i);
      fifo4_add(values, regionNumber);
      writeGnuPlot(fhd, i, regionNumber);
      printf("%d\t", i);
    }
    printf("\n");
    fclose(fhd);
  }
  strcpy(newFileName, filename);
  sprintf(buffer, "_RS%li_slope",regionSize);
  strcat(newFileName, buffer);
  strcat(newFileName, ".dat");
  printf("Write file: %s\n", newFileName);
  fhd=fopen(newFileName, "w");
  if(!fhd){
    printf("File could not be generated!\n");
  }
  else{
    if(fifo4_empty(values)==0){
      i=1;
      value2=(long int)fifo4_remove(values);
      value3=(long int) fifo4_remove(values);
    }
    while(fifo4_empty(values)==0){
      value1=value2;
      value2=value3;
      value3=(double) fifo4_remove(values);
      newValue = (value1-value3)/(float)2;
      writeGnuPlotFloat(fhd, ++i,newValue);
      if(newValue>=oldValue){
	oldValue=newValue;
	strcpy(newFileName, filename);
	sprintf(buffer, "_RS%li_maxValue",regionSize);
	strcat(newFileName, buffer);
	strcat(newFileName, ".dat");
	printf("Write file: %s\n", newFileName);
	maxFhd=fopen(newFileName, "w");
	sprintf(buffer, "%i %f\n",i, oldValue);
	fputs(buffer, maxFhd);
	fclose(maxFhd);
      }
    }
    fclose(fhd);
  }
  free_fifo4(values);
  return NO_ERROR;
}


/*
 *  getBestContrast: determines the best contrast threshold value for the image
 *
 *  Parameters:
 *
 *    imap  array of pointer to the channels of an input images
 *
 *    nc            size of imap => number of channels
 *
 *    labelIm       pointer to an image in which the resulting label image is stored
 *
 *    graph  either 4 or 8
 *
 *  Return values:
 *
 *   contrast value if everything is ok
 *
 *   ERROR          if there was an error
 *
 */
USHORT getBestContrast(IMAGE **imap, int nc, IMAGE *labelIm, int graph, int varianz)
{
  long int regionNumber, value1, value2=0.0, value3=0.0;
  int i;
  double newValue=0, oldValue=0;
  printf("determine best contrast value!\n");
  for(i=1; i<USHORT_MAX;i++){
    if(labelImage(imap, nc, labelIm, graph, varianz)==NULL){
      free_image(labelIm);
      return ERROR;
    }
    regionNumber = thresholdRegion_Contrast(imap, nc, labelIm, i);
    if(i==1){
      value2=regionNumber;
    }
    else if(i==2){
      value3=regionNumber;
    }
    else{
      value1=value2;
      value2=value3;
      value3=regionNumber;
      /* calculate slope */
      newValue = ((value1-value3)/(double)2);
      if(newValue>=oldValue){
	oldValue=newValue;
      }
      else
	return i-2;
    }
  }
  return ERROR;
}


/*
 *  segementImage:  segment multispectral high resolution satellite image
 *
 *  Parameters:
 *
 *    imap  array of pointer to the channels of an input images
 *
 *    nc            size of imap => number of channels
 *
 *    graph  either 4 or 8
 *
 *    varianz       varianz which is used to decide whether the pixel still belongs to
 *                  the region or not. A varianz of 0 means, that the pixel must have exactly the
 *                  same value in all channels than the region, in order to belong to it.
 *
 *    regionSize    size which each regions has at least to be before the algorithm stops
 *
 *    contrast      contrast threshold value which is used for merging the regions with similar contrast.
 *                  value < 0 => don't make merging of regions
 *                  value = 0 => determine best contrast value automatically
 *                  value > 0 => use this value as thresholdvalue
 *
 *    version       version of mcisrg algorithm. Versions are
 *                  0  (compare to whole region)
 *                  1  (compare to original seeds)
 *                  2  (compare to pixel neighbours)  
 *
 *    fndat         filename to store data files.  Authorised values are
 *                  NULL (do not store)
 *		    valid pointer to a char * containing a file name.
 *
 *  Return values:
 *
 *   region number  if everything is ok
 *
 *   ERROR          if there was an error
 *                  
 */
IMAGE *segmentImage(IMAGE **imap, int nc, int graph, int varianz, long int regionSize, int contrast, int version, char *fndat)
{
  long int i, regionNumber=0, oldRegionNumber=0;
  IMAGE *labelIm=NULL;
  char fname[256];
  
  for (i = 1; i < nc; i++){
    if ( (GetImNx(imap[0]) != GetImNx(imap[i])) || \
	 (GetImNy(imap[0]) != GetImNy(imap[i])) || \
	 (GetImDataType(imap[0]) != GetImDataType(imap[i])) ){
      sprintf(buf, "Bands of different datatype or dimension!"); errputstr(buf);
      return NULL;
    }
  }
  labelIm = (IMAGE *)create_image(t_LBL_TYPE, GetImNx(imap[0]), \
				  GetImNy(imap[0]),  GetImNz(imap[0]));
  if (labelIm==NULL){
      sprintf(buf, "segmentImage(): not enough memory\n"); errputstr(buf);
    return NULL;
  }
  for(i=2; i<=regionSize;i++){
    printf("%li: before determineRegionSize\n", i);
    if(labelImage(imap, nc, labelIm, graph, varianz)==NULL){
      sprintf(buf, "%li: an error occurred\n", i); errputstr(buf);
      sprintf(fname,"/tmp/ERROR.tif");
      write_tiff(labelIm, fname);
      sprintf(fname,"/tmp/ERROR[0].tif");
      write_tiff(imap[0], fname);
      free_image(labelIm);
      return NULL;
    }
    sprintf(fname,"/tmp/mcisrgINIT0.tif");
    /* write_tiff(imap[0], fname); */
    sprintf(fname,"/tmp/labelBEFORE%ld.tif", i);
    /* write_tiff(labelIm, fname); */
    if ((regionNumber = thresholdRegion_Size(labelIm, i)) == 0){
      free_image(labelIm);
      return NULL;
    }
    if(oldRegionNumber!=regionNumber){
      oldRegionNumber=regionNumber;
      printf("%li: after determineRegionSize - Number of Regions: %li\n", i, regionNumber);
      if(mcisrg(imap, nc, labelIm, graph, regionNumber, version) == ERROR){
	sprintf(fname,"/tmp/labelAFTER%ld.tif", i);
	free_image(labelIm);
	return NULL;
      }
      sprintf(fname,"/tmp/labelAFTER%ld.tif", i);
      /* write_tiff(labelIm, fname); */
      printf("%li: after mcisrg\n", i);
    }
  }
  sprintf(fname,"/tmp/mcisrgINIT1.tif");
  /* write_tiff(imap[0], fname); */
  if (fndat!=NULL)
    writeGnuPlotFiles(imap, nc, labelIm, graph, varianz, fndat, regionSize);
  if(contrast==0){
    contrast = getBestContrast(imap, nc, labelIm, graph, varianz);
    printf("best contrast value= %d\n", contrast);
  }
  sprintf(fname,"/tmp/mcisrgINIT2.tif");
  /* write_tiff(imap[0], fname); */
  
  if(contrast>0){ /* then make merge of regions with similar contrast value */
    if(labelImage(imap, nc, labelIm, graph, varianz)==NULL){
      free_image(labelIm);
      return NULL;
    }
    if ((regionNumber = thresholdRegion_Contrast(imap, nc, labelIm, contrast))==0){
      free_image(labelIm);
      return NULL;
    }
    sprintf(fname,"/tmp/mcisrgINIT3.tif");
    /* write_tiff(imap[0], fname); */
    if(mcisrg(imap, nc, labelIm, graph, regionNumber, version) == ERROR){
      free_image(labelIm);
      return NULL;
    }
  }
  sprintf(fname,"/tmp/mcisrgINIT.tif");
  /* write_tiff(imap[0], fname); */

  /* make open and closing */
  /*if(OpenClose(imap, nc, labelIm, graph, varianz, version)==ERROR){
    free_image(labelIm);
    return ERROR;
    }
    //delete regions which are too small
    if(labelImage(imap, nc, labelIm, graph, varianz)==NULL){
    free_image(labelIm);
    return ERROR;
    }
    if ((regionNumber = thresholdRegion_Size(labelIm, regionSize)) == 0){
    free_image(labelIm);
    return ERROR;
    }
    if(mcisrg(imap, nc, labelIm, graph, regionNumber, version) == ERROR){
    free_image(labelIm);
    return ERROR;
    }   */

  //  free_image(labelIm);
  //  return regionNumber;
  return labelIm;
}

ERROR_TYPE writeGnuPlot3D(IMAGE **imap, int nc, int graph, int regionSize, int varianz, char * fileName)
{
  long int regionNumber;
  int i,j;
  IMAGE * labelIm;
  char outputFName[1000], buffer[100];
  FILE * fhd;
  strncpy( outputFName, fileName, sizeof(outputFName)-4 );
  for( i = strlen(outputFName)-1; i > 0; i-- ){
    if( outputFName[i] == '.' ){
      strcpy( outputFName + i, ".dat" );
      break;
    }
  }
  if( i <= 0 ) strcat( outputFName, ".dat" );
  printf("write GnuPlot dat file: %s\n", outputFName);
  fhd=fopen(outputFName, "w");
  if(!fhd){
    sprintf(buf, "error in writeGnuPlot3D(): File could not be generated!\n"); errputstr(buf);
  }
  else{
    labelIm = (IMAGE *) create_image(t_LBL_TYPE, GetImNx(imap[0]), GetImNy(imap[0]), 1);
    for(i=1; i<=regionSize;i++){
      for(j=0; j<varianz;j++){
        //printf("%li: before determineRegionSize\n", i);
        labelImage(imap, nc, labelIm, graph, j);
        if ((regionNumber = thresholdRegion_Size(labelIm, i)) == ERROR){
          free_image(labelIm);
          return ERROR;
        }
        printf("%i %i %li\n",i, j, regionNumber);
        sprintf(buffer, "%i %i %li\n",i, j, regionNumber);
        fputs(buffer, fhd);
      }
      printf("\n");
      sprintf(buffer,"\n");
      fputs(buffer, fhd);
    }
    fclose(fhd);
  }
  return NO_ERROR;
}

/*@}*/
