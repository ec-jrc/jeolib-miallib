/***************************************************************************
                          regionMean.c  -  description

    Region mean stores the mean value for each channel of a region.
                             -------------------
    begin                : Thu Apr 22 2004
    authors              : by Dominik Brunner and Pierre.Soille@jrc.ec.europa.eu
    copyright            : (C) 2004 JRC
    email                : dominik.brunner@jrc.it and Pierre.Soille@jrc.ec.europa.eu
***************************************************************************/


#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "regionMean.h"
#include "mialib.h"

/*
 *	rminit: initialize region mean
 *
 *	Parameters:
 *
 *		rm		Pointer to a regionMean
 *
 *		nc		Number of channels of the image to which the means shall be stored.
 *		
 *
 *	Return values:
 *
 *		non-NULL	regionMean has been initialized
 *
 *		NULL		Insufficient memory;
 */
struct regionMean *rmInit(struct regionMean *rm, int nc){

  int i;
  float * mean;
  float * meanOriginal;

  //allocate space for channels
  if(!(mean=(float *) calloc(nc, sizeof(float)))){
    return NULL;
  }
  if(!(meanOriginal=(float *) calloc(nc, sizeof(float)))){
    return NULL;
  }

  //init the channels value with 0
  for(i=0; i<nc; i++){
    mean[i]=0.0;
    meanOriginal[i]=0.0;
  }

  rm->meanValue=mean;
  rm->meanValueOriginal=meanOriginal;

  rm->nc=nc;
  rm->count=0;
  rm->countOriginal=0;
  return rm;
}


/*
 *	rmAddValue: add value (from each channel) to mean values of region
 *
 *	Parameters:
 *
 *		rm			Pointer to a regionMean
 *
 *		imap		Array of IMAGE
 *
 *		offset		offset to values in IMAGE which shall be added.
 *		
 *
 *	Return values:
 *
 *		1			The values has been added
 *
 *		0			The values could not been added. The pointer rm or imap was NULL.
 */
#include "uc_def.h"
int uc_rmAddValue(struct regionMean *rm, IMAGE **imap, long int offset){
  int i, n;
  long int crtCount;

  float * mean, crtMean, newMean,crtValue;
  PIX_TYPE *value;
  if(!rm || !imap) return 0;

  mean= rm->meanValue;
  n = rm->nc;
  crtCount = rm->count;
  for(i=0; i<n;i++){
    value = (PIX_TYPE *) GetImPtr(imap[i]);
    crtMean= mean[i];
    crtValue = (float) value[offset];
    newMean=(float)((((double)crtMean)*crtCount)+crtValue)/((float)(crtCount+1));
    mean[i]=newMean;
  }
  crtCount++;
  rm->count=crtCount;
  return 1;
}
#include "uc_undef.h"

#include "us_def.h"
int us_rmAddValue(struct regionMean *rm, IMAGE **imap, long int offset){
  int i, n;
  long int crtCount;

  float * mean, crtMean, newMean,crtValue;
  PIX_TYPE *value;
  if(!rm || !imap) return 0;

  mean= rm->meanValue;
  n = rm->nc;
  crtCount = rm->count;
  for(i=0; i<n;i++){
    value = (PIX_TYPE *) GetImPtr(imap[i]);
    crtMean= mean[i];
    crtValue = (float) value[offset];
    newMean=(float)((((double)crtMean)*crtCount)+crtValue)/((float)(crtCount+1));
    mean[i]=newMean;
  }
  crtCount++;
  rm->count=crtCount;
  return 1;
}
#include "us_undef.h"

int rmAddValue(struct regionMean *rm, IMAGE **imap, long int offset){

  switch(GetImDataType(imap[0])){
  case t_UCHAR:
    return(uc_rmAddValue(rm, imap, offset));
    break;
  case t_USHORT:
    return(us_rmAddValue(rm, imap, offset));
    break;

  default:
    (void)sprintf(buf,"rmAddValue(): invalid pixel type\n"); errputstr(buf);
  }
  return 0;
}


/*
 *	rmGetDistanceToRM: calculates the distance between two pixels.
 *	The distance is the square root of the sum of (x2-x1)(x2-x1) 
 *	for each channel. For the calculation, all pixels in the mean
 * are considered.
 *
 *	Parameters:
 *
 *		rm			Pointer to a regionMean
 *
 *		imap		Array of IMAGE
 *
 *		offset		offset to a pixel in IMAGE.
 *		
 *
 *	Return values:
 *
 *		>=0			Distance
 *
 *		-1			The Distance could not been calculated. The pointer rm or imap was NULL.
 */
#include "uc_def.h"
double uc_rmGetDistanceToRM(struct regionMean *rm, IMAGE **imap, long int offset){
  int i, n;
  float * mean;
  PIX_TYPE *value;
  double tmp, distance=0;

  if(!rm || !imap) return -1;

  mean= rm->meanValue;
  n = rm->nc;

  for(i=0; i<n;i++){
    value = (PIX_TYPE *) GetImPtr(imap[i]);
    tmp=0;
    tmp = (mean[i]-((double) value[offset]));
    distance+=(tmp*tmp);
  }
  return sqrt(distance);
}
#include "uc_undef.h"

#include "us_def.h"
double us_rmGetDistanceToRM(struct regionMean *rm, IMAGE **imap, long int offset){
  int i, n;
  float * mean;
  PIX_TYPE *value;
  double tmp, distance=0;

  if(!rm || !imap) return -1;

  mean= rm->meanValue;
  n = rm->nc;

  for(i=0; i<n;i++){
    value = (PIX_TYPE *) GetImPtr(imap[i]);
    tmp=0;
    tmp = (mean[i]-((double) value[offset]));
    distance+=(tmp*tmp);
  }
  return sqrt(distance);
}
#include "us_undef.h"


double rmGetDistanceToRM(struct regionMean *rm, IMAGE **imap, long int offset){

  switch(GetImDataType(imap[0])){
  case t_UCHAR:
    return(uc_rmGetDistanceToRM(rm, imap, offset));
    break;
  case t_USHORT:
    return(us_rmGetDistanceToRM(rm, imap, offset));
    break;

  default:
    (void)sprintf(buf,"rmGetDistanceToRM(): invalid pixel type\n"); errputstr(buf);
  }
  return -1;
}


/*
 *	rmAddValue: add value (from each channel) to original mean values (seed values) of region.
 *
 *	Parameters:
 *
 *		rm			Pointer to a regionMean
 *
 *		imap		Array of IMAGE
 *
 *		offset		offset to values in IMAGE which shall be added.
 *
 *
 *	Return values:
 *
 *		1			The values has been added
 *
 *		0			The values could not been added. The pointer rm or imap was NULL.
 */
#include "uc_def.h"
int uc_rmAddValueOriginal(struct regionMean *rm, IMAGE **imap, long int offset){

  int i, n;
  long int crtCount;

  float * mean, crtMean, newMean,crtValue;
  PIX_TYPE *value;
  if(!rm || !imap) return 0;

  mean= rm->meanValueOriginal;
  n = rm->nc;
  crtCount = rm->countOriginal;
  for(i=0; i<n;i++){
    value = (PIX_TYPE *) GetImPtr(imap[i]);
    crtMean= mean[i];
    crtValue = (float) value[offset];
    newMean=(float)(((double)crtMean*crtCount)+crtValue)/((float)(crtCount+1));
    mean[i]=newMean;
  }
  crtCount++;
  rm->countOriginal=crtCount;
  uc_rmAddValue(rm, imap, offset);
  return 1;
}
#include "uc_undef.h"

#include "us_def.h"
int us_rmAddValueOriginal(struct regionMean *rm, IMAGE **imap, long int offset){

  int i, n;
  long int crtCount;

  float * mean, crtMean, newMean,crtValue;
  PIX_TYPE *value;
  if(!rm || !imap) return 0;

  mean= rm->meanValueOriginal;
  n = rm->nc;
  crtCount = rm->countOriginal;
  for(i=0; i<n;i++){
    value = (PIX_TYPE *) GetImPtr(imap[i]);
    crtMean= mean[i];
    crtValue = (float) value[offset];
    newMean=(float)(((double)crtMean*crtCount)+crtValue)/((float)(crtCount+1));
    mean[i]=newMean;
  }
  crtCount++;
  rm->countOriginal=crtCount;
  us_rmAddValue(rm, imap, offset);
  return 1;
}
#include "us_undef.h"

/*
 *	rmGetDistanceToOriginalRM: calculates the distance between two pixels.
 *	The distance is the square root of the sum of (x2-x1)(x2-x1)
 *	for each channel. For the caluclation, only the pixels belonging
 * to the original seeds are considered
 *
 *	Parameters:
 *
 *		rm			Pointer to a regionMean
 *
 *		imap		Array of IMAGE
 *
 *		offset		offset to a pixel in IMAGE.
 *
 *
 *	Return values:
 *
 *		>=0			Distance
 *
 *		-1			The Distance could not been calculated. The pointer rm or imap was NULL.
 */
#include "uc_def.h"
double uc_rmGetDistanceToOriginalRM(struct regionMean *rm, IMAGE **imap, long int offset)
{
  int i, n;
  float * mean;
  PIX_TYPE *value;
  double tmp, distance=0;

  if(!rm || !imap) return -1;

  mean= rm->meanValueOriginal;
  n = rm->nc;

  for(i=0; i<n;i++){
    value = (PIX_TYPE *) GetImPtr(imap[i]);
    tmp=0;
    tmp = (mean[i]-((double) value[offset]));
    distance+=(tmp*tmp);
  }
  return sqrt(distance);
}
#include "uc_undef.h"

#include "us_def.h"
double us_rmGetDistanceToOriginalRM(struct regionMean *rm, IMAGE **imap, long int offset)
{
  int i, n;
  float * mean;
  PIX_TYPE *value;
  double tmp, distance=0;

  if(!rm || !imap) return -1;

  mean= rm->meanValueOriginal;
  n = rm->nc;

  for(i=0; i<n;i++){
    value = (PIX_TYPE *) GetImPtr(imap[i]);
    tmp=0;
    tmp = (mean[i]-((double) value[offset]));
    distance+=(tmp*tmp);
  }
  return sqrt(distance);
}
#include "us_undef.h"



/*
 *	rmGetContrastCoefficient: calculates the contrast coefficient of a region and
 * a pixel. With this coefficient, ones can see if the pixel is brighter (coefficient > 0)
 * or darker (coefficient <0) than the region.
 *
 *	Parameters:
 *
 *		rm			Pointer to a regionMean
 *
 *		imap	Array of IMAGE
 *
 *		offset	offset to a pixel in IMAGE.
 *
 *
 *	Return values:
 *
 *		>0			Pixel is brighter than region
 *
 *   =0      Pixel is equal to region
 *           
 *		<0      Pixel is darker
 */
#include "uc_def.h"
double uc_rmGetContrastCoefficient(struct regionMean *rm, IMAGE **imap, long int offset){
  int i,n;
  float * mean = rm->meanValue;
  PIX_TYPE * value;
  double contrast=0;
  n = rm->nc;
  for(i=0; i<n; i++){
    value = (PIX_TYPE *) GetImPtr(imap[i]);
    contrast = 0;
    contrast += (((double) value[offset])-mean[i]);
  }
  return (contrast/((double) n));
}
#include "uc_undef.h"

#include "us_def.h"
double us_rmGetContrastCoefficient(struct regionMean *rm, IMAGE **imap, long int offset)
{
  int i,n;
  float * mean = rm->meanValue;
  PIX_TYPE * value;
  double contrast=0;
  n = rm->nc;
  for(i=0; i<n; i++){
    value = (PIX_TYPE *) GetImPtr(imap[i]);
    contrast = 0;
    contrast += (((double) value[offset])-mean[i]);
  }
  return (contrast/((double) n));
}
#include "us_undef.h"


/*
 *	clearRegionMean: Deletes all values in the regionMean, and sets
 * the attributes to their initial states
 *
 *	Parameters:
 *
 *		rm			Pointer to a regionMean
 *
 *
 *	Return values:
 *
 * none;
 *
 */
void clearRegionMean(struct regionMean * rm)
{
  int nc = rm->nc, i;
  float * mean=rm->meanValue;
  float * meanOriginal=rm->meanValueOriginal;

  //init the channels value with 0
  for(i=0; i<nc; i++){
    mean[i]=0.0;
    meanOriginal[i]=0.0;
  }

  rm->count=0;
  rm->countOriginal=0;
  return;
}

/*
 *	freeRegionMean: free region Mean
 *
 *	Parameters:
 *
 *		rm			Pointer to a regionMean
 *
 *
 *	Return values:
 *
 * none;
 *
 */
void freeRegionMean(struct regionMean * rm)
{
  free(rm->meanValue);
  free(rm->meanValueOriginal);
}
