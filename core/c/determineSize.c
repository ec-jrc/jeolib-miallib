/***************************************************************************
                          determineSize.c  -  description
                             -------------------
    begin                : Tue APR 13 2004  by Dominik Brunner
    copyright            : (C) 2004 European Commission
    email                : Pierre.Soille@jrc.ec.europa.eu
 ***************************************************************************/


#include <stdio.h>
#include <stdlib.h>
#include "mslabel.h"
#include "mialib.h"
#include "base.h"
#include "fifo.h"
#include "regionMean.h"
#include "math.h"
#include "determineSize.h"

struct status {
    unsigned int is_labeled : 1;
    unsigned int is_contrast_labeled : 1;
};


/*
 *  thresholdRegion_Size:  Algorithm to determine the size of the regions. If the thresholdvalue
 *                         is 0, then the values of the pixels in the image are replaced by the
 *                         corresponding size values of the region the pixel belongs to.
 *                         Otherwise the region gets an increasing region number if the size is
 *                         bigger then the threshold.
 *
 *  Parameters:
 *
 *    inputIm              input image which shall be thresholded. The result is stored in this image
 *
 *    threshold            region size must be bigger than this threshold value. Otherwise the values
 *                         of the pixels of this region are set to 0;
 *
 *
 *  Return values:
 *
 *     0                   if there is an Error
 *
 *     >1                  number of regions which are left after thresholding the image
 */
long int thresholdRegion_Size(IMAGE *inputIm, unsigned long int threshold)
{
  long int shft[27], i, label=SEED;
  int nx, ny;
  LBL_TYPE count,  k, offset, neighbourOffset, crtOffset, *pInputIm;
  IMAGE *im;			// Image for storing whether pixel already
  // processed (LABELED) or not (NOTLABELED)
  UCHAR *pIm;
  FIFO4 *nq, *rq;		// neighbourQueue, regionQueue

  int box[6];
  // initilize box for frame which shall be added in order to solve
  // problem with neighbours
  box[0] = 1;
  box[1] = 1;
  box[2] = 1;
  box[3] = 1;
  box[4] = 0;
  box[5] = 0;


  // check whether image has datatype unsigned long int
  if (GetImDataType(inputIm) != t_LBL_TYPE){
    sprintf(buf,"error in thresholdRegion_Size(): wrong data type for inputIm\n");
    return ERROR;
  }

  if (u32_addframebox(inputIm, box, BORDER) == ERROR){
    return ERROR;
  }
    
  im = (IMAGE *) create_image(t_UCHAR, GetImNx(inputIm), GetImNy(inputIm), 1);

  if (uc_framebox(im, box, BORDER) == ERROR){
    return ERROR;
  }

  nx= GetImNx(inputIm);
  ny= GetImNy(inputIm);

  if (set_seq_shift(GetImNx(inputIm), GetImNy(inputIm), GetImNz(inputIm), 8, shft) == ERROR){
    return ERROR;
  }

  nq = create_fifo4(nx * ny / 100L);
  rq = create_fifo4(nx * ny / 100L);

  pInputIm = (LBL_TYPE *) GetImPtr(inputIm);
  pIm = (UCHAR *) GetImPtr(im);
  for (i = 0; i < nx * ny; i++){
    count = 0;
    if ((*(pIm + i)) == NOTLABELED){
      offset = i;
      count++;
      // put this pixel to region queue
      fifo4_add(rq, (long int) offset);
      (*(pIm + offset)) = LABELED;
      // check the neighbours
      for (k = 0; k < 8; k++){
	neighbourOffset = offset + shft[k];
	if ((*(pIm + neighbourOffset)) == NOTLABELED){
	  //printf("offset: %i",(*(pInputIm + offset) ));
	  //printf("neighbourOffset: %i", (*(pInputIm + neighbourOffset)));
	  if ((*(pInputIm + neighbourOffset)) ==
	      (*(pInputIm + offset))){
	    fifo4_add(nq, (long int) (neighbourOffset));
	    (*(pIm + neighbourOffset)) = LABELED;

	    fifo4_add(rq, (long int) (neighbourOffset));
	    count++;
	  }
	}
      }
      while (fifo4_empty(nq) == 0){
	crtOffset = (long int) fifo4_remove(nq);
	for (k = 0; k < 8; k++){
	  neighbourOffset = crtOffset + shft[k];
	  if ((*(pIm + neighbourOffset)) == NOTLABELED){
	    if ((*(pInputIm + neighbourOffset)) == (*(pInputIm + offset)))
	      {
		fifo4_add(nq, (long int) (neighbourOffset));
		(*(pIm + neighbourOffset)) = LABELED;
		fifo4_add(rq, (long int) (neighbourOffset));
		count++;
	      }
	  }
	}
      }
      if((threshold == 0)){
	while (fifo4_empty(rq) == 0){
	  offset = (long int) fifo4_remove(rq);
	  (*(pInputIm + offset)) = count;
	}
	label++;
      }
      else if (count >= threshold){
	while (fifo4_empty(rq) == 0){
	  offset = (long int) fifo4_remove(rq);
	  (*(pInputIm + offset)) = label;
	}
	label++;
      }
      else{
	while(fifo4_empty(rq) == 0){
	  offset = (long int)fifo4_remove(rq);
	  (*(pInputIm + offset)) = 0;
	}
      }

    }
  }
  free_image(im);
  free_fifo4(nq);
  free_fifo4(rq);
  subframebox(inputIm, box);
  if(threshold >=0){
    return (label-1);
  }else{
    return NO_ERROR;
  }
}

/*
 *  thresholdRegion_Contrast: Algorithm to determine the contrast values of the regions.
 *                            If the thresholdvalue
 *                            is 0, then the values of the pixels in the image are replaced by the
 *                            corresponding contrast values of the region the pixel belongs to.
 *                            Otherwise the region gets an increasing region number if the size is
 *                            bigger then the threshold.
 *
 *  Parameters:      
 *
 *    imap            image array with the different channels of the image
 *
 *    nc                 number of channels
 *
 *    inputIm            input image which shall be thresholded. The result is stored in this image
 *
 *    threshold          contrast value of the region must be bigger than this threshold value.
 *                       Otherwise the values
 *                       of the pixels of this region are set to 0;
 *
 *
 *  Return values:
 *
 *     0                 if there is an Error
 *
 *     >1                number of regions which are left after thresholding the image
 */
long int thresholdRegion_Contrast(IMAGE **imap, int nc, IMAGE *inputIm, unsigned long int threshold)
{
  long int shft[27], i, label=SEED;
  int nx, ny;
  LBL_TYPE count, countContrastPixel, k, offset, neighbourOffset, crtOffset, *pInputIm;
  FIFO4 *nq, *rq, *cq;		// neighbourQueue, regionQueue, contrastQueue
  struct regionMean rm; //stores the region mean colour informatiion for the current region
  double contrast, distance;
  int box[6];

  struct status *crtStatus;
  // initilize box for frame which shall be added in order to solve
  // problem with neighbours
  box[0] = 1;
  box[1] = 1;
  box[2] = 1;
  box[3] = 1;
  box[4] = 0;
  box[5] = 0;

  // check whether image has datatype unsigned long int
  if (GetImDataType(inputIm) != t_LBL_TYPE){
    sprintf(buf,"error in thresholdRegion_Contrast(): wrong data type for inputIm\n");
    return ERROR;
  }

  for (i = 0; i < nc; i++){
    if (GetImDataType(imap[i])==t_UCHAR){
      if (generic_addframebox(imap[i], box, (UCHAR)BORDER) == ERROR){
	return ERROR;
      }
    }
    else if (GetImDataType(imap[i])==t_USHORT){
      if (us_addframebox(imap[i], box, (USHORT)BORDER) == ERROR){
	return ERROR;
      }
    }
    else{
      return ERROR;
    }
  }
  if (u32_addframebox(inputIm, box, BORDER) == ERROR){
    return ERROR;
  }

  //initialize regionMean
  if(rmInit(&rm, nc)==NULL){
    return ERROR;
  }
        
  nx= GetImNx(inputIm);
  ny= GetImNy(inputIm);

  crtStatus = (struct status *) calloc(nx*ny, sizeof(struct status));
  if (crtStatus==NULL){
    freeRegionMean(&rm);
    return ERROR;
  }
    
  for(i=0; i<(nx*ny);i++){
    //initialize  values considering a frame outside
    if((i<nx)||(i>=(nx*ny-nx))|| (i%nx == nx-1) || (i%nx == 0)){
      crtStatus[i].is_contrast_labeled=LABELED;
      crtStatus[i].is_labeled=LABELED;
    }else{
      crtStatus[i].is_contrast_labeled=NOTLABELED;
      crtStatus[i].is_labeled=NOTLABELED;
    }
  }

  if (set_seq_shift(GetImNx(inputIm), GetImNy(inputIm), GetImNz(inputIm), 8, shft) == ERROR){
    freeRegionMean(&rm);
    free(crtStatus);
    return ERROR;
  }

  nq = create_fifo4(nx * ny / 100L);
  rq = create_fifo4(nx * ny / 100L);
  cq = create_fifo4(nx * ny / 100L);

  pInputIm = (LBL_TYPE *) GetImPtr(inputIm);
  for (i = 0; i < nx * ny; i++){
    count = 0;
    countContrastPixel = 0;
    clearRegionMean(&rm);
    contrast=0;
    if (crtStatus[i].is_labeled == NOTLABELED){
      offset = i;
      count++;
      // put this pixel to region queue
      fifo4_add(rq, (long int) offset);
      rmAddValue(&rm, imap, (long int) offset);
      crtStatus[offset].is_labeled = LABELED;
      // check the neighbours
      for (k = 0; k < 8; k++){
	neighbourOffset = offset + shft[k];
	if ((crtStatus[neighbourOffset].is_labeled == NOTLABELED) && \
	    ((*(pInputIm + neighbourOffset)) ==(*(pInputIm + offset)))){
	  fifo4_add(nq, (long int) (neighbourOffset));
	  crtStatus[neighbourOffset].is_labeled = LABELED;
	  fifo4_add(rq, (long int) (neighbourOffset));
	  rmAddValue(&rm, imap, (long int) neighbourOffset);
	  count++;
	}else{
	  //check whether the pixel is already in the queue which stores the pixel to determine the contrast
	  if((crtStatus[neighbourOffset].is_contrast_labeled == NOTLABELED) && \
	     ((*(pInputIm + neighbourOffset)) != (*(pInputIm + offset)))){
	    fifo4_add(cq, neighbourOffset);
	    crtStatus[neighbourOffset].is_contrast_labeled = LABELED;
	    countContrastPixel++;
	  }
	}
      }
      while (fifo4_empty(nq) == 0){
	crtOffset = (long int) fifo4_remove(nq);
	for (k = 0; k < 8; k++){
	  neighbourOffset = crtOffset + shft[k];
	  if ((crtStatus[neighbourOffset].is_labeled == NOTLABELED) && ((*(pInputIm + neighbourOffset)) == (*(pInputIm +crtOffset)))){
	    fifo4_add(nq, (long int) (neighbourOffset));
	    crtStatus[neighbourOffset].is_labeled = LABELED;
	    fifo4_add(rq, (long int) (neighbourOffset));
	    rmAddValue(&rm, imap, (long int) neighbourOffset);
	    count++;
	  }
	  else{
	    //check whether the pixel is already in the queue which stores the pixel to determine the contrast
	    if((crtStatus[neighbourOffset].is_contrast_labeled == NOTLABELED) && ((*(pInputIm + neighbourOffset)) != (*(pInputIm + offset)))){
	      fifo4_add(cq, neighbourOffset);
	      crtStatus[neighbourOffset].is_contrast_labeled = LABELED;
	      countContrastPixel++;
	    }
	  }
	}
      }

      //determine the region contrast koeffizient
      //take all Pixel which surrounds the region and sum up the contrast values
      while(fifo4_empty(cq) == 0){
	offset = (long int) fifo4_remove(cq);
	distance =rmGetDistanceToRM(&rm, imap, offset);
	contrast+= distance;
	crtStatus[neighbourOffset].is_contrast_labeled = NOTLABELED;
      }
      contrast/=(double) countContrastPixel;
      //contrast = fabs(contrast);

      //all pixel belonging to the same region are stored in the rq.
      //Process now according to threshold value
      //printf("contrast: %f\n", contrast);
      if((threshold == 0)){
	while (fifo4_empty(rq) == 0){
	  offset = (long int) fifo4_remove(rq);
	  (*(pInputIm + offset)) = contrast;
	}
	label++;
      }
      else if (contrast>=threshold){
	while (fifo4_empty(rq) == 0){
	  offset = (long int) fifo4_remove(rq);
	  (*(pInputIm + offset)) = label;
	}
	label++;
      }else{
	while(fifo4_empty(rq) == 0){
	  offset = (long int)fifo4_remove(rq);
	  (*(pInputIm + offset)) = 0;
	}
      }
    }
  }
  free_fifo4(nq);
  free_fifo4(rq);
  free_fifo4(cq);
  freeRegionMean(&rm);
  free(crtStatus);
  for(i=0; i<nc; i++){
    subframebox(imap[i], box);
  }
  subframebox(inputIm, box);
    
  if(threshold >=0){
    return (label-1);
  }
  else{
    return NO_ERROR;
  }
}
