/***************************************************************************
                          label.c  -  description

  Extended version of fast breadth first stack algorithm (labeling algorithm)
  to find regions with same Pixel values following Pierre Soille.
  This version supports multispectral images!
                           -------------------
    begin                : Tue Apr 13 2004
    authors:             : Dominik Brunner and Pierre.Soille@jrc.it
    copyright            : (C) 2004 JRC
    email                : dominik.brunner@jrc.it and Pierre.Soille@jrc.it
 ***************************************************************************/


#include <stdio.h>
#include <stdlib.h>
#include "mslabel.h"
#include "mialib.h"
#include "base.h"
#include "fifo.h"

/** \addtogroup group_label
 *  @{
 */

/*
 *  labelImage:     detects regions in a multispectral image. The generated
 *                  label image is stored in the attribute labelIm
 *
 *  Parameters:
 *
 *    imArray       array of pointer to the channels of an input images
 *                  (a given image MUST only appear once in the array)
 *
 *    nc            size of imArray => number of channels
 *
 *    labelIm       pointer to an image in which the resulting label image is stored
 *
 *    graph  either 4 or 8
 *
 *    lambda       lambda which is used to decide whether the pixel still belongs to
 *                  the region or not. A lambda of 0 means, that the pixel must have exactly the
 *                  same value in all channels than the region, in order to belong to it.
 *                 [Should be a vector of lambda values, one per channel]
 *
 *
 *
 *  Return values:
 *
 *   labelIm        if everything is ok
 *
 *   NULL           if graph was not equal to 4 or 8
 *                  if the dimensions of the input images and the label image are not equal
 */

#include "uc_def.h"
IMAGE *uc_labelImage(IMAGE **imArray, int nc, IMAGE *labelIm, int graph, long int lambda)
{
  long int shft[27], offset, i, crtOffset, neighbourOffset;
  LBL_TYPE *pLabelIm, lval;
  PIX_TYPE *pInputIm;
  int nx, ny, k, bool, n;
  FIFO4 *hq;
  int box[6];
  box[0] = 1;
  box[1] = 1;
  box[2] = 1;
  box[3] = 1;
  box[4] = 0;
  box[5] = 0;

  // make checks on datatypes, and whether dimensions of images in
  // imArray and labelIm fit together
  if (nc < 1){
    return NULL;
  }

  for (i=1; i<nc; i++){
    if ( (GetImNx(imArray[0]) != GetImNx(imArray[i])) || (GetImNy(imArray[0]) != GetImNy(imArray[i])) \
	 || (GetImDataType(imArray[i]) != t_PIX_TYPE) ){
      return NULL;
    }
  }
  if (labelIm == NULL){
    labelIm=(IMAGE *)create_image(t_LBL_TYPE, GetImNx(imArray[0]), GetImNy(imArray[0]), GetImNz(imArray[0]));
    if (labelIm==NULL)
      return NULL;
  }
  else{
    if (GetImDataType(labelIm) != t_LBL_TYPE)
      return NULL;
    if ( (GetImNx(imArray[0]) != GetImNx(labelIm)) || (GetImNy(imArray[0]) != GetImNy(labelIm)) )
      return NULL;
  }

  //initialize labelImage
  pLabelIm = (LBL_TYPE *) GetImPtr(labelIm);
  for(i=0; i<GetImNx(labelIm)*GetImNy(labelIm); i++)
    pLabelIm[i]=0;

  for (i=0; i<nc; i++){
    if (generic_addframebox(imArray[i], box, BORDER) == ERROR){
      return NULL;
    }
  }
  if (u32_addframebox(labelIm, box, BORDER) == ERROR){
    return NULL;
  }
  pLabelIm = (LBL_TYPE *)GetImPtr(labelIm);

  if (set_seq_shift(GetImNx(labelIm), GetImNy(labelIm), GetImNz(labelIm), graph, shft) == ERROR){
    return NULL;
  }
    
  nx = GetImNx(labelIm);
  ny = GetImNy(labelIm);
  hq = create_fifo4(nx * ny / 100L);
  lval = LABELED;

  // framebox(labelIm, box, gval); addframebox used instead, see before
  for (i=0; i<nx*ny; i++){
    if (pLabelIm[i]== NOTLABELED){
      // store offset to pixel in image instead of values =>
      // easy handle of multichannel pictures
      offset = i;
      pLabelIm[offset]=++lval;
      // check the neighbours
      for (k = 0; k < graph; k++){
	bool = 1;
	neighbourOffset = offset + shft[k];
	// check whether neighbour has same value for all
	// channels
	if(pLabelIm[neighbourOffset] == NOTLABELED){
	  for (n = 0; n < nc; n++){
	    pInputIm = (PIX_TYPE *) GetImPtr(imArray[n]);
	    if ((( ((INT32) pInputIm[offset])-lambda) >pInputIm[neighbourOffset]) || \
		(( ((INT32) pInputIm[offset])+lambda)<pInputIm[neighbourOffset])){
	      bool = 0;
	      break;
	    }
	  }
	  // if yes => put in queue
	  if (bool){
	    fifo4_add(hq, (long int) (neighbourOffset));
	    pLabelIm[neighbourOffset] = lval;
	  }
	}
      }
      while (fifo4_empty(hq) == 0){
	crtOffset = (long int) fifo4_remove(hq);
	// check the neighbours
	for (k = 0; k < graph; k++){
	  // check whether Pixel has no label
	  neighbourOffset = crtOffset + shft[k];
	  if (pLabelIm[neighbourOffset] == NOTLABELED){
	    bool = 1;
	    // check whether neighbour has same value
	    // for all channels
	    for (n = 0; n < nc; n++){
	      pInputIm = (PIX_TYPE *) GetImPtr(imArray[n]);
	      /* srtOffset used here, was offset in previous version */
	      if (((  ((INT32) pInputIm[crtOffset])-lambda)>pInputIm[neighbourOffset]) || \
		   (( ((INT32) pInputIm[crtOffset])+lambda)<pInputIm[neighbourOffset])){
		bool = 0;
		break;
	      }
	    }
	    // if yes => put in queue
	    if (bool){
	      fifo4_add(hq,(long int) (neighbourOffset));
	      pLabelIm[neighbourOffset] = lval;
	    }
	  }
	}
      }
    }
  }
  free_fifo4(hq);
  for(i=0; i<nc; i++){
    subframebox(imArray[i], box);
  }
  subframebox(labelIm, box);
  return labelIm;
}
#include "uc_undef.h"

#include "us_def.h"
IMAGE *us_labelImage(IMAGE **imArray, int nc, IMAGE *labelIm, int graph, long int lambda)
{
  long int shft[27], offset, i, crtOffset, neighbourOffset;
  LBL_TYPE *pLabelIm, lval;
  PIX_TYPE *pInputIm;
  int nx, ny, k, bool, n;
  FIFO4 *hq;
  int box[6];
  box[0] = 1;
  box[1] = 1;
  box[2] = 1;
  box[3] = 1;
  box[4] = 0;
  box[5] = 0;

  // make checks on datatypes, and whether dimensions of images in
  // imArray and labelIm fit together
  if (nc < 1){
    return NULL;
  }

  for (i=1; i<nc; i++){
    if ( (GetImNx(imArray[0]) != GetImNx(imArray[i])) || (GetImNy(imArray[0]) != GetImNy(imArray[i])) \
	 || (GetImDataType(imArray[i]) != t_PIX_TYPE) ){
      return NULL;
    }
  }
  if (labelIm == NULL){
    labelIm=(IMAGE *)create_image(t_LBL_TYPE, GetImNx(imArray[0]), GetImNy(imArray[0]), GetImNz(imArray[0]));
    if (labelIm==NULL)
      return NULL;
  }
  else{
    if (GetImDataType(labelIm) != t_LBL_TYPE)
      return NULL;
    if ( (GetImNx(imArray[0]) != GetImNx(labelIm)) || (GetImNy(imArray[0]) != GetImNy(labelIm)) )
      return NULL;
  }

  //initialize labelImage
  pLabelIm = (LBL_TYPE *) GetImPtr(labelIm);
  for(i=0; i<GetImNx(labelIm)*GetImNy(labelIm); i++){
    pLabelIm[i]=0;
  }

  for (i = 0; i < nc; i++){
    if (us_addframebox(imArray[i], box, BORDER) == ERROR){
      return NULL;
    }
  }
  if (u32_addframebox(labelIm, box, BORDER) == ERROR){
    return NULL;
  }
  pLabelIm = (LBL_TYPE *) GetImPtr(labelIm);

  if (set_seq_shift(GetImNx(labelIm), GetImNy(labelIm), GetImNz(labelIm), graph, shft) == ERROR){
    return NULL;
  }
    
  nx = GetImNx(labelIm);
  ny = GetImNy(labelIm);
  hq = create_fifo4(nx * ny / 100L);
  lval = LABELED;

  // framebox(labelIm, box, gval); addframebox used instead, see before
  for (i=0; i<nx*ny; i++){
    if (pLabelIm[i]== NOTLABELED){
      // store offset to pixel in image instead of values =>
      // easy handle of multichannel pictures
      offset = i;
      pLabelIm[offset]=++lval;
      // check the neighbours
      for (k = 0; k < graph; k++){
	bool = 1;
	neighbourOffset = offset + shft[k];
	// check whether neighbour has same value for all
	// channels
	if(pLabelIm[neighbourOffset] == NOTLABELED){
	  for (n = 0; n < nc; n++){
	    pInputIm = (PIX_TYPE *) GetImPtr(imArray[n]);
	    if ((( ((INT32) pInputIm[offset])-lambda) >pInputIm[neighbourOffset]) || \
		(( ((INT32) pInputIm[offset])+lambda)<pInputIm[neighbourOffset])){
	      bool = 0;
	      break;
	    }
	  }
	  // if yes => put in queue
	  if (bool){
	    fifo4_add(hq, (long int) (neighbourOffset));
	    pLabelIm[neighbourOffset] = lval;
	  }
	}
      }
      while (fifo4_empty(hq) == 0){
	crtOffset = (long int) fifo4_remove(hq);
	// check the neighbours
	for (k = 0; k < graph; k++){
	  // check whether Pixel has no label
	  neighbourOffset = crtOffset + shft[k];
	  if (pLabelIm[neighbourOffset] == NOTLABELED){
	    bool = 1;
	    // check whether neighbour has same value
	    // for all channels
	    for (n = 0; n < nc; n++){
	      pInputIm = (PIX_TYPE *) GetImPtr(imArray[n]);
	      /* srtOffset used here, was offset in previous version */
	      if (((  ((INT32) pInputIm[crtOffset])-lambda)>pInputIm[neighbourOffset]) || \
		   (( ((INT32) pInputIm[crtOffset])+lambda)<pInputIm[neighbourOffset])){
		bool = 0;
		break;
	      }
	    }
	    // if yes => put in queue
	    if (bool){
	      fifo4_add(hq,(long int) (neighbourOffset));
	      pLabelIm[neighbourOffset] = lval;
	    }
	  }
	}
      }
    }
  }
  free_fifo4(hq);
  for(i=0; i<nc; i++){
    subframebox(imArray[i], box);
  }
  subframebox(labelIm, box);
  return labelIm;
}
#include "us_undef.h"


IMAGE *labelImage(IMAGE **imArray, int nc, IMAGE *labelIm, int graph, long int lambda)
{
  switch (GetImDataType(imArray[0])){

  case t_UCHAR:
    return(uc_labelImage(imArray, nc, labelIm, graph, lambda));
    break;

  case t_USHORT:
    return(us_labelImage(imArray, nc, labelIm, graph, lambda));
    break;

  default:
    (void)sprintf(buf,"labelImage(): invalid multichannel pixel type (%d)\n", GetImDataType(imArray[0])); errputstr(buf);
  }
  return(NULL);
}


/*@}*/
