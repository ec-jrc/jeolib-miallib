/***********************************************************************
Author(s): Pierre Soille
Copyright (C) 2006-2020 European Union (Joint Research Centre)

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
#include <math.h>
#include "miallib.h"
#include "fifo.h"




/** \addtogroup group_seg
 *  @{
 */


#include "uc_def.h"
ERROR_TYPE uc_IsPartitionEqual(IMAGE *im1, IMAGE *im2, int *result)
{
  long int i, npix;
  PIX_TYPE *p1, *p2, maxi;
  G_TYPE *pg;
  int *lut;

  p1 = (PIX_TYPE *)GetImPtr(im1);
  p2 = (PIX_TYPE *)GetImPtr(im2);
  npix = GetImNPix(im1);

  /* get min & max values */
  pg = min_max(im1);
  if (pg == NULL)
    return ERROR;
  maxi = pg[1].uc_val;
  free((char *)pg);


  /* create lut */
  lut=(int *)calloc((size_t)maxi+1, sizeof(int));
  if (lut == NULL){
    (void)sprintf(buf,"uc_IsPartitionEqual(): not enough memory for lut!\n"); errputstr(buf);
    return ERROR;
  }
  for(i=0;i<maxi+1;i++)
    lut[i]=-1;

  /* init lut */
  for(i=0;i<npix;i++,p1++,p2++)
    if(lut[*p1]<0)
      lut[*p1]=*p2;

  p1 = (PIX_TYPE *)GetImPtr(im1);
  p2 = (PIX_TYPE *)GetImPtr(im2);
  for(i=0;i<npix;i++,p1++,p2++){
    if (*p2!=lut[*p1]){
      *result=i;
      break;
    }
  }

  free(lut);
  return NO_ERROR;
}
#include "uc_undef.h"

#include "us_def.h"
ERROR_TYPE us_IsPartitionEqual(IMAGE *im1, IMAGE *im2, int *result)
{
  long int i, npix;
  PIX_TYPE *p1, *p2, maxi;
  G_TYPE *pg;
  int *lut;

  p1 = (PIX_TYPE *)GetImPtr(im1);
  p2 = (PIX_TYPE *)GetImPtr(im2);
  npix = GetImNPix(im1);

  /* get min & max values */
  pg = min_max(im1);
  if (pg == NULL)
    return ERROR;
  maxi = pg[1].us_val;
  free((char *)pg);


  /* create lut */
  lut=(int *)calloc((size_t)maxi+1, sizeof(int));
  if (lut == NULL){
    (void)sprintf(buf,"us_IsPartitionEqual(): not enough memory for lut!\n"); errputstr(buf);
    return ERROR;
  }
  for(i=0;i<maxi+1;i++)
    lut[i]=-1;

  /* init lut */
  for(i=0;i<npix;i++,p1++,p2++)
    if(lut[*p1]<0)
      lut[*p1]=*p2;

  p1 = (PIX_TYPE *)GetImPtr(im1);
  p2 = (PIX_TYPE *)GetImPtr(im2);
  for(i=0;i<npix;i++,p1++,p2++){
    if (*p2!=lut[*p1]){
      *result=i;
      break;
    }
  }

  free(lut);
  return NO_ERROR;
}
#include "us_undef.h"


#include "u32_def.h"
ERROR_TYPE u32_IsPartitionEqual(IMAGE *im1, IMAGE *im2, int *result)
{
  long int i, npix;
  PIX_TYPE *p1, *p2, maxi;
  G_TYPE *pg;
  int *lut;

  p1 = (PIX_TYPE *)GetImPtr(im1);
  p2 = (PIX_TYPE *)GetImPtr(im2);
  npix = GetImNPix(im1);

  /* get min & max values */
  pg = min_max(im1);
  if (pg == NULL)
    return ERROR;
  maxi = pg[1].u32_val;
  free((char *)pg);


  /* create lut */
  lut=(int *)calloc((size_t)maxi+1, sizeof(int));
  if (lut == NULL){
    (void)sprintf(buf,"uc_IsPartitionEqual(): not enough memory for lut!\n"); errputstr(buf);
    return ERROR;
  }
  for(i=0;i<maxi+1;i++)
    lut[i]=-1;

  /* init lut */
  for(i=0;i<npix;i++,p1++,p2++)
    if(lut[*p1]<0)
      lut[*p1]=*p2;

  p1 = (PIX_TYPE *)GetImPtr(im1);
  p2 = (PIX_TYPE *)GetImPtr(im2);
  for(i=0;i<npix;i++,p1++,p2++){
    if (*p2!=lut[*p1]){
      *result=i;
      break;
    }
  }

  free(lut);
  return NO_ERROR;
}
#include "u32_undef.h"

ERROR_TYPE IsPartitionEqual(IMAGE *im1, IMAGE *im2, int *result)
{

  /* check for possible errors */
  if (szcompat(im1, im2) != NO_ERROR){
    (void)sprintf(buf,"ERROR in IsPartitionEqual(im1, im2, result): \
                images of different size and/or type\n"); errputstr(buf);
    return(ERROR);
  }

  switch (GetImDataType(im1)){

  case t_UCHAR:
    return(uc_IsPartitionEqual(im1, im2, result));
    break;
  case t_USHORT:
    return(us_IsPartitionEqual(im1, im2, result));
    break;
  case t_UINT32:
    return(u32_IsPartitionEqual(im1, im2, result));
    break;
  default:
    (void)sprintf(buf, "ERROR in IsPartitionEqual(): \
                invalid ImDataType\n"); errputstr(buf);
    return(ERROR);
  }
  return ERROR;
}


#include "u32_def.h"
ERROR_TYPE u32_IsPartitionFiner(IMAGE *im1, IMAGE *im2, int graph, unsigned long int *res)
{
  /* scan pixels of im1, look for all pixels with same label as current pixel
     while checking that the label in im2 does not vary.
     1 pixel thick border values are not checked for.
     First: 16 August 2006 (snow above 3000m anno della prugna e del 'safter')
  */

  IMAGE *i0; /* for flagging already queued pixels */
  FIFO4 *q;
  PIX_TYPE *p1, *p2, lbl1, lbl2;
  UCHAR *p0;
  unsigned long int nx, ny, nz, i, npix, ofs, ofsk;
  int box[BOXELEM];
  long int shft[27];
  int k;

  BOX_2D;
  nx=GetImNx(im1);
  ny=GetImNy(im1);
  nz=GetImNz(im1);
  npix=GetImNPix(im1);
  *res=-1;

  /* init i0, queue, and shft array */
  i0= (IMAGE *)create_image(t_UCHAR, nx, ny, nz);
  if (i0 == NULL){
    (void)sprintf(buf,"ul_ispartitionfiner(): not enough memory for flag image!\n"); errputstr(buf);
    return ERROR;
  }
  uc_framebox(i0, box, 1);
  q = create_fifo4(500);
  if (q == NULL){
    free_image(i0);
    return ERROR;
  }
  if (set_seq_shift(nx, ny, nz, graph, shft) == ERROR){
    free_image(i0);
    free_fifo4(q);
    return ERROR;
  }

  /* here we go */
  p0=(UCHAR *)GetImPtr(i0);
  p1=(PIX_TYPE *)GetImPtr(im1);
  p2=(PIX_TYPE *)GetImPtr(im2);

  for(i=0;i<npix;i++){
    if(p0[i]==0){
      p0[i]=1;
      lbl1=p1[i];
      lbl2=p2[i];
      for(k=0;k<graph;k++){
	ofsk=i+shft[k];
	if( (p1[ofsk]==lbl1) && (p0[ofsk]==0)){
	  p0[ofsk]=1;
	  fifo4_add(q,ofsk);
	}
      }
      while ( (ofs=fifo4_remove(q)) != 0){
	if(p2[ofs]!=lbl2){
	  printf("ofs=%d\n", (int)ofs);
	  *res=ofs;
	  free_image(i0);
	  free_fifo4(q);
	  return NO_ERROR;
	}
	for(k=0;k<graph;k++){
	  ofsk=ofs+shft[k];
	  if( (p1[ofsk]==lbl1) && (p0[ofsk]==0) ){
	    p0[ofsk]=1;
	    fifo4_add(q,ofsk);
	  }
	}
      }
    }
  }

  free_image(i0);
  free_fifo4(q);
  return NO_ERROR;
}
#include "u32_undef.h"

ERROR_TYPE IsPartitionFiner(IMAGE *im1, IMAGE *im2, int graph, unsigned long int *res)
{
  /*
  ** im1:
  ** im2:
  ** graph: connectivity used for creating the partitions
  ** *res: returned value -1 if partition of im1 is finer than that im2
          otherwise offset of first pixel encountered that violates this
	  ordering
  **
  ** returns: ERROR or NO_ERROR
  */

  if (szcompat(im1, im2) != NO_ERROR){
    (void)sprintf(buf,"ERROR in ispartitionfiner(im1, im2, graph, res): \
                images of different size or depth\n"); errputstr(buf);
    return(ERROR);
  }

  switch (GetImDataType(im1)){

  case t_UINT32:
    return(u32_IsPartitionFiner(im1, im2, graph, res));
    break;

  default:
    (void)sprintf(buf,"ispartitionfiner(): invalid pixel type\n"); errputstr(buf);
    return(ERROR);
  }
  return(ERROR);
}

/*@}*/
