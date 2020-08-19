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

/**
 * @file  mcisrg.c
 * @author Dominik Brunner and Pierre Soille
 * @date
 *
 * @details see also \cite brunner-soille2007
 *
 */






/***************************************************************************
                            mcisrg.c  -  description

  mcisrg stands for multi channel improved seeded region growing algorithm.
  This is a adapted version for multispectral images of the algorithm
  introduced by Andrew Mehnert and Paul Jackway
                            -------------------
    begin                : Thu Apr 22 2004
***************************************************************************/



#define DELTA_MAX  DOUBLE_MAX


#include <stdio.h>
#include <stdlib.h>
#include "miallib.h"
#include "fifo.h"
#include "pqueueExact.h"
#include "regionMean.h"
#include "math.h"
#include "base.h"
#include "mcisrg.h"

extern int us_rmAddValueOriginal(struct regionMean *rm, IMAGE **imap, long int offset);
extern int us_rmAddValue(struct regionMean *rm, IMAGE **imap, long int offset);
extern double us_rmGetDistanceToRM(struct regionMean *rm, IMAGE **imap, long int offset);
extern double us_rmGetDistanceToOriginalRM(struct regionMean *rm, IMAGE **imap, long int offset);


/** \addtogroup group_seg
 *  @{
 */



/*
 *  mcisrg:   multichannel improved seeded region growing algorithm.
 *
 *  Parameters:
 *
 *      IMAGE **imap:	      multichannel image
 *
 *      int nc:			            number of channels
 *
 *      IMAGE *seedsIm:	        image of seeds
 *
 *      int graph:       graph (neighbour relationship)
 *
 *      long int regionNumber:  number of Regions
 *
 *      version                 version of mcisrg algorithm. Versions are
 *                              0  (compare to whole region)
 *                              1  (compare to original seeds)
 *                              2  (compare to pixel neighbours)
 *
 *
 *  Return values:
 *
 *      ERROR     if error occured
 *
 *      NO_ERROR  if no error occured
 */

#include "uc_def.h"
ERROR_TYPE uc_mcisrg(IMAGE **imap, int nc, IMAGE *seedsIm, int graph, long int regionNumber, int version)
{
  long int l, k, shft[27], offset, current, ofsk, ofsj, nxny;
  double delta, deltacrt=0.0, hprior,j;
  float mean;
  PIX_TYPE **pIm;
  LBL_TYPE *pSeeds, *pSeedsCrt, *pSeedsK, *pSeedsO, *pSeedsO2, val=0, crtSeedValue;
  FIFO4 *nhq, *hq;
  struct pqueue *pq;
  struct regionMean * rm, crtRegionMean;
  int n, nx, ny;
  int box[6];
  int i;
  char fname[50];

  PQDATUM apqd[1];
  struct node *pqd;

  // initilize box for frame which shall be added in order to solve
  // problem with neighbours
  box[0] = 1;
  box[1] = 1;
  box[2] = 1;
  box[3] = 1;
  box[4] = 0;
  box[5] = 0;

  // make checks on datatypes, and wheather dimensions of images in
  // imap and labelIm fit together
  if ((nc < 1) || (regionNumber < 1)){
    (void)sprintf(buf,"mcisrg(): invalid nc or regionNumber\n"); errputstr(buf);
    return ERROR;
  }

  //make check whether version has an authorised value
  if(!((version == WHOLE_REGION) || (version == ORIGINAL_SEED) || (version == PIXEL_NEIGHBOUR))){
    printf("parameter value is %i. Only 0,1,2 are allowed.\n", version);
    return ERROR;
  }

  for (i = 0; i < nc; i++){
    if ((GetImNx(imap[i]) != GetImNx(seedsIm)) || (GetImNy(imap[i]) != GetImNy(seedsIm)) \
	|| (GetImDataType(imap[i]) != GetImDataType(imap[0])) ){
      (void)sprintf(buf,"mcisrg(): at least one channel is not matching the image of seeds (different size)\n"); errputstr(buf);
      return ERROR;
    }
  }
  if (GetImDataType(seedsIm) != t_LBL_TYPE){
    return ERROR;
  }
  //add border
  for (i = 0; i < nc; i++){
    if (generic_addframebox(imap[i], box, BORDER) == ERROR){
      return ERROR;
    }
  }
  if (u32_addframebox(seedsIm, box, BORDER) == ERROR){
    return ERROR;
  }

  pIm = (PIX_TYPE **)calloc(nc, sizeof(PIX_TYPE *));

  nx = GetImNx(seedsIm);
  ny = GetImNy(seedsIm);

  nhq = create_fifo4((nx*ny)/100L); /* neighbouring queue */
  if (nhq == NULL){
    free((PIX_TYPE **)pIm);
    return ERROR;
  }

  hq = create_fifo4((nx*ny)/100L); /* holding queue */
  if (hq == NULL){
    free_fifo4(nhq);
    free((PIX_TYPE **)pIm);
    return ERROR;
  }

  pq = (struct pqueue *)pqExactInit(NULL, 100);  /* priority queue */
  if (pq == NULL){
    free_fifo4(nhq);
    free_fifo4(hq);
    free((PIX_TYPE **) pIm);
    return ERROR;
  }

  // add frameboxes so that neighbours are always in image
  for (i = 0; i < nc; i++){
    pIm[i]=(PIX_TYPE *) GetImPtr(imap[i]);
  }

  if (set_seq_shift(GetImNx(seedsIm), GetImNy(seedsIm), GetImNz(seedsIm), graph, shft) == ERROR){
    free_fifo4(nhq);
    free_fifo4(hq);
    free((PIX_TYPE **) pIm);
    return ERROR;
  }

  pSeeds = (LBL_TYPE *) GetImPtr(seedsIm);

  //update nx,ny,nz after adding border
  nx = GetImNx(seedsIm);
  ny = GetImNy(seedsIm);

  //initialize regionMean and initialise the neighbouring queue NHQ
  //(with their offset to origin)
  rm = (struct regionMean *) (calloc(regionNumber+1, sizeof(struct regionMean)));
  for(i=0; i<regionNumber+1; i++){
    rmInit(rm+i, nc);
  }
  rmInit(&crtRegionMean, nc);
  nxny=nx*ny;
  for(offset=0; offset<nxny; offset++){
    pSeedsCrt=pSeeds+offset;
    if((*pSeedsCrt)>=SEED){
      uc_rmAddValueOriginal(rm+(*pSeedsCrt), imap, offset);
    }
    else if ((*pSeedsCrt)==NOSEED){ /* usually, there will be less NOSEED then SEED pixels */
      for (k=0; k < graph; k++){
	current = offset + shft[k];
	pSeedsO = pSeeds +current;
	if (*pSeedsO >= SEED){
	  fifo4_add(nhq, (long int) offset);
	  *(pSeedsCrt)=IN_NHQ;
	  break;
	}
      }
    }
  }

  sprintf(fname,"/tmp/mcisrgBEFORE.tif");
  write_tiff(imap[0], fname);

  /* here we go */
  while ( (fifo4_empty(nhq)==0) || (pqExactPeek(pq, apqd) != NULL) ){
    //examine the neighbours of pixels in the neighbouring holding queue
    while ( (offset = (long int)fifo4_remove(nhq)) ){
      delta=DELTA_MAX;
      for (k=0; k < graph; k++){
	ofsk=offset+shft[k];
	pSeedsO=pSeeds + ofsk;
	crtSeedValue=*pSeedsO;
	if (crtSeedValue>=SEED){
	  //switch for different versions of algorithm
	  if(version==PIXEL_NEIGHBOUR){
	    clearRegionMean(&crtRegionMean);
	    uc_rmAddValue(&crtRegionMean, imap, ofsk);
	    for(l=0; l<graph;l++){
	      ofsj = offset+shft[l];
	      pSeedsO2 = pSeeds + ofsj;
	      if((*pSeedsO2 == crtSeedValue) && (ofsj!=ofsk)){
		uc_rmAddValue(&crtRegionMean, imap, ofsj);
	      }
	    }
	    deltacrt=uc_rmGetDistanceToRM(&crtRegionMean, imap, offset);
	  }
	  else if(version==ORIGINAL_SEED){
	    deltacrt=uc_rmGetDistanceToOriginalRM(rm+(*pSeedsO), imap, offset);
	  }
	  else if(version==WHOLE_REGION){
	    deltacrt=uc_rmGetDistanceToRM(rm+(*pSeedsO), imap, offset);
	  }
	  if (deltacrt < delta){
	    delta=deltacrt;
	    val = crtSeedValue;//stores the region-label as value
	  }
	}
      }

      pqd = (PQDATUM )malloc(sizeof(struct node));
      if (pqd == NULL){
	(void)sprintf(buf,"ERROR in mcisrg() not enough memory\n");  errputstr(buf);
	for(i=0; i<regionNumber+1; i++){
	  freeRegionMean(rm+i);
	}
	free(rm);
	free(pIm);
	free_fifo4(nhq);
	free_fifo4(hq);
	freeExact_pq(pq);
	return 1;
      }
      pqd->prio = (unsigned int) delta;
      pqd->val   = val;
      pqd->offset= offset;
      pqExactMinInsert(pq, pqd);
      *(pSeeds+offset) = IN_PQ;
    }

    if (pqExactPeek(pq, apqd) != NULL){
      hprior = (*apqd)->prio;
      pqExactMinRemove(pq, apqd);
      offset=apqd[0]->offset;
      pSeedsO=pSeeds+offset;
      if (*pSeedsO == IN_PQ){
	*pSeedsO=(*apqd)->val;//assign the region label to the pixel
	fifo4_add(hq, apqd[0]->offset);
      }
      free((char*) *apqd);
      while (pqExactPeek(pq, apqd) != NULL)  {
	if (((*apqd)->prio) != hprior) break;
	pqExactMinRemove(pq, apqd);
	offset=apqd[0]->offset;
	pSeedsO=pSeeds+offset;
	if (*pSeedsO == IN_PQ){
	  *pSeedsO=(*apqd)->val;//assign the region label to the pixel
	  fifo4_add(hq, apqd[0]->offset);
	}
	free((char*) *apqd);
      }
    }
    while ((offset = (long int)fifo4_remove(hq)) ){
      pSeedsO = pSeeds+offset;
      //update color information
      uc_rmAddValue(rm+(*pSeedsO), imap, offset);
      for (k=0; k < graph; k++){
	pSeedsK = pSeedsO + shft[k];
	if ((*pSeedsK == NOSEED) || (*pSeedsK == IN_PQ)){
	  fifo4_add(nhq, (long int)(offset+shft[k]));
	  *pSeedsK=IN_NHQ;
	}
      }
    }
  }

  sprintf(fname,"/tmp/mcisrgAFTER.tif");
  write_tiff(imap[0], fname);

  //admit new color value to each channel in image
  nxny=nx*ny;
  for(offset=0; offset<nxny; offset++){
    pSeedsCrt=pSeeds+offset;
    for (n=0; n<nc; n++){
      mean = *(((rm+ *(pSeeds+offset))->meanValue)+n);
      if(modf((double)mean, &j)>=0.5)
	*(pIm[n]+offset) = (PIX_TYPE) ceil((double) mean);
      else
	*(pIm[n]+offset) = (PIX_TYPE) floor((double) mean);
    }
  }

  for(i=0; i<regionNumber+1; i++){
    freeRegionMean(rm+i);
  }
  free(rm);
  free(pIm);
  freeRegionMean(&crtRegionMean);
  free_fifo4(nhq);
  free_fifo4(hq);
  freeExact_pq(pq);

  //substract border
  for(i=0; i<nc; i++)
    subframebox(imap[i], box);
  subframebox(seedsIm, box);

  return NO_ERROR;
}
#include "uc_undef.h"


#include "us_def.h"
ERROR_TYPE us_mcisrg(IMAGE **imap, int nc, IMAGE *seedsIm, int graph, long int regionNumber, int version)
{
  long int l, k, shft[27], offset, current, ofsk, ofsj, nxny;
  double delta, deltacrt=0.0, hprior,j;
  float mean;
  PIX_TYPE **pIm;
  LBL_TYPE *pSeeds, *pSeedsCrt, *pSeedsK, *pSeedsO, *pSeedsO2, val=0, crtSeedValue;
  FIFO4 *nhq, *hq;
  struct pqueue *pq;
  struct regionMean * rm, crtRegionMean;
  int n, nx, ny;
  int box[6];
  int i;
  char fname[50];

  PQDATUM apqd[1];
  struct node *pqd;

  // initilize box for frame which shall be added in order to solve
  // problem with neighbours
  box[0] = 1;
  box[1] = 1;
  box[2] = 1;
  box[3] = 1;
  box[4] = 0;
  box[5] = 0;

  // make checks on datatypes, and wheather dimensions of images in
  // imap and labelIm fit together
  if ((nc < 1) || (regionNumber < 1)){
    (void)sprintf(buf,"mcisrg(): invalid nc or regionNumber\n"); errputstr(buf);
    return ERROR;
  }

  //make check whether version has an authorised value
  if(!((version == WHOLE_REGION) || (version == ORIGINAL_SEED) || (version == PIXEL_NEIGHBOUR))){
    printf("parameter value is %i. Only 0,1,2 are allowed.\n", version);
    return ERROR;
  }

  for (i = 0; i < nc; i++){
    if ((GetImNx(imap[i]) != GetImNx(seedsIm)) || (GetImNy(imap[i]) != GetImNy(seedsIm)) \
	|| (GetImDataType(imap[i]) != GetImDataType(imap[0])) ){
      (void)sprintf(buf,"mcisrg(): at least one channel is not matching the image of seeds (different size)\n"); errputstr(buf);
      return ERROR;
    }
  }
  if (GetImDataType(seedsIm) != t_LBL_TYPE){
    return ERROR;
  }
  //add border
  for (i = 0; i < nc; i++){
    if (us_addframebox(imap[i], box, BORDER) == ERROR){
      return ERROR;
    }
  }
  if (u32_addframebox(seedsIm, box, BORDER) == ERROR){
    return ERROR;
  }

  pIm = (PIX_TYPE **)calloc(nc, sizeof(PIX_TYPE *));

  nx = GetImNx(seedsIm);
  ny = GetImNy(seedsIm);

  nhq = create_fifo4((nx*ny)/100L); /* neighbouring queue */
  if (nhq == NULL){
    free((PIX_TYPE **)pIm);
    return ERROR;
  }

  hq = create_fifo4((nx*ny)/100L); /* holding queue */
  if (hq == NULL){
    free_fifo4(nhq);
    free((PIX_TYPE **)pIm);
    return ERROR;
  }

  pq = (struct pqueue *)pqExactInit(NULL, 100);  /* priority queue */
  if (pq == NULL){
    free_fifo4(nhq);
    free_fifo4(hq);
    free((PIX_TYPE **) pIm);
    return ERROR;
  }

  // add frameboxes so that neighbours are always in image
  for (i = 0; i < nc; i++){
    pIm[i]=(PIX_TYPE *) GetImPtr(imap[i]);
  }

  if (set_seq_shift(GetImNx(seedsIm), GetImNy(seedsIm), GetImNz(seedsIm), graph, shft) == ERROR){
    free_fifo4(nhq);
    free_fifo4(hq);
    free((PIX_TYPE **) pIm);
    return ERROR;
  }

  pSeeds = (LBL_TYPE *) GetImPtr(seedsIm);

  //update nx,ny,nz after adding border
  nx = GetImNx(seedsIm);
  ny = GetImNy(seedsIm);

  //initialize regionMean and initialise the neighbouring queue NHQ
  //(with their offset to origin)
  rm = (struct regionMean *) (calloc(regionNumber+1, sizeof(struct regionMean)));
  for(i=0; i<regionNumber+1; i++){
    rmInit(rm+i, nc);
  }
  rmInit(&crtRegionMean, nc);
  nxny=nx*ny;
  for(offset=0; offset<nxny; offset++){
    pSeedsCrt=pSeeds+offset;
    if((*pSeedsCrt)>=SEED){
      us_rmAddValueOriginal(rm+(*pSeedsCrt), imap, offset);
    }
    else if ((*pSeedsCrt)==NOSEED){ /* usually, there will be less NOSEED then SEED pixels */
      for (k=0; k < graph; k++){
	current = offset + shft[k];
	pSeedsO = pSeeds +current;
	if (*pSeedsO >= SEED){
	  fifo4_add(nhq, (long int) offset);
	  *(pSeedsCrt)=IN_NHQ;
	  break;
	}
      }
    }
  }

  sprintf(fname,"/tmp/mcisrgBEFORE.tif");
  write_tiff(imap[0], fname);

  /* here we go */
  while ( (fifo4_empty(nhq)==0) || (pqExactPeek(pq, apqd) != NULL) ){
    //examine the neighbours of pixels in the neighbouring holding queue
    while ( (offset = (long int)fifo4_remove(nhq)) ){
      delta=DELTA_MAX;
      for (k=0; k < graph; k++){
	ofsk=offset+shft[k];
	pSeedsO=pSeeds + ofsk;
	crtSeedValue=*pSeedsO;
	if (crtSeedValue>=SEED){
	  //switch for different versions of algorithm
	  if(version==PIXEL_NEIGHBOUR){
	    clearRegionMean(&crtRegionMean);
	    us_rmAddValue(&crtRegionMean, imap, ofsk);
	    for(l=0; l<graph;l++){
	      ofsj = offset+shft[l];
	      pSeedsO2 = pSeeds + ofsj;
	      if((*pSeedsO2 == crtSeedValue) && (ofsj!=ofsk)){
		us_rmAddValue(&crtRegionMean, imap, ofsj);
	      }
	    }
	    deltacrt=us_rmGetDistanceToRM(&crtRegionMean, imap, offset);
	  }
	  else if(version==ORIGINAL_SEED){
	    deltacrt=us_rmGetDistanceToOriginalRM(rm+(*pSeedsO), imap, offset);
	  }
	  else if(version==WHOLE_REGION){
	    deltacrt=us_rmGetDistanceToRM(rm+(*pSeedsO), imap, offset);
	  }
	  if (deltacrt < delta){
	    delta=deltacrt;
	    val = crtSeedValue;//stores the region-label as value
	  }
	}
      }

      pqd = (PQDATUM )malloc(sizeof(struct node));
      if (pqd == NULL){
	(void)sprintf(buf,"ERROR in mcisrg() not enough memory\n");  errputstr(buf);
	for(i=0; i<regionNumber+1; i++){
	  freeRegionMean(rm+i);
	}
	free(rm);
	free(pIm);
	free_fifo4(nhq);
	free_fifo4(hq);
	freeExact_pq(pq);
	return 1;
      }
      pqd->prio = (unsigned int) delta;
      pqd->val   = val;
      pqd->offset= offset;
      pqExactMinInsert(pq, pqd);
      *(pSeeds+offset) = IN_PQ;
    }

    if (pqExactPeek(pq, apqd) != NULL){
      hprior = (*apqd)->prio;
      pqExactMinRemove(pq, apqd);
      offset=apqd[0]->offset;
      pSeedsO=pSeeds+offset;
      if (*pSeedsO == IN_PQ){
	*pSeedsO=(*apqd)->val;//assign the region label to the pixel
	fifo4_add(hq, apqd[0]->offset);
      }
      free((char*) *apqd);
      while (pqExactPeek(pq, apqd) != NULL)  {
	if (((*apqd)->prio) != hprior) break;
	pqExactMinRemove(pq, apqd);
	offset=apqd[0]->offset;
	pSeedsO=pSeeds+offset;
	if (*pSeedsO == IN_PQ){
	  *pSeedsO=(*apqd)->val;//assign the region label to the pixel
	  fifo4_add(hq, apqd[0]->offset);
	}
	free((char*) *apqd);
      }
    }
    while ((offset = (long int)fifo4_remove(hq)) ){
      pSeedsO = pSeeds+offset;
      //update color information
      us_rmAddValue(rm+(*pSeedsO), imap, offset);
      for (k=0; k < graph; k++){
	pSeedsK = pSeedsO + shft[k];
	if ((*pSeedsK == NOSEED) || (*pSeedsK == IN_PQ)){
	  fifo4_add(nhq, (long int)(offset+shft[k]));
	  *pSeedsK=IN_NHQ;
	}
      }
    }
  }

  sprintf(fname,"/tmp/mcisrgAFTER.tif");
  write_tiff(imap[0], fname);

  //admit new color value to each channel in image
  nxny=nx*ny;
  for(offset=0; offset<nxny; offset++){
    pSeedsCrt=pSeeds+offset;
    for (n=0; n<nc; n++){
      mean = *(((rm+ *(pSeeds+offset))->meanValue)+n);
      if(modf((double)mean, &j)>=0.5)
	*(pIm[n]+offset) = (PIX_TYPE) ceil((double) mean);
      else
	*(pIm[n]+offset) = (PIX_TYPE) floor((double) mean);
    }
  }

  for(i=0; i<regionNumber+1; i++){
    freeRegionMean(rm+i);
  }
  free(rm);
  free(pIm);
  freeRegionMean(&crtRegionMean);
  free_fifo4(nhq);
  free_fifo4(hq);
  freeExact_pq(pq);

  //substract border
  for(i=0; i<nc; i++)
    subframebox(imap[i], box);
  subframebox(seedsIm, box);

  return NO_ERROR;
}
#include "us_undef.h"


ERROR_TYPE mcisrg(IMAGE **imap, int nc, IMAGE *seedsIm, int graph, long int regionNumber, int version)
{
  switch (GetImDataType(imap[0])){
  case t_UCHAR:
    return(uc_mcisrg(imap, nc, seedsIm, graph, regionNumber, version));
    break;
  case t_USHORT:
    return(us_mcisrg(imap, nc, seedsIm, graph, regionNumber, version));
    break;

  default:
    (void)sprintf(buf,"mcisrg(): invalid pixel type\n"); errputstr(buf);
  }
  return(ERROR);
}

/*@}*/
