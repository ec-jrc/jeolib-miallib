
/***********************************************************************
Author(s): Pierre Soille
Copyright (C) 2004-2022 European Union (Joint Research Centre)

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
#include "miallib.h"
#include "fifo.h"

#ifndef max
#define max(a,b) ((a)>(b)?(a):(b))
#endif

/** @defgroup group_compose Morphological Compositing functions
 *  Functions related to Morphological Image Compositing \cite soille2006pami
 *  https://doi.org/10.1109/TPAMI.2006.99
 *  @{
 */


/*
  COMPOSITING: first 28-9-2004
*/

#include "pqueue.h"



#include "uc_def.h" /* for mask, marker and g */
#define GIM_TYPE      UCHAR
#define MARKERIM_TYPE UCHAR
#define PIXLBL_TYPE   USHORT
ERROR_TYPE us_compose(IMAGE *mark, IMAGE *mask, IMAGE *g, IMAGE *lbl, int graph)
{
  /*
    mark:  indicates index of image each pixel
           (0 value => no marker), overwritten by output.
	   Note: border of mark is set to 0!!!
    mask:  segmentation function for watershed propagation
    g:     image with number of overlap for each pixel
           (0=>outside of ROI)  border of g is set to 0!!!
           New version where g is actually created by gorder.
    lbl:   lbl(x)=\sum_2^i |  f_i(x) is in ROI of f_i
    graph: integer for connectivity
  */

  struct pqueue **pq; /* array of priority queues */
  PQDATUM apqd[1];
  struct node *pqd;

  G_TYPE *pgval;
  GIM_TYPE card, maxii, *pg;
  PIX_TYPE *pmask;
  MARKERIM_TYPE *pmark;
  PIXLBL_TYPE *plbl;

  int i,j,k;
  long int npix, ofs, ofsk;
  long int shift[27];
  int box[6];

  /* test image types and set border */
  if ( (GetImDataType(g)!=t_UCHAR) || (GetImDataType(mark)!=t_UCHAR) ){
    (void)sprintf(buf,"ERROR in compose(mark,mask,g,graph): \
                invalid type for g or mark\n"); errputstr(buf);
    return ERROR;
  }
  if (GetImNy(g) == 1)
    {BOX_1D;}
  else if (GetImNz(g) == 1)
    {BOX_2D;}
  else
    {BOX_3D;}
  uc_framebox(g,box,0);
  uc_framebox(mark,box,0);

  /* get max value of g */
  pgval = min_max(g);
  if (pgval == NULL)
    return(ERROR);
  maxii  = pgval[1].uc_val;
  free((char *)pgval);

  /* allocate maxii+1 priority queues */
  pq=(struct pqueue **)calloc(maxii+1,sizeof(struct pqueue *));
  for (i=2; i<=maxii; i++){
    pq[i] = (struct pqueue *)pqinit(NULL, 1024);  /* priority queue */
    if (pq[i] == NULL){
      for (j=2; j<i; j++)
	free_pq(pq[j]);
      return ERROR;
    }
  }
  /* initialise priority queues */
  npix =GetImNPix(g);
  pg   =(GIM_TYPE *)GetImPtr(g);
  pmark=(MARKERIM_TYPE *)GetImPtr(mark);
  pmask=(PIX_TYPE *)GetImPtr(mask);
  plbl=(PIXLBL_TYPE *)GetImPtr(lbl);
  set_seq_shift(GetImNx(g), GetImNy(g), GetImNz(g), graph, shift);
  for(i=0; i<npix; i++){
    if(pmark[i]){
      for(k=0;k<graph;k++){
	ofs=i+shift[k];
	if (pmark[ofs]==0){
	  if (plbl[i] & plbl[ofs]){
	    if (pg[i]){
	      if (pg[i]!=pg[ofs])
		continue;
	    }
	    card=pg[ofs];
	    if(card>1){
	      pqd = (PQDATUM )malloc(sizeof(struct node));
	      pqd->prio = pmask[ofs];
	      pqd->val =  pmark[i];
	      pqd->offset= ofs;
	      pqmininsert(pq[card], pqd);
	      pg[ofs]=0; /* reset number of overlap */
	    }
	  }
	}
      }
    }
  }
  printf("message: composition heap initialised\n");
  /* here we go */
  for (i=2;i<=maxii;i++){
    while ( pqpeek(pq[i], apqd) != NULL ){
      pqminremove(pq[i], apqd);
      ofs = apqd[0]->offset;
      pmark[ofs] = apqd[0]->val;
      for(k=0;k<graph;k++){
	ofsk=ofs+shift[k];
	if ( (pmark[ofsk]==0) && (pg[ofsk]==i) ){
	  card=pg[ofsk];
	  if(card>1){
	    pqd = (PQDATUM )malloc(sizeof(struct node));
	    /* if card > i: do not compute max!!! */
	    if (card>i)
	      pqd->prio = pmask[ofsk];
	    else
	      pqd->prio = max(pmask[ofsk],apqd[0]->prio);
	    pqd->val =  pmark[ofs];
	    pqd->offset= ofsk;
	    pqmininsert(pq[card], pqd);
	    pg[ofsk]=0; /* reset number of overlap */
	  }
	}
      }
      free((char*) *apqd);
    }
  }
  /* set pseudo-indice values to actual indices */

  for (j=2; j<=maxii; j++)
    free_pq(pq[j]);
  free(pq);
  return NO_ERROR;
}
#undef GIM_TYPE
#undef MARKERIM_TYPE
#undef PIXLBL_TYPE
#include "uc_undef.h"



#include "uc_def.h" /* for mask, marker and g */
#define GIM_TYPE      UCHAR
#define MARKERIM_TYPE UCHAR
#define PIXLBL_TYPE   UINT32
ERROR_TYPE u32_compose(IMAGE *mark, IMAGE *mask, IMAGE *g, IMAGE *lbl, int graph)
{
  /*
    mark:  indicates index of image each pixel
           (0 value => no marker), overwritten by output.
	   Note: border of mark is set to 0!!!
    mask:  segmentation function for watershed propagation
    g:     image with number of overlap for each pixel
           (0=>outside of ROI)  border of g is set to 0!!!
           New version where g is actually created by gorder.
    lbl:   lbl(x)=\sum_2^i |  f_i(x) is in ROI of f_i
    graph: integer for connectivity
  */

  struct pqueue **pq; /* array of priority queues */
  PQDATUM apqd[1];
  struct node *pqd;

  G_TYPE *pgval;
  GIM_TYPE card, maxii, *pg;
  PIX_TYPE *pmask;
  MARKERIM_TYPE *pmark;
  PIXLBL_TYPE *plbl;

  int i,j,k;
  long int npix, ofs, ofsk;
  long int shift[27];
  int box[6];

  /* test image types and set border */
  if ( (GetImDataType(g)!=t_UCHAR) || (GetImDataType(mark)!=t_UCHAR) ){
    (void)sprintf(buf,"ERROR in compose(mark,mask,g,graph): \
                invalid type for g or mark\n"); errputstr(buf);
    return ERROR;
  }
  if (GetImNy(g) == 1)
    {BOX_1D;}
  else if (GetImNz(g) == 1)
    {BOX_2D;}
  else
    {BOX_3D;}
  uc_framebox(g,box,0);
  uc_framebox(mark,box,0);

  /* get max value of g */
  pgval = min_max(g);
  if (pgval == NULL)
    return(ERROR);
  maxii  = pgval[1].uc_val;
  free((char *)pgval);

  /* allocate maxii+1 priority queues */
  pq=(struct pqueue **)calloc(maxii+1,sizeof(struct pqueue *));
  for (i=2; i<=maxii; i++){
    pq[i] = (struct pqueue *)pqinit(NULL, 1024);  /* priority queue */
    if (pq[i] == NULL){
      for (j=2; j<i; j++)
	free_pq(pq[j]);
      return ERROR;
    }
  }
  /* initialise priority queues */
  npix =GetImNPix(g);
  pg   =(GIM_TYPE *)GetImPtr(g);
  pmark=(MARKERIM_TYPE *)GetImPtr(mark);
  pmask=(PIX_TYPE *)GetImPtr(mask);
  plbl=(PIXLBL_TYPE *)GetImPtr(lbl);
  set_seq_shift(GetImNx(g), GetImNy(g), GetImNz(g), graph, shift);
  for(i=0; i<npix; i++){
    if(pmark[i]){
      for(k=0;k<graph;k++){
	ofs=i+shift[k];
	if (pmark[ofs]==0){
	  if (plbl[i] & plbl[ofs]){
	    if (pg[i]){
	      if (pg[i]!=pg[ofs])
		continue;
	    }
	    card=pg[ofs];
	    if(card>1){
	      pqd = (PQDATUM )malloc(sizeof(struct node));
	      pqd->prio = pmask[ofs];
	      pqd->val =  pmark[i];
	      pqd->offset= ofs;
	      pqmininsert(pq[card], pqd);
	      pg[ofs]=0; /* reset number of overlap */
	    }
	  }
	}
      }
    }
  }
  printf("message: composition heap initialised\n");
  /* here we go */
  for (i=2;i<=maxii;i++){
    while ( pqpeek(pq[i], apqd) != NULL ){
      pqminremove(pq[i], apqd);
      ofs = apqd[0]->offset;
      pmark[ofs] = apqd[0]->val;
      for(k=0;k<graph;k++){
	ofsk=ofs+shift[k];
	if ( (pmark[ofsk]==0) && (pg[ofsk]==i) ){
	  card=pg[ofsk];
	  if(card>1){
	    pqd = (PQDATUM )malloc(sizeof(struct node));
	    /* if card > i: do not compute max!!! */
	    if (card>i)
	      pqd->prio = pmask[ofsk];
	    else
	      pqd->prio = max(pmask[ofsk],apqd[0]->prio);
	    pqd->val =  pmark[ofs];
	    pqd->offset= ofsk;
	    pqmininsert(pq[card], pqd);
	    pg[ofsk]=0; /* reset number of overlap */
	  }
	}
      }
      free((char*) *apqd);
    }
  }
  /* set pseudo-indice values to actual indices */

  for (j=2; j<=maxii; j++)
    free_pq(pq[j]);
  free(pq);
  return NO_ERROR;
}
#undef GIM_TYPE
#undef MARKERIM_TYPE
#undef PIXLBL_TYPE
#include "uc_undef.h"



/* ERROR_TYPE uc_compose(IMAGE *mark, IMAGE *mask, IMAGE *g, IMAGE *lbl, int graph) */
/* { */
/*   switch(GetImDataType(mark)){ */
/*   case t_UCHAR: */
/*     return(uc_uc_compose(mark,mask,g,lbl,graph)); */
/*     break; */
/*   default: */
/*     (void)sprintf(buf,"compose(): invalid pixel type for mark image\n"); errputstr(buf); */
/*     return(ERROR); */
/*   } */
/*   return(NO_ERROR); */
/* } */

ERROR_TYPE compose(IMAGE *mark, IMAGE *mask, IMAGE *g, IMAGE *lbl, int graph)
{
  if ( (szgeocompat(mark, mask) != NO_ERROR) || \
       (szgeocompat(mask, g) != NO_ERROR)    || \
       (szgeocompat(mask, lbl) != NO_ERROR)  ){
    (void)sprintf(buf,"ERROR in compose(mark,mask,g,graph): \
                images of different size\n"); errputstr(buf);
    return ERROR;
  }

  if ( (GetImDataType(mark)!=t_UCHAR) || (GetImDataType(mask)!=t_UCHAR) \
       || (GetImDataType(g)!=t_UCHAR) ){
    (void)sprintf(buf,"ERROR in compose(mark,mask,g,lbl,graph): \
                image mark, mask, and g must be of type UCHAR\n");
    errputstr(buf);
    return ERROR;
  }

/*   if ( (GetImDataType(lbl)!=t_USHORT) || (GetImDataType(lbl)!=t_UINT32) ){ */
/*     (void)sprintf(buf,"ERROR in compose(mark,mask,g,lbl,graph): \ */
/*                 image lbl must be opf type USHORT or UINT32\n"); */
/*     errputstr(buf); */
/*     return ERROR; */
/*   } */

  switch(GetImDataType(lbl)){
  case t_USHORT:
    return(us_compose(mark,mask,g,lbl,graph));
    break;
  case t_UINT32:
    return(u32_compose(mark,mask,g,lbl,graph));
    break;
  default:
    (void)sprintf(buf,"compose(): invalid pixel type for lbl image\n"); errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}

#include "uc_def.h"
#define HST1D_TYPE_MAX INT32_MAX
ERROR_TYPE uc_resolveLabels(IMAGE *imlbl, IMAGE *imlut, IMAGE *imlutback, int graph)
{
  UINT32 *ngbval, maxfreq;
  int i, j, k, kmax=GetImNx(imlutback);
  long int shft[27];
  long int npix=GetImNPix(imlbl);
  int box[6];
  PIX_TYPE *plbl=(PIX_TYPE *)GetImPtr(imlbl);
  PIX_TYPE *p1, *p2, maxlbl, lblmaj;
  HST1D_TYPE *plut=(HST1D_TYPE *)GetImPtr(imlut);
  HST1D_TYPE *plutback=(HST1D_TYPE *)GetImPtr(imlutback), crtlbl;
  FIFO4 *q, *qall;
  G_TYPE *pg;

  /* take graph into account */
  if (set_seq_shift(GetImNx(imlbl), GetImNy(imlbl), GetImNz(imlbl), graph, shft) == ERROR)
    return ERROR;

  /* get min & max values */
  pg = min_max(imlbl);
  if (pg == NULL)
    return(ERROR);
  maxlbl = pg[1].uc_val;
  free((char *)pg);
  ngbval=(UINT32 *)calloc(maxlbl+1,sizeof(UINT32));
  if (ngbval==NULL)
    return ERROR;

  q = create_fifo4(1024);
  if (q == NULL){
    free((char*)ngbval);
    return ERROR;
  }
  qall = create_fifo4(1024);
  if (qall == NULL){
    free((char*)ngbval);
    free_fifo4(q);
    return ERROR;
  }

  if (GetImNy(imlbl) == 1)
    {BOX_1D;}
  else if (GetImNz(imlbl) == 1)
    {BOX_2D;}
  else
    {BOX_3D;}
  /* last bit of imlbl used for flagging and borders !
     That is, maximum label value should be PIX_MAX xor PIX_MSB */
  uc_framebox(imlbl, box, PIX_MSB);

  /* scan image */
  for(i=0;i<npix;i++,plbl++){
    if ( (*plbl & PIX_MSB) || (plut[*plbl]==0) || (*plbl == 0) )
      continue;
    crtlbl=*plbl;
    *plbl &= PIX_MSB;
    fifo4_add(q, (long int)plbl);
    fifo4_add(qall, (long int)plbl);
    memset(ngbval,0x0,(maxlbl+1)*sizeof(UINT32));
    while ((p1 = (PIX_TYPE *)fifo4_remove(q))){
      for (k=0; k < graph; k++){
	p2 = p1 + shft[k];
	if (*p2<PIX_MSB){
	  if ( (plut[*p2] == 0) && (plutback[*p2] & plutback[crtlbl]) )  /* make sure
									    the composite label
									    contains the pure label
									 2005-08-09*/
	    ngbval[*p2]+=1;
	  else if (*p2==crtlbl){
	    *p2 &= PIX_MSB;
	    fifo4_add(q, (long int)p2);
	    fifo4_add(qall, (long int)p2);
	  }
	}
      }
    }
    maxfreq=0;
    lblmaj=0;
    for (j=1; j<maxlbl+1; j++){
      if (ngbval[j]>maxfreq){
	lblmaj=j;
	maxfreq=ngbval[j];
      }
    }
    if(lblmaj==0){
      // printf("message: lblmaj=0 in resolvelabels: choose first pure label\n");
      crtlbl=plutback[crtlbl];
      for (j=1;j<HST1D_TYPE_MAX;j=j<<1){
	if(crtlbl & j){
	  for(k=0;k<kmax;k++){
	    if(plutback[k]==j){
	      lblmaj=k;
	      break;
	    }
	  }
	}
      }
    }
    if (lblmaj==0)
       printf("SHOULD NEVER HAPPEN!!!: composite label %d plain label %d\n", crtlbl, (int)lblmaj);

    while ((p1 = (PIX_TYPE *)fifo4_remove(qall)))
      *p1=lblmaj;
  }
  uc_framebox(imlbl, box, 0x0);

  free((char*)ngbval);
  free_fifo4(q);
  free_fifo4(qall);

  return NO_ERROR;
}
#undef HST1D_TYPE_MAX
#include "uc_undef.h"


#include "us_def.h"
#define HST1D_TYPE_MAX INT32_MAX
ERROR_TYPE us_resolveLabels(IMAGE *imlbl, IMAGE *imlut, IMAGE *imlutback, int graph)
{
  UINT32 *ngbval, maxfreq;
  int flag;
  int i, j, k, kmax=GetImNx(imlutback);
  long int shft[27];
  long int npix=GetImNPix(imlbl);
  int box[6];
  PIX_TYPE *plbl=(PIX_TYPE *)GetImPtr(imlbl);
  PIX_TYPE *p1, *p2, maxlbl, lblmaj;
  HST1D_TYPE *plut=(HST1D_TYPE *)GetImPtr(imlut);
  HST1D_TYPE *plutback=(HST1D_TYPE *)GetImPtr(imlutback), crtlbl, toto;
  FIFO4 *q, *qall;
  G_TYPE *pg;

  /* take graph into account */
  if (set_seq_shift(GetImNx(imlbl), GetImNy(imlbl), GetImNz(imlbl), graph, shft) == ERROR)
    return ERROR;

  /* get min & max values */
  pg = min_max(imlbl);
  if (pg == NULL)
    return(ERROR);
  maxlbl = pg[1].us_val;
  free((char *)pg);
  ngbval=(UINT32 *)calloc(maxlbl+1,sizeof(UINT32));
  if (ngbval==NULL)
    return ERROR;

  q = create_fifo4(1024);
  if (q == NULL){
    free((char*)ngbval);
    return ERROR;
  }
  qall = create_fifo4(1024);
  if (qall == NULL){
    free((char*)ngbval);
    free_fifo4(q);
    return ERROR;
  }

  if (GetImNy(imlbl) == 1)
    {BOX_1D;}
  else if (GetImNz(imlbl) == 1)
    {BOX_2D;}
  else
    {BOX_3D;}
  /* last bit of imlbl used for flagging and borders !
     That is, maximum label value should be PIX_MAX xor PIX_MSB */
  us_framebox(imlbl, box, PIX_MSB);

  /* scan image */
  for(i=0;i<npix;i++,plbl++){
    if ( (*plbl & PIX_MSB) || (plut[*plbl]==0) || (*plbl == 0) )
      continue;
    crtlbl=*plbl;
    *plbl |= PIX_MSB;
    fifo4_add(q, (long int)plbl);
    fifo4_add(qall, (long int)plbl);
    memset(ngbval,0x0,(maxlbl+1)*sizeof(UINT32));
    while ((p1 = (PIX_TYPE *)fifo4_remove(q))){
      for (k=0; k < graph; k++){
	p2 = p1 + shft[k];
	if (*p2<PIX_MSB){
	  if ( (plut[*p2] == 0) && (plutback[*p2] & plutback[crtlbl]) )  /* make sure
									    the composite label
									    contains the pure label
									 2005-08-09*/
	    ngbval[*p2]+=1;
	  else if (*p2==crtlbl){
	    *p2 |= PIX_MSB;
	    fifo4_add(q, (long int)p2);
	    fifo4_add(qall, (long int)p2);
	  }
	}
      }
    }
    maxfreq=0;
    lblmaj=0;
    for (j=1; j<maxlbl+1; j++){
      if (ngbval[j]>maxfreq){
	lblmaj=j;
	maxfreq=ngbval[j];
      }
    }
    if(lblmaj==0){
      flag=0;
      toto=crtlbl;
      // printf("message: lblmaj=0 in resolvelabels: choose first pure label\n");
      crtlbl=plutback[crtlbl];
      if (crtlbl==0)
	  printf("GLOUP: plutback[%d]=%d\n", (int)crtlbl, (int)(plutback[crtlbl]));
      for (j=1;j<HST1D_TYPE_MAX;j=j<<1){
	if(crtlbl & j){
	  for(k=1;k<kmax;k++){
	    if(plutback[k]==j){
	      lblmaj=k;
              // printf("message: lblmaj=%d\n", lblmaj);
	      flag=1;
	      break;
	    }
	  }
	  if (flag)
	    break;
	}
      }
    }
    if (lblmaj==0)
       printf("SHOULD NEVER HAPPEN!!!: plutback[%d]= %d plain label %d\n", (int)toto, (int)crtlbl, (int)lblmaj);
    while ((p1 = (PIX_TYPE *)fifo4_remove(qall)))
      *p1=lblmaj;
  }
  us_framebox(imlbl, box, 0x0);

  free((char*)ngbval);
  free_fifo4(q);
  free_fifo4(qall);

  return NO_ERROR;
}
#undef HST1D_TYPE_MAX
#include "us_undef.h"



ERROR_TYPE resolveLabels(IMAGE *imlbl, IMAGE *imlut, IMAGE *imlutback, int graph)
{

  /* used primarly for image composition */
  switch (GetImDataType(imlbl)){

  case t_UCHAR:
    return(uc_resolveLabels(imlbl, imlut, imlutback, graph));
    break;

  case t_USHORT:
    return(us_resolveLabels(imlbl, imlut, imlutback, graph));
    break;

  default:
    (void)sprintf(buf,"resolveLabels(): invalid pixel type\n"); errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}


#include "uc_def.h" /* GLOUP: refers to g image instead of lbl */
#define LOCAL_LBL_TYPE USHORT
ERROR_TYPE us_gorder(IMAGE *imlbl, IMAGE *g, int n)
{
  PIX_TYPE *pgim;
  LOCAL_LBL_TYPE *plbl, maxlbl;
  long int i, npix=GetImNPix(g);
  double *doo;  /* doo[i]=number of images available for region with label i */
  size_t *indx, *irank;
  G_TYPE *pg;

  /* get min & max values */
  pg = min_max(imlbl);
  if (pg == NULL)
    return(ERROR);
  maxlbl = pg[1].us_val;
  free((char *)pg);
  doo=(double *)calloc(maxlbl+1,sizeof(double));
  indx=(size_t *)calloc(maxlbl+1,sizeof(size_t));
  irank=(size_t *)calloc(maxlbl+1,sizeof(size_t));

  pgim=(PIX_TYPE *)GetImPtr(g);
  plbl=(LOCAL_LBL_TYPE *)GetImPtr(imlbl);
  for(i=0;i<npix;i++,pgim++,plbl++)
    doo[*plbl]=*pgim;

  if (n!=0){
    for (i=1;i<=n; i++)
      doo[i<<n]=1;
  }

  for (i=0;i<maxlbl+1;i++)
    indx[i]=i;

  indexx(maxlbl+1, doo, indx);
  for (i=0;i<maxlbl+1;i++)
    irank[indx[i]]=i;

  plbl=(LOCAL_LBL_TYPE *)GetImPtr(imlbl);
  for(i=0;i<npix;i++,plbl++)
    *plbl=irank[*plbl];

  free(doo);
  free(indx);
  free(irank);
  return NO_ERROR;
}
#undef LOCAL_LBL_TYPE
#include "uc_undef.h"


#include "uc_def.h" /* GLOUP: refers to g image instead of lbl */
#define LOCAL_LBL_TYPE UINT32
ERROR_TYPE u32_gorder(IMAGE *imlbl, IMAGE *g, int n)
{
  PIX_TYPE *pgim;
  LOCAL_LBL_TYPE *plbl, maxlbl;
  long int i, npix=GetImNPix(g);
  double *doo;  /* doo[i]=number of images available for region with label i */
  size_t *indx, *irank;
  G_TYPE *pg;

  /* get min & max values */
  pg = min_max(imlbl);
  if (pg == NULL)
    return(ERROR);
  maxlbl = pg[1].u32_val;
  free((char *)pg);
  doo=(double *)calloc(maxlbl+1,sizeof(double));
  indx=(size_t *)calloc(maxlbl+1,sizeof(size_t));
  irank=(size_t *)calloc(maxlbl+1,sizeof(size_t));

  pgim=(PIX_TYPE *)GetImPtr(g);
  plbl=(LOCAL_LBL_TYPE *)GetImPtr(imlbl);
  for(i=0;i<npix;i++,pgim++,plbl++)
    doo[*plbl]=*pgim;

  if (n!=0){
    for (i=1;i<=n; i++)
      doo[i<<n]=1;
  }

  for (i=0;i<maxlbl+1;i++)
    indx[i]=i;

  indexx(maxlbl+1, doo, indx);
  for (i=0;i<maxlbl+1;i++)
    irank[indx[i]]=i;

  plbl=(LOCAL_LBL_TYPE *)GetImPtr(imlbl);
  for(i=0;i<npix;i++,plbl++)
    *plbl=irank[*plbl];

  free(doo);
  free(indx);
  free(irank);
  return NO_ERROR;
}
#undef LOCAL_LBL_TYPE
#include "uc_undef.h"


ERROR_TYPE gorder(IMAGE *lbl, IMAGE *g, int n)
{
  /* used primarly for image composition */
  if (szgeocompat(g, lbl) != NO_ERROR){
    (void)sprintf(buf,"gorder(): input images must have the same size\n"); errputstr(buf);
    return ERROR;
  }

  if (GetImDataType(g) != t_UCHAR){
    (void)sprintf(buf,"gorder(): invalid pixel type for g image\n");
    errputstr(buf);
    return(ERROR);
  }

  switch (GetImDataType(lbl)){
  case t_USHORT:
    return(us_gorder(lbl, g, n));
    break;
  case t_UINT32:
    return(u32_gorder(lbl, g, n));
    break;
  default:
    (void)sprintf(buf,"gorder(): invalid pixel type for lbl image\n");
    errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}

/**@}*/
