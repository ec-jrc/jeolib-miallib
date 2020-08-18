/***********************************************************************
Author(s): Pierre Soille
Copyright (C) 2010-2020 European Union (Joint Research Centre)

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

/** @file
 *  Dissimilarity based alpha-omega connected components \cite soille2011ismm
 *  [Version suitable for grey level images]
 *  @author Pierre Soille
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "miallib.h"
#include "pqueue.h"
#include "fifo.h"
#include "op.h"


/** \addtogroup group_label
 *  @{
 */

#include "uc_def.h"
#define CC_LBL_TYPE UINT32
#define t_CC_LBL_TYPE t_UINT32
#define BORDER_VAL        0x80000000
#define FIRST_LBL         0x00000200 /* 512 */
#define LBL_BIT           0x7FFFFE00
#define BORDER_OR_LBL_BIT 0x8FFFFE00
#define R_BIT             0x000000FF
IMAGE *uc_labelccdissim(IMAGE *im, IMAGE *imh, IMAGE *imv, int rg, int rl)
{
  /* First 2010-10-18: for transition regions: the ultimate solution?  First brina this morning  */
  PIX_TYPE *p, *prmax, *plut, *ph, *pv;
  IMAGE *imlbl, *imrmax, *imlut;
  CC_LBL_TYPE *plbl, lbl=FIRST_LBL;
  unsigned long int npix, i, j, ofs, ofsq, ofsk;
  int rk, rcrt=0, rlcrt, prio, mincc, maxcc, reset=0;
  long int  k, *shft;
  int n=4, nx=GetImNx(im), ny, nz;
  int doffset[4]={-nx,-1,0,0};
  PIX_TYPE *pdir[4];
  
  FIFO4 *q;
  int box[BOXELEM];
  
  PQDATUM apqd[1];
  struct node *pqd;
  struct pqueue *pq;
  
  ny = GetImNy(im);
  nz = GetImNz(im);
  npix=GetImNPix(im);

  imlbl= (IMAGE *)create_image(t_CC_LBL_TYPE, nx, ny, nz);
  if (imlbl == NULL){
    (void)sprintf(buf,"_labelccdissim(): not enough memory!\n"); errputstr(buf);
    return NULL;
  }
  imrmax= (IMAGE *)create_image(t_PIX_TYPE, nx, ny, nz);
  if (imrmax == NULL){
    (void)sprintf(buf,"_labelccdissim(): not enough memory!\n"); errputstr(buf);
    free_image(imlbl);
    return NULL;
  }
  imlut= (IMAGE *)create_image(t_PIX_TYPE, nx, ny, nz);
  if (imlut == NULL){
    (void)sprintf(buf,"_labelccdissim(): not enough memory!\n"); errputstr(buf);
    free_image(imlbl);
    free_image(imrmax);
    return NULL;
  }
  plut   = (PIX_TYPE *)GetImPtr(imlut);
  generic_blank(imrmax, PIX_MAX);
  prmax   = (PIX_TYPE *)GetImPtr(imrmax);
  pq = pqinit(NULL, 10000);  /* priority queue */
  if (pq == NULL){
    free_image(imlbl);
    free_image(imrmax);
    return NULL;
  }
  q = create_fifo4(500); 
  if (q == NULL){
    free_image(imlbl);
    free_image(imrmax);
    free_pq(pq);
    return NULL;
  }

  if (n!=4){ // only 4-connectivity for now
    free_image(imlbl);
    free_image(imrmax);
    free_pq(pq);
    return NULL;
  }
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL){
    free_image(imlbl);
    free_image(imrmax);
    free_pq(pq);
    free_fifo4(q);
    return NULL;
  }
  shft[0]=-nx;
  shft[1]=-1;
  shft[2]=1;
  shft[3]=nx;

  /*  Take SE  into account  */
  BOX_2D;
   
  if (u32_framebox(imlbl,box,BORDER_VAL)==ERROR){
    free_image(imlbl);
    free_pq(pq);
    free_fifo4(q);
    free_image(imrmax);
    free((char*)shft);
    return NULL;
  }
  i32_arithcst(imlbl,R_BIT+1,OR_op);

  /* Here we go */
  p   = (PIX_TYPE *)GetImPtr(im);
  ph  = (PIX_TYPE *)GetImPtr(imh);
  pv  = (PIX_TYPE *)GetImPtr(imv);
  pdir[0]=pv;
  pdir[1]=ph;
  pdir[2]=ph;
  pdir[3]=pv;
  plbl= (CC_LBL_TYPE *)GetImPtr(imlbl);
  for (i=0;i<npix;i++){
    //printf("coucou0 ");
    if (plbl[i]<FIRST_LBL){ /* not yet labelled */
      rlcrt=rl;
      mincc=maxcc=p[i];
      plbl[i]=lbl;
      if (rlcrt>=prmax[i]) // SPEED-UP
	rlcrt=prmax[i]-1;
      /* init queue */
      for (k=0; k<n; k++){
	ofsk=i+shft[k];
	if (plbl[ofsk]&BORDER_OR_LBL_BIT){
	  if (plbl[ofsk]&BORDER_VAL){
	    continue;
	  }
	  if (rlcrt>= *(pdir[k]+i+doffset[k])) // abs(mincc-p[ofsk]))
	      rlcrt= *(pdir[k]+i+doffset[k])-1; //abs(mincc-p[ofsk])-1;
	  continue;
	}
	rk= *(pdir[k]+i+doffset[k]); //abs(mincc-p[ofsk]);
	//if (rlcrt>=prmax[ofsk]) // SPEED-UP
	//rlcrt=prmax[ofsk]-1;
	if (rk>rlcrt)
	  continue;
	plbl[ofsk]=rk;
	pqd = (PQDATUM )malloc(sizeof(struct node));
	pqd->prio = rk;
	pqd->offset= (long int)ofsk;
	pqmininsert(pq, pqd);	
      }
      /* here we go */
      if( pqpeek(pq, apqd) != NULL)
	rcrt=apqd[0]->prio;      
      while (pqpeek(pq, apqd) != NULL){
	pqminremove(pq, apqd);
	ofs=apqd[0]->offset;
	prio=apqd[0]->prio;
        free((char*) *apqd);
	if (plbl[ofs]&LBL_BIT)
	    continue;

	if (prio>rcrt){ /* nth 'layer' done */
	  while ( (ofsq=fifo4_remove(q)) != 0){
	    plbl[ofsq]=lbl;
	    //prmax[ofsq]=rcrt; // SPEED-UP
	  }
	  rcrt=prio;
	  if (plbl[ofs]&LBL_BIT)
	    continue;
	}

	fifo4_add(q, (long int)ofs);

        if (prmax[ofs]<=rcrt){ // SPEED-UP
	  rlcrt=rcrt-1;
	  reset=1;
	  break; //leave while loop
	}

	if (p[ofs]<mincc)
	  mincc=p[ofs];
	if (p[ofs]>maxcc)
	  maxcc=p[ofs];

	if ( (rg<maxcc-mincc) || (rcrt>rlcrt) ){ /* reset */
	  rlcrt=rcrt-1;
	  reset=1;
	  break; //leave while loop
	}
	for (k=0; k<n; k++){
	  ofsk=ofs+shft[k];
	  if (plbl[ofsk]&BORDER_OR_LBL_BIT){
	    if (plbl[ofsk]&BORDER_VAL){
	      continue;
	    }
	    if (plbl[ofsk]!=lbl){
	      if (rlcrt>=  *(pdir[k]+ofs+doffset[k])){// abs(p[ofs]-p[ofsk])){
		rlcrt= *(pdir[k]+ofs+doffset[k])-1; //abs(p[ofs]-p[ofsk])-1;
		if (rcrt>rlcrt){
		  reset=1;
		  break;
		}
	      }
	    }
	    continue;
	  }
	  rk=*(pdir[k]+ofs+doffset[k]); //(p[ofs]-p[ofsk]);
	  if (rk>rlcrt)
	    continue;
	  if (rk<plbl[ofsk]){
            plbl[ofsk]=rk;
	    pqd = (PQDATUM )malloc(sizeof(struct node));
	    pqd->prio = rk;
	    pqd->offset= (long int)ofsk;
	    pqmininsert(pq, pqd);
	  }
	}
	if (reset)
	  break;
      }
      if (reset){
	  for (j=1; j<pq->size; j++){
	    if (pq->d[j] != NULL){
	      ofsq=pq->d[j]->offset;
	      if(!(plbl[ofsq]&LBL_BIT)){
	        // prmax[ofsq]=plbl[ofsq];// SPEED-UP
		plbl[ofsq]=R_BIT+1;
	      }
	      free((char*) (pq->d[j]) );
	    }
	  }
	  pq->size=1;

	  while ( (ofsq=fifo4_remove(q)) != 0){
	    if (plbl[ofsq]&LBL_BIT){
	      printf("SHOULD NEVER HAPPEN!\n");
	      continue;
	    }
	    prmax[ofsq]=rcrt;// SPEED-UP
	    plbl[ofsq]=R_BIT+1;  //=rcrt; // =R_BIT+1;  // rcrt;
	  }
	  reset=0;
	}  // end reset
	while ( (ofsq=fifo4_remove(q)) != 0){
	  plbl[ofsq]=lbl;
	}

	plut[lbl-511]=rlcrt;
      lbl++;
    }
  }
  free_image(imrmax);
  free_image(imlut);
  free_pq(pq);
  free_fifo4(q);
  free((char*)shft);
  for (i=0;i<npix;i++) /* set first label to 1 */
    plbl[i]-=511;
  u32_framebox(imlbl,box,0);
  //printf("labelccfast zu Ende new speeded up 3");
  return(imlbl);
}
#undef CC_LBL_TYPE
#undef t_CC_LBL_TYPE
#undef BORDER_VAL
#include "uc_undef.h"



#include "uc_def.h"
#define CC_LBL_TYPE UINT32
#define t_CC_LBL_TYPE t_UINT32
#define BORDER_VAL        0x80000000
#define FIRST_LBL         0x00000200 /* 512 */
#define LBL_BIT           0x7FFFFE00
#define BORDER_OR_LBL_BIT 0x8FFFFE00
#define R_BIT             0x000000FF
IMAGE *us_labelccdissim(IMAGE *im, IMAGE *imh, IMAGE *imv, int rg, int rl)
{
  /* First 2010-10-18: for transition regions: the ultimate solution?  First brina this morning  */
  PIX_TYPE *p, *prmax, *plut, *ph, *pv;
  IMAGE *imlbl, *imrmax, *imlut;
  CC_LBL_TYPE *plbl, lbl=FIRST_LBL;
  unsigned long int npix, i, j, ofs, ofsq, ofsk;
  int rk, rcrt=0, rlcrt, prio, mincc, maxcc, reset=0;
  long int  k, *shft;
  int n=4, nx=GetImNx(im), ny, nz;
  int doffset[4]={-nx,-1,0,0};
  PIX_TYPE *pdir[4];
  
  FIFO4 *q;
  int box[BOXELEM];
  
  PQDATUM apqd[1];
  struct node *pqd;
  struct pqueue *pq;
  
  ny = GetImNy(im);
  nz = GetImNz(im);
  npix=GetImNPix(im);

  imlbl= (IMAGE *)create_image(t_CC_LBL_TYPE, nx, ny, nz);
  if (imlbl == NULL){
    (void)sprintf(buf,"_labelccdissim(): not enough memory!\n"); errputstr(buf);
    return NULL;
  }
  imrmax= (IMAGE *)create_image(t_PIX_TYPE, nx, ny, nz);
  if (imrmax == NULL){
    (void)sprintf(buf,"_labelccdissim(): not enough memory!\n"); errputstr(buf);
    free_image(imlbl);
    return NULL;
  }
  imlut= (IMAGE *)create_image(t_PIX_TYPE, nx, ny, nz);
  if (imlut == NULL){
    (void)sprintf(buf,"_labelccdissim(): not enough memory!\n"); errputstr(buf);
    free_image(imlbl);
    free_image(imrmax);
    return NULL;
  }
  plut   = (PIX_TYPE *)GetImPtr(imlut);
  generic_blank(imrmax, PIX_MAX);
  prmax   = (PIX_TYPE *)GetImPtr(imrmax);
  pq = pqinit(NULL, 10000);  /* priority queue */
  if (pq == NULL){
    free_image(imlbl);
    free_image(imrmax);
    return NULL;
  }
  q = create_fifo4(500); 
  if (q == NULL){
    free_image(imlbl);
    free_image(imrmax);
    free_pq(pq);
    return NULL;
  }

  if (n!=4){ // only 4-connectivity for now
    free_image(imlbl);
    free_image(imrmax);
    free_pq(pq);
    return NULL;
  }
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL){
    free_image(imlbl);
    free_image(imrmax);
    free_pq(pq);
    free_fifo4(q);
    return NULL;
  }
  shft[0]=-nx;
  shft[1]=-1;
  shft[2]=1;
  shft[3]=nx;

  /*  Take SE  into account  */
  BOX_2D;
   
  if (u32_framebox(imlbl,box,BORDER_VAL)==ERROR){
    free_image(imlbl);
    free_pq(pq);
    free_fifo4(q);
    free_image(imrmax);
    free((char*)shft);
    return NULL;
  }
  i32_arithcst(imlbl,R_BIT+1,OR_op);

  /* Here we go */
  p   = (PIX_TYPE *)GetImPtr(im);
  ph  = (PIX_TYPE *)GetImPtr(imh);
  pv  = (PIX_TYPE *)GetImPtr(imv);
  pdir[0]=pv;
  pdir[1]=ph;
  pdir[2]=ph;
  pdir[3]=pv;
  plbl= (CC_LBL_TYPE *)GetImPtr(imlbl);
  for (i=0;i<npix;i++){
    //printf("coucou0 ");
    if (plbl[i]<FIRST_LBL){ /* not yet labelled */
      rlcrt=rl;
      mincc=maxcc=p[i];
      plbl[i]=lbl;
      if (rlcrt>=prmax[i]) // SPEED-UP
	rlcrt=prmax[i]-1;
      /* init queue */
      for (k=0; k<n; k++){
	ofsk=i+shft[k];
	if (plbl[ofsk]&BORDER_OR_LBL_BIT){
	  if (plbl[ofsk]&BORDER_VAL){
	    continue;
	  }
	  if (rlcrt>= *(pdir[k]+i+doffset[k])) // abs(mincc-p[ofsk]))
	      rlcrt= *(pdir[k]+i+doffset[k])-1; //abs(mincc-p[ofsk])-1;
	  continue;
	}
	rk= *(pdir[k]+i+doffset[k]); //abs(mincc-p[ofsk]);
	//if (rlcrt>=prmax[ofsk]) // SPEED-UP
	//rlcrt=prmax[ofsk]-1;
	if (rk>rlcrt)
	  continue;
	plbl[ofsk]=rk;
	pqd = (PQDATUM )malloc(sizeof(struct node));
	pqd->prio = rk;
	pqd->offset= (long int)ofsk;
	pqmininsert(pq, pqd);	
      }
      /* here we go */
      if( pqpeek(pq, apqd) != NULL)
	rcrt=apqd[0]->prio;      
      while (pqpeek(pq, apqd) != NULL){
	pqminremove(pq, apqd);
	ofs=apqd[0]->offset;
	prio=apqd[0]->prio;
        free((char*) *apqd);
	if (plbl[ofs]&LBL_BIT)
	    continue;

	if (prio>rcrt){ /* nth 'layer' done */
	  while ( (ofsq=fifo4_remove(q)) != 0){
	    plbl[ofsq]=lbl;
	    //prmax[ofsq]=rcrt; // SPEED-UP
	  }
	  rcrt=prio;
	  if (plbl[ofs]&LBL_BIT)
	    continue;
	}

	fifo4_add(q, (long int)ofs);

        if (prmax[ofs]<=rcrt){ // SPEED-UP
	  rlcrt=rcrt-1;
	  reset=1;
	  break; //leave while loop
	}

	if (p[ofs]<mincc)
	  mincc=p[ofs];
	if (p[ofs]>maxcc)
	  maxcc=p[ofs];

	if ( (rg<maxcc-mincc) || (rcrt>rlcrt) ){ /* reset */
	  rlcrt=rcrt-1;
	  reset=1;
	  break; //leave while loop
	}
	for (k=0; k<n; k++){
	  ofsk=ofs+shft[k];
	  if (plbl[ofsk]&BORDER_OR_LBL_BIT){
	    if (plbl[ofsk]&BORDER_VAL){
	      continue;
	    }
	    if (plbl[ofsk]!=lbl){
	      if (rlcrt>=  *(pdir[k]+ofs+doffset[k])){// abs(p[ofs]-p[ofsk])){
		rlcrt= *(pdir[k]+ofs+doffset[k])-1; //abs(p[ofs]-p[ofsk])-1;
		if (rcrt>rlcrt){
		  reset=1;
		  break;
		}
	      }
	    }
	    continue;
	  }
	  rk=*(pdir[k]+ofs+doffset[k]); //(p[ofs]-p[ofsk]);
	  if (rk>rlcrt)
	    continue;
	  if (rk<plbl[ofsk]){
            plbl[ofsk]=rk;
	    pqd = (PQDATUM )malloc(sizeof(struct node));
	    pqd->prio = rk;
	    pqd->offset= (long int)ofsk;
	    pqmininsert(pq, pqd);
	  }
	}
	if (reset)
	  break;
      }
      if (reset){
	  for (j=1; j<pq->size; j++){
	    if (pq->d[j] != NULL){
	      ofsq=pq->d[j]->offset;
	      if(!(plbl[ofsq]&LBL_BIT)){
	        // prmax[ofsq]=plbl[ofsq];// SPEED-UP
		plbl[ofsq]=R_BIT+1;
	      }
	      free((char*) (pq->d[j]) );
	    }
	  }
	  pq->size=1;

	  while ( (ofsq=fifo4_remove(q)) != 0){
	    if (plbl[ofsq]&LBL_BIT){
	      printf("SHOULD NEVER HAPPEN!\n");
	      continue;
	    }
	    prmax[ofsq]=rcrt;// SPEED-UP
	    plbl[ofsq]=R_BIT+1;  //=rcrt; // =R_BIT+1;  // rcrt;
	  }
	  reset=0;
	}  // end reset
	while ( (ofsq=fifo4_remove(q)) != 0){
	  plbl[ofsq]=lbl;
	}

	plut[lbl-511]=rlcrt;
      lbl++;
    }
  }
  free_image(imrmax);
  free_image(imlut);
  free_pq(pq);
  free_fifo4(q);
  free((char*)shft);
  for (i=0;i<npix;i++) /* set first label to 1 */
    plbl[i]-=511;
  u32_framebox(imlbl,box,0);
  //printf("labelccfast zu Ende new speeded up 3");
  return(imlbl);
}
#undef CC_LBL_TYPE
#undef t_CC_LBL_TYPE
#undef BORDER_VAL
#include "uc_undef.h"



IMAGE *labelccdissim(IMAGE *im, IMAGE *imh, IMAGE *imv, int rg, int rl)
{
  switch (GetImDataType(im)){

  case t_UCHAR:
    return(uc_labelccdissim(im,imh,imv,rg,rl));
    break;
    
  case t_USHORT:
    return(us_labelccdissim(im,imh,imv,rg,rl));
    break;
    
  default:
    (void)sprintf(buf,"labelccdissim(): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}

/*@}*/
