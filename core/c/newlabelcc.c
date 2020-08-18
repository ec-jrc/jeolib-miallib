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
#define CC_LBL_TYPE   UINT32
#define t_CC_LBL_TYPE t_UINT32
#define PIXCC_MSB     0x80000000
#define INACTIVE_BIT  0x40000000
#define PIXCC_LTSB    0xC0000000
#define PIXCC_LBLB    0x3FFFFFFF
IMAGE *uc_labelccattr(IMAGE *im, int graph, int rg, int rl)
{
  /* First 2011-09-21: for any attribute  even non increasing like var and correct ... */
  PIX_TYPE *p, val, mincc, maxcc;
  int exiso, flag;
  IMAGE *imlbl;
  CC_LBL_TYPE *plbl, lbl=2;
  unsigned long int npix, i, refofs, ofs, ofsk;
  int prio;
  long int  k, *shft;
  int nx, ny, nz;

  FIFO4 *q;
  int box[BOXELEM];
  
  PQDATUM apqd[1];
  struct node *pqd;
  struct pqueue *pq;
  
  nx = GetImNx(im);
  ny = GetImNy(im);
  nz = GetImNz(im);
  npix=GetImNPix(im);

  imlbl= (IMAGE *)create_image(t_CC_LBL_TYPE, nx, ny, nz);
  if (imlbl == NULL){
    (void)sprintf(buf,"_labelccdissim(): not enough memory!\n"); errputstr(buf);
    return NULL;
  }
  
  pq = pqinit(NULL, GetImNPix(imlbl)/100L);  /* priority queue */
  if (pq == NULL){
    free_image(imlbl);
    return NULL;
  }

  if (graph!=4){ // only 4-connectivity for now
    free_image(imlbl);
    free_pq(pq);
    return NULL;
  }
  shft = (long int *)calloc(graph, sizeof(long int));
  if (shft == NULL){
    free_image(imlbl);
    free_pq(pq);
    return NULL;
  }
  shft[0]=-nx;
  shft[1]=-1;
  shft[2]=1;
  shft[3]=nx;

  q = create_fifo4(500); 
  if (q == NULL){
    free_image(imlbl);
    free_pq(pq);
    free((char*)shft);
    return NULL;
  }

  /*  Take SE  into account  */
  BOX_2D;
   
  u32_blank(imlbl,1);
  if (u32_framebox(imlbl,box,0)==ERROR){
    free_image(imlbl);
    free_pq(pq);
    free((char*)shft);
    free_fifo4(q);
    return NULL;
  }
  
  /* scan the image while labelling 0-CCs and
   insert first pixel with priority equal to external isolation */
  p   = (PIX_TYPE *)GetImPtr(im);
  plbl= (CC_LBL_TYPE *)GetImPtr(imlbl);
  for (i=nx; i<npix; i++){
    if (plbl[i]==1){
      plbl[i]=lbl;
      val=p[i];
      fifo4_add(q, (long int)i);
      exiso=PIX_MAX+1;
      while ( (ofs=fifo4_remove(q)) ){
	for (k=0; k<graph; k++){
          ofsk=ofs+shft[k];
	  if ( (plbl[ofsk]!=lbl) && (plbl[ofsk]!=0) ){
	    if (p[ofsk]==val){
	      plbl[ofsk]=lbl;
	      fifo4_add(q, (long int)ofsk);
	    }
	    else if (abs((int)p[ofs]-(int)p[ofsk])<exiso)
	      exiso=abs((int)p[ofs]-(int)p[ofsk]);
	  }
	}
      }
      if ( exiso<=rl ){
	pqd = (PQDATUM )malloc(sizeof(struct node));
	pqd->prio = exiso;
	pqd->val= lbl;
	pqd->offset= (long int)i;
	pqmininsert(pq, pqd);
      }
      lbl++;
    }
  }	   

  //u32_dumpxyz(imlbl, 0, 0, 0, 20 , 20);

  while (pqpeek(pq, apqd) != NULL){
    pqminremove(pq, apqd);
    refofs=apqd[0]->offset;
    prio=apqd[0]->prio;
    lbl=apqd[0]->val;
    free((char*) *apqd);

    if(plbl[refofs]==lbl){ /* still active: compute prio-CC of refofs */
      plbl[refofs]|=PIXCC_MSB;
      fifo4_add(q, (long int)refofs);
      mincc=maxcc=p[refofs];
      flag=0;
      while ( (ofs=fifo4_remove(q)) ){
	for (k=0; k<graph; k++){
          ofsk=ofs+shft[k];
	  if ((plbl[ofsk]!=0) && !(plbl[ofsk]&PIXCC_MSB)){ /* not on border and not yet visited */
	    if ( abs((int)p[ofsk]-(int)p[ofs]) <= prio ){ /* in prio-CC */
	      if (!flag){
		if (plbl[ofsk]&INACTIVE_BIT){ /* will violate constraints */
		  flag=1;
		}
		if (mincc>p[ofsk]){
		  mincc=p[ofsk];
		  if ((maxcc-mincc)>rg){
		    flag=1;
		  }
		}
		else if (maxcc<p[ofsk]){
		  maxcc=p[ofsk];
		  if ((maxcc-mincc)>rg){
		    flag=1;
		  }
		}
	      }
	      plbl[ofsk]|=PIXCC_MSB;
	      fifo4_add(q, (long int)ofsk);
	    }
	  }
	}
      }
      
      if (flag==1){ /* reset visited prio-CC to inactive */
	plbl[refofs]^=PIXCC_LTSB;
        fifo4_add(q, (long int)refofs);
        while ( (ofs=fifo4_remove(q)) ){
	  for (k=0; k<graph; k++){
	    ofsk=ofs+shft[k];
	    if (plbl[ofsk]&PIXCC_MSB){
	      plbl[ofsk]^=PIXCC_LTSB;
	      fifo4_add(q, (long int)ofsk);
	    }
	  }
	}
      }
      else { /* cut level not reached: repropagate */
        plbl[refofs]^=PIXCC_MSB;
	fifo4_add(q, (long int)refofs);
        exiso=PIX_MAX+1;
	while ( (ofs=fifo4_remove(q)) ){
	  for (k=0; k<graph; k++){
	    ofsk=ofs+shft[k];
	    if (plbl[ofsk] & PIXCC_MSB){
	      plbl[ofsk]=lbl;
	      fifo4_add(q, (long int)ofsk);
	    }
	    else if ( (plbl[ofsk]!=lbl) && plbl[ofsk] ) {
	      if ( abs((int)p[ofs]-(int)p[ofsk]) < exiso )
		exiso=abs((int)p[ofs]-(int)p[ofsk]);
	    }
	  }
	}
	if ( exiso<=rl ){
	  pqd = (PQDATUM )malloc(sizeof(struct node));
	  pqd->prio=exiso;
	  pqd->val=lbl;
	  pqd->offset=(long int)refofs;
	  pqmininsert(pq, pqd);
	}
      }
    }
  }
#ifdef OPENMP
#pragma omp parallel for
#endif 
  for (i=0; i<npix; i++)
    plbl[i]&=PIXCC_LBLB;
  free_pq(pq);
  free((char*)shft);
  free_fifo4(q);
  return imlbl;
}
#undef CC_LBL_TYPE
#undef t_CC_LBL_TYPE
#undef PIXCC_MSB
#undef INACTIVE_BIT
#undef PIXCC_LTSB 
#undef PIXCC_LBLB
#include "uc_undef.h"

IMAGE *labelccattr(IMAGE *im, int graph, int rg, int rl)
{
  switch (GetImDataType(im)){

  case t_UCHAR:
    return(uc_labelccattr(im,graph,rg,rl));
    break;
    
  default:
    (void)sprintf(buf,"labelccattr(): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}

/*@}*/
